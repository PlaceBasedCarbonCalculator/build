#!/usr/bin/env Rscript
#
# Upload of the 2026-09-06 domestic EPC rebuild to Azure blob storage.
# =============================================================================
#
# WHAT THIS IS FOR
#
# tar_make() was re-run on 2026-09-06 to pick up two rounds of fixes to
# epc_summarise_certs() in R/epc_summary.R that the published EPC data was
# built before. Both rounds are the same class of bug: a certificate whose
# free-text description matched two categories was counted in both, so the
# category counts summed past epc_total and the "_other" residual each set is
# the remainder of went negative.
#
#   7e6d1d3, 2026-08-28  "roof room(s)" was matched as a regex, so "(s)" was a
#                        group and the pattern never matched - roofd_room was
#                        always zero. Dual fuel (mineral + wood) matched
#                        mainfuel_biomass as well as its own category. A check
#                        for negative residuals was added, which is what
#                        surfaced the next round.
#
#   820ec0e, 2026-09-03  A pitched roof described as also having flat roof
#                        insulation counted as both roofd_pitched and
#                        roofd_flat (5 certificates). Certificates naming two
#                        heating systems counted twice across
#                        mainheatdesc_storageheater / _portableheater /
#                        _heatpump / _community (610 certificates).
#
# Neither round is in what the site serves today:
#
#   epc_dom (LSOA)        live blob is 2026-08-25, built before both rounds.
#   *_epc_dom (4 levels)  live blobs are 2026-09-03, built at 10:04 UTC that
#                         day; the 820ec0e edits were made at 11:47 UTC and
#                         committed at 12:59, so they missed that round.
#   epc_dom_summary.zip   live bulk download is 20260825, same as the LSOA bin.
#
# This script uploads the eleven files that rebuild produced and nothing else.
#
#
# WHAT IT WILL AND WILL NOT DO
#
#   * It uploads only the 11 files listed in the manifest below - five jsonbin
#     datasets, each an index (.json.gz) and the binary the index names, plus
#     one bulk download zip.
#
#   * It NEVER deletes and NEVER overwrites. Every destination is a new
#     date-stamped name; before uploading it lists the container and drops any
#     entry whose destination already exists, then re-checks that blob
#     immediately before writing it. The 2026-08-25 and 2026-09-03 blobs the
#     live site reads today stay in place and remain the rollback.
#
#   * It is one-time. On success it writes a receipt CSV next to itself and
#     refuses to run again while that file exists.
#
#   * It is dry-run by default. Pass --go to actually transfer.
#
#
# USAGE
#
#   Rscript RScripts/azure_upload_20260906_epc_dom.R        # dry run
#   Rscript RScripts/azure_upload_20260906_epc_dom.R --go   # transfer
#
# From RStudio, set the same options as variables before sourcing:
#
#   PBCC_UPLOAD_GO <- TRUE
#   source("RScripts/azure_upload_20260906_epc_dom.R")
#
#   Options            command line     interactive variable / env var
#     perform upload     --go             PBCC_UPLOAD_GO      <- TRUE
#     skip local MD5     --no-md5         PBCC_UPLOAD_NO_MD5  <- TRUE
#     restrict the run   --only=NAME      PBCC_UPLOAD_ONLY    <- "NAME"
#
#   --only=NAME takes either a container name (pbcc-jsonbin | pbcc-data) or one
#   destination blob name. index_la_epc_dom_2026-09-06.json.gz is 4 KB and is
#   the cheapest way to prove the credential before starting 64 MB.
#
#   Credentials come from the environment and are never written to disk:
#
#     PBCC_STORAGE_ACCOUNT   storage account name        (default "pbcc")
#     PBCC_STORAGE_SAS       a SAS token with create+write, OR
#     PBCC_STORAGE_KEY       the account key
#
#
# WHAT IS BEING UPLOADED - 11 files, 64 MB
#
#   pbcc-jsonbin, 10 files
#     epc_dom                 the per-LSOA bin, read by the retrofit tool and
#                             by the LSOA report's retrofit card
#     la_epc_dom, ward_epc_dom, parish_epc_dom, constituency_epc_dom
#                             the four area levels, read by the area reports
#
#     All five are genuinely new data: every level is aggregated from
#     certificates counted by the corrected code, so every level changes. They
#     have to be published together in any case, because reports/la-report.js
#     holds ONE date per dataset and substitutes the level into the name.
#
#   pbcc-data, 1 file, under the bulk/ prefix
#     bulk/epc_dom_summary_20260906.zip
#                             the per-LSOA summary CSV download, the same
#                             table that feeds the epc_dom bin
#
# NOT IN THIS UPLOAD
#
#   epc_dom.pmtiles           The certificate points map layer. It is built
#       from the certificates themselves (geojson_epc_dom), not from the
#       summary, and the fixes changed only how certificates are counted into
#       categories, so tar_make() did not rebuild it.
#   The retrofit map tiles    retrofit_lsoa_data WAS rebuilt on 2026-09-06,
#       but pmtiles_retrofit was not re-run, so no new tileset exists to
#       upload. zones_retrofit_20260828.pmtiles still carries the pre-fix
#       EPC-derived variables. Run pmtiles_retrofit and upload separately to
#       bring the map choropleth in line with the reports.
#   epc_domestic_20260829.zip and epc_nondomestic_20260725.zip
#       Record-level downloads, unaffected by the summary code.
#
#
# AFTER THE UPLOAD
#
# Nothing on the site changes until it names the new files. Four places:
#
#   reports/la-report.js     dates.epc_dom     2026-09-03 -> 2026-09-06
#   reports/lsoa.html        epc_dom bin       2026-08-25 -> 2026-09-06
#   retrofit/datasets.js     epc_dom bin       2026-08-25 -> 2026-09-06
#   data/index.html          bulk zip link + its JSON-LD Dataset entry
#                                              20260825  -> 20260906
#
# The old blobs are left in place, so the site keeps working either way and can
# be re-pointed whenever it suits.
#
# =============================================================================

suppressPackageStartupMessages(library(AzureStor))

INTERACTIVE <- interactive()
args <- if (INTERACTIVE) character(0) else commandArgs(trailingOnly = TRUE)

truthy <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  if (is.logical(x)) return(isTRUE(x[1]))
  toupper(as.character(x)[1]) %in% c("1", "TRUE", "YES", "Y")
}

DO_IT <- ("--go" %in% args) ||
  truthy(get0("PBCC_UPLOAD_GO", ifnotfound = NULL)) ||
  truthy(Sys.getenv("PBCC_UPLOAD_GO", ""))

PUT_MD5 <- !("--no-md5" %in% args) &&
  !truthy(get0("PBCC_UPLOAD_NO_MD5", ifnotfound = NULL)) &&
  !truthy(Sys.getenv("PBCC_UPLOAD_NO_MD5", ""))

only_arg <- grep("^--only=", args, value = TRUE)
ONLY <- if (length(only_arg)) {
  sub("^--only=", "", only_arg[1])
} else {
  o <- get0("PBCC_UPLOAD_ONLY", ifnotfound = NULL)
  if (is.null(o)) { o <- Sys.getenv("PBCC_UPLOAD_ONLY", "") }
  if (nzchar(as.character(o)[1])) as.character(o)[1] else NA_character_
}

bad <- setdiff(args, c("--go", "--no-md5", only_arg))
if (length(bad)) stop("Unknown argument(s): ", paste(bad, collapse = " "))

# Never quit() in an interactive session - that closes the whole R session.
bail <- function(status = 0) {
  if (!INTERACTIVE) { quit(save = "no", status = status) }
  restarts <- vapply(computeRestarts(), function(r) r[[1]], character(1))
  if ("abort" %in% restarts) { invokeRestart("abort") }
  stop("Stopped - see the message above.", call. = FALSE)
}

script_dir <- tryCatch({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(f)) dirname(normalizePath(sub("^--file=", "", f[1]))) else getwd()
}, error = function(e) getwd())

RECEIPT <- file.path(script_dir, if (is.na(ONLY))
  "azure_upload_20260906_epc_dom_receipt.csv"
else
  sprintf("azure_upload_20260906_epc_dom_receipt_%s.csv",
          gsub("[^A-Za-z0-9]+", "_", ONLY)))

OUT <- "F:/GitHub/PlaceBasedCarbonCalculator/build/outputdata"
JB  <- file.path(OUT, "jsonbin")

BUILD_DATE <- "2026-09-06"   # the jsonbin date stamp
BULK_DATE  <- "20260906"     # the bulk download date stamp


# =============================================================================
# THE MANIFEST
#
# container : destination container
# dest      : destination blob name, exactly as it will appear
# src       : absolute source path
# =============================================================================

e <- function(container, dest, src) {
  data.frame(container = container, dest = dest, src = src,
             stringsAsFactors = FALSE)
}

# The per-LSOA bin and the four area levels. Named explicitly rather than
# globbed from the directory, so what runs is what you can read here.
datasets <- c("epc_dom",
              "la_epc_dom", "ward_epc_dom", "parish_epc_dom",
              "constituency_epc_dom")

# Read the binary name out of each index rather than deriving it, so a
# regenerated index cannot silently orphan its data file - publishing a
# mismatched pair would point the website at byte ranges in the wrong file,
# which reads as another zone's data rather than as an error.
bin_of <- function(idx) {
  con <- gzfile(file.path(JB, idx), open = "rb")
  meta <- tryCatch(jsonlite::fromJSON(con)$meta, finally = close(con))
  if (is.null(meta$bin_file) || !nzchar(meta$bin_file))
    stop(idx, " has no meta$bin_file")
  meta$bin_file
}

manifest <- do.call(rbind, lapply(datasets, function(ds) {
  idx <- sprintf("index_%s_%s.json.gz", ds, BUILD_DATE)
  bin <- bin_of(idx)
  want <- sprintf("data_%s_%s.bin", ds, BUILD_DATE)
  if (!identical(bin, want))
    stop("Index/binary mismatch in ", idx, ": it names ", bin, ", expected ",
         want, ". Nothing uploaded.")
  rbind(e("pbcc-jsonbin", idx, file.path(JB, idx)),
        e("pbcc-jsonbin", bin, file.path(JB, bin)))
}))

# The bulk download. pbcc-data keeps these under a bulk/ prefix; the container
# also holds a per-postcode JSON tree, which is why the listing below is
# restricted to that prefix.
bulk_zip <- sprintf("epc_dom_summary_%s.zip", BULK_DATE)
manifest <- rbind(manifest,
                  e("pbcc-data", paste0("bulk/", bulk_zip),
                    file.path(OUT, "bulk", bulk_zip)))

if (!is.na(ONLY)) {
  keep <- manifest$container == ONLY | manifest$dest == ONLY
  if (!any(keep))
    stop("--only= matched nothing. Give either a container (",
         paste(unique(manifest$container), collapse = ", "),
         ") or one destination blob name from the manifest.")
  manifest <- manifest[keep, ]
  message("--only=", ONLY, ": restricted to ", nrow(manifest),
          " of the full manifest")
}

if (any(duplicated(paste(manifest$container, manifest$dest))))
  stop("Two manifest entries target the same destination blob")


# =============================================================================
# PREFLIGHT
# =============================================================================

fmt_size <- function(b) {
  if (is.na(b)) return("        -")
  u <- c("B", "KB", "MB", "GB"); i <- 1
  while (b >= 1024 && i < 4) { b <- b / 1024; i <- i + 1 }
  sprintf("%7.1f %-2s", b, u[i])
}

if (file.exists(RECEIPT))
  stop("This script has already been run - receipt at:\n  ", RECEIPT,
       "\nDelete the receipt to run again. Nothing already uploaded will be ",
       "overwritten either way.")

message("Manifest: ", nrow(manifest), " files")

manifest$size <- file.size(manifest$src)
absent <- manifest[is.na(manifest$size) | manifest$size == 0, ]
if (nrow(absent) > 0) {
  message("\nMissing or empty source files:")
  for (i in seq_len(nrow(absent))) message("  ", absent$src[i])
  stop(nrow(absent), " source file(s) missing or empty. Nothing uploaded.")
}
message("All ", nrow(manifest), " source files present, ",
        trimws(fmt_size(sum(manifest$size))), " total")

account <- Sys.getenv("PBCC_STORAGE_ACCOUNT", "pbcc")
sas     <- Sys.getenv("PBCC_STORAGE_SAS", "")
key     <- Sys.getenv("PBCC_STORAGE_KEY", "")
if (!nzchar(sas) && !nzchar(key))
  stop("Set PBCC_STORAGE_SAS (preferred) or PBCC_STORAGE_KEY in the ",
       "environment. Do not put credentials in this file.")

endp <- storage_endpoint(sprintf("https://%s.blob.core.windows.net", account),
                         key = if (nzchar(key)) key else NULL,
                         sas = if (nzchar(sas)) sas else NULL)

containers <- unique(manifest$container)
conts <- setNames(lapply(containers, function(x) storage_container(endp, x)), containers)

existing <- list()
for (cn in containers) {
  pre <- if (cn == "pbcc-data") "bulk/" else NULL
  blobs <- list_blobs(conts[[cn]], info = "all", prefix = pre)
  existing[[cn]] <- setNames(as.numeric(blobs$size), blobs$name)
  message("Container ", cn, if (is.null(pre)) "" else " (prefix bulk/)", ": ",
          length(blobs$name), " existing blobs")
}

manifest$remote_size <- mapply(function(cn, d) {
  s <- existing[[cn]][d]
  if (is.na(s)) NA_real_ else as.numeric(s)
}, manifest$container, manifest$dest)

manifest$action <- ifelse(is.na(manifest$remote_size), "UPLOAD", "SKIP-EXISTS")

message("\n", strrep("=", 100))
message("PLAN")
message(strrep("=", 100))
message(sprintf("%-12s %-52s %-10s %s", "ACTION", "DESTINATION", "SIZE", "CONTAINER"))
for (i in seq_len(nrow(manifest))) {
  message(sprintf("%-12s %-52s %-10s %s", manifest$action[i], manifest$dest[i],
                  trimws(fmt_size(manifest$size[i])), manifest$container[i]))
}

todo <- manifest[manifest$action == "UPLOAD", ]
skip <- manifest[manifest$action == "SKIP-EXISTS", ]

message("\n", strrep("-", 100))
message("to upload : ", nrow(todo), " files, ", trimws(fmt_size(sum(todo$size))))
message("skipped   : ", nrow(skip), " files already in the container")
if (nrow(skip) > 0) {
  message("\nAlready present, will NOT be touched. Nothing in this manifest ",
          "has been published before, so any entry here means something else ",
          "wrote to the container since 2026-09-06 and is worth understanding:")
  for (i in seq_len(nrow(skip))) {
    same <- isTRUE(skip$size[i] == skip$remote_size[i])
    message(sprintf("  %-52s local %s  remote %s  %s", skip$dest[i],
                    trimws(fmt_size(skip$size[i])),
                    trimws(fmt_size(skip$remote_size[i])),
                    if (same) "(same size)" else "(DIFFERENT SIZE - investigate)"))
  }
}

if (!DO_IT) {
  message("\nDRY RUN - nothing was uploaded. This is what the script does by ",
          "default; it never transfers anything until you ask it to.")
  if (INTERACTIVE) {
    message("\nTo perform the upload from this session:")
    message('    PBCC_UPLOAD_GO <- TRUE')
    message('    source("RScripts/azure_upload_20260906_epc_dom.R")')
    message("\nTo prove the credential on one small file first:")
    message('    PBCC_UPLOAD_ONLY <- "index_la_epc_dom_2026-09-06.json.gz"')
  } else {
    message("\nRe-run with --go to transfer:")
    message("    Rscript RScripts/azure_upload_20260906_epc_dom.R --go")
  }
  bail(0)
}
if (nrow(todo) == 0) {
  message("\nNothing to upload; every destination already exists.")
  bail(0)
}


# =============================================================================
# UPLOAD
# =============================================================================

message("\n", strrep("=", 100))
message("UPLOADING ", nrow(todo), " files")
message(strrep("=", 100))

todo$result <- NA_character_
todo$uploaded_size <- NA_real_

for (i in seq_len(nrow(todo))) {
  cn   <- todo$container[i]
  dest <- todo$dest[i]
  cont <- conts[[cn]]

  message(sprintf("\n[%d/%d] %s -> %s/%s  (%s)", i, nrow(todo),
                  basename(todo$src[i]), cn, dest, trimws(fmt_size(todo$size[i]))))

  # Re-check immediately before writing, in case another run or another person
  # created this blob since the listing above.
  now <- tryCatch(list_blobs(cont, info = "name", prefix = dest),
                  error = function(e) character(0))
  if (dest %in% now) {
    message("  SKIPPED - appeared in the container since the listing")
    todo$result[i] <- "skipped-appeared"
    next
  }

  ok <- tryCatch({
    storage_upload(cont, src = todo$src[i], dest = dest, put_md5 = PUT_MD5)
    TRUE
  }, error = function(err) {
    message("  FAILED: ", conditionMessage(err))
    todo$result[i] <<- paste("failed:", conditionMessage(err))
    FALSE
  })
  if (!ok) next

  # Verify by size. Catches a truncated or interrupted transfer; with put_md5
  # the service has also stored a content MD5 for the blob.
  chk <- tryCatch(list_blobs(cont, info = "all", prefix = dest),
                  error = function(e) NULL)
  got <- if (!is.null(chk) && dest %in% chk$name) as.numeric(chk$size[match(dest, chk$name)]) else NA_real_
  todo$uploaded_size[i] <- got

  if (!is.na(got) && got == todo$size[i]) {
    message("  OK - ", trimws(fmt_size(got)), " confirmed")
    todo$result[i] <- "uploaded"
  } else {
    message("  WARNING - size mismatch: local ", trimws(fmt_size(todo$size[i])),
            ", remote ", trimws(fmt_size(got)))
    todo$result[i] <- "uploaded-size-mismatch"
  }
}


# =============================================================================
# RESULT
# =============================================================================

n_ok  <- sum(todo$result == "uploaded", na.rm = TRUE)
n_bad <- nrow(todo) - n_ok

message("\n", strrep("=", 100))
message("uploaded successfully : ", n_ok, " / ", nrow(todo))
if (n_bad > 0) {
  message("needing attention     : ", n_bad)
  for (i in which(todo$result != "uploaded")) {
    message("  ", todo$dest[i], "  ->  ", todo$result[i])
  }
}
message("skipped (pre-existing): ", nrow(skip))

receipt <- rbind(
  data.frame(container = todo$container, dest = todo$dest, src = todo$src,
             local_size = todo$size, remote_size = todo$uploaded_size,
             result = todo$result, stringsAsFactors = FALSE),
  if (nrow(skip) > 0)
    data.frame(container = skip$container, dest = skip$dest, src = skip$src,
               local_size = skip$size, remote_size = skip$remote_size,
               result = "skipped-already-present", stringsAsFactors = FALSE)
)
receipt$run_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

if (n_bad == 0) {
  write.csv(receipt, RECEIPT, row.names = FALSE)
  message("\nReceipt written to ", RECEIPT)
  if (is.na(ONLY)) {
    message("This script will refuse to run again while that file exists.")
    message("\nThe corrected EPC data is on Azure. The website working tree ",
            "already names the 2026-09-06 files, so deploy the website next.")
  } else {
    message("This is a scoped run (--only=", ONLY, "), so it does NOT mark the ",
            "whole job done.")
  }
} else {
  partial <- sub("[.]csv$", "_partial.csv", RECEIPT)
  write.csv(receipt, partial, row.names = FALSE)
  message("\nPartial receipt written to ", partial)
  message("No final receipt, so the script can be re-run to retry the ",
          "failures. Files that did upload will be skipped, not overwritten.")
  bail(1)
}
