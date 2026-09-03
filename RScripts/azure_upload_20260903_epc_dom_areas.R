#!/usr/bin/env Rscript
#
# Upload of the 2026-09-03 area EPC bins to Azure blob storage.
# =============================================================================
#
# WHAT THIS IS FOR
#
# Ward and parish domestic EPC counts are now aggregated straight from the
# certificate points rather than from the per-LSOA summary split by population
# weights (see epc_summarise_domestic_areas() in R/epc_summary.R). EPCs carry a
# location, so nothing has to be estimated: each certificate is counted in the
# ward and parish it actually falls in. That removes the small-area
# approximation the weights existed to soften - most visibly in the City of
# London, where one LSOA covers a dozen tiny wards and the weighted split put
# dwellings in the wrong ones.
#
# This script uploads the four area_epc_dom bins produced by that build and
# nothing else.
#
#
# WHAT IT WILL AND WILL NOT DO
#
#   * It uploads only the 8 files listed in the manifest below - four datasets,
#     each an index (.json.gz) and the binary the index names.
#
#   * It NEVER deletes and NEVER overwrites. Every destination is a new
#     date-stamped name; before uploading it lists the container and drops any
#     entry whose destination already exists, then re-checks that blob
#     immediately before writing it. A collision means something has changed
#     since this manifest was written and is worth understanding first.
#
#   * It is one-time. On success it writes a receipt CSV next to itself and
#     refuses to run again while that file exists.
#
#   * It is dry-run by default. Pass --go to actually transfer.
#
#
# USAGE
#
#   Rscript RScripts/azure_upload_20260903_epc_dom_areas.R        # dry run
#   Rscript RScripts/azure_upload_20260903_epc_dom_areas.R --go   # transfer
#
# From RStudio, set the same options as variables before sourcing:
#
#   PBCC_UPLOAD_GO <- TRUE
#   source("RScripts/azure_upload_20260903_epc_dom_areas.R")
#
#   Options            command line     interactive variable / env var
#     perform upload     --go             PBCC_UPLOAD_GO      <- TRUE
#     skip local MD5     --no-md5         PBCC_UPLOAD_NO_MD5  <- TRUE
#     restrict the run   --only=NAME      PBCC_UPLOAD_ONLY    <- "NAME"
#
#   Credentials come from the environment and are never written to disk:
#
#     PBCC_STORAGE_ACCOUNT   storage account name        (default "pbcc")
#     PBCC_STORAGE_SAS       a SAS token with create+write, OR
#     PBCC_STORAGE_KEY       the account key
#
#
# WHAT IS BEING UPLOADED - 8 files, 19.3 MB, all to pbcc-jsonbin
#
#   Genuinely new data - the reason for this release
#     ward_epc_dom, parish_epc_dom
#
#   Byte-identical to the live 2026-09-02 blob, uploaded anyway so the four
#   levels of this dataset share one date
#     la_epc_dom, constituency_epc_dom
#
#   The la and constituency figures are unchanged by design: those levels still
#   aggregate the per-LSOA summary, because a whole-LSOA assignment holds for
#   areas that large. Both were checked against the live blob's Content-MD5
#   before this manifest was written and match exactly, so re-pointing the site
#   at the 2026-09-03 names for those two is optional - the 2026-09-02 names
#   serve identical bytes.
#
#
# AFTER THE UPLOAD
#
# Nothing on the site changes until the area report dataset registry names the
# new files. The two that matter are:
#
#   ward_epc_dom     2026-09-02 -> 2026-09-03
#   parish_epc_dom   2026-09-02 -> 2026-09-03
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
  "azure_upload_20260903_epc_dom_areas_receipt.csv"
else
  sprintf("azure_upload_20260903_epc_dom_areas_receipt_%s.csv",
          gsub("[^A-Za-z0-9]+", "_", ONLY)))

JB <- "F:/GitHub/PlaceBasedCarbonCalculator/build/outputdata/jsonbin"


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

pairs <- c(
  "index_ward_epc_dom_2026-09-03.json.gz",          # * new: counted from points
  "index_parish_epc_dom_2026-09-03.json.gz",        # * new: counted from points
  "index_la_epc_dom_2026-09-03.json.gz",            #   re-stamp, MD5 matches live
  "index_constituency_epc_dom_2026-09-03.json.gz"   #   re-stamp, MD5 matches live
)

# Read the binary name out of each index rather than deriving it, so a
# regenerated index cannot silently orphan its data file.
bin_of <- function(idx) {
  con <- gzcon(file(file.path(JB, idx), open = "rb"))
  on.exit(close(con))
  meta <- jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = ""))$meta
  if (is.null(meta$bin_file) || !nzchar(meta$bin_file))
    stop(idx, " has no meta$bin_file")
  meta$bin_file
}

manifest <- do.call(rbind, lapply(pairs, function(idx) {
  b <- bin_of(idx)
  rbind(e("pbcc-jsonbin", idx, file.path(JB, idx)),
        e("pbcc-jsonbin", b,   file.path(JB, b)))
}))

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
  blobs <- list_blobs(conts[[cn]], info = "all")
  existing[[cn]] <- setNames(as.numeric(blobs$size), blobs$name)
  message("Container ", cn, ": ", length(blobs$name), " existing blobs")
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
  message("\nAlready present, will NOT be touched:")
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
    message('    source("RScripts/azure_upload_20260903_epc_dom_areas.R")')
  } else {
    message("\nRe-run with --go to transfer:")
    message("    Rscript RScripts/azure_upload_20260903_epc_dom_areas.R --go")
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

n_ok   <- sum(todo$result == "uploaded", na.rm = TRUE)
n_bad  <- nrow(todo) - n_ok

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
    message("\nThe new ward and parish EPC data is on Azure. The site serves it ",
            "once the area report dataset registry names the 2026-09-03 files.")
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
