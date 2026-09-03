#!/usr/bin/env Rscript
#
# Upload of the 2026-09-02 area-weighting rebuild to Azure blob storage.
# =============================================================================
#
# WHAT THIS IS FOR
#
# The ward and parish reports used to assign each LSOA whole to whichever area
# held its population-weighted centroid. That left 5,901 of 11,344 parishes and
# 43 English and Welsh wards with no data at all, because an LSOA holds about
# 1,500 people and a rural parish often holds far fewer, so one LSOA covers
# several parishes and the centroid lands in only one of them.
#
# The build now splits each LSOA between the areas it covers, weighted by where
# its residents actually live (build/R/area_weights.R, the `area_weights`
# target). Every area dataset was rebuilt on 2026-09-02 as a result, and so was
# lsoa_overview, which now carries the same disambiguated ward and parish names
# the reports themselves use.
#
# This script follows RScripts/azure_upload_20260902.R, which uploaded the
# 2026-08 set earlier the same day; the guarantees below are the same ones.
#
# Deploy order: run this script, confirm it reports every entry uploaded, then
# deploy the website. The website working tree already references these names,
# so until they are on Azure the four area reports and the LSOA report header
# would fail.
#
#
# WHAT IT WILL AND WILL NOT DO
#
#   * It uploads only the 70 files listed in the manifest below - 35 datasets,
#     each an index (.json.gz) and the binary it names in meta$bin_file. The
#     manifest is generated from an explicit list of dataset names rather than
#     globbed from the directory, so what runs is what you can read here.
#
#   * It NEVER deletes and NEVER overwrites, exactly as the previous script:
#     it lists the container, drops any entry whose destination already exists,
#     and re-checks that blob immediately before writing it. The 2026-08 blobs
#     the live site reads today stay in place and remain the rollback.
#
#   * It is one-time. On success it writes a receipt CSV next to itself and
#     refuses to run again while that file exists.
#
#   * It is dry-run by default. Pass --go, or set PBCC_UPLOAD_GO <- TRUE before
#     sourcing it, to actually transfer.
#
#
# USAGE
#
#   Rscript RScripts/azure_upload_20260902_area_weights.R          # dry run
#   Rscript RScripts/azure_upload_20260902_area_weights.R --go     # transfer
#
# Or interactively:
#
#   PBCC_UPLOAD_GO <- TRUE
#   source("RScripts/azure_upload_20260902_area_weights.R")
#
#   Options            command line     interactive variable / env var
#     perform upload     --go             PBCC_UPLOAD_GO      <- TRUE
#     skip local MD5     --no-md5         PBCC_UPLOAD_NO_MD5  <- TRUE
#     restrict the run   --only=NAME      PBCC_UPLOAD_ONLY    <- "NAME"
#
#   --only=NAME takes either the container name (pbcc-jsonbin) or one
#   destination blob name. index_la_community_pics_2026-09-02.json.gz is 4 KB
#   and is the cheapest way to prove the credential before starting 284 MB.
#
#   Credentials come from the environment and are never written to disk:
#
#     PBCC_STORAGE_ACCOUNT   storage account name        (default "pbcc")
#     PBCC_STORAGE_SAS       a SAS token with create+write, OR
#     PBCC_STORAGE_KEY       the account key
#
#
# WHAT IS BEING UPLOADED - 70 files, 284 MB, all to pbcc-jsonbin
#
# Every one of these was compared against the blob the website reads today, by
# Content-MD5 from a HEAD request against the local file's digest. The results:
#
#   Genuinely new data - the weighting changed what these contain
#     ward_*  and parish_*, all nine datasets each: access, community_pics,
#       emissions, epc_dom, gas_electric, population, prices, pt_frequency,
#       vehicle_summary
#     lsoa_overview  (ward and parish names now match the report pages)
#     la_epc_dom, constituency_epc_dom - see the note below
#
#   Byte-identical to the live blob, uploaded anyway under the new date
#     la_* and constituency_* for access, community_pics, gas_electric,
#       population, prices, pt_frequency, vehicle_summary
#
#     Local authorities and constituencies deliberately keep the old whole-LSOA
#     assignment: they are far larger than an LSOA and already had complete
#     coverage, so their figures are unchanged and this was verified rather
#     than assumed. They are re-stamped because reports/la-report.js holds ONE
#     date per dataset and substitutes the level into the name, so all four
#     levels of a dataset must exist at the same date.
#
#   The two epc_dom exceptions
#     la_epc_dom and constituency_epc_dom differ from the live blob in
#     formatting only, not in value. Their count columns were integers and are
#     now doubles - the aggregation multiplies by a weight of exactly 1 - so
#     the JSON encodes 123 as 123.0 and each record grew by about 280 bytes.
#     Every value is identical; this was checked by re-running the aggregation
#     both ways and comparing to 1e-9.
#
# NOT IN THIS UPLOAD
#
#   la_emissions and constituency_emissions   Not rebuilt: they do not use the
#       weighted lookup, so tar_make() left them at 2026-08-29 and the website
#       still points there. Nothing to upload and nothing to bump.
#   Everything at LSOA level except lsoa_overview, all tilesets, and all bulk
#       downloads. None of them depends on area_weights and none was rebuilt.
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
  "azure_upload_20260902_area_weights_receipt.csv"
else
  sprintf("azure_upload_20260902_area_weights_receipt_%s.csv",
          gsub("[^A-Za-z0-9]+", "_", ONLY)))

OUT <- "F:/GitHub/PlaceBasedCarbonCalculator/build/outputdata"
JB  <- file.path(OUT, "jsonbin")

BUILD_DATE <- "2026-09-02"


# =============================================================================
# THE MANIFEST
#
# Named by dataset. Each becomes two blobs, the index and the binary it points
# into; both must land or the dataset is unreadable, so they are never listed
# separately.
# =============================================================================

# The eight datasets aggregated to all four levels by R/area_summaries.R and
# R/family_portraits.R. reports/la-report.js holds one date for each and
# substitutes the level, so all four levels must be published together.
per_level <- c("access", "community_pics", "epc_dom", "gas_electric",
               "population", "prices", "pt_frequency", "vehicle_summary")

datasets <- c(
  as.vector(outer(c("la", "ward", "parish", "constituency"), per_level,
                  function(lev, ds) paste0(lev, "_", ds))),
  # Per-capita emissions. Only ward and parish use the weighted lookup, so only
  # these two were rebuilt; la_emissions and constituency_emissions stay at
  # 2026-08-29 and are not touched here.
  "ward_emissions",
  "parish_emissions",
  # The LSOA context record. Rebuilt because it now shows each LSOA's ward and
  # parish under the same name the ward and parish reports use.
  "lsoa_overview"
)

e <- function(container, dest, src) {
  data.frame(container = container, dest = dest, src = src,
             stringsAsFactors = FALSE)
}

manifest <- do.call(rbind, lapply(sort(datasets), function(ds) {
  idx <- sprintf("index_%s_%s.json.gz", ds, BUILD_DATE)
  bin <- sprintf("data_%s_%s.bin", ds, BUILD_DATE)
  rbind(e("pbcc-jsonbin", idx, file.path(JB, idx)),
        e("pbcc-jsonbin", bin, file.path(JB, bin)))
}))

if (!is.na(ONLY)) {
  keep <- manifest$container == ONLY | manifest$dest == ONLY
  if (!any(keep))
    stop("--only= matched nothing. Give either the container (pbcc-jsonbin) ",
         "or one destination blob name from the manifest.")
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

message("Manifest: ", nrow(manifest), " files (", length(datasets), " datasets)")

manifest$size <- file.size(manifest$src)
absent <- manifest[is.na(manifest$size) | manifest$size == 0, ]
if (nrow(absent) > 0) {
  message("\nMissing or empty source files:")
  for (i in seq_len(nrow(absent))) message("  ", absent$src[i])
  stop(nrow(absent), " source file(s) missing or empty. Nothing uploaded.")
}
message("All ", nrow(manifest), " source files present, ",
        trimws(fmt_size(sum(manifest$size))), " total")

# Each index must name the binary that travels with it. A mismatch means the
# pair was built at different times, and publishing it would point the website
# at byte ranges in the wrong file - which reads as another area's data rather
# than as an error.
idx_rows <- manifest[grepl("\\.json\\.gz$", manifest$dest), ]
for (i in seq_len(nrow(idx_rows))) {
  con <- gzfile(idx_rows$src[i], open = "rb")
  meta <- tryCatch(jsonlite::fromJSON(con)$meta, finally = close(con))
  want <- sub("^index_", "data_", sub("\\.json\\.gz$", ".bin", idx_rows$dest[i]))
  if (!identical(meta$bin_file, want))
    stop("Index/binary mismatch in ", idx_rows$dest[i], ": it names ",
         meta$bin_file, ", expected ", want, ". Nothing uploaded.")
}
message("All ", nrow(idx_rows), " indexes name the binary they ship with")

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
message(sprintf("%-12s %-56s %s", "ACTION", "DESTINATION", "SIZE"))
for (i in seq_len(nrow(manifest))) {
  message(sprintf("%-12s %-56s %s", manifest$action[i], manifest$dest[i],
                  trimws(fmt_size(manifest$size[i]))))
}

todo <- manifest[manifest$action == "UPLOAD", ]
skip <- manifest[manifest$action == "SKIP-EXISTS", ]

message("\n", strrep("-", 100))
message("to upload : ", nrow(todo), " files, ", trimws(fmt_size(sum(todo$size))))
message("skipped   : ", nrow(skip), " files already in the container")
if (nrow(skip) > 0) {
  message("\nAlready present, will NOT be touched. Nothing in this manifest ",
          "has been published before, so any entry here means something else ",
          "wrote to the container since 2026-09-02 and is worth understanding:")
  for (i in seq_len(nrow(skip))) {
    same <- isTRUE(skip$size[i] == skip$remote_size[i])
    message(sprintf("  %-56s local %s  remote %s  %s", skip$dest[i],
                    trimws(fmt_size(skip$size[i])),
                    trimws(fmt_size(skip$remote_size[i])),
                    if (same) "(same size)" else "(DIFFERENT SIZE - investigate)"))
  }
}

if (!DO_IT) {
  message("\nDRY RUN - nothing was uploaded. This is what the script does by ",
          "default; it never transfers anything until you ask it to.")
  if (INTERACTIVE) {
    message("\nTo perform the upload from this session, set the flag and source ",
            "this file again:")
    message('    PBCC_UPLOAD_GO <- TRUE')
    message('    source("RScripts/azure_upload_20260902_area_weights.R")')
    message("\nTo prove the credential on one small file first:")
    message('    PBCC_UPLOAD_ONLY <- "index_la_community_pics_2026-09-02.json.gz"')
  } else {
    message("\nRe-run with --go to transfer:")
    message("    Rscript RScripts/azure_upload_20260902_area_weights.R --go")
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
  message("\nReceipt written: ", RECEIPT)
  message("Every file in the manifest is now in the container. Deploy the ",
          "website next.")
} else {
  message("\nNo receipt written - ", n_bad, " entries need attention. Fix ",
          "those and run again; the no-overwrite rule means a re-run can only ",
          "fill in what failed.")
}
