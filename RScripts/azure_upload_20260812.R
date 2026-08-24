#!/usr/bin/env Rscript
#
# One-time upload of the 2026-08 build outputs to Azure blob storage.
# =============================================================================
#
# WHAT THIS IS FOR
#
# The website working tree (PlaceBasedCarbonCalculator.github.io, branch
# `audit`) now references the current build outputs. None of those files is on
# Azure yet. Until they are, the site must not be deployed - every tool would
# 404 at once. This script uploads exactly the files the site references and
# nothing else.
#
# Deploy order: run this script, confirm it reports every entry uploaded or
# already present, then deploy the website.
#
#
# WHAT IT WILL AND WILL NOT DO
#
#   * It uploads only the 131 files listed in the manifest below. The manifest
#     is written out in full - every source path and every destination blob
#     name - rather than being globbed from a directory, so what runs is what
#     you can read here.
#
#   * It NEVER deletes. There is no delete call in this file.
#
#   * It NEVER overwrites. Before uploading anything it lists each destination
#     container and drops any manifest entry whose destination blob already
#     exists, then re-checks that individual blob immediately before writing
#     it. A skipped entry is reported with both sizes so you can see whether
#     the existing blob is the same file.
#
#     Nothing here is expected to collide: every destination except
#     GBsolar.pmtiles is a new date-stamped name, and GBsolar.pmtiles is a new
#     layer. A reported collision means something has changed since
#     2026-08-12 and is worth understanding before you go further.
#
#   * It is one-time. On success it writes a receipt CSV next to itself and
#     refuses to run again while that file exists. Delete the receipt to
#     re-run; the no-overwrite rule still applies, so a re-run can only fill
#     in whatever failed the first time.
#
#   * It is dry-run by default. Pass --go to actually transfer.
#
#
# USAGE
#
#   Rscript RScripts/azure_upload_20260812.R          # dry run, uploads nothing
#   Rscript RScripts/azure_upload_20260812.R --go     # perform the upload
#
#   Options:
#     --go          perform the upload (default is a dry run)
#     --no-md5      skip the local MD5 pass (faster; loses integrity checking)
#     --only=NAME   restrict the run, either to one destination container
#                   (pbcc-pmtiles | pbcc-jsonbin | pbcc-data) or to a single
#                   destination blob name from the manifest. Useful for
#                   proving the credential on one small file before starting
#                   19 GB of tiles. A scoped run writes its own receipt and
#                   does not mark the whole job done.
#
#   Credentials come from the environment and are never written to disk:
#
#     PBCC_STORAGE_ACCOUNT   storage account name        (default "pbcc")
#     PBCC_STORAGE_SAS       a SAS token with create+write on the three
#                            containers, OR
#     PBCC_STORAGE_KEY       the account key
#
#   A SAS limited to create/write (no delete) is the safer of the two: it makes
#   the "never deletes" guarantee true at the service, not just in this script.
#
#
# WHAT IS BEING UPLOADED - 131 files, 22.5 GB
#
#   pbcc-pmtiles   17 tilesets, 19.3 GB
#   pbcc-jsonbin  107 capBin files, 1.4 GB (53 index/data pairs + 1 lone .bin)
#   pbcc-data      7 bulk download zips, 1.8 GB, under the bulk/ prefix
#
# Of that, epc_dom_20251120.zip (1.2 GB) and three of the jsonbin datasets are
# unchanged and should already be present, so the actual transfer will be
# smaller - the plan printed at the start of the run gives the real figure.
#
# The manifest was derived from the website working tree - every .pmtiles,
# index_*.json.gz and bulk zip name it references - and each index's binary was
# read from that index's own meta$bin_file rather than assumed from the naming
# convention. All 53 agreed with the convention; all 53 binaries are present.
#
# Deliberately NOT uploaded:
#
#   index_postcode_2026-08-08.json.gz  the postcode dataset is registered in
#       databin.js as bin-only ({ bin: 'data_postcode_2026-08-08.bin' }); its
#       offsets come from postcodes_*.pmtiles, so the index is never fetched.
#   uprn_points_20260811.pmtiles  built and tiled, but nothing on the site
#       consumes it. Add it below if the viewer at
#       LandOwnership/docs/uprn_pmtiles.html is to go live.
#   buildings_heights_20260808.zip  3.9 GB, built but not linked from
#       data/index.html.
#   the *_high / *_medium / *_low / *_verylow tilesets, which are the zoom
#       bands that get tile-joined into the archives listed below.
#
#
# TWO THINGS TO KNOW BEFORE RUNNING
#
# 1. data_postcode_2026-08-08.bin and postcodes_20260809.pmtiles carry byte
#    offsets into each other. Both are in the manifest. If one uploads and the
#    other does not, postcode lookups read the wrong bytes and show another
#    postcode's data without erroring. If the run is interrupted, check both
#    landed before deploying.
#
# 2. Content-Type is set by AzureStor from the file extension: .json.gz becomes
#    "application/gzip", where the blobs already live carry the older alias
#    "application/x-gzip", and .zip becomes "application/zip" against a live
#    "application/x-zip-compressed". Both are cosmetic - the browser decides
#    whether to decompress from Content-Encoding, which is not set here and is
#    not set on the existing blobs either, so capBin keeps decompressing the
#    .json.gz itself as it does today. Checked against the live headers on
#    2026-08-12.
#
# =============================================================================

suppressPackageStartupMessages(library(AzureStor))

args     <- commandArgs(trailingOnly = TRUE)
DO_IT    <- "--go" %in% args
PUT_MD5  <- !("--no-md5" %in% args)
only_arg <- grep("^--only=", args, value = TRUE)
ONLY     <- if (length(only_arg)) sub("^--only=", "", only_arg[1]) else NA_character_

bad <- setdiff(args, c("--go", "--no-md5", only_arg))
if (length(bad)) stop("Unknown argument(s): ", paste(bad, collapse = " "))

script_dir <- tryCatch({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(f)) dirname(normalizePath(sub("^--file=", "", f[1]))) else getwd()
}, error = function(e) getwd())

# A scoped run (--only=) gets its own receipt, so that finishing one container
# or one file does not block the full run. Only an unscoped clean run writes
# the receipt that marks the whole job done.
RECEIPT <- file.path(script_dir, if (is.na(ONLY))
  "azure_upload_20260812_receipt.csv"
else
  sprintf("azure_upload_20260812_receipt_%s.csv", gsub("[^A-Za-z0-9]+", "_", ONLY)))

# --- roots -------------------------------------------------------------------
# Absolute, because the three sources live in different repos and one is not in
# a repo at all.

OUT   <- "F:/GitHub/PlaceBasedCarbonCalculator/build/outputdata"
LAND  <- "F:/GitHub/PlaceBasedCarbonCalculator/LandOwnership/output"
SOLAR <- "F:/DTM_DSM/large_rasters/Solar"


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

# --- 1. Map tiles -> pbcc-pmtiles --------------------------------------------
# Rebuilt this week. The transport tiles carry 2023/2024/2025 for all five
# modes; the PBCC tiles carry 2022 grades rather than 2019.

tiles <- rbind(
  e("pbcc-pmtiles", "zones_transport_20260811.pmtiles",     file.path(OUT, "transport/zones_transport_20260811.pmtiles")),
  e("pbcc-pmtiles", "buildings_transport_20260811.pmtiles", file.path(OUT, "transport/buildings_transport_20260811.pmtiles")),
  e("pbcc-pmtiles", "zones_pbcc_20260811.pmtiles",          file.path(OUT, "pbcc/zones_pbcc_20260811.pmtiles")),
  e("pbcc-pmtiles", "buildings_pbcc_20260811.pmtiles",      file.path(OUT, "pbcc/buildings_pbcc_20260811.pmtiles")),
  e("pbcc-pmtiles", "zones_retrofit_20260808.pmtiles",      file.path(OUT, "retrofit/zones_retrofit_20260808.pmtiles")),
  e("pbcc-pmtiles", "buildings_retrofit_20260808.pmtiles",  file.path(OUT, "retrofit/buildings_retrofit_20260808.pmtiles")),
  e("pbcc-pmtiles", "epc_dom_20260728.pmtiles",             file.path(OUT, "epc_dom_20260728.pmtiles")),
  e("pbcc-pmtiles", "epc_nondom_20260728.pmtiles",          file.path(OUT, "epc_nondom_20260728.pmtiles")),
  e("pbcc-pmtiles", "uprn_unknown_20260728.pmtiles",        file.path(OUT, "uprn_unknown_20260728.pmtiles")),
  e("pbcc-pmtiles", "postcodes_20260809.pmtiles",           file.path(OUT, "postcodes_20260809.pmtiles")),
  e("pbcc-pmtiles", "wards_20260728.pmtiles",               file.path(OUT, "wards_20260728.pmtiles")),
  e("pbcc-pmtiles", "la_20260728.pmtiles",                  file.path(OUT, "la_20260728.pmtiles")),
  e("pbcc-pmtiles", "parish_20260728.pmtiles",              file.path(OUT, "parish_20260728.pmtiles")),
  e("pbcc-pmtiles", "westminster_20260728.pmtiles",         file.path(OUT, "westminster_20260728.pmtiles")),

  # New raster layer for the retrofit tool. Undated on purpose: the layer has
  # no rebuild cadence yet and retrofit/datasets.js references it by this bare
  # name. If it is ever re-rendered, date-stamp it and bump the site at the
  # same time.
  e("pbcc-pmtiles", "GBsolar.pmtiles",                      file.path(SOLAR, "GBsolar.pmtiles")),

  # Rebuilt by the LandOwnership pipeline. These replace undated 2022
  # snapshots (inspire.pmtiles, landowners.pmtiles) which stay in place and
  # keep serving until the website change is deployed - so this upload cannot
  # break the live site, and the old files remain as the rollback.
  e("pbcc-pmtiles", "inspire_20260811.pmtiles",             file.path(LAND, "inspire_pmtiles/inspire_20260811.pmtiles")),
  e("pbcc-pmtiles", "landowners_20260811.pmtiles",          file.path(LAND, "landowner_pmtiles/landowners_20260811.pmtiles"))
)

# --- 2. capBin datasets -> pbcc-jsonbin --------------------------------------
# Each dataset is an index (.json.gz) plus the binary it names in meta$bin_file.
# Both halves must land or the dataset is unreadable, so they are listed as
# pairs and expanded together.

JB <- file.path(OUT, "jsonbin")

pairs <- c(
  # rebuilt 2026-08-08 / 08-10 / 08-11
  "index_access_2026-08-08.json.gz",
  "index_community_pics_2026-08-08.json.gz",
  "index_epc_dom_2026-08-08.json.gz",
  "index_historical_domestic_gas_elec_2026-08-08.json.gz",
  "index_historical_emission_2026-08-08.json.gz",
  "index_population_2026-08-08.json.gz",
  "index_pt_frequency_2026-08-10.json.gz",
  "index_vehicle_summary_2026-08-08.json.gz",
  # council tax bands. voa_2010 was rebuilt 2026-08-11 and is now GB-wide
  # (43,064 records: 33,755 England, 7,392 Scotland, 1,917 Wales); voa_2020 is
  # unchanged and remains England and Wales only.
  "index_voa_2010_2026-08-11.json.gz",
  "index_voa_2020_2026-07-13.json.gz",
  # local authority
  "index_la_access_2026-08-08.json.gz",
  "index_la_epc_dom_2026-08-08.json.gz",
  "index_la_gas_electric_2026-08-08.json.gz",
  "index_la_prices_2026-08-08.json.gz",
  "index_la_pt_frequency_2026-08-10.json.gz",
  "index_la_vehicle_summary_2026-08-08.json.gz",
  "index_la_community_pics_2026-07-24.json.gz",
  "index_la_population_2026-07-24.json.gz",
  # ward
  "index_ward_access_2026-08-08.json.gz",
  "index_ward_epc_dom_2026-08-08.json.gz",
  "index_ward_gas_electric_2026-08-08.json.gz",
  "index_ward_prices_2026-08-08.json.gz",
  "index_ward_pt_frequency_2026-08-10.json.gz",
  "index_ward_vehicle_summary_2026-08-08.json.gz",
  "index_ward_community_pics_2026-07-24.json.gz",
  "index_ward_population_2026-07-24.json.gz",
  "index_ward_emissions_2026-07-15.json.gz",
  # parish
  "index_parish_access_2026-08-08.json.gz",
  "index_parish_epc_dom_2026-08-08.json.gz",
  "index_parish_gas_electric_2026-08-08.json.gz",
  "index_parish_prices_2026-08-08.json.gz",
  "index_parish_pt_frequency_2026-08-10.json.gz",
  "index_parish_vehicle_summary_2026-08-08.json.gz",
  "index_parish_community_pics_2026-07-24.json.gz",
  "index_parish_population_2026-07-24.json.gz",
  "index_parish_emissions_2026-07-13.json.gz",
  # westminster constituency
  "index_constituency_access_2026-08-08.json.gz",
  "index_constituency_epc_dom_2026-08-08.json.gz",
  "index_constituency_gas_electric_2026-08-08.json.gz",
  "index_constituency_prices_2026-08-08.json.gz",
  "index_constituency_pt_frequency_2026-08-10.json.gz",
  "index_constituency_vehicle_summary_2026-08-08.json.gz",
  "index_constituency_community_pics_2026-07-24.json.gz",
  "index_constituency_population_2026-07-24.json.gz",
  "index_constituency_emissions_2026-07-15.json.gz",
  # boundary geometry
  "index_bounds_lsoa_2026-07-14.json.gz",
  "index_bounds_la_2026-07-14.json.gz",
  "index_bounds_ward_2026-07-14.json.gz",
  "index_bounds_parish_2026-07-14.json.gz",
  "index_bounds_constituency_2026-07-14.json.gz",
  # unchanged since their last upload, listed so the run is complete and
  # self-checking; each will report as already present and be skipped
  "index_lsoa_overview_2026-07-15.json.gz",
  "index_oac_emissions_2026-07-15.json.gz",
  "index_prices_2026-07-25.json.gz"
)

# Read the binary name out of each index rather than deriving it, so a
# hand-edited or regenerated index cannot silently orphan its data file.
bin_of <- function(idx) {
  con <- gzcon(file(file.path(JB, idx), open = "rb"))
  on.exit(close(con))
  meta <- jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = ""))$meta
  if (is.null(meta$bin_file) || !nzchar(meta$bin_file))
    stop(idx, " has no meta$bin_file")
  meta$bin_file
}

jsonbin <- do.call(rbind, lapply(pairs, function(idx) {
  b <- bin_of(idx)
  rbind(e("pbcc-jsonbin", idx, file.path(JB, idx)),
        e("pbcc-jsonbin", b,   file.path(JB, b)))
}))

# The postcode dataset is the exception: registered bin-only, no index.
# Lockstep partner of postcodes_20260809.pmtiles above.
jsonbin <- rbind(jsonbin,
  e("pbcc-jsonbin", "data_postcode_2026-08-08.bin", file.path(JB, "data_postcode_2026-08-08.bin")))

# --- 3. Bulk downloads -> pbcc-data/bulk -------------------------------------
# The seven files linked from data/index.html. epc_dom_20251120.zip is
# unchanged and should already be present; it is listed so the run confirms
# every advertised download exists.

bulk <- rbind(
  e("pbcc-data", "bulk/pbcc_lsoa_20260713.zip",        file.path(OUT, "bulk/pbcc_lsoa_20260713.zip")),
  e("pbcc-data", "bulk/access_proximity_20260414.zip", file.path(OUT, "bulk/access_proximity_20260414.zip")),
  e("pbcc-data", "bulk/epc_nondomestic_20260725.zip",  file.path(OUT, "bulk/epc_nondomestic_20260725.zip")),
  e("pbcc-data", "bulk/pt_frequency_20260810.zip",     file.path(OUT, "bulk/pt_frequency_20260810.zip")),
  e("pbcc-data", "bulk/household_clusters_20260414.zip", file.path(OUT, "bulk/household_clusters_20260414.zip")),
  e("pbcc-data", "bulk/epc_dom_summary_20260808.zip",  file.path(OUT, "bulk/epc_dom_summary_20260808.zip")),
  e("pbcc-data", "bulk/epc_dom_20251120.zip",          file.path(OUT, "bulk/epc_dom_20251120.zip"))
)

manifest <- rbind(tiles, jsonbin, bulk)

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

# Existing blobs. pbcc-data is listed under the bulk/ prefix only - the
# container also holds a per-postcode JSON tree, and listing all of it would
# be pointless and slow.
existing <- list()
for (cn in containers) {
  pre <- if (cn == "pbcc-data") "bulk/" else NULL
  blobs <- list_blobs(conts[[cn]], info = "all", prefix = pre)
  existing[[cn]] <- setNames(as.numeric(blobs$size), blobs$name)
  message("Container ", cn, if (is.null(pre)) "" else " (prefix bulk/)",
          ": ", length(blobs$name), " existing blobs")
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
  message("\nDRY RUN - nothing was uploaded. Re-run with --go to transfer.")
  quit(save = "no", status = 0)
}
if (nrow(todo) == 0) {
  message("\nNothing to upload; every destination already exists.")
  quit(save = "no", status = 0)
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

  # Verify by size. Catches a truncated or interrupted transfer; with
  # put_md5 the service has also stored a content MD5 for the blob.
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
    message("\nEvery file the site references is now on Azure. It can be deployed.")
  } else {
    message("This is a scoped run (--only=", ONLY, "), so it does NOT mark the ",
            "whole job done.\nRe-run without --only= to upload the rest. ",
            "Do not deploy the site until that has finished.")
  }
} else {
  partial <- sub("\\.csv$", "_partial.csv", RECEIPT)
  write.csv(receipt, partial, row.names = FALSE)
  message("\nPartial receipt written to ", partial)
  message("No final receipt, so the script can be re-run to retry the ",
          "failures. Files that did upload will be skipped, not overwritten.")
  message("\nDO NOT deploy the site until every entry has uploaded.")
  quit(save = "no", status = 1)
}
