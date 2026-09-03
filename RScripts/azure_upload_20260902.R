#!/usr/bin/env Rscript
#
# Upload of the 2026-08-25..29 build outputs to Azure blob storage.
# =============================================================================
#
# WHAT THIS IS FOR
#
# The website working tree (PlaceBasedCarbonCalculator.github.io, branch `dev`)
# has been updated to reference this set of build outputs. None of them is on
# Azure yet. Until they are, the site must not be deployed - the PBCC, transport
# and retrofit maps, the LSOA report and the four area reports would all fail at
# once. This script uploads exactly the files the site now references and that
# are not already there, and nothing else.
#
# Deploy order: run this script, confirm it reports every entry uploaded or
# already present, then deploy the website.
#
# It follows RScripts/azure_upload_20260812.R, which uploaded the previous set;
# the guarantees below are the same ones.
#
#
# WHAT IT WILL AND WILL NOT DO
#
#   * It uploads only the 130 files listed in the manifest below. The manifest is
#     written out in full - every source path and every destination blob name -
#     rather than being globbed from a directory, so what runs is what you can
#     read here.
#
#   * It NEVER deletes. There is no delete call in this file. The files the site
#     references today stay in place and keep serving until the website change
#     is deployed, so this upload cannot break the live site and the old blobs
#     remain as the rollback.
#
#   * It NEVER overwrites. Before uploading anything it lists each destination
#     container and drops any manifest entry whose destination blob already
#     exists, then re-checks that individual blob immediately before writing it.
#     A skipped entry is reported with both sizes so you can see whether the
#     existing blob is the same file.
#
#     Some destinations here are expected to collide, on purpose: a handful of
#     datasets have not been rebuilt since their last upload, so their current
#     build name is the name already published (the bounds_* set, prices,
#     voa_2020, the *_community_pics and *_population sets, and three of the
#     bulk zips). They are listed so the run confirms every file the site
#     references is present, and they will report as SKIP-EXISTS. A collision on
#     any *other* entry means something has changed since 2026-09-02 and is
#     worth understanding before you go further.
#
#   * It checks every .pmtiles source really is a PMTiles archive before
#     uploading it. buildings_pbcc_20260811.pmtiles was once published as an
#     MBTiles (SQLite) file under a .pmtiles name, and every read of it failed
#     in the browser with "Wrong magic number for PMTiles archive" - a fault
#     that only showed up once the site pointed at it. Seven bytes of preflight
#     make that impossible to repeat.
#
#   * It is one-time. On success it writes a receipt CSV next to itself and
#     refuses to run again while that file exists. Delete the receipt to re-run;
#     the no-overwrite rule still applies, so a re-run can only fill in whatever
#     failed the first time.
#
#   * It is dry-run by default. Pass --go, or set PBCC_UPLOAD_GO <- TRUE before
#     sourcing it, to actually transfer. See USAGE below.
#
#   * It never calls quit() in an interactive session. An earlier version did,
#     which closed the whole RStudio session as soon as the dry run finished.
#
#
# USAGE
#
# From a shell:
#
#   Rscript RScripts/azure_upload_20260902.R          # dry run, uploads nothing
#   Rscript RScripts/azure_upload_20260902.R --go     # perform the upload
#
# From RStudio or any interactive session, where there is no command line to
# pass flags on, set the same options as variables before sourcing:
#
#   source("RScripts/azure_upload_20260902.R")        # dry run, uploads nothing
#
#   PBCC_UPLOAD_GO <- TRUE
#   source("RScripts/azure_upload_20260902.R")        # perform the upload
#
# Either way the run is a DRY RUN unless you ask for the transfer: it prints the
# full plan, says what it would upload and what it would skip, and stops. That
# is working correctly, not failing - the plan is the thing to read before
# committing to 12.6 GB. Set the flag and run it again to transfer.
#
#   Options            command line     interactive variable / env var
#     perform upload     --go             PBCC_UPLOAD_GO      <- TRUE
#     skip local MD5     --no-md5         PBCC_UPLOAD_NO_MD5  <- TRUE
#     restrict the run   --only=NAME      PBCC_UPLOAD_ONLY    <- "NAME"
#
#   --only=NAME restricts the run either to one destination container
#   (pbcc-pmtiles | pbcc-jsonbin | pbcc-data) or to a single destination blob
#   name from the manifest. Useful for proving the credential on one small file
#   before starting 8.7 GB of tiles - index_oac_emissions_2026-08-29.json.gz is
#   449 bytes and does the job. A scoped run writes its own receipt and does not
#   mark the whole job done.
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
# WHAT IS BEING UPLOADED - 130 files, 12.6 GB
#
#   pbcc-pmtiles   14 tilesets,        8.7 GB
#   pbcc-jsonbin  109 capBin files,    1.4 GB (54 index/data pairs + 1 lone .bin)
#   pbcc-data       7 bulk zips,       2.5 GB, under the bulk/ prefix
#
# This publishes the CURRENT BUILD NAME of every dataset the website uses, not
# only the ones whose contents changed. After the run the container holds one
# complete, consistently dated 2026-08 snapshot, so a later reader does not have
# to work out which of several date stamps is the live one for each dataset.
#
# Roughly 4 GB of that is data whose bytes did not change. Each candidate was
# compared against its live counterpart before this manifest was written:
# Content-MD5 from a HEAD request against the local file's digest for bins and
# zips, and for tilesets a PMTiles header comparison of the root directory, leaf
# directories and tile data region (a whole-file compare is useless there, since
# the metadata block carries a build timestamp and always differs). Those
# comparisons are recorded here so the cost is a deliberate choice rather than
# an accident:
#
#   Genuinely new data
#     zones/buildings_pbcc, zones/buildings_transport, zones/buildings_retrofit
#     epc_dom, historical_emission, lsoa_overview, oac_emissions
#     <level>_emissions and <level>_epc_dom, for all four of
#       la / ward / parish / constituency
#     bulk/pbcc_lsoa, bulk/epc_dom_summary, bulk/epc_domestic
#
#   Byte-identical to the live blob, uploaded anyway under the new date
#     access, community_pics, historical_domestic_gas_elec, population,
#     pt_frequency, vehicle_summary, voa_2010
#     <level>_access, <level>_gas_electric, <level>_prices,
#       <level>_pt_frequency, <level>_vehicle_summary
#     postcodes.pmtiles + data_postcode.bin (see the lockstep note below)
#     bulk/pt_frequency - same single CSV, same CRC, as the published
#       pt_frequency_20260810.zip; only the file name inside the zip differs
#
#   Not rebuilt since their last upload, so the current build name IS the live
#   name; listed for completeness and expected to report SKIP-EXISTS
#     bounds_lsoa / bounds_ward / bounds_parish / bounds_constituency / bounds_la
#     prices, voa_2020, <level>_community_pics, <level>_population
#     bulk/household_clusters, bulk/access_proximity, bulk/epc_nondomestic
#
# THE POSTCODE PAIR
#
#   data_postcode_2026-08-25.bin and postcodes_20260825.pmtiles carry byte
#   offsets into each other. Both are in the manifest and must both land: if one
#   uploads and the other does not, postcode lookups read the wrong bytes and
#   show another postcode's data without erroring. If the run is interrupted,
#   check both are present before deploying.
#
#   They are safe to publish as a pair because they are a matched re-stamp of
#   what is already live: the new pmtiles is identical to postcodes_20260809 -
#   across its root directory, leaf directories and entire tile data section - so
#   it carries the same baked bin_offset/bin_clen values, and the new .bin has
#   the same MD5 as the live data_postcode_2026-08-08.bin those offsets point
#   into. The offsets therefore still agree.
#
#   NOTE for whoever runs the next postcode build: outputdata/postcodes.geojson
#   was rebuilt on 2026-08-28 but postcodes.pmtiles has not been retiled since
#   2026-07-13, so a genuinely new postcode tileset is still pending. When it
#   lands, upload the new pmtiles and the new .bin together and bump both names
#   in retrofit/datasets.js in the same commit.
#
# STILL NOT HERE
#
#   index_postcode_2026-08-25.json.gz  15.7 MB. The postcode dataset is
#       registered in retrofit/datasets.js as bin-only
#       ({ bin: 'data_postcode_2026-08-25.bin' }) because its ~1.5M-record index
#       would be a huge download; the byte ranges come from the pmtiles instead,
#       so the index is never fetched by anything.
#   buildings_heights_20260825.zip  3.9 GB, built but not linked from
#       data/index.html (the Building Heights card has no download button).
#   the *_high / *_medium / *_low / *_verylow tilesets, which are the zoom bands
#       that get tile-joined into the archives listed below.
#   uprn_points_20260811.pmtiles  built and tiled, but nothing on the site
#       consumes it.
#   GBsolar.pmtiles, oszoom_names.pmtiles, buildings_v2.pmtiles, the landuse
#       layers, DTM/DSM  live and undated, and not produced by this build tree.
#
# =============================================================================

suppressPackageStartupMessages(library(AzureStor))

# Options come from the command line under Rscript. In an interactive session
# (RStudio, source()) there is no command line to read - commandArgs() there
# belongs to RStudio, not to this script - so the same options are taken from
# variables you set first, or from the environment. See USAGE above.
INTERACTIVE <- interactive()
args <- if (INTERACTIVE) character(0) else commandArgs(trailingOnly = TRUE)

# TRUE for TRUE / 1 / "yes" / "y" in any of the three places, FALSE otherwise.
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

# Ending the run. Under Rscript that means exiting with a status. In an
# interactive session it must NOT mean quit(), which closes the whole R session -
# the first version of this script did that, and sourcing it in RStudio ended the
# session as soon as the dry run finished. Here we just stop evaluating the file.
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

# A scoped run (--only=) gets its own receipt, so that finishing one container
# or one file does not block the full run. Only an unscoped clean run writes the
# receipt that marks the whole job done.
RECEIPT <- file.path(script_dir, if (is.na(ONLY))
  "azure_upload_20260902_receipt.csv"
else
  sprintf("azure_upload_20260902_receipt_%s.csv", gsub("[^A-Za-z0-9]+", "_", ONLY)))

# --- roots -------------------------------------------------------------------

OUT <- "F:/GitHub/PlaceBasedCarbonCalculator/build/outputdata"
JB  <- file.path(OUT, "jsonbin")


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
# The six tool tilesets were confirmed to be genuinely new tile data, not a
# re-stamp: each differs from the currently published tileset in its PMTiles
# header (tile data length, addressed tile count) as well as its bytes.
#
#   zones/buildings_pbcc      2026-08-21 -> 2026-08-29
#   zones/buildings_transport 2026-08-21 -> 2026-08-28
#   zones/buildings_retrofit  2026-08-08 -> 2026-08-28/29
#
# postcodes moves 2026-08-09 -> 2026-08-25 as a re-stamp, in lockstep with
# data_postcode_2026-08-25.bin below - see THE POSTCODE PAIR in the header.
#
# The last seven have not been retiled since their last upload, so these are the
# names already live and they will report SKIP-EXISTS.

tiles <- rbind(
  e("pbcc-pmtiles", "zones_pbcc_20260829.pmtiles",           file.path(OUT, "pbcc/zones_pbcc_20260829.pmtiles")),
  e("pbcc-pmtiles", "buildings_pbcc_20260829.pmtiles",       file.path(OUT, "pbcc/buildings_pbcc_20260829.pmtiles")),
  e("pbcc-pmtiles", "zones_transport_20260828.pmtiles",      file.path(OUT, "transport/zones_transport_20260828.pmtiles")),
  e("pbcc-pmtiles", "buildings_transport_20260828.pmtiles",  file.path(OUT, "transport/buildings_transport_20260828.pmtiles")),
  e("pbcc-pmtiles", "zones_retrofit_20260828.pmtiles",       file.path(OUT, "retrofit/zones_retrofit_20260828.pmtiles")),
  e("pbcc-pmtiles", "buildings_retrofit_20260829.pmtiles",   file.path(OUT, "retrofit/buildings_retrofit_20260829.pmtiles")),

  e("pbcc-pmtiles", "postcodes_20260825.pmtiles",            file.path(OUT, "postcodes_20260825.pmtiles")),

  e("pbcc-pmtiles", "epc_dom_20260728.pmtiles",              file.path(OUT, "epc_dom_20260728.pmtiles")),
  e("pbcc-pmtiles", "epc_nondom_20260728.pmtiles",           file.path(OUT, "epc_nondom_20260728.pmtiles")),
  e("pbcc-pmtiles", "uprn_unknown_20260728.pmtiles",         file.path(OUT, "uprn_unknown_20260728.pmtiles")),
  e("pbcc-pmtiles", "wards_20260728.pmtiles",                file.path(OUT, "wards_20260728.pmtiles")),
  e("pbcc-pmtiles", "la_20260728.pmtiles",                   file.path(OUT, "la_20260728.pmtiles")),
  e("pbcc-pmtiles", "parish_20260728.pmtiles",               file.path(OUT, "parish_20260728.pmtiles")),
  e("pbcc-pmtiles", "westminster_20260728.pmtiles",          file.path(OUT, "westminster_20260728.pmtiles"))
)

# --- 2. capBin datasets -> pbcc-jsonbin --------------------------------------
# Each dataset is an index (.json.gz) plus the binary it names in meta$bin_file.
# Both halves must land or the dataset is unreadable, so they are listed as
# index names only and expanded into pairs below.

# One entry per dataset, at its current build name. A "*" marks the ones whose
# binary actually differs from what is published; the rest are re-stamps or, for
# the ones already at this date, files that will simply be confirmed present.

pairs <- c(
  # --- LSOA-level, read by the map tools and reports/lsoa.html ---------------
  "index_epc_dom_2026-08-25.json.gz",                    # *
  "index_historical_emission_2026-08-28.json.gz",        # *
  "index_lsoa_overview_2026-08-29.json.gz",              # *
  "index_oac_emissions_2026-08-29.json.gz",              # *
  "index_access_2026-08-25.json.gz",
  "index_community_pics_2026-08-26.json.gz",
  "index_historical_domestic_gas_elec_2026-08-26.json.gz",
  "index_population_2026-08-26.json.gz",
  "index_pt_frequency_2026-08-25.json.gz",
  "index_vehicle_summary_2026-08-26.json.gz",
  # Council tax registers. voa_2010 (bands) is GB-wide; voa_2020
  # (type/bedrooms/age) is England and Wales only and legitimately has no record
  # for a Scottish zone - see the dwelling stock section of retrofit/ui.js.
  "index_voa_2010_2026-08-25.json.gz",
  "index_voa_2020_2026-07-13.json.gz",                   # already at this date
  "index_prices_2026-07-25.json.gz",                     # already at this date

  # --- Area-level aggregates, read by reports/la-report.js ------------------
  # la_emissions also carries the 'GB' national comparison row used by every
  # report card at every level. The four <level>_emissions bins are what let
  # ward, parish and constituency reports stop showing LA figures, now that
  # la-report.js reads the level's own bin rather than the retired
  # pbcc-data/la_emissions/v2/ folder.
  "index_la_emissions_2026-08-29.json.gz",               # *
  "index_ward_emissions_2026-08-29.json.gz",             # *
  "index_parish_emissions_2026-08-29.json.gz",           # *
  "index_constituency_emissions_2026-08-29.json.gz",     # *
  "index_la_epc_dom_2026-08-26.json.gz",                 # *
  "index_ward_epc_dom_2026-08-26.json.gz",               # *
  "index_parish_epc_dom_2026-08-26.json.gz",             # *
  "index_constituency_epc_dom_2026-08-26.json.gz",       # *
  "index_la_access_2026-08-26.json.gz",
  "index_ward_access_2026-08-26.json.gz",
  "index_parish_access_2026-08-26.json.gz",
  "index_constituency_access_2026-08-26.json.gz",
  "index_la_gas_electric_2026-08-26.json.gz",
  "index_ward_gas_electric_2026-08-26.json.gz",
  "index_parish_gas_electric_2026-08-26.json.gz",
  "index_constituency_gas_electric_2026-08-26.json.gz",
  "index_la_prices_2026-08-26.json.gz",
  "index_ward_prices_2026-08-26.json.gz",
  "index_parish_prices_2026-08-26.json.gz",
  "index_constituency_prices_2026-08-26.json.gz",
  "index_la_pt_frequency_2026-08-26.json.gz",
  "index_ward_pt_frequency_2026-08-26.json.gz",
  "index_parish_pt_frequency_2026-08-26.json.gz",
  "index_constituency_pt_frequency_2026-08-26.json.gz",
  "index_la_vehicle_summary_2026-08-26.json.gz",
  "index_ward_vehicle_summary_2026-08-26.json.gz",
  "index_parish_vehicle_summary_2026-08-26.json.gz",
  "index_constituency_vehicle_summary_2026-08-26.json.gz",
  # These eight are already at their current build date
  "index_la_community_pics_2026-07-24.json.gz",
  "index_ward_community_pics_2026-07-24.json.gz",
  "index_parish_community_pics_2026-07-24.json.gz",
  "index_constituency_community_pics_2026-07-24.json.gz",
  "index_la_population_2026-07-24.json.gz",
  "index_ward_population_2026-07-24.json.gz",
  "index_parish_population_2026-07-24.json.gz",
  "index_constituency_population_2026-07-24.json.gz",

  # --- Boundary geometry, read by reports/area-map.js -----------------------
  # Already at their current build date
  "index_bounds_lsoa_2026-07-14.json.gz",
  "index_bounds_la_2026-07-14.json.gz",
  "index_bounds_ward_2026-07-14.json.gz",
  "index_bounds_parish_2026-07-14.json.gz",
  "index_bounds_constituency_2026-07-14.json.gz"
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

# The postcode dataset is the exception: registered bin-only in
# retrofit/datasets.js, so its index is never fetched and is not uploaded.
# Lockstep partner of postcodes_20260825.pmtiles above - both must land.
jsonbin <- rbind(jsonbin,
  e("pbcc-jsonbin", "data_postcode_2026-08-25.bin", file.path(JB, "data_postcode_2026-08-25.bin")))

# --- 3. Bulk downloads -> pbcc-data/bulk -------------------------------------
# All seven downloads advertised on data/index.html, at their current build
# name. The first three changed; pt_frequency is a re-stamp (same single CSV,
# same CRC, only the name inside the zip differs); the last three are already at
# their current build date and will report SKIP-EXISTS.
#
# epc_domestic_20260829.zip replaces epc_dom_20251120.zip and is also a format
# change: a GeoPackage rather than a GeoJSON, matching the non-domestic release.
# The old blob is left in place, so any existing link to it keeps working.

bulk <- rbind(
  e("pbcc-data", "bulk/pbcc_lsoa_20260828.zip",         file.path(OUT, "bulk/pbcc_lsoa_20260828.zip")),
  e("pbcc-data", "bulk/epc_dom_summary_20260825.zip",   file.path(OUT, "bulk/epc_dom_summary_20260825.zip")),
  e("pbcc-data", "bulk/epc_domestic_20260829.zip",      file.path(OUT, "bulk/epc_domestic_20260829.zip")),
  e("pbcc-data", "bulk/pt_frequency_20260825.zip",      file.path(OUT, "bulk/pt_frequency_20260825.zip")),
  e("pbcc-data", "bulk/household_clusters_20260414.zip", file.path(OUT, "bulk/household_clusters_20260414.zip")),
  e("pbcc-data", "bulk/access_proximity_20260414.zip",  file.path(OUT, "bulk/access_proximity_20260414.zip")),
  e("pbcc-data", "bulk/epc_nondomestic_20260725.zip",   file.path(OUT, "bulk/epc_nondomestic_20260725.zip"))
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

# Every .pmtiles source must actually be a PMTiles archive. A v3 archive starts
# with the seven ASCII bytes "PMTiles"; an MBTiles file starts "SQLite format 3"
# and is the mistake this guards against (see the header).
pm <- manifest[grepl("\\.pmtiles$", manifest$dest), ]
if (nrow(pm) > 0) {
  wrong <- character(0)
  for (i in seq_len(nrow(pm))) {
    con <- file(pm$src[i], open = "rb")
    magic <- rawToChar(readBin(con, "raw", 7))
    close(con)
    if (!identical(magic, "PMTiles"))
      wrong <- c(wrong, sprintf("  %s  starts \"%s\"", pm$dest[i], magic))
  }
  if (length(wrong) > 0) {
    message("\nNot PMTiles archives:")
    for (w in wrong) message(w)
    stop("Refusing to publish a non-PMTiles file under a .pmtiles name. ",
         "Nothing uploaded.")
  }
  message("All ", nrow(pm), " tilesets carry the PMTiles magic number")
}

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
# container also holds a per-postcode JSON tree, and listing all of it would be
# pointless and slow.
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
  message("\nDRY RUN - nothing was uploaded. This is what the script does by ",
          "default; it never transfers anything until you ask it to.")
  if (INTERACTIVE) {
    message("\nTo perform the upload from this session, set the flag and source ",
            "this file again:")
    message('    PBCC_UPLOAD_GO <- TRUE')
    message('    source("RScripts/azure_upload_20260902.R")')
    message("\nTo prove the credential on one small file first, set this too ",
            "(it uploads a 449-byte index and writes its own receipt):")
    message('    PBCC_UPLOAD_ONLY <- "index_oac_emissions_2026-08-29.json.gz"')
  } else {
    message("\nRe-run with --go to transfer:")
    message("    Rscript RScripts/azure_upload_20260902.R --go")
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
  bail(1)
}
