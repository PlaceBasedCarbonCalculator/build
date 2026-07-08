# Export simplified GeoJSON boundaries for the website's report pages to show
# a small locator map of the area being reported. Two variants:
#  - export_boundary_geojsons: one file per area,
#    outputdata/json/bounds_{level}/{AREACODE}.geojson
#  - export_boundary_bin: single range-requestable binary + index,
#    outputdata/jsonbin/data_bounds_{level}_<date>.bin
# Both are currently built (boundary_jsons_* and boundary_bin_* targets) until
# the website switches to the binary format.

#' Write one simplified GeoJSON per feature of a boundary layer
#'
#' @description Simplifies the boundaries (in British National Grid metres,
#'   preserving topology per feature), reprojects to WGS84, and writes one
#'   small GeoJSON file per area named `<code>.geojson`. Used by the
#'   `boundary_jsons_*` targets; consumed by the website report pages
#'   (reports/area-map.js).
#' @param bounds sf polygon layer of boundaries (e.g. the `bounds_wards` target).
#' @param idcol Name of the column holding the area code (e.g. "WD25CD").
#' @param path Output directory, created if missing.
#' @param tolerance Simplification tolerance in metres (default 40 m, which
#'   keeps a typical file to a few KB while remaining visually faithful at
#'   locator-map scale).
#' @return The output path, invisibly.
#' @keywords internal
export_boundary_geojsons <- function(bounds, idcol, path, tolerance = 40) {

  b <- bounds[, idcol]

  # Simplify in projected metres, then serve in WGS84
  crs <- sf::st_crs(b)
  if (is.na(crs) || is.null(crs$epsg) || crs$epsg != 27700) {
    b <- sf::st_transform(b, 27700)
  }
  b <- sf::st_simplify(b, dTolerance = tolerance, preserveTopology = TRUE)
  b <- sf::st_transform(b, 4326)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  for (i in seq_len(nrow(b))) {
    id <- b[[idcol]][i]
    if (is.na(id) || id == "" || id == "Unparished") {
      next
    }
    outfile <- file.path(path, paste0(id, ".geojson"))
    suppressWarnings(
      sf::st_write(b[i, ], outfile, driver = "GeoJSON",
                   delete_dsn = TRUE, quiet = TRUE)
    )
  }

  invisible(path)
}

#' Export simplified boundaries as a single binary of compressed GeoJSON
#'
#' @description Binary-file equivalent of `export_boundary_geojsons()`:
#'   simplifies the boundaries (in British National Grid metres, preserving
#'   topology per feature), reprojects to WGS84, serialises each area to a
#'   single-feature GeoJSON FeatureCollection string (the same structure the
#'   per-file version produced, coordinates rounded to 7 decimal places) and
#'   packs them into a date-stamped `data_<name>_*.bin` plus lookup index via
#'   `write_json_bin()`, so the website can fetch one area's boundary with an
#'   HTTP range request. Features with an NA, empty or "Unparished" code are
#'   skipped, matching `export_boundary_geojsons()`.
#' @param bounds sf polygon layer of boundaries (e.g. the `bounds_wards` target).
#' @param idcol Name of the column holding the area code (e.g. "WD25CD");
#'   codes must be unique.
#' @param name Dataset name used in the output file names (e.g. "bounds_ward").
#' @param path Output folder for the bin and index files.
#' @param tolerance Simplification tolerance in metres (default 40 m, which
#'   keeps a typical feature to a few KB while remaining visually faithful at
#'   locator-map scale).
#' @param quality Brotli compression level 0-11; see `write_json_bin()`.
#' @return Character vector of the three file paths written (bin, index,
#'   gzipped index), invisibly.
#' @keywords internal
export_boundary_bin <- function(bounds, idcol, name,
                                path = "outputdata/jsonbin",
                                tolerance = 40, quality = 11) {

  # Fail fast, before the (slow) simplification; write_json_bin re-checks
  if (missing(name) || length(name) != 1 || !grepl("^[A-Za-z0-9_-]+$", name)) {
    stop("name must be a single string of letters, numbers, '_' or '-'")
  }

  b <- bounds[, idcol]

  b <- b[!(is.na(b[[idcol]]) | b[[idcol]] == "" | b[[idcol]] == "Unparished"), ]
  if (anyDuplicated(b[[idcol]])) {
    stop("idcol '", idcol, "' contains duplicate codes: ",
         paste(unique(b[[idcol]][duplicated(b[[idcol]])]), collapse = ", "))
  }

  # Simplify in projected metres, then serve in WGS84
  crs <- sf::st_crs(b)
  if (is.na(crs) || is.null(crs$epsg) || crs$epsg != 27700) {
    b <- sf::st_transform(b, 27700)
  }
  b <- sf::st_simplify(b, dTolerance = tolerance, preserveTopology = TRUE)
  b <- sf::st_transform(b, 4326)

  message("Converting GeoJSON ", Sys.time())

  json <- vapply(seq_len(nrow(b)), function(i) {
    yyjsonr::write_geojson_str(b[i, ],
      json_opts = yyjsonr::opts_write_json(digits = 7))
  }, character(1))
  names(json) <- b[[idcol]]

  write_json_bin(json, path = path, name = name, quality = quality,
                 meta = list(format = "geojson", crs = "EPSG:4326",
                             tolerance_m = tolerance))
}
