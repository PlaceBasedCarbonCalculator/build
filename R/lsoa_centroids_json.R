#' Build a combined LSOA/Data Zone centroid lookup for the website
#'
#' @description Combines the England & Wales 2021 LSOA population-weighted
#'   centroids with the Scottish 2022 Data Zone centroids into a single
#'   coordinate lookup and writes it as one compact JSON object of the form
#'   `{"E01000001":[lng,lat], ...}` (WGS84, longitude first). The website loads
#'   this file lazily to pan/zoom the map to a report target that has been
#'   deep-linked via `?report=<code>` (or resolved from a postcode search)
#'   without requiring the user to click the zone. Coordinates are rounded to 5
#'   decimal places (~1 m), which keeps the file small (~1.3 MB uncompressed).
#'
#'   The output file is written to `outputdata/json/lsoa_centroids.json`. Copy
#'   (or deploy) it to `data/lsoa_centroids.json` in the website repository.
#'
#' @param centroids_lsoa21 sf POINT data frame of E&W 2021 LSOA
#'   population-weighted centroids, with an `LSOA21CD` column (the
#'   `centroids_lsoa21` target).
#' @param centroids_dz22 sf POINT data frame of Scottish 2022 Data Zone
#'   centroids, with `LSOA21CD` (and `LSOA21NM`) columns (the `centroids_dz22`
#'   target).
#' @param path Output directory for the JSON file; created if missing.
#' @param rounddp Decimal places to round coordinates to (default 5, ~1 m).
#' @return The path of the JSON file written (invisibly).
#' @keywords internal
make_lsoa_centroids_json <- function(centroids_lsoa21, centroids_dz22,
                                     path = "outputdata/json", rounddp = 5) {

  # Harmonise columns so the two sources can be row-bound
  ew <- centroids_lsoa21[, "LSOA21CD"]
  sc <- centroids_dz22[, "LSOA21CD"]
  cents <- rbind(ew, sc)

  # Ensure WGS84 (lng/lat); the ONS/SG source files are British National Grid
  cents <- sf::st_transform(cents, 4326)

  # Extract coordinates (X = lng, Y = lat) and round to keep the file small
  coords <- sf::st_coordinates(cents)
  lng <- round(coords[, "X"], rounddp)
  lat <- round(coords[, "Y"], rounddp)
  codes <- cents$LSOA21CD

  # Drop any centroid missing a code or coordinate
  keep <- !is.na(codes) & !is.na(lng) & !is.na(lat)
  codes <- codes[keep]
  lng <- lng[keep]
  lat <- lat[keep]

  # Build a named list of [lng, lat] pairs -> serialises to {"code":[lng,lat]}
  lookup <- stats::setNames(
    lapply(seq_along(codes), function(i) c(lng[i], lat[i])),
    codes
  )

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  outfile <- file.path(path, "lsoa_centroids.json")
  # auto_unbox keeps [lng,lat] as a 2-element array; digits = NA preserves the
  # already-rounded precision without jsonlite's default 4-dp truncation.
  writeLines(jsonlite::toJSON(lookup, auto_unbox = TRUE, digits = NA), outfile)

  invisible(outfile)
}
