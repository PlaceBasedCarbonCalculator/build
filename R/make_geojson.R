#' Write an sf object to GeoJSON for tippecanoe
#'
#' @description Writes zones/points to a GeoJSON file ready to be turned into
#'   PMTiles by `make_pmtiles()`. Transforms to WGS84 (EPSG:4326) if needed,
#'   sets coordinate precision to 6 decimal places, and overwrites any existing
#'   file at `path`. Used by the various `geojson_*` targets (wards, parish,
#'   LA, EPC points, postcodes, etc.).
#' @param z An sf data frame of zones or points.
#' @param path File path for the output GeoJSON (including file name).
#' @return The output `path`, after writing the file as a side effect. Warns if
#'   `z` has more than 15 columns, as wide attribute tables bloat tiles.
#' @keywords internal
make_geojson <- function(z, path = "outputs/zones.geojson"){

  if(ncol(z) > 15){
    warning("Thats a lot of columns for the GeoJSON, are they all needed?")
  }
  if(file.exists(path)){
    unlink(path)
  }

  if(!sf::st_is_longlat(z)){
    z <- sf::st_transform(z, 4326)
  }
  sf::st_precision(z) <- 1000000
  sf::st_write(obj = z, dsn = path, delete_dsn = FALSE)

  return(path)
}



