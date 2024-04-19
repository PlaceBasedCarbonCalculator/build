#' Function to make geojson for tippecanoe (zones)
#' @param z sf data frame with zones
#' @param path folder to save results
#' @example

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
  sf::st_write(z,path, delete_dsn = TRUE)

  return(path)
}



