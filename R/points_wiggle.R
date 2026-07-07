#' Randomly offset points that share identical coordinates
#'
#' @description Finds points with duplicated geometry (e.g. all the UPRNs of
#'   flats in the same building) and nudges each duplicate by a small random
#'   offset so they render as separate points on the map. The first point at
#'   each location keeps its true position. Used before writing the EPC and
#'   UPRN GeoJSON point layers in `_targets.R`.
#' @param x An sf POINT data frame in lng/lat (EPSG:4326); errors otherwise.
#' @return `x` with duplicated points offset by up to ~0.00005 degrees (about
#'   5 m) in each axis. Row order is not preserved.
#' @keywords internal
wiggle_points  = function(x){

  if(!sf::st_is_longlat(x)){
    stop("Not lng/lat data")
  }

  dup <- duplicated(sf::st_geometry(x))

  x_dup <- x[dup,]
  x <- x[!dup,]

  geom2 <- purrr::map(sf::st_geometry(x_dup), wiggle)
  geom2 <- sf::st_as_sfc(geom2)
  sf::st_crs(geom2) <- 4326

  sf::st_geometry(x_dup) <- geom2
  x <- rbind(x, x_dup)

  x


}

#' Offset a single point by a small random amount
#'
#' @description Helper for `wiggle_points()`: adds a uniform random offset of
#'   +/- 0.00005 degrees (rounded to 6 dp) to each coordinate of one point.
#' @param x A single sfg POINT geometry.
#' @return The offset POINT geometry.
#' @keywords internal
wiggle <- function(x){
  x[[1]] <- x[[1]] + round(runif(1,-0.00005,0.00005),6)
  x[[2]] <- x[[2]] + round(runif(1,-0.00005,0.00005),6)
  x
}
