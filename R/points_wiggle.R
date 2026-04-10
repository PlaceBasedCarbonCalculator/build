# Slightly randomise locations of duplicated points
# E.g. all UPRNs for flats in the same location.
#' Wiggle Points
#'
#' @description Perform processing for wiggle points.
#' @param x){ Input object or parameter named `x){`.
#' @return The function result, typically a data frame or list used in the pipeline.
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

#' Wiggle
#'
#' @description Perform processing for wiggle.
#' @param x){ Input object or parameter named `x){`.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
wiggle <- function(x){
  x[[1]] <- x[[1]] + round(runif(1,-0.00005,0.00005),6)
  x[[2]] <- x[[2]] + round(runif(1,-0.00005,0.00005),6)
  x
}
