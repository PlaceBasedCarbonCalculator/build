#' Find the nearest 2021 OA centroid to each 2011 LSOA centroid
#'
#' @description For each 2011 LSOA population-weighted centroid, finds the
#'   nearest 2021 Output Area centroid (k = 1 nearest neighbour). Used by the
#'   `lookup_oa2021_lsoa2011` target so LSOA-level results can be represented
#'   by a single OA (e.g. for the ONS isochrone accessibility analysis).
#' @param centroids_oa21 sf POINT data frame of 2021 OA centroids (`OA21CD`).
#' @param centroids_lsoa11 sf POINT data frame of 2011 LSOA centroids
#'   (`LSOA11CD`).
#' @return A data frame with `LSOA11CD`, `nearest_OA2021` and
#'   `nearest_OA2021_dist` (metres).
#' @keywords internal
oa2021tolsoa2011 <- function(centroids_oa21, centroids_lsoa11){

  nn = nngeo::st_nn(centroids_lsoa11, centroids_oa21, k = 1, returnDist = TRUE)
  summary(unlist(nn$dist))
  centroids_lsoa11$nearest_OA2021 = centroids_oa21$OA21CD[unlist(nn$nn)]
  centroids_lsoa11$nearest_OA2021_dist = unlist(nn$dist)

  centroids_lsoa11 = sf::st_drop_geometry(centroids_lsoa11)
  centroids_lsoa11 = centroids_lsoa11[,c("LSOA11CD","nearest_OA2021","nearest_OA2021_dist")]
  centroids_lsoa11
}


#' Find the nearest 2021 OA centroid to each 2021 LSOA centroid
#'
#' @description For each 2021 LSOA population-weighted centroid, finds the
#'   nearest 2021 Output Area centroid (k = 1 nearest neighbour). Used by the
#'   `lookup_oa2021_lsoa2021` target, which the accessibility (`access_poi_*`)
#'   targets use to pick a representative OA isochrone per LSOA.
#' @param centroids_oa21 sf POINT data frame of 2021 OA centroids (`OA21CD`).
#' @param centroids_lsoa21 sf POINT data frame of 2021 LSOA centroids
#'   (`LSOA21CD`).
#' @return A data frame with `LSOA21CD`, `nearest_OA2021` and
#'   `nearest_OA2021_dist` (metres).
#' @keywords internal
oa2021tolsoa2021 <- function(centroids_oa21, centroids_lsoa21){

  nn = nngeo::st_nn(centroids_lsoa21, centroids_oa21, k = 1, returnDist = TRUE)
  summary(unlist(nn$dist))
  centroids_lsoa21$nearest_OA2021 = centroids_oa21$OA21CD[unlist(nn$nn)]
  centroids_lsoa21$nearest_OA2021_dist = unlist(nn$dist)

  centroids_lsoa21 = sf::st_drop_geometry(centroids_lsoa21)
  centroids_lsoa21 = centroids_lsoa21[,c("LSOA21CD","nearest_OA2021","nearest_OA2021_dist")]
  centroids_lsoa21
}


