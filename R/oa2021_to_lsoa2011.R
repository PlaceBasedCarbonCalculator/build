# Match 2021 OA centroids to 2011 LSOA centroids

#' Oa Tolsoa
#'
#' @description Perform processing for oa2021tolsoa2011.
#' @param centroids_oa21 OA 2021 centroid geometries.
#' @param centroids_lsoa11){ Centroid geometries used for area matching.
#' @return The function result, typically a data frame or list used in the pipeline.
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


#' Oa Tolsoa
#'
#' @description Perform processing for oa2021tolsoa2021.
#' @param centroids_oa21 OA 2021 centroid geometries.
#' @param centroids_lsoa21){ Centroid geometries used for area matching.
#' @return The function result, typically a data frame or list used in the pipeline.
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


