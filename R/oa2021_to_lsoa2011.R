# Match 2021 OA centroids to 2011 LSOA centroids

oa2021tolsoa2011 <- function(centroids_oa21, centroids_lsoa11){

  nn = nngeo::st_nn(centroids_lsoa11, centroids_oa21, k = 1, returnDist = TRUE)
  summary(unlist(nn$dist))
  centroids_lsoa11$nearest_OA2021 = centroids_oa21$OA21CD[unlist(nn$nn)]
  centroids_lsoa11$nearest_OA2021_dist = unlist(nn$dist)

  centroids_lsoa11 = sf::st_drop_geometry(centroids_lsoa11)
  centroids_lsoa11 = centroids_lsoa11[,c("LSOA11CD","nearest_OA2021","nearest_OA2021_dist")]
  centroids_lsoa11
}


oa2021tolsoa2021 <- function(centroids_oa21, centroids_lsoa21){

  nn = nngeo::st_nn(centroids_lsoa21, centroids_oa21, k = 1, returnDist = TRUE)
  summary(unlist(nn$dist))
  centroids_lsoa21$nearest_OA2021 = centroids_oa21$OA21CD[unlist(nn$nn)]
  centroids_lsoa21$nearest_OA2021_dist = unlist(nn$dist)

  centroids_lsoa21 = sf::st_drop_geometry(centroids_lsoa21)
  centroids_lsoa21 = centroids_lsoa21[,c("LSOA21CD","nearest_OA2021","nearest_OA2021_dist")]
  centroids_lsoa21
}


# foo = centroids_lsoa11[centroids_lsoa11$nearest_OA2021_dist > 4000,]
#
#   qtm(foo) +
#   qtm(centroids_oa21[centroids_oa21$OA21CD %in% foo$nearest_OA2021,], dots.col = "red")
#
