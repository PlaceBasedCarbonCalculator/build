#' Read OS Points of Interest and attach analysis categories
#'
#' @description Unzips and reads the OS Points of Interest GeoPackage
#'   (secure data) and joins the project's own POI type categorisation from
#'   `poi_types.csv`. Used by the `poi` target, the destination set for the
#'   accessibility analysis.
#' @param path Path to the OS POI download zip.
#' @param path_types Path to `poi_types.csv` mapping
#'   groupname/categoryname/classname to analysis types.
#' @return An sf POINT data frame of POIs with category columns.
#' @keywords internal
read_os_poi <- function(path, path_types){
  dir.create(file.path(tempdir(),"pois"))
  unzip(path, exdir = file.path(tempdir(),"pois"))
  pois <- sf::read_sf( file.path(tempdir(),"pois","poi_5111956","poi_5111956.gpkg"))
  pois <- pois[,c("ref_no","name","groupname","categoryname","classname", "brand","qualifier_type","qualifier_data")]


  types = read.csv(path_types)
  pois = dplyr::left_join(pois, types, by = c("groupname","categoryname","classname"))
  pois
}
