#' Read Os Poi
#'
#' @description Read os poi from disk into an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @param path_types){ Input object or parameter named `path_types){`.
#' @return A data frame containing the loaded dataset.
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
