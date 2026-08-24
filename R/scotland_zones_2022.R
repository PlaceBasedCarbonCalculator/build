#' Read 2022 Scottish Intermediate Zone boundaries
#'
#' @description Unzips and reads the 2022 Intermediate Zone boundary shapefile
#'   (mean high water clipped). Used by the `bounds_iz22` target, which feeds
#'   the Scottish household "community photo" clustering.
#' @param path Boundaries folder containing `SG_IntermediateZoneBdry_2022.zip`
#'   (the `dl_boundaries` target).
#' @return An sf data frame with `IZCode`, `IZName` and geometry.
#' @keywords internal
read_intermidiate_zones_2022 = function(path = "../inputdata/boundaries"){

  dir.create(file.path(tempdir(),"zones"))
  unzip(file.path(path,"SG_IntermediateZoneBdry_2022.zip"),exdir = file.path(tempdir(),"zones"))

  bounds = sf::read_sf(file.path(tempdir(),"zones","SG_IntermediateZoneBdry_2022_MHW.shp"))

  unlink(file.path(tempdir(),"zones"), recursive = TRUE)

  bounds = bounds[,c("IZCode","IZName","geometry")]

  bounds

}
