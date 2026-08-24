#' Read 2022 Scottish Data Zone boundaries
#'
#' @description Unzips and reads the 2022 Data Zone boundary shapefile (mean
#'   high water clipped), keeps only the zone code (renamed `DataZone22`) and
#'   makes the geometries valid. Used by the `bounds_dz22` target and combined
#'   with E&W LSOAs in `combine_lsoa_bounds()`.
#' @param path Boundaries folder containing `SG_DataZoneBdry_2022.zip` (the
#'   `dl_boundaries` target).
#' @return An sf data frame with `DataZone22` and geometry.
#' @keywords internal
read_dz2022_bounds = function(path = "../inputdata/boundaries/SG_DataZoneBdry_2022.zip"){
  path = file.path(path, "SG_DataZoneBdry_2022.zip")

  #Unzip the file
  dir.create(file.path(tempdir(), "scotbounds"), showWarnings = FALSE)
  unzip(path, exdir = file.path(tempdir(), "scotbounds"))

  bounds = sf::read_sf(file.path(tempdir(), "scotbounds","SG_DataZoneBdry_2022_MHW.shp"))

  bounds = bounds[,c("DZCode")]

  #Rename the columns to match the 2011 version
  names(bounds) = c("DataZone22","geometry")

  #Remove the temporary directory
  unlink(file.path(tempdir(), "scotbounds"), recursive = TRUE)

  bounds = sf::st_make_valid(bounds)

  bounds

}
