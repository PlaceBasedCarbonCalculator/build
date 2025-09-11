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
