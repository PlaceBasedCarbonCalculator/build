read_intermidiate_zones_2022 = function(path = "../inputdata/boundaries"){

  dir.create(file.path(tempdir(),"zones"))
  unzip(file.path(path,"SG_IntermediateZoneBdry_2022.zip"),exdir = file.path(tempdir(),"zones"))

  bounds = sf::read_sf(file.path(tempdir(),"zones","SG_IntermediateZoneBdry_2022_MHW.shp"))

  unlink(file.path(tempdir(),"zones"), recursive = TRUE)

  bounds = bounds[,c("IZCode","IZName","geometry")]

  bounds

}
