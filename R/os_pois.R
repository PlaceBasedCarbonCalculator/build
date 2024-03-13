read_os_poi <- function(path, path_types){
  dir.create(file.path(tempdir(),"pois"))
  unzip(path, exdir = file.path(tempdir(),"pois"))
  pois <- sf::read_sf( file.path(tempdir(),"pois","poi_5111956","poi_5111956.gpkg"))
  pois <- pois[,c("ref_no","name","groupname","categoryname","classname", "brand","qualifier_type","qualifier_data")]

  # types = group_by(st_drop_geometry(pois),
  #                  groupname,categoryname,classname) %>%
  #   summarise(count = n())
  # types = types[order(types$groupname, types$categoryname),]
  #
  # types_npt = read.csv("../../atumscot/npt/inputdata/poi_types.csv")
  # types_npt$count = NULL
  # types = left_join(types, types_npt, by = c("groupname","categoryname","classname"))
  # write.csv(types,"../inputdata/poi/poi_types.csv", row.names = FALSE)

  types = read.csv(path_types)
  pois = dplyr::left_join(pois, types, by = c("groupname","categoryname","classname"))
  pois
}
