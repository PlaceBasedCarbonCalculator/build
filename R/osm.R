read_osm_pbf_landuse = function(path = file.path(parameters$path_data,"osm")){

  poly = osmextract::oe_read(file.path(path,"united-kingdom-latest.osm.pbf"),
                             layer  = "multipolygons",
                             extra_tags = c("landcover","landuse","leisure","tourism")
                             )

  poly = poly[,c("osm_id","landcover","landuse","aeroway","amenity",
                 "leisure","man_made","military","natural","tourism")]

  rs = sf::st_drop_geometry(poly)
  rs$osm_id = NULL
  rs[] = lapply(rs[], function(x){!is.na(x)})
  rs = rowSums(rs)

  poly = poly[rs > 0,]

  #landuse = as.data.frame(table(poly$natural))
  landuse = c("industrial","retail","commercial","allotments","quarry",
              "cemetery","railway","landfill","military","education",
              "residential")
  amenity = c("university","college","parking")
  leisure = c("theme_park","water_park","golf_course","nature_reserve",
              "park")
  tourism = c("theme_park","water_park")
  natural = c("wetland","wood","water","heath")

  good = poly$landuse %in% landuse |
    poly$amenity %in% amenity |
    poly$leisure %in% leisure |
    poly$tourism %in% tourism |
    poly$natural %in% natural |
    !is.na(poly$military)

  poly = poly[good,]
  poly = sf::st_transform(poly, 27700)
  poly = sf::st_make_valid(poly)

  poly$area = as.numeric(sf::st_area(poly))
  poly = poly[poly$area > 100,]

  poly

}
