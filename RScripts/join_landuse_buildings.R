library(sf)
library(targets)

tar_load(bounds_lsoa_GB_full_landuse)
tar_load(zoomstack_buildings_lst_4326)
tar_load(bounds_lsoa_GB_full)



buildings_heights = zoomstack_buildings_lst_4326$high
buildings_heights = sf::st_transform(buildings_heights, 27700)
buildings_heights = dplyr::group_split(buildings_heights, LSOA21CD)

bounds_lsoa_GB_full_landuse$area = NULL
bounds_missing = bounds_lsoa_GB_full[!bounds_lsoa_GB_full$LSOA21CD %in% bounds_lsoa_GB_full_landuse$LSOA21CD,]
bounds_missing$type = "residential"
bounds_lsoa_GB_full_landuse = rbind(bounds_lsoa_GB_full_landuse, bounds_missing)
bounds = dplyr::group_split(bounds_lsoa_GB_full_landuse, LSOA21CD)

names(bounds) = sapply(bounds, function(x){x$LSOA21CD[1]})
names(buildings_heights) = sapply(buildings_heights, function(x){x$LSOA21CD[1]})
buildings_missing = list(NULL, NULL)
nms = names(bounds)[!names(bounds) %in% names(buildings_heights)]
#TODO Missing "S01010206" "S01010226"
names(buildings_missing) = nms

buildings_heights = c(buildings_heights, buildings_missing)
buildings_heights = buildings_heights[!is.na(names(buildings_heights))]

#"E01011217" missing in bounds_land_use
# bounds_missing = bounds_lsoa_GB_full[bounds_lsoa_GB_full$LSOA21CD == "E01011217",]
# bounds_missing$type = "residential"
# bounds_missing = list(E01011217 = bounds_missing)
# bounds = c(bounds, bounds_missing)


buildings_heights = buildings_heights[order(names(buildings_heights))]
bounds_simple = bounds_lsoa_GB_full[order(bounds_lsoa_GB_full$LSOA21CD),]
bounds_simple = dplyr::group_split(bounds_simple, LSOA21CD)
names(bounds_simple) = sapply(bounds_simple, function(x){x$LSOA21CD[1]})

bounds = bounds[order(names(bounds))]
buildings_heights = buildings_heights[order(names(buildings_heights))]
bounds_simple = bounds_simple[order(names(bounds_simple))]

summary(names(bounds_simple) == names(buildings_heights))
summary(names(bounds_simple) == names(bounds))

future::plan("future::multisession", workers = 20)
res = furrr::future_pmap(list(buildings_heights,
                       bounds,
                       bounds_simple),
                  .f = match_buildings,
                  .progress = TRUE)

res_buildings = lapply(res, `[[`, 1)
res_buildings = dplyr::bind_rows(res_buildings)

res_zones = lapply(res, `[[`, 2)
res_zones = dplyr::bind_rows(res_zones)

saveRDS(res_buildings, "data/buildings_withtype.Rds")
saveRDS(res_zones, "data/lsoa21_withtype.Rds")

#qtm(res_zones, fill = "type")

foo = data.frame(bounds = names(bounds), build = names(buildings_heights)[1:42646])

sel = "E01000007"

build = buildings_heights[names(buildings_heights) == sel][[1]]
land = bounds[names(bounds) == sel][[1]]
bound = bounds_lsoa_GB_full[bounds_lsoa_GB_full$LSOA21CD == sel,]

#profvis::profvis(match_buildings(build, land, bound))

match_buildings = function(build, land, bound){

  if(is.null(build)){
    return(list(buildings = build, zones = land))
  }

  land$LSOA21CD = NULL
  if(any(!sf::st_is_valid(build))){
    build = sf::st_make_valid(build)
  }

  #qtm(build) + qtm(land)
  build = suppressWarnings(sf::st_join(build, land, largest = TRUE))
  buff = st_buffer(build, 20)

  area2 = dplyr::group_by(buff, type)
  area2 = dplyr::summarise(area2)

  area2_geom = sf::st_geometry_type(area2)
  area2_p = area2[area2_geom == "POLYGON",]
  area2_mp = area2[area2_geom == "MULTIPOLYGON",]
  area2_mp = suppressWarnings(sf::st_cast(area2_mp, "POLYGON"))
  area2 = rbind(area2_p, area2_mp)
  area2$zoneid = seq(1, nrow(area2))

  area2 = suppressWarnings(sf::st_intersection(area2, bound))
  area2 = sf::st_simplify(area2, preserveTopology = TRUE, 1)

  return(list(buildings = build, zones = area2))

  #tm_shape(area2) + tm_fill(col = "type", alpha = 0.6) + qtm(build, fill = "type")

  #tm_shape(conc) + tm_fill(col = "type", alpha = 0.6) + qtm(build2, fill = "type")

  #qtm(build2, fill = "type") + qtm(bound, fill = NULL)

  # area = dplyr::group_by(build2, type)
  # area = dplyr::summarise(area)
  # area = sf::st_simplify(area, dTolerance = 1, preserveTopology = TRUE)
  #
  # conc = sf::st_concave_hull(area, ratio = 0.8)
  # conc = sf::st_intersection(conc, bound)



}



# nrow(buildings_heights)
# buildings_heights = st_join(buildings_heights, bounds_lsoa_GB_full_landuse, largest = TRUE)
# nrow(buildings_heights)
# Sys.time()
