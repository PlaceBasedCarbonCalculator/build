# Testing SI Model

library(simodels)
library(dplyr)
library(sf)
library(tmap)
tmap_mode("view")
tar_load(poi)
tar_load(bounds_lsoa21_super_generalised)
tar_load(population)

population = population[,c("year","LSOA11CD","all_ages")]
population = population[population$year == 2020,]

si_zones = dplyr::left_join(bounds_lsoa21_super_generalised, population, by = c("LSOA21CD" = "LSOA11CD"))
si_zones = si_zones[grepl("Leeds",si_zones$LSOA21NM ),]

retail = poi[poi$groupname == "Retail",]
retail = retail[si_zones,]

grid = sf::st_make_grid(retail, cellsize = c(500,500), what = "centers")
grid = sf::st_as_sf(data.frame(grid_id = 1:length(grid),geometry = grid))


retail$grid_id = sf::st_nearest_feature(retail, grid)

# calculate weighting of each grid point
retail_grid = retail %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(grid_id) %>%
  dplyr::summarise(size = n())



retail_grid = dplyr::left_join(retail_grid, grid, by = "grid_id")
retail_grid = sf::st_as_sf(retail_grid)
qtm(retail_grid, dots.col = "size")

si_zones = sf::st_transform(si_zones, 4326)
si_zones = sf::st_make_valid(si_zones)
retail_grid = sf::st_transform(retail_grid, 4326)


od = simodels::si_to_od(si_zones, retail_grid, max_dist = 10000)

gravity_model = function(beta, d, m, n) {
  m * n * exp(-beta * d / 1000)
}


od$interaction = gravity_model(0.9, d = od$distance_euclidean,
                               m = od$origin_all_ages,
                               n = od$destination_size)

constrain_production = function(grp, out, conts) {
  dt = data.table::data.table(grp = grp, out = out, conts = conts)
  dt = dt[,out := out / sum(out) * data.table::first(conts), grp]
  return(dt$out)
}

od$interaction = constrain_production(od$O, od$interaction, od$origin_all_ages)
summary(od$interaction - od$interaction2)

od$interaction[is.na(od$interaction)] <- 0

od_sub = od[od$interaction > 1,]
od_sub = od_sub[order(od_sub$interaction),]

plot(od_sub["interaction"], logz=TRUE)



