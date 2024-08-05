tar_load(bounds_lsoa_GB_full_landuse)
tar_load(buildings_heights)

library(sf)

bounds_lsoa_GB_full_landuse$area = NULL

buildings_heights = buildings_heights[,c("building","building_part","height_max","geometry")]
buildings_heights$id = 1:nrow(buildings_heights)

nrow(buildings_heights)
buildings_heights = st_join(buildings_heights, bounds_lsoa_GB_full_landuse, largest = TRUE)
nrow(buildings_heights)
Sys.time()
