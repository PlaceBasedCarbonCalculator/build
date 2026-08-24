
#' Build the national building footprint layer from OSM, OS and INSPIRE
#'
#' @description Combines OSM building footprints with OS Zoomstack buildings
#'   (only where they don't overlap OSM), then splits the footprints along
#'   INSPIRE cadastral parcel boundaries so terraces become individual
#'   properties. Slivers (area < 1 m2 or perimeter/area > 10) and duplicate
#'   geometries are removed. Used by the `buildings` target, input to
#'   `add_building_heights()`. Memory-hungry (~35M polygons).
#' @param osm_buildings OSM footprints (`osm_buildings` target).
#' @param os_buildings OS Zoomstack local buildings (`os_buildings` target).
#' @param inspire E&W INSPIRE parcels (`inspire` target).
#' @param inspire_scotland Scottish INSPIRE parcels (`inspire_scotland`).
#' @return An sf POLYGON data frame with `osm_id`, `building`,
#'   `building_part` and `INSPIREID`.
#' @keywords internal
combine_os_osm_buildings = function(osm_buildings, os_buildings, inspire, inspire_scotland){

  osm_buildings = sf::st_transform(osm_buildings, 27700)
  osm_buildings = sf::st_make_valid(osm_buildings)

  inspire = inspire[,"INSPIREID"]
  inspire = inspire[!duplicated(inspire$GEOMETRY),]

  inspire_scotland = inspire_scotland[,"inspireid"]
  names(inspire_scotland) = c("INSPIREID","GEOMETRY")
  sf::st_geometry(inspire_scotland) = "GEOMETRY"
  inspire_scotland = sf::st_cast(inspire_scotland, "POLYGON")
  inspire_scotland = inspire_scotland[!duplicated(inspire_scotland$GEOMETRY),]

  inspire = rbind(inspire, inspire_scotland)
  rm(inspire_scotland)

  # Only keep OS buildings that don't intersect with OSM
  inter = sf::st_intersects(os_buildings, osm_buildings)
  os_buildings = os_buildings[lengths(inter) == 0,]
  os_buildings$osm_id = NA
  os_buildings$building = NA
  os_buildings$building_part = NA

  buildings = rbind(osm_buildings, os_buildings)
  rm(osm_buildings, os_buildings, inter)


  # Split buildings with inspire polygons
  buildings$id <- 1:nrow(buildings)
  gc()
  buildings2 = sf::st_intersection(buildings, inspire) #EW only

  # buildings that don't intersect
  buildings3 = buildings[!buildings$id %in% buildings2$id,]
  buildings3$INSPIREID = NA_character_

  buildings = rbind(buildings2, buildings3)

  rm(inspire, buildings2, buildings3)

  gt = sf::st_geometry_type(buildings)

  buldings_gc = buildings[gt == "GEOMETRYCOLLECTION",]
  buldings_gc = sf::st_collection_extract(buldings_gc, "POLYGON")

  buldings_mp = buildings[gt == "MULTIPOLYGON",]
  buldings_mp = sf::st_cast(buldings_mp, "POLYGON")

  buldings_poly = buildings[gt == "POLYGON",]

  buildings = rbind(buldings_poly, buldings_mp, buldings_gc)
  rm(gt, buldings_poly, buldings_mp, buldings_gc)
  gc()

  # Remove slithers
  buildings$area = as.numeric(sf::st_area(buildings))
  buildings = buildings[buildings$area > 1,]
  buildings$perimiter <- as.numeric(sf::st_perimeter(buildings))
  buildings$apratio <- buildings$perimiter / buildings$area
  buildings <- buildings[buildings$apratio < 10,]
  buildings = buildings[,c("osm_id","building","building_part","INSPIREID")]

  # Remove Duplicates
  buildings = buildings[!duplicated(buildings$geometry),]

  buildings

}

#' Add LiDAR-derived heights to building footprints
#'
#' @description Assigns each building to a 10 km OS grid tile (by centroid)
#'   and, in parallel, extracts min/max height and volume from the
#'   DSM-minus-DTM difference rasters via `building_height_internal()`.
#'   Buildings outside the available tiles (e.g. Northern Ireland) are
#'   dropped. Used by the `buildings_heights` target; note the raster path
#'   defaults to a local drive so this target is not fully reproducible.
#' @param buildings Building footprints (`buildings` target).
#' @param os_10k_grid OS 10 km grid squares (`os_10k_grid` target).
#' @param path_raster Folder of `<tile>.tiff` height-difference rasters.
#' @return An sf data frame of buildings with `height_max`, `height_min`
#'   and `volume`.
#' @keywords internal
add_building_heights = function(buildings, os_10k_grid, path_raster = "F:/DTM_DSM/GB_10k/Difference/"){

  # Simple Join takes ages
  cents = sf::st_centroid(buildings)
  fls = list.files(path_raster)
  fls_nm = gsub(".tiff","",fls)
  os_10k_grid = os_10k_grid[os_10k_grid$tile_name %in% fls_nm,]
  inter = sf::st_intersects(os_10k_grid, cents)
  attributes(inter)$class = "list"
  names(inter) = os_10k_grid$tile_name
  inter = unlist(inter)

  buildings$id = 1:nrow(buildings)
  buildings = buildings[buildings$id %in% inter,] #Remove NI (35667905 to 35114085)

  inter = data.frame(grid = names(inter), id = unname(inter))
  inter$grid = substr(inter$grid,1,4)
  buildings$grid = inter$grid[match(buildings$id, inter$id)]

  buildings = dplyr::group_by(buildings, grid)
  buildings = dplyr::group_split(buildings)

  future::plan("future::multisession", workers = 10)
  buildings = furrr::future_map(buildings, building_height_internal,
                                 path_raster = path_raster,
                                 .progress = TRUE,
                                 .options = furrr::furrr_options(seed = TRUE)
                                 )
  future::plan("sequential")



  buildings = bind_sf(buildings)
  buildings

}

#' Load the OS British National Grid 10 km squares
#'
#' @description Reads the 10 km grid layer from the OS BNG grids GeoPackage.
#'   Used by the `os_10k_grid` target to tile the building-heights raster
#'   extraction.
#' @param path Path to `os_bng_grids.gpkg`.
#' @return An sf data frame of 10 km grid squares with `tile_name`.
#' @keywords internal
load_os_10k_grid = function(path){
  sf::read_sf(path, layer = "10km_grid")
}



#' Extract heights for one grid tile's buildings
#'
#' @description Worker for `add_building_heights()`: loads the tile's
#'   height-difference raster and uses `exactextractr` to compute each
#'   building's min/max height and area-weighted volume.
#' @param buildings_sub Buildings whose centroids fall in one 10 km tile.
#' @param path_raster Folder of `<tile>.tiff` rasters.
#' @return `buildings_sub` with `height_max`, `height_min`, `volume`.
#' @keywords internal
building_height_internal = function(buildings_sub, path_raster){

  r_diff = terra::rast(file.path(path_raster,paste0(buildings_sub$grid[1],".tiff")))
  terra::crs(r_diff) = "epsg:27700"

  heights = exactextractr::exact_extract(r_diff,
                                         buildings_sub,
                                         c('min', 'max','weighted_sum'),
                                         weights = "area",
                                         progress = FALSE)

  buildings_sub$height_max = round(heights$max,1)
  buildings_sub$height_min = round(heights$min,1)
  buildings_sub$volume = round(heights$weighted_sum,0)

  buildings_sub

}
