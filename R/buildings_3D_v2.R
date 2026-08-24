# Combine OSM/OS buildings with INSPIRE parcels - v2 (fast) -------------------
#
# NEW CODE (2026-07): faster replacement for combine_os_osm_buildings() in
# buildings_3D.R (which is unchanged, for cross-comparison). The v1 target
# (`buildings`) took ~8.7 h, dominated by a single-threaded
# sf::st_intersection() of ~30M buildings against ~25M INSPIRE parcels.
# Differences from v1:
#
#  1. The buildings x INSPIRE intersection runs in DuckDB via
#     duckdb_clip_join() (multi-threaded, RTREE-indexed, chunked), with
#     left = TRUE reproducing v1's "keep non-intersecting buildings with
#     NA INSPIREID" behaviour.
#  2. st_make_valid() on the OSM buildings runs only on geometries that
#     fail st_is_valid().
#
# Semantic difference vs v1 (intentional): duckdb_clip_join() keeps only
# polygonal intersection pieces, so buildings that merely touch a parcel
# along an edge no longer generate degenerate line/point slivers (v1
# filtered most of these out later via the area > 1 rule anyway).
#
# Suggested target definition (add alongside, do NOT replace, the v1
# target while comparing):
#
# tar_target(buildings_v2,{
#   combine_os_osm_buildings_v2(osm_buildings, os_buildings, inspire, inspire_scotland)
# }),


#' Combine OSM, OS and INSPIRE building footprints (v2, fast)
#'
#' @description Faster reimplementation of `combine_os_osm_buildings()` -
#'   see the header of this file. Same inputs, filters and output columns.
#' @param osm_buildings OSM building footprints (`osm_buildings` target).
#' @param os_buildings OS building footprints (`os_buildings` target).
#' @param inspire INSPIRE cadastral parcels E&W (`inspire` target).
#' @param inspire_scotland Scottish INSPIRE parcels
#'   (`inspire_scotland` target).
#' @return An `sf` object of building polygons with osm_id, building,
#'   building_part, INSPIREID.
#' @keywords internal
combine_os_osm_buildings_v2 = function(osm_buildings, os_buildings, inspire, inspire_scotland){

  osm_buildings = sf::st_transform(osm_buildings, 27700)
  invalid = !sf::st_is_valid(osm_buildings)
  invalid[is.na(invalid)] = TRUE
  if(any(invalid)){
    message(Sys.time(), " repairing ", sum(invalid), " invalid OSM geometries")
    sf::st_geometry(osm_buildings)[invalid] = sf::st_make_valid(sf::st_geometry(osm_buildings)[invalid])
  }

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

  # Split buildings with inspire polygons; left join keeps buildings that
  # intersect no parcel, with NA INSPIREID (v1's buildings3)
  buildings$id <- 1:nrow(buildings)
  gc()
  buildings = duckdb_clip_join(buildings, inspire, zone_col = "INSPIREID",
                               left = TRUE)
  rm(inspire)

  # As v1: explode multi-part results into single polygons
  gt = sf::st_geometry_type(buildings)
  buldings_mp = buildings[gt == "MULTIPOLYGON",]
  buldings_poly = buildings[gt == "POLYGON",]
  buldings_mp = suppressWarnings(sf::st_cast(buldings_mp, "POLYGON"))
  buildings = rbind(buldings_poly, buldings_mp)
  rm(gt, buldings_poly, buldings_mp)
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
