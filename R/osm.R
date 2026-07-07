#' Extract land use polygons from the OSM UK extract
#'
#' @description Reads multipolygons from the united-kingdom OSM pbf and keeps
#'   those with a relevant land use: selected landuse/amenity/leisure/tourism/
#'   natural values plus anything military, larger than 100 m2, transformed
#'   to EPSG:27700. Used by the `osm_land` target, combined with OS layers in
#'   `combine_land_use()`.
#' @param path Folder containing `united-kingdom-latest.osm.pbf`.
#' @return An sf data frame of land use polygons with tag columns and `area`.
#' @keywords internal
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
              "park","stadium")
  tourism = c("theme_park","water_park","attraction")
  natural = c("wetland","wood","water","heath","scrub")

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


#' Extract building footprints from the OSM UK extract
#'
#' @description Reads multipolygons tagged `building` from the
#'   united-kingdom OSM pbf and casts them to single polygons. Used by the
#'   `osm_buildings` target, combined with OS/INSPIRE data in
#'   `combine_os_osm_buildings()`.
#' @param path Folder containing `united-kingdom-latest.osm.pbf`.
#' @return An sf POLYGON data frame with `osm_id`, `building` and
#'   `building_part`.
#' @keywords internal
read_osm_pbf_buildings = function(path = file.path(parameters$path_data,"osm")){

  poly = osmextract::oe_read(file.path(path,"united-kingdom-latest.osm.pbf"),
                             layer  = "multipolygons",
                             extra_tags = c("building","building:part")
  )

  poly = poly[,c("osm_id","building","building_part")]
  poly = poly[!is.na(poly$building),]

  poly = sf::st_cast(poly, "POLYGON")

  poly

}
