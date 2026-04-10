#' Combine Land Use
#'
#' @description Combine land use inputs into a single consolidated result.
#' @details This function is used to prepare intermediate analysis tables for later pipeline targets.
#' @param os_land Input object or parameter named `os_land`.
#' @param os_greenspace Input object or parameter named `os_greenspace`.
#' @param osm_land){ Input object or parameter named `osm_land){`.
#' @return A combined data frame or table merging the provided inputs.
#' @keywords internal
combine_land_use = function(os_land, os_greenspace, osm_land){

  #TODO: landuse:farmyard is a residential land use that can overlap with non-residential
  #leisure:garden leisure:park  are natural
  #leisure:stadium is non-residential

  #OSM start with
  landuse = c("industrial", "retail", "military", "commercial", "landfill","quarry","recreation_ground","railway")
  military = c("danger_area", "shooting_range", "range", "training_area")
  tourism = c("theme_park","water_park","attraction")
  natural = c("wood","heath","wetland","scrub")
  leisure = c("golf_course","nature_reserve","theme_park","water_park","park","stadium")
  amenity = c("parking")

  osm_land = osm_land[osm_land$landuse %in% landuse |
                        osm_land$military %in% military |
                        osm_land$tourism %in% tourism |
                        osm_land$natural %in% natural |
                        osm_land$amenity %in% amenity |
                        osm_land$leisure %in% leisure,]

  # Natural land
  #TODO: Add danger areas?
  lnd_nature = osm_land[!is.na(osm_land$natural),]
  lnd_nature2 = os_land[os_land$type %in% c("water","greenspace"),]

  lnd_nature = lnd_nature[!duplicated(lnd_nature$geometry),]
  lnd_nature2 = lnd_nature2[!duplicated(lnd_nature2$geometry),]

  lnd_nature_all = c(lnd_nature$geometry, lnd_nature2$geometry, os_greenspace$geometry)
  lnd_nature_all = sf::st_cast(lnd_nature_all, "MULTIPOLYGON")
  lnd_nature_all = sf::st_cast(lnd_nature_all, "POLYGON")

  gs_inter = sf::st_intersects(lnd_nature_all)
  greenspace_solo = lnd_nature_all[lengths(gs_inter) == 1,]
  greenspace_inter = lnd_nature_all[lengths(gs_inter) > 1,]

  greenspace_inter = sf::st_union(greenspace_inter)

  lnd_rem = c(greenspace_inter, greenspace_solo)
  lnd_rem = sf::st_as_sf(lnd_rem)
  lnd_rem$type = "natural"
  names(lnd_rem)[1] = "geometry"
  sf::st_geometry(lnd_rem) = "geometry"

  rm(lnd_nature, lnd_nature2, greenspace_inter, greenspace_solo, lnd_nature_all)

  # Man Made
  lnd_man = osm_land[is.na(osm_land$natural),]
  lnd_man = lnd_man[,c("landuse","military","tourism","amenity","leisure")]
  lnd_man$type = dplyr::if_else(is.na(lnd_man$landuse), lnd_man$military, lnd_man$landuse)
  lnd_man$type = dplyr::if_else(is.na(lnd_man$type), lnd_man$tourism, lnd_man$type)
  lnd_man$type = dplyr::if_else(is.na(lnd_man$type), lnd_man$leisure, lnd_man$type)
  lnd_man$type = dplyr::if_else(is.na(lnd_man$type), lnd_man$amenity, lnd_man$type)
  lnd_man = lnd_man[lnd_man$type %in% c(landuse,military,tourism),]
  lnd_man = lnd_man[,"type"]
  lnd_man = rbind(lnd_man, os_land[!os_land$type %in% c("water","greenspace","woodland"),])

  lm_inter = sf::st_intersects(lnd_man)
  lm_solo = lnd_man[lengths(lm_inter) == 1,]
  lm_inter = lnd_man[lengths(lm_inter) > 1,]
  # Group by type and union
  lm_inter = dplyr::group_by(lm_inter, type)
  lm_inter = dplyr::summarise(lm_inter)
  lm_inter = sf::st_make_valid(lm_inter)
  lm_inter = nngeo::st_remove_holes(lm_inter, 10000)

  lm_inter = sf::st_cast(lm_inter,"MULTIPOLYGON")
  lm_inter = sf::st_cast(lm_inter,"POLYGON")

  lm_inter2 = sf::st_overlaps(lm_inter) # Just touching is fine
  lm_inter_solo = lm_inter[lengths(lm_inter2) == 0,]
  lm_inter_inter = lm_inter[lengths(lm_inter2) > 0,]

  lm_inter_inter = sf::st_cast(lm_inter_inter, "MULTIPOLYGON")
  lm_inter_inter$area = as.numeric(sf::st_area(lm_inter_inter))
  lm_inter_inter = lm_inter_inter[order(lm_inter_inter$area, decreasing = TRUE),]

  lm_inter3 = sf::st_overlaps(lm_inter_inter)
  # loop over and difference
  for(i in 1:nrow(lm_inter_inter)){
    sub = lm_inter_inter[i,]
    sub_inter = lm_inter_inter[lm_inter3[[i]],]
    sub = sf::st_difference(sub, sf::st_union(sub_inter))
    lm_inter_inter$geometry[[i]] = sub$geometry[[1]]
  }
  gt = sf::st_geometry_type(lm_inter_inter)
  lm_inter_intermp = lm_inter_inter[gt == "MULTIPOLYGON",]
  lm_inter_interp = lm_inter_inter[gt == "POLYGON",]
  lm_inter_interp = sf::st_cast(lm_inter_interp, "MULTIPOLYGON")
  lm_inter_inter = rbind(lm_inter_interp, lm_inter_intermp)
  lm_inter_inter$area = NULL

  lm_all = rbind(lm_solo, lm_inter_solo, lm_inter_inter)

  landcover = rbind(lm_all, lnd_rem)
  landcover = sf::st_cast(landcover, "MULTIPOLYGON")
  landcover = sf::st_cast(landcover, "POLYGON")
  landcover$area = as.numeric(sf::st_area(landcover))
  landcover = landcover[landcover$area > 10,]
  landcover
}

#' Split Lsoa Landuse
#'
#' @description Perform processing for split lsoa landuse.
#' @param landcover Input object or parameter named `landcover`.
#' @param bounds_lsoa_GB_full){ Input object or parameter named `bounds_lsoa_GB_full){`.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
split_lsoa_landuse = function(landcover, bounds_lsoa_GB_full){

  if( FALSE){
    # TESTING MODE
    buff = sf::st_buffer(bounds_lsoa_GB_full[bounds_lsoa_GB_full$LSOA21CD == "W01000542",],10000)
    #buff = sf::st_buffer(sf::st_sfc(sf::st_point(c(210668.20566749386, 554110.83121316193)), crs = 27700),100)
    bounds_lsoa_GB_full = bounds_lsoa_GB_full[buff,]
    landcover = landcover[buff,]
  }

  # Clean polygons
  # Error when duplicated points in the polygon
  # TopologyException: side location conflict at 210668.20566749386 554110.83121316193. This can occur if the input geometry is invalid.
  # TODO: Move this upstream
  landcover = sf::st_simplify(landcover,preserveTopology = TRUE, dTolerance = 0.1)

  # Resolve overlapping landuse
  over = sf::st_overlaps(landcover)
  over = lengths(over) > 0
  landcover_noover = landcover[!over, ]
  landcover_over = landcover[over, ]

  rm(landcover)

  # Rank land types
  landcover_over$type = factor(landcover_over$type,
   levels =  c("natural","nature_reserve",
               "landfill","quarry",
               "danger_area", "shooting_range", "range", "training_area",
               "industrial", "retail", "military", "commercial","recreation_ground","railway",
               "attraction",
               "golf_course","theme_park","water_park","park","stadium",
               "parking",
               "Air Transport","Education","Medical Care","Road Transport","Water Transport"
  ))
  landcover_over = landcover_over[order(landcover_over$type, decreasing = TRUE),]

  landcover_over$area = as.numeric(sf::st_area(landcover_over))
  landcover_over_small = landcover_over[landcover_over$area < 1e6,]
  landcover_over_large = landcover_over[landcover_over$area >= 1e6,]
  landcover_over_large = remove_small_holes(landcover_over_large)

  landcover_over = rbind(landcover_over_large, landcover_over_small)

  #landcover_over = sf::st_intersection(landcover_over)

  # Resolve overalpping land uses
  inter = sf::st_intersects(landcover_over)
  attributes(inter)$class = "list"

  g = igraph::graph_from_adj_list(inter)
  g = igraph::components(g)
  #memb = g$membership
  landcover_over$cluster = g$membership

  landcover_over = dplyr::group_split(landcover_over, cluster)
  landcover_over = purrr::map(landcover_over,try_inter,
                                    .progress = "Intersecting landcover")
  landcover_over <- dplyr::bind_rows(landcover_over)

  landcover_over_geom = sf::st_geometry_type(landcover_over)
  landcover_over_p = landcover_over[landcover_over_geom == "POLYGON",]
  landcover_over_mp = landcover_over[landcover_over_geom == "MULTIPOLYGON",]
  landcover_over_gc = landcover_over[landcover_over_geom == "GEOMETRYCOLLECTION",]
  landcover_over_mp = suppressWarnings(sf::st_cast(landcover_over_mp, "POLYGON"))
  landcover_over_gc = sf::st_collection_extract(landcover_over_gc, "POLYGON")
  landcover_over = rbind(landcover_over_p, landcover_over_mp, landcover_over_gc)

  rm(landcover_over_p, landcover_over_mp, landcover_over_gc)

  landcover_over = landcover_over[,names(landcover_noover)]
  landcover_over <- sf::st_make_valid(landcover_over)
  landcover = rbind(landcover_noover, landcover_over)

  # foo = landcover_over[landcover_over$area < 1e5,]
  # foo$id = as.character(1:nrow(foo))
  # st_precision(foo) = 0.001
  #st_intersects(foo)
  #qtm(foo[c(1,3:12),], fill = "id")

  rm(landcover_noover, landcover_over)


  # Remove Slivers
  landcover$area = as.numeric(sf::st_area(landcover))
  landcover = landcover[landcover$area > 1,]
  landcover$area = NULL

  lsoa_nonres = suppressWarnings(sf::st_intersection(bounds_lsoa_GB_full,landcover))
  lsoa_nonres = sf::st_collection_extract(lsoa_nonres,"POLYGON")
  lsoa_nonres = lsoa_nonres[!duplicated(lsoa_nonres$geometry),]

  lsoa_nointer = bounds_lsoa_GB_full[!bounds_lsoa_GB_full$LSOA21CD %in% lsoa_nonres$LSOA21CD,]

  lsoa_nonres = dplyr::group_split(lsoa_nonres,LSOA21CD, type)
  lsoa_nonres = purrr::map(lsoa_nonres, function(x){
    if(nrow(x)>1){
      geom = sf::st_union(x$geometry)
      x = sf::st_drop_geometry(x[1,])
      x$geometry = geom
      x = sf::st_as_sf(x)
    }
    x
  }, .progress = "Unioning non-residential")
  lsoa_nonres = dplyr::bind_rows(lsoa_nonres)
  lsoa_nonres = lsoa_nonres[as.numeric(sf::st_area(lsoa_nonres)) > 1,] # Remove tiny slivers
  lsoa_nonres = sf::st_make_valid(lsoa_nonres)

  #summary(sf::st_is_valid(lsoa_nonres))

  # Combine non-residential into a single geometry
  nonres_union = lsoa_nonres["geometry"]
  nonres_union = sf::st_cast(nonres_union,"MULTIPOLYGON")
  nonres_union = sf::st_cast(nonres_union,"POLYGON")
  nonres_inter = sf::st_intersects(nonres_union)
  nonres_inter = lengths(nonres_inter)
  x_inter = nonres_union[nonres_inter > 1,]
  x_solo = nonres_union[nonres_inter == 1,]

  inter2 = sf::st_intersects(x_inter)
  attributes(inter2)$class = "list"

  g = igraph::graph_from_adj_list(inter2)
  g = igraph::components(g)
  memb = g$membership
  x_inter$cluster = g$membership

  x_inter = dplyr::group_split(x_inter, cluster)
  x_inter = purrr::map(x_inter, sf::st_union, .progress = "Unioning non residential")
  x_inter <- sf::st_sfc(unlist(x_inter, recursive = FALSE), crs = 27700)
  x_inter <- sf::st_make_valid(x_inter)
  nonres_union <- c(x_inter, x_solo$geometry)

  # Sort as later group_split also sorts
  bounds_lsoa_GB_full = bounds_lsoa_GB_full[order(bounds_lsoa_GB_full$LSOA21CD),]

  inter3 <- sf::st_intersects(bounds_lsoa_GB_full, nonres_union)
  attributes(inter3)$class = "list"

  bounds_list <- dplyr::group_split(bounds_lsoa_GB_full, LSOA21CD, .keep = TRUE)

  nms <- sapply(bounds_list, function(x){x$LSOA21CD})
  if(!all(nms == bounds_lsoa_GB_full$LSOA21CD)){
    stop("Res and non res LSOA order does not match")
  }

  lsoa_res <- purrr::map2(bounds_list, inter3, function(x, y){
    suppressWarnings(sf::st_difference(x, sf::st_union(nonres_union[y])))
  }, .progress = "Difference")
  lsoa_res <- dplyr::bind_rows(lsoa_res)
  lsoa_res <- sf::st_cast(lsoa_res, "MULTIPOLYGON")

  # Remove Slivers
  lsoa_res <- suppressWarnings(sf::st_cast(lsoa_res, "POLYGON"))
  lsoa_res$area <- as.numeric(sf::st_area(lsoa_res))
  lsoa_res <- lsoa_res[lsoa_res$area > 1,]
  lsoa_res$perimiter <- as.numeric(sf::st_perimeter(lsoa_res))
  lsoa_res$apratio <- lsoa_res$perimiter / lsoa_res$area
  lsoa_res <- lsoa_res[lsoa_res$apratio < 0.2,]
  lsoa_res <- dplyr::group_by(lsoa_res, LSOA21CD)
  lsoa_res <- dplyr::summarise(lsoa_res)

  lsoa_res = sf::st_cast(lsoa_res, "MULTIPOLYGON")
  lsoa_res$type = "residential"

  lsoa_nointer = sf::st_cast(lsoa_nointer, "MULTIPOLYGON")
  lsoa_nointer$type = "residential"

  res = rbind(lsoa_res, lsoa_nonres, lsoa_nointer)
  res$area = as.numeric(sf::st_area(res))
  res = res[res$area > 10,]
  res



}


#' Fast St Difference
#'
#' @description Perform processing for fast st difference.
#' @param x Input data object.
#' @param y){ Input object or parameter named `y){`.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
fast_st_difference = function(x, y){
  inter = lengths(sf::st_intersects(y))
  y_inter = sf::st_union(y[inter > 1,])
  y_solo = sf::st_combine(y[inter == 1,])
  message(Sys.time()," Start difference")
  sf::st_difference(x,sf::st_union(y_inter,y_solo))
}



#' Remove Small Holes
#'
#' @description Perform processing for remove small holes.
#' @param df Input object or parameter named `df`.
#' @param min_size Input object or parameter named `min_size`.
#' @param max_ap_ratio Input object or parameter named `max_ap_ratio`.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
remove_small_holes = function(df, min_size = 5, max_ap_ratio = 1){
  geom = purrr::map(sf::st_geometry(df), remove_small_holes_single,
                    min_size = min_size,
                    max_ap_ratio = max_ap_ratio,
                    .progress = TRUE)
  sf::st_geometry(df) = sf::st_sfc(geom, crs = 27700)
  df
}





#' Remove Small Holes Single
#'
#' @description Perform processing for remove small holes single.
#' @param x Input data object.
#' @param min_size Input object or parameter named `min_size`.
#' @param max_ap_ratio Input object or parameter named `max_ap_ratio`.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
remove_small_holes_single = function(x, min_size = 5, max_ap_ratio = 1){
  if(length(x) > 1){
    vals = purrr::map(x[seq(2, length(x))], hole_area)
    areas = sapply(vals,`[`,1)
    perimeters = sapply(vals,`[`,2)
    ap_ratio = perimeters / areas
    is_small = areas < min_size
    is_thin = ap_ratio > max_ap_ratio
    x = sf::st_polygon(x[c(TRUE, !(is_small | is_thin))])
  }
  x
}

#' Hole Area
#'
#' @description Perform processing for hole area.
#' @param y){ Input object or parameter named `y){`.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
hole_area = function(y){
  y = sf::st_sfc(sf::st_polygon(list(y)), crs = 27700)
  c(as.numeric(sf::st_area(y)), as.numeric(sf::st_perimeter(y)))
}

#' Try Inter
#'
#' @description Perform processing for try inter.
#' @param x){ Input object or parameter named `x){`.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
try_inter = function(x){
  res = suppressMessages(suppressWarnings(try(sf::st_intersection(x), silent = TRUE)))
  if(inherits(res, "try-error")){
    res = sf::st_intersection(sf::st_set_precision(x, 10))
  }
  res
}



