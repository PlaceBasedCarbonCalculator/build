combine_land_use = function(os_land, os_greenspace, osm_land){

  #OSM start with
  landuse = c("industrial", "retail", "military", "commercial", "landfill","quarry","recreation_ground","railway")
  military = c("danger_area", "shooting_range", "range", "training_area")
  tourism = c("theme_park","attraction")
  natural = c("wood","heath","wetland","scrub")
  leisure = c("golf_course","nature_reserve","water_park","park")
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
  lnd_man = lnd_man[,c("landuse","military","tourism")]
  lnd_man$type = dplyr::if_else(is.na(lnd_man$landuse), lnd_man$military, lnd_man$landuse)
  lnd_man$type = dplyr::if_else(is.na(lnd_man$type), lnd_man$tourism, lnd_man$type)
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

split_lsoa_landuse = function(landcover, bounds_lsoa_GB_full){

  if( FALSE){
    # TESTING MODE
    buff = sf::st_buffer(bounds_lsoa_GB_full[1,],10000)
    bounds_lsoa_GB_full = bounds_lsoa_GB_full[buff,]
    landcover = landcover[buff,]
  }

  landcover$area = NULL

  lsoa_nonres = suppressWarnings(sf::st_intersection(bounds_lsoa_GB_full,landcover))
  lsoa_nonres = sf::st_collection_extract(lsoa_nonres,"POLYGON")
  lsoa_nonres = lsoa_nonres[!duplicated(lsoa_nonres$geometry),]

  lsoa_nonres = dplyr::group_split(lsoa_nonres,LSOA21CD, type)
  lsoa_nonres = purrr::map(lsoa_nonres, function(x){
    if(nrow(x)>1){
      geom = sf::st_union(x$geometry)
      x = sf::st_drop_geometry(x[1,])
      x$geometry = geom
      x = sf::st_as_sf(x)
    }
    x
  }, .progress = "Unioning residential")
  lsoa_nonres = dplyr::bind_rows(lsoa_nonres)
  lsoa_nonres = lsoa_nonres[as.numeric(sf::st_area(lsoa_nonres)) > 1,] # Remove tiny slivers
  lsoa_nonres = sf::st_make_valid(lsoa_nonres)

  #summary(sf::st_is_valid(lsoa_nonres))

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

  inter3 <- sf::st_intersects(bounds_lsoa_GB_full, nonres_union)
  attributes(inter3)$class = "list"

  bounds_list <- dplyr::group_split(bounds_lsoa_GB_full, LSOA21CD, .keep = TRUE)

  lsoa_res <- purrr::map2(bounds_list, inter3, function(x, y){
    suppressWarnings(sf::st_difference(x, sf::st_union(nonres_union[y])))
  }, .progress = "Difference")
  lsoa_res <- dplyr::bind_rows(lsoa_res)
  lsoa_res <- sf::st_cast(lsoa_res, "MULTIPOLYGON")

  # Remove Slivers
  lsoa_res <- sf::st_cast(lsoa_res, "POLYGON")
  lsoa_res$area <- as.numeric(sf::st_area(lsoa_res))
  lsoa_res <- lsoa_res[lsoa_res$area > 1,]
  lsoa_res$perimiter <- as.numeric(sf::st_perimeter(lsoa_res))
  lsoa_res$apratio <- lsoa_res$perimiter / lsoa_res$area
  lsoa_res <- lsoa_res[lsoa_res$apratio < 0.2,]
  lsoa_res <- dplyr::group_by(lsoa_res, LSOA21CD)
  lsoa_res <- dplyr::summarise(lsoa_res)

  lsoa_res = sf::st_cast(lsoa_res, "MULTIPOLYGON")
  lsoa_res$type = "residential"

  res = rbind(lsoa_res, lsoa_nonres)
  res$area = as.numeric(sf::st_area(res))
  res = res[res$area > 10,]
  res
}


fast_st_difference = function(x, y){
  inter = lengths(sf::st_intersects(y))
  y_inter = sf::st_union(y[inter > 1,])
  y_solo = sf::st_combine(y[inter == 1,])
  message(Sys.time()," Start difference")
  sf::st_difference(x,sf::st_union(y_inter,y_solo))
}

# fast_st_intersects = function(x, y){
#   intersections <- sf::st_intersects(x, y)
#   res <- purrr::map(1:dim(x)[1], function(ix){
#     suppressWarnings(sf::st_intersection(x = x[ix,], y = y[intersections[[ix]],]))
#   })
#   dplyr::bind_rows(res)
# }
#
# bind_sf = function(x) {
#   if (length(x) == 0) stop("Empty list")
#   geom_name = attr(x[[1]], "sf_column")
#   geom_types = purrr::map_chr(x, function(x){class(x[[attributes(x)$sf_column]])[1]})
#   if(length(unique(geom_types)) != 1){
#     # Change to sfc_GEOMETRY
#     x = purrr::map(x, sf::st_cast)
#   }
#   x = data.table::rbindlist(x, use.names = FALSE)
#   x[[geom_name]] = sf::st_sfc(x[[geom_name]], recompute_bbox = TRUE)
#   x = sf::st_as_sf(x)
#   x
# }
#
# bench::mark(r1 = fast_st_intersects(x,y),
#             r2 = sf::st_intersection(x, y), check = FALSE
#             )
#
# r1 = fast_st_intersects(x1,y)
# r2 = sf::st_intersection(x1, y)
# summary(sf::st_is_empty(r1))
# r1$geometry[[384]]
#
# r1b = sf::st_collection_extract(r1)
# r2b = sf::st_collection_extract(r2)
#
# tmap_options(check.and.fix = TRUE)
# qtm(r1b, fill = "red") + qtm(r2b, fill = "blue")
#
#
# library(sf)
# library(dplyr)
# library(purrr)
# library(progress)
#
# intersections <- st_intersects(x = xFeatures, y = yFeatures)
#
# pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(xFeatures)[1])
#
# intersectFeatures <- map_dfr(1:dim(xFeatures)[1], function(ix){
#   pb$tick()
#   st_intersection(x = xFeatures[ix,], y = yFeatures[intersections[[ix]],])
# })
