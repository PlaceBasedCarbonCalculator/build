# LSOA land-use split - v2 (fast) ---------------------------------------------
#
# NEW CODE (2026-07): faster replacement for split_lsoa_landuse() in
# land_uses.R (which is unchanged, for cross-comparison). The v1 target
# (`bounds_lsoa_GB_full_landuse`) took ~44.6 h, almost all of it in five
# sequential single-threaded geometry stages. v2 parallelises each of them
# with furrr (the operations are per-cluster / per-zone independent and
# deterministic, so results match v1 up to row order):
#
#  1. per-cluster st_intersection of overlapping landcover (try_inter)
#  2. the landcover x LSOA st_intersection (chunked over LSOAs, each chunk
#     only ships the landcover polygons it can touch)
#  3. per LSOA/type union of non-residential pieces
#  4. per-cluster union of the non-residential layer
#  5. per-LSOA st_difference to derive the residential remainder (each
#     zone only ships the non-residential geometries it intersects)
#
# Suggested target definition (add alongside, do NOT replace, the v1
# target while comparing):
#
# tar_target(bounds_lsoa_GB_full_landuse_v2,{
#   split_lsoa_landuse_v2(landcover, bounds_lsoa_GB_full)
# }),


#' Split LSOA boundaries into residential/non-residential land uses (v2)
#'
#' @description Faster reimplementation of `split_lsoa_landuse()` - see the
#'   header of this file. Same cleaning rules, type ranking, sliver
#'   thresholds and output columns.
#' @param landcover Land-use polygons (`landcover` target).
#' @param bounds_lsoa_GB_full Full-resolution LSOA boundaries.
#' @param workers Number of parallel workers (default: all cores minus 1).
#' @return An `sf` data frame of LSOA x land-use type polygons, including
#'   the residential remainder of every zone.
#' @keywords internal
split_lsoa_landuse_v2 = function(landcover, bounds_lsoa_GB_full, workers = NULL){

  if(is.null(workers)){
    workers = max(1, future::availableCores() - 1)
  }
  future::plan("multisession", workers = workers)
  on.exit(future::plan("sequential"), add = TRUE)

  # Clean polygons (error when duplicated points in the polygon)
  landcover = sf::st_simplify(landcover, preserveTopology = TRUE, dTolerance = 0.1)

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

  # Resolve overlapping land uses, in parallel per intersection cluster
  inter = sf::st_intersects(landcover_over)
  attributes(inter)$class = "list"

  g = igraph::graph_from_adj_list(inter)
  g = igraph::components(g)
  landcover_over$cluster = g$membership

  landcover_over = dplyr::group_split(landcover_over, cluster)
  landcover_over = furrr::future_map(landcover_over, try_inter,
                                     .progress = TRUE,
                                     .options = furrr::furrr_options(seed = TRUE))
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

  rm(landcover_noover, landcover_over)

  # Remove Slivers
  landcover$area = as.numeric(sf::st_area(landcover))
  landcover = landcover[landcover$area > 1,]
  landcover$area = NULL

  # LSOA x landcover intersection, chunked over LSOAs. Subsets are built in
  # the main process so each worker only receives the zones of its chunk
  # and the landcover polygons those zones can touch (not the full layers)
  lc_hits = sf::st_intersects(bounds_lsoa_GB_full, landcover)
  chunks = split(seq_len(nrow(bounds_lsoa_GB_full)),
                 ceiling(seq_along(lc_hits) / 500))
  chunks = chunks[vapply(chunks, function(i){length(unlist(lc_hits[i])) > 0}, TRUE)]
  bounds_sub = purrr::map(chunks, function(i){bounds_lsoa_GB_full[i, ]})
  lc_sub = purrr::map(chunks, function(i){landcover[sort(unique(unlist(lc_hits[i]))), ]})
  lsoa_nonres = furrr::future_map2(bounds_sub, lc_sub, function(b, lc){
    suppressWarnings(sf::st_intersection(b, lc))
  }, .progress = TRUE, .options = furrr::furrr_options(seed = TRUE))
  lsoa_nonres = dplyr::bind_rows(lsoa_nonres)
  rm(lc_hits, chunks, bounds_sub, lc_sub)

  lsoa_nonres = sf::st_collection_extract(lsoa_nonres,"POLYGON")
  lsoa_nonres = lsoa_nonres[!duplicated(lsoa_nonres$geometry),]

  lsoa_nointer = bounds_lsoa_GB_full[!bounds_lsoa_GB_full$LSOA21CD %in% lsoa_nonres$LSOA21CD,]

  lsoa_nonres = dplyr::group_split(lsoa_nonres,LSOA21CD, type)
  lsoa_nonres = furrr::future_map(lsoa_nonres, function(x){
    if(nrow(x)>1){
      geom = sf::st_union(x$geometry)
      x = sf::st_drop_geometry(x[1,])
      x$geometry = geom
      x = sf::st_as_sf(x)
    }
    x
  }, .progress = TRUE, .options = furrr::furrr_options(seed = TRUE))
  lsoa_nonres = dplyr::bind_rows(lsoa_nonres)
  lsoa_nonres = lsoa_nonres[as.numeric(sf::st_area(lsoa_nonres)) > 1,] # Remove tiny slivers
  lsoa_nonres = sf::st_make_valid(lsoa_nonres)

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
  x_inter$cluster = g$membership

  x_inter = dplyr::group_split(x_inter, cluster)
  x_inter = furrr::future_map(x_inter, sf::st_union, .progress = TRUE,
                              .options = furrr::furrr_options(seed = TRUE))
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

  # Pre-subset the non-residential geometries per zone so the (large)
  # union layer is not exported to every worker
  nonres_by_zone <- purrr::map(inter3, function(y){nonres_union[y]})

  lsoa_res <- furrr::future_map2(bounds_list, nonres_by_zone, function(x, y){
    suppressWarnings(sf::st_difference(x, sf::st_union(y)))
  }, .progress = TRUE, .options = furrr::furrr_options(seed = TRUE))
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

  # Zones intersecting no landcover at all (0-row safe, unlike v1)
  if(nrow(lsoa_nointer) > 0){
    lsoa_nointer = sf::st_cast(lsoa_nointer, "MULTIPOLYGON")
    lsoa_nointer$type = "residential"
    res = rbind(lsoa_res, lsoa_nonres, lsoa_nointer)
  } else {
    res = rbind(lsoa_res, lsoa_nonres)
  }
  res$area = as.numeric(sf::st_area(res))
  res = res[res$area > 10,]
  res

}
