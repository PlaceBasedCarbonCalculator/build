# Buildings x LSOA - v2 (fast) ------------------------------------------------
#
# NEW CODE (2026-07): faster replacements for process_buildings_high() (~32 h)
# and process_buildings_generic() (~6 h at "med" scale) in os_zoomstack.R
# (which are unchanged, for cross-comparison). Differences from v1:
#
#  1. The zone polygons are loaded into DuckDB once (with an RTREE index),
#     not re-imported for every 2M-building chunk (high) or joined with
#     single-threaded sf::st_join (med).
#  2. Boundary-straddling buildings are clipped inside the join query
#     (ST_Intersection in the SELECT), so the sequential per-building
#     split_merge()/split_buildings() loop disappears entirely.
#  3. st_make_valid() runs only on geometries that fail st_is_valid().
#
# Semantic differences vs v1 (all intentional):
#  - Only polygonal intersection pieces are kept; buildings that merely
#    touch a zone along an edge or corner produced degenerate line/point
#    slivers in v1, v2 drops them explicitly.
#  - Buildings intersecting no zone are dropped (inner join), matching
#    ddbs_join; pass left = TRUE to duckdb_clip_join() to keep them.
#
# Suggested target definitions (add alongside, do NOT replace, the v1
# targets while comparing):
#
# tar_target(buildings_lsoa_4326_high_v2,{
#   process_buildings_high_v2(buildings_heights, bounds_lsoa_GB_full)
# }),
# tar_target(buildings_lsoa_4326_med_v2,{
#   process_buildings_generic_v2(path = file.path(parameters$path_data,"os_zoomstack/OS_Open_Zoomstack/OS_Open_Zoomstack.gpkg"),
#                                bounds_lsoa_GB_full, scale = "med")
# }),


#' Spatial join + clip against zone polygons inside DuckDB
#'
#' @description Joins every feature of `x` to the zone(s) it intersects and
#'   clips features straddling a zone boundary to the zone (features fully
#'   within a zone keep their original geometry). The zones are loaded into
#'   an in-memory DuckDB with an RTREE index once; `x` is processed in
#'   chunks. Only `x`'s geometry crosses into DuckDB - attribute columns
#'   are re-attached in R by row id, so wide inputs cost nothing extra.
#'   Only polygonal result pieces are kept (edge/corner touches yield
#'   degenerate lines/points, which are dropped).
#' @param x `sf` object to join/clip (any attribute columns, projected CRS).
#' @param zones `sf` polygon layer; cast to MULTIPOLYGON internally.
#' @param zone_col Name of the zone id column in `zones`, attached to the
#'   result.
#' @param chunk_size Features of `x` per query (memory / progress
#'   granularity).
#' @param left If TRUE, features of `x` intersecting no zone are kept with
#'   NA in `zone_col` and their original geometry (like a left join).
#' @return `sf`: all columns of `x`, plus `zone_col`, one row per feature x
#'   intersected zone, geometry clipped to the zone. Same CRS as `x`.
#' @keywords internal
duckdb_clip_join = function(x, zones, zone_col = "LSOA21CD",
                            chunk_size = 2e6, left = FALSE){

  if(!requireNamespace("duckdb", quietly = TRUE) ||
     !requireNamespace("DBI", quietly = TRUE)){
    stop("duckdb_clip_join requires the duckdb and DBI packages")
  }

  crs_in = sf::st_crs(x)
  zones = sf::st_cast(zones, "MULTIPOLYGON", warn = FALSE)
  zone_vals = zones[[zone_col]]

  con = DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;")

  message(Sys.time(), " loading ", nrow(zones), " zones into DuckDB")
  zdf = data.frame(zid = seq_len(nrow(zones)))
  zdf$wkb = sf::st_as_binary(sf::st_geometry(zones))
  duckdb::duckdb_register(con, "zones_src", zdf)
  DBI::dbExecute(con, "CREATE TABLE zones AS SELECT zid, ST_GeomFromWKB(wkb) AS geom FROM zones_src")
  duckdb::duckdb_unregister(con, "zones_src")
  DBI::dbExecute(con, "CREATE INDEX zones_rtree ON zones USING RTREE (geom)")
  rm(zdf)

  message(Sys.time(), " starting spatial join, nrow = ", nrow(x))

  geom_all = sf::st_geometry(x)
  attr_all = sf::st_drop_geometry(x)

  idx = split(seq_len(nrow(x)), ceiling(seq_len(nrow(x)) / chunk_size))

  result = vector("list", length(idx))
  for(k in seq_along(idx)){
    i = idx[[k]]

    bdf = data.frame(rid = i)
    bdf$wkb = sf::st_as_binary(geom_all[i])
    duckdb::duckdb_register(con, "b_src", bdf)
    DBI::dbExecute(con, "CREATE OR REPLACE TABLE b AS SELECT rid, ST_GeomFromWKB(wkb) AS geom FROM b_src")
    duckdb::duckdb_unregister(con, "b_src")

    res_k = DBI::dbGetQuery(con, "
      SELECT b.rid, z.zid,
             ST_AsWKB(CASE WHEN ST_Within(b.geom, z.geom) THEN b.geom
                           ELSE ST_Intersection(b.geom, z.geom) END) AS wkb
      FROM b JOIN zones z ON ST_Intersects(b.geom, z.geom)")

    g = sf::st_as_sfc(structure(res_k$wkb, class = "WKB"), crs = crs_in)
    out_k = attr_all[res_k$rid, , drop = FALSE]
    out_k[[zone_col]] = zone_vals[res_k$zid]
    if(left){
      out_k$.rid = res_k$rid
    }
    out_k = sf::st_sf(out_k, geometry = g)

    # Keep polygonal pieces only
    gt = as.character(sf::st_geometry_type(out_k))
    if(any(gt == "GEOMETRYCOLLECTION")){
      gc = suppressWarnings(
        sf::st_collection_extract(out_k[gt == "GEOMETRYCOLLECTION", ], "POLYGON")
      )
      out_k = rbind(out_k[gt != "GEOMETRYCOLLECTION", ], gc)
    }
    out_k = out_k[!sf::st_is_empty(out_k), ]
    out_k = out_k[!is.na(sf::st_dimension(out_k)) & sf::st_dimension(out_k) == 2, ]

    result[[k]] = out_k
    message(Sys.time(), " chunk ", k, "/", length(idx), " joined, rows = ", nrow(out_k))
  }

  res = dplyr::bind_rows(result)

  if(left){
    unmatched = setdiff(seq_len(nrow(x)), unique(res$.rid))
    res$.rid = NULL
    if(length(unmatched) > 0){
      extra = attr_all[unmatched, , drop = FALSE]
      extra[[zone_col]] = NA
      extra = sf::st_sf(extra, geometry = geom_all[unmatched])
      res = rbind(res, extra)
    }
  }

  message(Sys.time(), " spatial join and clip complete nrow = ", nrow(res))
  res
}


#' Process high-detail zoomstack buildings (v2, DuckDB join + clip)
#'
#' @description Faster replacement for `process_buildings_high()` - see the
#'   header of this file. Joins every building to the LSOA(s) it intersects
#'   and clips boundary-straddling buildings, all inside one persistent
#'   DuckDB connection.
#' @param buildings_heights An `sf` object with building heights
#'   (`height_max`) and geometry, in a projected CRS.
#' @param bounds_lsoa_GB_full Full-resolution LSOA boundaries with
#'   `LSOA21CD`.
#' @param chunk_size Number of buildings per query.
#' @return An `sf` object (EPSG:4326) with columns height, id, LSOA21CD.
#' @keywords internal
process_buildings_high_v2 = function(buildings_heights, bounds_lsoa_GB_full,
                                     chunk_size = 2e6) {

  buildings_heights <- buildings_heights[, c("height_max", "geometry")]
  names(buildings_heights)[names(buildings_heights) == "height_max"] <- "height"
  buildings_heights$id <- 1:nrow(buildings_heights)

  buildings_out = duckdb_clip_join(buildings_heights,
                                   bounds_lsoa_GB_full[, "LSOA21CD"],
                                   zone_col = "LSOA21CD",
                                   chunk_size = chunk_size)

  # Transform, then repair only the geometries that need it
  buildings_out <- sf::st_transform(buildings_out, 4326)
  invalid = !sf::st_is_valid(buildings_out)
  invalid[is.na(invalid)] = TRUE
  if(any(invalid)){
    message(Sys.time(), " repairing ", sum(invalid), " invalid geometries")
    sf::st_geometry(buildings_out)[invalid] = sf::st_make_valid(sf::st_geometry(buildings_out)[invalid])
  }

  buildings_out
}


#' Process zoomstack buildings/urban areas (v2, DuckDB join + clip)
#'
#' @description Faster replacement for `process_buildings_generic()` - see
#'   the header of this file. Same layer selection and scale filtering,
#'   with the sf::st_join + split_merge() steps replaced by
#'   `duckdb_clip_join()`.
#' @param path Path to the OS Open Zoomstack GeoPackage.
#' @param bounds Full-resolution LSOA boundaries with `LSOA21CD`.
#' @param scale One of "med", "low", "verylow".
#' @param chunk_size Number of features per query.
#' @return An `sf` object (EPSG:4326) joined to LSOA boundaries.
#' @keywords internal
process_buildings_generic_v2 = function(path = "../inputdata/os_zoomstack/OS_Open_Zoomstack/OS_Open_Zoomstack.gpkg",
                                        bounds, scale = "med",
                                        chunk_size = 2e6) {

  if(scale == "med"){
    layer = "district_buildings"
  } else if (scale == "low") {
    layer = "urban_areas"
  } else if (scale == "verylow"){
    layer = "urban_areas"
  } else {
    stop("Unknown scale")
  }

  b <- sf::st_read(path, layer = layer, quiet = TRUE)

  if(scale == "low"){
    b = b[b$type == "Regional",]
    b$type = NULL
  }
  if(scale == "verylow"){
    b = b[b$type == "National",]
    b$type = NULL
  }

  b <- change_geom_name(b)
  b$id <- 1:nrow(b)

  b = duckdb_clip_join(b, bounds[, "LSOA21CD"], zone_col = "LSOA21CD",
                       chunk_size = chunk_size)

  b <- sf::st_transform(b, 4326)
  invalid = !sf::st_is_valid(b)
  invalid[is.na(invalid)] = TRUE
  if(any(invalid)){
    sf::st_geometry(b)[invalid] = sf::st_make_valid(sf::st_geometry(b)[invalid])
  }

  b
}
