# Compare v1 (existing targets) with the new _v2 functions --------------------
#
# Run interactively from the build/ directory. Nothing here is sourced by
# the pipeline. The v1 results are read straight from the targets store, so
# no v1 code needs to be re-run; the v2 functions are run on a small sample
# of zones and compared.

library(targets)
tar_source() # loads R/ including the new *_v2.R files

tar_load(parameters)

# ---- 1. England & Wales synthetic households --------------------------------

tar_load(synth_pop_seed)
v1_ew = dplyr::ungroup(tar_read(census21_synth_households))

set.seed(1)
sample_lsoa = sample(unique(v1_ew$LSOA21CD), 50)

t_v2 = system.time({
  v2_ew = sythetic_census_v2(path_data = file.path(parameters$path_data, "population"),
                             synth_pop_seed,
                             zone_subset = sample_lsoa)
})
print(t_v2)
#  user  system elapsed 
#  138.98   23.21  109.58 
# v1 took ~3.5 days for ~35,700 zones => ~8.5 s/zone; compare t_v2 / 50

# Per-zone diagnostics side by side
diag_v1 = dplyr::distinct(v1_ew[v1_ew$LSOA21CD %in% sample_lsoa,
                                c("LSOA21CD","MAE","conv","error_margins")])
diag_v2 = dplyr::distinct(v2_ew[, c("LSOA21CD","MAE","conv","error_margins")])
diag = dplyr::left_join(diag_v1, diag_v2, by = "LSOA21CD", suffix = c("_v1","_v2"))
print(summary(diag[, c("MAE_v1","MAE_v2")]))

# Cell-level agreement (v2 rows differ by integerisation noise only)
cmp = dplyr::full_join(
  v1_ew[v1_ew$LSOA21CD %in% sample_lsoa,
        c("LSOA21CD","hhSize5","CarVan5","Tenure5","AccType5","hhComp15","households")],
  v2_ew[, c("LSOA21CD","hhSize5","CarVan5","Tenure5","AccType5","hhComp15","households")],
  by = c("LSOA21CD","hhSize5","CarVan5","Tenure5","AccType5","hhComp15"),
  suffix = c("_v1","_v2"))
cmp[is.na(cmp)] = 0
cat("households cor:", cor(cmp$households_v1, cmp$households_v2), "\n")
cat("mean abs diff per cell:", mean(abs(cmp$households_v1 - cmp$households_v2)), "\n")
zone_tot = dplyr::summarise(dplyr::group_by(cmp, LSOA21CD),
                            v1 = sum(households_v1), v2 = sum(households_v2))
print(summary(zone_tot$v1 - zone_tot$v2))

# ---- 2. Scotland synthetic households ---------------------------------------

tar_load(synth_pop_seed_scotland)
v1_scot = dplyr::ungroup(tar_read(scot_synth_households))

set.seed(1)
sample_dz = sample(unique(v1_scot$LSOA21CD), 50)

t_v2s = system.time({
  v2_scot = sythetic_census_scot_v2(path_data = file.path(parameters$path_data, "population_scotland"),
                                    synth_pop_seed_scotland,
                                    zone_subset = sample_dz)
})
print(t_v2s)

cmp_s = dplyr::full_join(
  v1_scot[v1_scot$LSOA21CD %in% sample_dz,
          c("LSOA21CD","householdComp10","CarVan5","Tenure5","hhSize5","AccType7","households")],
  v2_scot[, c("LSOA21CD","householdComp10","CarVan5","Tenure5","hhSize5","AccType7","households")],
  by = c("LSOA21CD","householdComp10","CarVan5","Tenure5","hhSize5","AccType7"),
  suffix = c("_v1","_v2"))
cmp_s[is.na(cmp_s)] = 0
cat("households cor:", cor(cmp_s$households_v1, cmp_s$households_v2), "\n")
cat("mean abs diff per cell:", mean(abs(cmp_s$households_v1 - cmp_s$households_v2)), "\n")

# ---- 3. High-detail buildings -----------------------------------------------
# Compare on a subset of buildings (the full run is the slow part). Uses the
# stored inputs; v1 result read from the store for the same ids.

tar_load(buildings_heights)
tar_load(bounds_lsoa_GB_full)

set.seed(1)
n_test = 200000
rows = sort(sample(nrow(buildings_heights), n_test))

t_b = system.time({
  v2_b = process_buildings_high_v2(buildings_heights[rows, ], bounds_lsoa_GB_full)
})
print(t_b) # scale to nrow(buildings_heights) for the full-run estimate

v1_b = tar_read(buildings_lsoa_4326_high)

# Total footprint area per LSOA should agree closely (differences come from
# the dropped line/point slivers and validity repairs)
a2 = dplyr::summarise(dplyr::group_by(sf::st_drop_geometry(
       dplyr::mutate(v2_b, area = as.numeric(sf::st_area(v2_b)))), LSOA21CD),
       area_v2 = sum(area))
print(head(a2))

# ---- 4. Pipeline-audit v2 functions (2026-07) -------------------------------
# The same pattern applies to the other audit rewrites; run each v2 function
# and compare against the stored v1 target. Uncomment as needed.

## uprn_historical (v1 ~3.7 h; v2 aggregates in DuckDB)
# t_u = system.time({
#   v2_u = load_uprn_historical_v2(file.path(parameters$path_data,"os_uprn/osopenuprn_2020_2025_all.zip"))
# })
# print(t_u)
# v1_u = tar_read(uprn_historical)
# stopifnot(nrow(v1_u) == nrow(v2_u))
# i = sample(nrow(v1_u), 1e5)
# print(all.equal(as.data.frame(v1_u[i,]), as.data.frame(v2_u[match(v1_u$UPRN[i], v2_u$UPRN),]),
#                 check.attributes = FALSE))

## inspire (v1 ~7.3 h sequential; v2 parallel over LA zips)
# t_i = system.time({
#   v2_i = load_inspire_v2(file.path(parameters$path_data,"INSPIRE"))
# })
# print(t_i)
# v1_i = tar_read(inspire)
# print(c(v1 = nrow(v1_i), v2 = nrow(v2_i)))  # identical processing => equal

## bounds_lsoa_GB_full_landuse (v1 ~44.6 h; v2 parallelises the 5 slow stages)
# tar_load(landcover)
# t_l = system.time({
#   v2_l = split_lsoa_landuse_v2(landcover, bounds_lsoa_GB_full)
# })
# print(t_l)
# v1_l = tar_read(bounds_lsoa_GB_full_landuse)
# a_cmp = dplyr::full_join(
#   dplyr::summarise(dplyr::group_by(sf::st_drop_geometry(v1_l), LSOA21CD, type), a1 = sum(area)),
#   dplyr::summarise(dplyr::group_by(sf::st_drop_geometry(v2_l), LSOA21CD, type), a2 = sum(area)),
#   by = c("LSOA21CD","type"))
# print(summary(a_cmp$a1 - a_cmp$a2))

## buildings (v1 ~8.7 h; v2 clips against INSPIRE in DuckDB)
# tar_load(osm_buildings); tar_load(os_buildings); tar_load(inspire); tar_load(inspire_scotland)
# t_c = system.time({
#   v2_c = combine_os_osm_buildings_v2(osm_buildings, os_buildings, inspire, inspire_scotland)
# })
# print(t_c)

## buildings_lsoa_4326_med (v1 ~4.7 h; v2 via duckdb_clip_join)
# t_m = system.time({
#   v2_m = process_buildings_generic_v2(
#     path = file.path(parameters$path_data,"os_zoomstack/OS_Open_Zoomstack/OS_Open_Zoomstack.gpkg"),
#     bounds_lsoa_GB_full, scale = "med")
# })
# print(t_m)
