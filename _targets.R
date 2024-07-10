# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(sf)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble","sf","readODS","readxl","dplyr","tidyr","smoothr",
               "osmextract","nngeo","pbapply","stplanr","rmapshaper",
               "igraph","plyr","terra","furrr","future") # packages that your targets need to run
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(


# Set Up ------------------------------------------------------------------

# Detect when parameter file has changed:
tar_target(name = param_file, command = "parameters.json", format = "file"),
# Check Renviron exists, create output directory, load params:
tar_target(parameters, {
  p = jsonlite::read_json(param_file, simplifyVector = TRUE)
  p
}),

# Download Input Datasets -------------------------------------------------
# Population
tar_target(population_2002_2020,{
  path = file.path(parameters$path_data,"population")
  dowload_lsoa_population(path)
  pop = build_lsoa_population(path)
  pop
}),

tar_target(population_oa21,{
  load_oa_population(path = file.path(parameters$path_data,"population"))
}),

tar_target(population_2021,{
  dl_nomis
  load_population_2021(path = file.path(parameters$path_data,"nomis"))
}),

tar_target(population_2022,{
  length(population_2002_2020)
  build_lsoa_population_2022(path = file.path(parameters$path_data,"population"))
}),

tar_target(population_scot,{
  load_scotland_population(path = file.path(parameters$path_data,"population_scotland"))
}),

tar_target(population,{
  combine_populations(population_2002_2020, population_2021, population_2022, population_scot, lookup_lsoa_2011_21)
}),

tar_target(lsoa_11_21_tools,{
  lsoa_convert_2011_2021_pre_data(lookup_lsoa_2011_21, population_2021)
}),






# Gas and Electricity
tar_target(dl_gas_electric,{
  dowload_gas_electric(path = file.path(parameters$path_data,"gas_electric"))
}),
tar_target(domestic_gas_11,{
  load_lsoa_gas(dl_gas_electric)
}),
tar_target(domestic_electricity_11,{
  load_lsoa_electric(dl_gas_electric)
}),
tar_target(domestic_gas,{
  lsoa_gas_to_2021(domestic_gas_11, lsoa_11_21_tools)
}),
tar_target(domestic_electricity,{
  lsoa_electric_to_2021(domestic_electricity_11, lsoa_11_21_tools)
}),
tar_target(domestic_gas_emissions,{
  calculate_gas_emissions(domestic_gas, emissions_factors, population)
}),
tar_target(domestic_electricity_emissions,{
  calculate_electricity_emissions(domestic_electricity, emissions_factors, population)
}),
tar_target(nondomestic_gas,{
  load_msoa_gas_nondom(dl_gas_electric)
}),
tar_target(nondomestic_electricity,{
  load_msoa_electric_nondom(dl_gas_electric)
}),

# Boundaries
tar_target(dl_boundaries,{
  download_boundaries(path = file.path(parameters$path_data,"boundaries"))
}),
tar_target(bounds_la,{
  read_bounds_la(dl_boundaries)
}),
tar_target(bounds_wards,{
  read_bounds_wards(dl_boundaries)
}),
tar_target(bounds_parish,{
  read_bounds_parish(dl_boundaries)
}),
tar_target(bounds_westminster,{
  read_bounds_westminster(dl_boundaries)
}),
tar_target(bounds_lsoa21_full,{
  read_bounds_lsoa_full(dl_boundaries)
}),
tar_target(bounds_lsoa11_full,{
  read_bounds_lsoa11_full(dl_boundaries)
}),
tar_target(bounds_lsoa21_generalised,{
  read_bounds_lsoa_generalised(dl_boundaries)
}),
tar_target(bounds_lsoa21_super_generalised,{
  read_bounds_lsoa_super_generalised(dl_boundaries)
}),
tar_target(bounds_dz11,{
  read_bounds_dz11(dl_boundaries)
}),
tar_target(centroids_lsoa11,{
  read_centroids(dl_boundaries)
}),
tar_target(centroids_dz11,{
  read_centroids_dz11(dl_boundaries)
}),
tar_target(centroids_oa21,{
  read_centroids_oa21(dl_boundaries)
}),
tar_target(bounds_postcodes,{
  read_postcodes(path = file.path(parameters$path_secure_data,"Postcodes/Postcode Polygons/Postcodes_20200826.zip"))
}),
tar_target(bounds_postcode_area,{
  make_postcode_areas(bounds_postcodes)
}),
tar_target(lookup_lsoa_2011_21,{
  load_LSOA_2011_2021_lookup(dl_boundaries)
}),
tar_target(lookup_lsoa_2001_11,{
  load_LSOA_2001_2011_lookup(dl_boundaries)
}),
tar_target(lookup_OA_LSOA_MSOA_classifications,{
  load_OA_LSOA_MSOA_class_2011_lookup(dl_boundaries)
}),

tar_target(bounds_lsoa_GB_full,{
  combine_lsoa_bounds(bounds_lsoa21_full, bounds_dz11, keep = 1)
}),
tar_target(bounds_lsoa_GB_generalised,{
  combine_lsoa_bounds(bounds_lsoa21_generalised, bounds_dz11, keep = 0.2)
}),
tar_target(bounds_lsoa_GB_super_generalised,{
  combine_lsoa_bounds(bounds_lsoa21_super_generalised, bounds_dz11, keep = 0.05)
}),

# Points of Interest
tar_target(poi,{
  read_os_poi(path = file.path(parameters$path_secure_data,"OS/Points of Intrest/2023/Download_2300307.zip"),
              path_types = file.path(parameters$path_data,"poi/poi_types.csv"))
}),

# Land Use
tar_target(os_land,{
  zoomstack_sites(dl_os_zoomstack)
}),

tar_target(os_greenspace,{
  load_os_greenspace(path = file.path(parameters$path_data,"os_greenspace"))
}),

tar_target(osm_land,{
  read_osm_pbf_landuse(path = file.path(parameters$path_data,"osm"))
}),

tar_target(osm_buildings,{
  read_osm_pbf_buildings(path = file.path(parameters$path_data,"osm"))
}),


tar_target(landcover,{
  combine_land_use(os_land, os_greenspace, osm_land)
}),

tar_target(bounds_lsoa_GB_full_landuse,{
  split_lsoa_landuse(landcover, bounds_lsoa_GB_full)
}),


# Inspire polygons
tar_target(inspire,{
  load_inspire(path = file.path(parameters$path_data,"INSPIRE"))
}),

tar_target(inspire_scotland,{
  load_inspire_scotland(path = file.path(parameters$path_data,"INSPIRE_scotland"))
}),


# Contextual Data
tar_target(dl_area_classifications,{
  download_area_classifications(path = file.path(parameters$path_data,"area_classifications"))
}),
tar_target(area_classifications,{
  load_area_classifications(dl_area_classifications)
}),

# Census
tar_target(dl_nomis,{
  dowload_nomis(path = file.path(parameters$path_data,"nomis"))
}),


# Income
tar_target(dl_income,{
  dowload_income_msoa(path = file.path(parameters$path_data,"income"))
}),

tar_target(income_msoa,{
  dl_income
  load_msoa_income(path = file.path(parameters$path_data,"income"))
}),

tar_target(experian_income,{
  load_experian_income(path = file.path(parameters$path_secure_data,"CREDS Data/Tim Share/From Malcolm/Experian.zip"))
}),

tar_target(income_lsoa,{
  estimate_income(experian_income, income_msoa, lookup_lsoa_2001_11, lookup_OA_LSOA_MSOA_classifications, lookup_lsoa_2011_21)
}),


# Car Registration Statistics
tar_target(dl_vehicle_registrations,{
  download_dft_vehicle_registrations(path = file.path(parameters$path_data,"vehicle_registrations"))
}),
tar_target(vehicle_registrations,{
  # Long running target ~2 hour
  load_dft_vehicle_registrations(dl_vehicle_registrations)
}),
tar_target(ulev_registrations,{
  # Long running target ~2 hour
  load_dft_ulev_registrations(dl_vehicle_registrations)
}),
tar_target(ev_registrations,{
  # Long running target ~2 hour
  load_dft_ev_registrations(dl_vehicle_registrations)
}),
# Car Emissions
tar_target(car_emissions_11,{
  load_car_emissions(path = file.path(parameters$path_secure_data,"CREDS Data/github-secure-data/Historical_Car_Emissions_LSOA.zip"))
}),
tar_target(car_emissions_perkm,{
  car_emissions_to_21(car_emissions_11, lsoa_11_21_tools)
}),
tar_target(car_emissions,{
  calculate_car_emissions(car_km_lsoa, car_emissions_perkm, population)
}),

#Car &  Van km (2009-2011 LSOA)
tar_target(car_km_2009_2011,{
  read_motoring_along(path = file.path(parameters$path_secure_data,"CREDS Data/Tim Share/From Tim/MOT Data RACv9.3"))
}),

tar_target(car_km_pc,{
  read_mot_km_pc(path = file.path(parameters$path_secure_data,"CARS/Anoymised MOT/clean/postcode_total_vkm_2005_2023.Rds"))
}),

tar_target(car_km_lsoa_11,{
  extraplote_car_km_trends(car_km_pc, car_km_2009_2011, centroids_lsoa11, centroids_dz11)
}),

tar_target(car_km_lsoa,{
  car_km_11_to_21(car_km_lsoa_11, lsoa_11_21_tools)
}),

# Public Transport Frequency
tar_target(pt_frequency,{
  load_pt_frequency(parameters$path_data)
}),

tar_target(pt_json,{
  ptf = pt_frequency
  names(ptf) = gsub("Morning_Peak","MorningPeak",names(ptf))
  names(ptf) = gsub("Afternoon_Peak","AfternoonPeak",names(ptf))
  ptf = tidyr::pivot_longer(ptf, cols = tph_weekday_MorningPeak_2004_2:tph_daytime_avg_2023_4,
                            names_prefix = "tph_",
                            names_sep = "_",
                            names_to = c("day","time","year","mode"))
  ptf = tidyr::pivot_wider(ptf, names_from = c("day","time","mode"), values_from = "value", id_cols = c("zone_id","year"))
  export_zone_json(ptf, idcol = "zone_id", rounddp = 2, path = "outputdata/json/pt_frequency", dataframe = "columns",
                   reduce = FALSE)
}),

tar_target(transport_lsoa_data,{
  select_transport_vars(pt_frequency)
}),


# TODO: EPCs

# Housing (Age, Building Type, Non-Gas Emissions, heating)
tar_target(building_age_2011,{
  load_building_age_2011(path = file.path(parameters$path_secure_data,"CDRC/building age price"))
}),

tar_target(housing_type_2021,{
  dl_nomis
  load_housing_type_2021(path = file.path(parameters$path_data,"nomis"))
}),

tar_target(central_heating_2021,{
  dl_nomis
  load_central_heating_2021(path = file.path(parameters$path_data,"nomis"))
}),

tar_target(central_heating_2011,{
  load_central_heating_2011(path = file.path(parameters$path_data,"nomis","2011"))
}),

tar_target(central_heating_2011_21,{
  central_heating_2011_to_2021(central_heating_2011, lsoa_11_21_tools)
}),

tar_target(other_heating_emissions,{
  calculate_other_heating(central_heating_2021, central_heating_2011_21, domestic_gas, population)
}),

# Travel to Work
tar_target(travel2work,{
  load_travel2work(path = file.path(parameters$path_secure_data,"LSOA Flow Data/Public/WM12EW[CT0489]_lsoa.zip"),
                   cents = centroids_lsoa11)
}),

# Travel to School
tar_target(dl_pct,{
  download_pct(path = file.path(parameters$path_data,"pct"))
}),

#TODO: travel emissions
# Other Travel

# Consumption
tar_target(dl_consumption,{
  download_consumption_footprint(path = file.path(parameters$path_data,"consumption"))
}),

tar_target(consumption_uk,{
  load_consumption_footprint(dl_consumption)
}),

tar_target(consumption_income,{
  load_consumption_income(path = file.path(parameters$path_data,"consumption"))
}),

tar_target(consumption_emissions,{
  calculate_consumption_lsoa(consumption_uk, consumption_income, population, income_lsoa, domestic_electricity)
}),


# Accessibility Analysis
tar_target(access_poi_circle_15min,{
  zones = sf::st_buffer(centroids_lsoa11, 1609.34 * 0.75)# 15 min * 3 mph
  access_counts(zones, poi, centroids_oa21, population_oa21)
}),

tar_target(access_poi_circle_30min,{
  zones = sf::st_buffer(centroids_lsoa11, 1609.34 * 1.5)
  access_counts(zones, poi, centroids_oa21, population_oa21)
}),

tar_target(access_poi_circle_45min,{
  zones = sf::st_buffer(centroids_lsoa11, 1609.34 * 2.25)
  access_counts(zones, poi, centroids_oa21, population_oa21)
}),

tar_target(access_poi_circle_60min,{
  zones = sf::st_buffer(centroids_lsoa11, 1609.34 * 3)
  access_counts(zones, poi, centroids_oa21, population_oa21)
}),

tar_target(lookup_oa2021_lsoa2011,{
  oa2021tolsoa2011(centroids_oa21, centroids_lsoa11)
}),

tar_target(access_poi_iso_15min,{
  zones = ons_isochrones[ons_isochrones$iso_cutoff == 900,] # 15 min
  zones = zones[zones$OA21CD %in% lookup_oa2021_lsoa2011$nearest_OA2021,]
  access_counts(zones, poi, centroids_oa21, population_oa21)
}),

tar_target(access_poi_iso_30min,{
  zones = ons_isochrones[ons_isochrones$iso_cutoff == 1800,]
  zones = zones[zones$OA21CD %in% lookup_oa2021_lsoa2011$nearest_OA2021,]
  access_counts(zones, poi, centroids_oa21, population_oa21)
}),

tar_target(access_poi_iso_45min,{
  zones = ons_isochrones[ons_isochrones$iso_cutoff == 2700,]
  zones = zones[zones$OA21CD %in% lookup_oa2021_lsoa2011$nearest_OA2021,]
  access_counts(zones, poi, centroids_oa21, population_oa21)
}),

tar_target(access_poi_iso_60min,{
  zones = ons_isochrones[ons_isochrones$iso_cutoff == 3600,]
  zones = zones[zones$OA21CD %in% lookup_oa2021_lsoa2011$nearest_OA2021,]
  access_counts(zones, poi, centroids_oa21, population_oa21)
}),

#TODO: add the summary data and JSON creation, fucntions in assecc_proximity_funcs.R
tar_target(access_proximity,{
  summarise_access_proximity(access_poi_circle_15min,access_poi_iso_15min,
                             access_poi_circle_30min,access_poi_iso_30min,
                             access_poi_circle_45min,access_poi_iso_45min,
                             access_poi_circle_60min,access_poi_iso_60min,
                             lookup_oa2021_lsoa2011,area_classifications
  )
}),



# Isochrones
tar_target(ons_isochrones,{
  load_ons_isochrones(path = file.path(parameters$path_secure_data,"ONS Isochrones"))
}),

# Flights
#TODO: Switch to permanent paths
tar_target(flights_od,{
  load_flights_od()
}),

tar_target(flights_airports,{
  load_flights_airports(bounds_la = bounds_la)
}),

tar_target(flights_total_emissions,{
  get_flights_total_emissions(flights_od, flights_airports)
}),

tar_target(flights_lsoa_emissions,{
  get_flights_lsoa_emissions(flights_total_emissions, income_lsoa, population)
}),

# Transit Stops

# Emissions Footprints
tar_target(emissions_factors,{
  load_emissions_factors()
}),

tar_target(lsoa_emissions_all,{
  combine_lsoa_emissions(flights_lsoa_emissions,consumption_emissions,
                         car_emissions,domestic_electricity_emissions,
                         domestic_gas_emissions,other_heating_emissions)
}),

# PLEF Forecasts
tar_target(PLEF,{
  load_plef(path = file.path(parameters$path_data,"PLEF"))
}),

tar_target(PLEF_emissions,{
  forcast_emissions_plef(lsoa_emissions_all, PLEF)
}),

tar_target(lsoa_emissions_all_forcasts,{
  dplyr::left_join(lsoa_emissions_all, PLEF_emissions, by = "LSOA21CD")
}),

# OS data
tar_target(dl_os_zoomstack,{
  download_os_zoomstack(path = file.path(parameters$path_data,"os_zoomstack"))
}),

tar_target(os_buildings,{
  zoomstack_buildings_high(dl_os_zoomstack)
}),

tar_target(os_10k_grid,{
  load_os_10k_grid(path = file.path(parameters$path_data,"os_grid/os_bng_grids.gpkg"))
}),

tar_target(buildings,{
  combine_os_osm_buildings(osm_buildings, os_buildings, inspire, inspire_scotland)
}),

#TODO: Make reproducible
tar_target(buildings_heights,{
  add_building_heights(buildings, os_10k_grid, path_raster = "F:/DTM_DSM/GB_10k/Difference/")
}),

tar_target(zoomstack_buildings_lst_4326,{
  # Long running target ~ 9 hours
  zoomstack_buildings_lsoa(dl_os_zoomstack, bounds_lsoa_GB_full, bounds_lsoa_GB_generalised, bounds_lsoa_GB_super_generalised)
}),

# Scenarios

# LA Summaries

# Build GeoJSON
tar_target(lsoa_map_data,{
  select_map_outputs(lsoa_emissions_all, year = 2020)
}),

tar_target(zones_high,{
  join_for_geojson(lsoa_map_data, bounds_lsoa_GB_full)
}),

tar_target(zones_medium,{
  join_for_geojson(lsoa_map_data, bounds_lsoa_GB_generalised)
}),

tar_target(zones_low,{
  join_for_geojson(lsoa_map_data, bounds_lsoa_GB_super_generalised)
}),

tar_target(zones_transport_high,{
  join_for_geojson(transport_lsoa_data, bounds_lsoa_GB_full)
}),

tar_target(zones_transport_medium,{
  join_for_geojson(transport_lsoa_data, bounds_lsoa_GB_generalised)
}),

tar_target(zones_transport_low,{
  join_for_geojson(transport_lsoa_data, bounds_lsoa_GB_super_generalised)
}),


tar_target(buildings_high,{
  join_for_geojson(lsoa_map_data, zoomstack_buildings_lst_4326$high)
}),

tar_target(buildings_medium,{
  join_for_geojson(lsoa_map_data, zoomstack_buildings_lst_4326$medium)
}),

tar_target(buildings_low,{
  join_for_geojson(lsoa_map_data, zoomstack_buildings_lst_4326$low)
}),

tar_target(buildings_verylow,{
  join_for_geojson(lsoa_map_data, zoomstack_buildings_lst_4326$verylow)
}),

tar_target(geojson_buildings_high,{
  make_geojson(buildings_high, "outputdata/buildings_high.geojson")
}, format = "file"),

tar_target(geojson_buildings_medium,{
  make_geojson(buildings_medium, "outputdata/buildings_medium.geojson")
}, format = "file"),

tar_target(geojson_buildings_low,{
  make_geojson(buildings_low, "outputdata/buildings_low.geojson")
}, format = "file"),

tar_target(geojson_buildings_verylow,{
  make_geojson(buildings_verylow, "outputdata/buildings_verylow.geojson")
}, format = "file"),

tar_target(geojson_zones_high,{
  make_geojson(zones_high, "outputdata/zones_high.geojson")
}, format = "file"),

tar_target(geojson_zones_medium,{
  make_geojson(zones_medium, "outputdata/zones_medium.geojson")
}, format = "file"),

tar_target(geojson_zones_low,{
  make_geojson(zones_low, "outputdata/zones_low.geojson")
}, format = "file"),

tar_target(geojson_zones_transport_high,{
  make_geojson(zones_transport_high, "outputdata/zones_transport_high.geojson")
}, format = "file"),

tar_target(geojson_zones_transport_medium,{
  make_geojson(zones_transport_medium, "outputdata/zones_transport_medium.geojson")
}, format = "file"),

tar_target(geojson_zones_transport_low,{
  make_geojson(zones_transport_low, "outputdata/zones_transport_low.geojson")
}, format = "file"),

tar_target(geojson_wards,{
  make_geojson(bounds_wards, "outputdata/wards.geojson")
}, format = "file"),

tar_target(geojson_parish,{
  make_geojson(bounds_parish, "outputdata/parish.geojson")
}, format = "file"),

tar_target(geojson_westminster,{
  make_geojson(bounds_westminster, "outputdata/westminster.geojson")
}, format = "file"),

tar_target(geojson_la,{
  make_geojson(bounds_la, "outputdata/la.geojson")
}, format = "file"),

# Build PMTiles
tar_target(pmtiles_zones_high,{
  make_pmtiles(geojson_zones_high, "zones_high.geojson","zones_high.pmtiles",
               name = "zones", shared_borders = TRUE, extend_zoom = TRUE,
               coalesce = TRUE, min_zoom = 12, max_zoom = 13)
}, format = "file"),

tar_target(pmtiles_zones_medium,{
  length(pmtiles_zones_high)
  make_pmtiles(geojson_zones_medium, "zones_medium.geojson","zones_medium.pmtiles",
               name = "zones", shared_borders = TRUE,
               coalesce = TRUE, min_zoom = 9, max_zoom = 11)
}, format = "file"),

tar_target(pmtiles_zones_low,{
  length(pmtiles_zones_medium)
  make_pmtiles(geojson_zones_low, "zones_low.geojson","zones_low.pmtiles",
               name = "zones", shared_borders = TRUE,
               coalesce = TRUE, min_zoom = 4, max_zoom = 8)
}, format = "file"),

tar_target(pmtiles_zones_merge,{
  length(pmtiles_zones_low)
  join_pmtiles("zones.pmtiles",
               c("zones_high.pmtiles","zones_medium.pmtiles","zones_low.pmtiles"))
}),

tar_target(pmtiles_zones_transport_high,{
  length(pmtiles_zones_merge)
  make_pmtiles(geojson_zones_transport_high, "zones_transport_high.geojson","zones_transport_high.pmtiles",
               name = "zones", shared_borders = TRUE, extend_zoom = TRUE,
               coalesce = TRUE, min_zoom = 12, max_zoom = 13)
}, format = "file"),

tar_target(pmtiles_zones_transport_medium,{
  length(pmtiles_zones_transport_high)
  make_pmtiles(geojson_zones_transport_medium, "zones_transport_medium.geojson","zones_transport_medium.pmtiles",
               name = "zones", shared_borders = TRUE,
               coalesce = TRUE, min_zoom = 9, max_zoom = 11)
}, format = "file"),

tar_target(pmtiles_zones_transport_low,{
  length(pmtiles_zones_transport_medium)
  make_pmtiles(geojson_zones_transport_low, "zones_transport_low.geojson","zones_transport_low.pmtiles",
               name = "zones", shared_borders = TRUE,
               coalesce = TRUE, min_zoom = 4, max_zoom = 8)
}, format = "file"),

tar_target(pmtiles_zones_transport_merge,{
  length(pmtiles_zones_transport_low)
  join_pmtiles("zones_transport.pmtiles",
               c("zones_transport_high.pmtiles","zones_transport_medium.pmtiles","zones_transport_low.pmtiles"))
}),


tar_target(pmtiles_buildings_high,{
  length(pmtiles_zones_transport_merge)
  make_pmtiles(geojson_buildings_high, "buildings_high.geojson","buildings_high.pmtiles",
               name = "buildings", shared_borders = TRUE, extend_zoom = TRUE,
               coalesce = TRUE, min_zoom = 14, max_zoom = 15)
}, format = "file"),

tar_target(pmtiles_buildings_medium,{
  length(pmtiles_buildings_high)
  make_pmtiles(geojson_buildings_medium, "buildings_medium.geojson","buildings_medium.pmtiles",
               name = "buildings", simplification = 2, shared_borders = TRUE,
               coalesce = TRUE, min_zoom = 10, max_zoom = 13)
}, format = "file"),

tar_target(pmtiles_buildings_low,{
  length(pmtiles_buildings_medium)
  make_pmtiles(geojson_buildings_low, "buildings_low.geojson","buildings_low.pmtiles",
               name = "buildings", simplification = 1, shared_borders = TRUE,
               coalesce = TRUE, min_zoom = 7, max_zoom = 9)
}, format = "file"),

tar_target(pmtiles_buildings_verylow,{
  length(pmtiles_buildings_low)
  make_pmtiles(geojson_buildings_verylow, "buildings_verylow.geojson","buildings_verylow.pmtiles",
               name = "buildings", simplification = 1, shared_borders = TRUE,
               coalesce = TRUE, min_zoom = 4, max_zoom = 6)
}, format = "file"),

tar_target(pmtiles_buildings_merge,{
  length(pmtiles_buildings_verylow)
  join_pmtiles("buildings.pmtiles",
               c("buildings_high.pmtiles","buildings_medium.pmtiles",
                 "buildings_low.pmtiles","buildings_verylow.pmtiles"))
}),

tar_target(pmtiles_la,{
  make_pmtiles(geojson_la, "la.geojson","la.pmtiles",
               name = "la", shared_borders = TRUE, extend_zoom = TRUE,
               coalesce = FALSE, min_zoom = 6, max_zoom = 12)
}, format = "file"),

tar_target(pmtiles_westminster,{
  make_pmtiles(geojson_westminster, "westminster.geojson","westminster.pmtiles",
               name = "westminster", shared_borders = TRUE, extend_zoom = TRUE,
               coalesce = FALSE, min_zoom = 6, max_zoom = 12)
}, format = "file"),

tar_target(pmtiles_parish,{
  make_pmtiles(geojson_parish, "parish.geojson","parish.pmtiles",
               name = "parish", shared_borders = TRUE, extend_zoom = TRUE,
               coalesce = FALSE, min_zoom = 6, max_zoom = 12)
}, format = "file"),

tar_target(pmtiles_wards,{
  make_pmtiles(geojson_wards, "wards.geojson","wards.pmtiles",
               name = "wards", shared_borders = TRUE, extend_zoom = TRUE,
               coalesce = FALSE, min_zoom = 6, max_zoom = 12)
}, format = "file"),

# Build JSON
tar_target(build_lsoa_jsons,{
  export_zone_json(lsoa_emissions_all_forcasts, path = "outputdata/json/zones")
}),

tar_target(build_access_jsons,{
  export_zone_json(access_proximity, idcol = "LSOA11CD", rounddp = 2, path = "outputdata/json/access", dataframe = "columns")
})

)
