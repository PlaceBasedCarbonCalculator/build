# Main analysis pipeline using `targets` package:
# Use `tar_make()` to build or `tar_visnetwork()` to see flow chart

# Load packages required to define the pipeline:
library(targets)
library(sf)
# Set target options:
# packages that your targets need to run
tar_option_set(
  packages = c("tibble","sf","readODS","readxl","dplyr","tidyr","smoothr",
               "osmextract","nngeo","pbapply","stplanr","rmapshaper",
               "igraph","plyr","terra","furrr","future","humanleague",
               "jsonlite","readr","lubridate","purrr","yyjsonr"),
  error = "continue" # If a target fails will attempt to run other targets
)

options(clustermq.scheduler = "multiprocess")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Target List
list(

# Set Up ------------------------------------------------------------------

# Detect when parameter file has changed:
# Parameter file defines paths to data so must be correct
tar_target(name = param_file, command = "parameters.json", format = "file"),
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
  combine_populations2(population_households_historical, population_scot, dwellings_tax_band_scotland)
}),

tar_target(population_summary,{
  summarise_population(population)
}),

tar_target(lsoa_11_21_tools,{
  lsoa_convert_2011_2021_pre_data(lookup_lsoa_2011_21, population)
}),

tar_target(households_cenus11,{
  load_cenus11_households(path = file.path(parameters$path_data,"population/cenus2011_QS402UK_LSOA_Households_AcommodationType.csv"))
}),

# Demographics
tar_target(NSSEC_ethinic_residents,{
  read_NSSEC_ethinic(path = file.path(parameters$path_data,"population/census2021EW_Resdidents_NSSEC10_Ethnicity_LSOA_partial.csv"))
}),

tar_target(NSSEC_household,{
  read_household_nssec(path = file.path(parameters$path_data,"population/census2021EW_HouseholdComposition.zip"),
                       path1 = file.path(parameters$path_data,"population/census2021EW_Households_HouseholdComposition15_LSOA.csv"),
                       path2 = file.path(parameters$path_data,"population/census2021EW_RefPerson_NSSEC10_Houshold15_LSOA_partial.csv"),
                       path_msoa = file.path(parameters$path_data,"population/census2021EW_RefPerson_NSSEC10_Houshold15_MSOA_partial.csv"),
                       lookup_postcode_OA_LSOA_MSOA_2021
                       )
}),

tar_target(household_clusters,{
  build_household_types(NSSEC_household, NSSEC_ethinic_residents)
}),

tar_target(household_pics_json,{
  sub = select_household_pics(household_clusters)
  export_zone_json(sub, idcol = "LSOA21CD", rounddp = 2, path = "outputdata/json/community_pics", dataframe = "columns")
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
tar_target(postcode_gas_electricity,{
  load_postcode_gas_electricity(path = file.path(parameters$path_data,"gas_electric/postcode"))
}),
tar_target(postcode_gas_electricity_emissions,{
  calculate_postcode_gas_electric_emissions(postcode_gas_electricity, emissions_factors)
}),

tar_target(geojson_postcode,{
  sub = prep_postcode_gas_electic(postcode_gas_electricity_emissions, bounds_postcodes_2024)
  make_geojson(sub, "outputdata/postcodes.geojson")
}, format = "file"),

# EPC Points
tar_target(epc_dom_summary,{
  epc_summarise_domestic(path = file.path(parameters$path_data,"epc/GB_domestic_epc.Rds"),
                         bounds_lsoa_GB_full)
}),

tar_target(build_epc_dom_jsons,{
  export_zone_json(epc_dom_summary, idcol = "LSOA21CD",
                   path = "outputdata/json/epc_dom", rounddp = 2, dataframe = "rows")
}),

tar_target(retrofit_lsoa_data,{
  select_retofit_vars(epc_dom_summary, population)
}),


tar_target(geojson_epc_dom,{
  sub = readRDS(file.path(parameters$path_data,"epc/GB_domestic_epc.Rds"))
  sub = wiggle_points(sub)
  make_geojson(sub, "outputdata/epc_dom.geojson")
}, format = "file"),

tar_target(geojson_epc_nondom,{
  sub = readRDS(file.path(parameters$path_data,"epc/GB_nondomestic_epc.Rds"))
  sub = wiggle_points(sub)
  make_geojson(sub, "outputdata/epc_nondom.geojson")
}, format = "file"),

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
tar_target(bounds_dz22,{
  read_dz2022_bounds(dl_boundaries)
}),
tar_target(bounds_iz22,{
  read_intermidiate_zones_2022(dl_boundaries)
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
tar_target(centroids_oa11,{
  read_centroids_oa11(dl_boundaries)
}),

tar_target(centroids_oa01,{
  read_centroids_oa01(dl_boundaries)
}),

tar_target(centroids_lsoa21,{
  read_centroids_lsoa21(dl_boundaries)
}),
tar_target(bounds_postcodes_2020,{
  read_postcodes(path = file.path(parameters$path_secure_data,"Postcodes/Postcode Polygons/Postcodes_20200826.zip"))
}),
tar_target(bounds_postcodes_2024,{
  read_postcodes(path = file.path(parameters$path_secure_data,"Postcodes/Postcode Polygons/Postcodes_20240401.zip"))
}),
tar_target(bounds_postcodes_2015,{
  read_postcodes(path = file.path(parameters$path_secure_data,"Postcodes/Postcode Polygons/Postcodes_20150401.zip"))
}),
tar_target(bounds_postcode_area,{
  make_postcode_areas(bounds_postcodes_2024)
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
tar_target(lookup_MSOA_2011_21,{
  load_MSOA_2011_2021_lookup(dl_boundaries)
}),
tar_target(lookup_OA_LSOA_MSOA_2021,{
  load_OA_LSOA_MSOA_2021_lookup(dl_boundaries)
}),

tar_target(lookup_postcode_OA_LSOA_MSOA_2021,{
  load_postcode_OA_LSOA_MSOA_class_2021_lookup(dl_boundaries)
}),

tar_target(lookup_DataZone_2022,{
  read_datazone_lookup_2022(dl_boundaries)
}),

tar_target(SOAC_11,{
  OAC_to_2021(lookup_OA_LSOA_MSOA_classifications, lookup_lsoa_2011_21)
}),

tar_target(oac11lsoa21,{
  OAC11_lsoa21(centroids_oa11, bounds_lsoa21_full, lookup_OA_LSOA_MSOA_classifications)
}),

tar_target(oac01lsoa21,{
  OAC01_lsoa21(centroids_oa01, bounds_lsoa21_full, oac01)
}),

tar_target(oac01,{
  read_OAC01(path = file.path(parameters$path_data,"area_classifications/2001/OAC_2001.Rds"))
}),

tar_target(oac21,{
  load_OAC21(path = file.path(parameters$path_data,"area_classifications/oac21ew.csv"))
}),

tar_target(lsoa21_OAC21_summary,{
  OAC21_lsoa21(oac21, lookup_postcode_OA_LSOA_MSOA_2021)
}),

tar_target(lsoa_admin,{
  lsoa_admin_summary(bounds_lsoa_GB_full, bounds_wards, bounds_parish, bounds_westminster, bounds_la)
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
tar_target(uprn,{
  load_uprn(path = file.path(parameters$path_data,"os_uprn"))
}),
tar_target(uprn_bng,{
  load_uprn_27700(path = file.path(parameters$path_data,"os_uprn"))
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

tar_target(vehicle_cenus21,{
  load_census_2021_vehicles(path = file.path(parameters$path_data,"nomis"))
}),

tar_target(households_cenus21,{
  load_census_2021_households(path = file.path(parameters$path_data,"nomis"))
}),


# Income
tar_target(dl_income,{
  dowload_income_msoa(path = file.path(parameters$path_data,"income"))
}),

tar_target(income_msoa,{
  dl_income
  load_msoa_income(path = file.path(parameters$path_data,"income"))
}),

# tar_target(experian_income,{
#   load_experian_income(path = file.path(parameters$path_secure_data,"CREDS Data/Tim Share/From Malcolm/Experian.zip"))
# }),

tar_target(income_lsoa_msoa,{
  match_income_lsoa_msoa(income_msoa,lookup_MSOA_2011_21,lookup_OA_LSOA_MSOA_2021)
}),

# tar_target(income_lsoa,{
#   estimate_income(experian_income, income_msoa, lookup_lsoa_2001_11, lookup_OA_LSOA_MSOA_classifications, lookup_lsoa_2011_21)
# }),

tar_target(income_bands_lsoa,{
  make_income_bands_lsoa(NSSEC_household,income_msoa,lookup_OA_LSOA_MSOA_classifications,lookup_lsoa_2011_21)
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
tar_target(vehicle_registrations_21,{
  vehicle_reg_to_21(vehicle_registrations,lsoa_11_21_tools,"vehicle_registrations")
}),
tar_target(ulev_registrations_21,{
  vehicle_reg_to_21(ulev_registrations,lsoa_11_21_tools,"ulev_registrations")
}),
tar_target(ev_registrations_21,{
  vehicle_reg_to_21(ev_registrations,lsoa_11_21_tools,"ev_registrations")
}),

# Car Emissions
tar_target(car_emissions_11,{
  sub = load_car_emissions(path = file.path(parameters$path_secure_data,"CREDS Data/github-secure-data/Historical_Car_Emissions_LSOA.zip"))
  car_emissions_to_21(sub, lsoa_11_21_tools)
}),
tar_target(car_emissions_perkm,{
  car_emissions_post2018(car_emissions_11,vehicle_registrations_21,ulev_registrations_21)
}),
tar_target(car_emissions,{
  calculate_car_emissions(car_km_lsoa_21, car_emissions_perkm, population)
}),

#Car &  Van km (2009-2011 LSOA)
tar_target(car_km_2009_2011,{
  sub = read_motoring_along(path = file.path(parameters$path_secure_data,"CREDS Data/Tim Share/From Tim/MOT Data RACv9.3"))
  #car_km_2009_2011_to_2021(sub, lsoa_11_21_tools)
  sub
}),



tar_target(car_km_pc,{
  read_mot_km_pc(path = file.path(parameters$path_secure_data,"CARS/Anoymised MOT/clean/postcode_total_vkm_2005_2023.Rds"))
}),

# tar_target(car_km_lsoa_11,{
#   extraplote_car_km_trends(car_km_pc, car_km_2009_2011, centroids_lsoa11, centroids_dz11, population)
# }),

tar_target(car_km_lsoa_21,{
  extraplote_car_km_trends2(car_km_pc, car_km_2009_2011, centroids_lsoa21,
                            centroids_dz11, vehicle_registrations_21, lookup_lsoa_2011_21)
}),

# tar_target(car_km_lsoa,{
#   car_km_11_to_21(car_km_lsoa_11, lsoa_11_21_tools)
# }),

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
  sub = load_central_heating_2011(path = file.path(parameters$path_data,"nomis","2011"))
  central_heating_2011_to_2021(sub, lsoa_11_21_tools)
}),

tar_target(other_heating_emissions,{
  calculate_other_heating(central_heating_2021, central_heating_2011, domestic_gas, population)
}),

# House Prices
tar_target(house_prices,{
  load_house_prices(path = file.path(parameters$path_data,"house_price_age"), lsoa_11_21_tools)
}),

tar_target(house_transactions,{
  load_house_transactions(path = file.path(parameters$path_data,"house_price_age"), lsoa_11_21_tools)
}),

# VOA House by Council Tax
tar_target(dwellings_tax_band,{
  load_voa_CTSOP1(path = file.path(parameters$path_data,"voa"))
}),

tar_target(households_scotland,{
  read_scotland_households(path = file.path(parameters$path_data,"population_scotland/household-estimates.xlsx"))
}),

tar_target(dwellings_tax_band_scotland,{
  load_scotland_council_tax(path = file.path(parameters$path_data,"council_tax_scotland"))
}),

tar_target(dwellings_type,{
  load_voa_CTSOP3(path = file.path(parameters$path_data,"voa"))
}),

tar_target(dwellings_age,{
  load_voa_CTSOP4(path = file.path(parameters$path_data,"voa"))
}),

tar_target(population_households_historical,{
  extrapolate_population_households(households_cenus11,households_cenus21,
                                    lookup_lsoa_2011_21,dwellings_tax_band,
                                    population_2002_2020,population_2021,
                                    population_2022)
}),

tar_target(dwellings_type_backcast,{
  backcast_dwelling_types(dwellings_tax_band, dwellings_type)
}),

tar_target(voa_json_2010,{
  sub = summarise_voa_post2010(dwellings_tax_band)
  export_zone_json(sub, idcol = "LSOA21CD", rounddp = 1, path = "outputdata/json/voa_2010", dataframe = "columns",
                   reduce = FALSE)
}),

tar_target(voa_json_2020,{
  sub = summarise_voa_post2020(dwellings_type, dwellings_age)
  export_zone_json(sub, idcol = "LSOA21CD", rounddp = 1, path = "outputdata/json/voa_2020", dataframe = "columns",
                   reduce = FALSE)
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

tar_target(consumption_syth_pop,{
  consumption_footprint_syth_pop(synth_households_lcfs_2020,synth_households_lcfs_2018,
                                 synth_households_lcfs_2016,synth_households_lcfs_2014,
                                 synth_households_lcfs_2012,synth_households_lcfs_2010)
}),

tar_target(consumption_lookup,{
  load_consumption_lookup(path = file.path(parameters$path_data,"consumption/PBCC_lookup.xlsx"))
}),


tar_target(consumption_emissions,{
  #calculate_consumption_lsoa(consumption_uk, consumption_income, population, income_lsoa, domestic_electricity) # Old method
  calculate_consumption_lsoa(consumption_syth_pop, population, consumption_uk, consumption_lookup)

}),

# Surveys




# Accessibility Analysis
tar_target(access_poi_circle_15min,{
  zones = sf::st_buffer(centroids_lsoa21, 1609.34 * 0.75)# 15 min * 3 mph
  access_counts(zones, poi, centroids_oa21, population_oa21, lookup_oa2021_lsoa2021)
}),

tar_target(access_poi_circle_30min,{
  zones = sf::st_buffer(centroids_lsoa21, 1609.34 * 1.5)
  access_counts(zones, poi, centroids_oa21, population_oa21, lookup_oa2021_lsoa2021)
}),

tar_target(access_poi_circle_45min,{
  zones = sf::st_buffer(centroids_lsoa21, 1609.34 * 2.25)
  access_counts(zones, poi, centroids_oa21, population_oa21, lookup_oa2021_lsoa2021)
}),

tar_target(access_poi_circle_60min,{
  zones = sf::st_buffer(centroids_lsoa21, 1609.34 * 3)
  access_counts(zones, poi, centroids_oa21, population_oa21, lookup_oa2021_lsoa2021)
}),

tar_target(lookup_oa2021_lsoa2011,{
  oa2021tolsoa2011(centroids_oa21, centroids_lsoa11)
}),

tar_target(lookup_oa2021_lsoa2021,{
  oa2021tolsoa2021(centroids_oa21, centroids_lsoa21)
}),

tar_target(access_poi_iso_15min,{
  zones = ons_isochrones[ons_isochrones$iso_cutoff == 900,] # 15 min
  zones = zones[zones$OA21CD %in% lookup_oa2021_lsoa2021$nearest_OA2021,]
  access_counts(zones, poi, centroids_oa21, population_oa21, lookup_oa2021_lsoa2021)
}),

tar_target(access_poi_iso_30min,{
  zones = ons_isochrones[ons_isochrones$iso_cutoff == 1800,]
  zones = zones[zones$OA21CD %in% lookup_oa2021_lsoa2021$nearest_OA2021,]
  access_counts(zones, poi, centroids_oa21, population_oa21, lookup_oa2021_lsoa2021)
}),

tar_target(access_poi_iso_45min,{
  zones = ons_isochrones[ons_isochrones$iso_cutoff == 2700,]
  zones = zones[zones$OA21CD %in% lookup_oa2021_lsoa2021$nearest_OA2021,]
  access_counts(zones, poi, centroids_oa21, population_oa21, lookup_oa2021_lsoa2021)
}),

tar_target(access_poi_iso_60min,{
  zones = ons_isochrones[ons_isochrones$iso_cutoff == 3600,]
  zones = zones[zones$OA21CD %in% lookup_oa2021_lsoa2021$nearest_OA2021,]
  access_counts(zones, poi, centroids_oa21, population_oa21, lookup_oa2021_lsoa2021)
}),

#TODO: add the summary data and JSON creation, fucntions in assecc_proximity_funcs.R
tar_target(access_proximity,{
  summarise_access_proximity(access_poi_circle_15min,access_poi_iso_15min,
                             access_poi_circle_30min,access_poi_iso_30min,
                             access_poi_circle_45min,access_poi_iso_45min,
                             access_poi_circle_60min,access_poi_iso_60min,
                             lookup_oa2021_lsoa2021,area_classifications
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
  get_flights_lsoa_emissions(flights_total_emissions, consumption_emissions)
}),

# Transit Stops

# Emissions Footprints
tar_target(emissions_factors,{
  load_emissions_factors()
}),

tar_target(lsoa_emissions_all,{
  combine_lsoa_emissions(flights_lsoa_emissions,consumption_emissions,
                         car_emissions,domestic_electricity_emissions,
                         domestic_gas_emissions,other_heating_emissions,
                         max_year = 2020)
}),

# PLEF Forecasts
tar_target(PLEF,{
  load_plef(path = file.path(parameters$path_data,"PLEF"))
}),

# tar_target(PLEF_emissions,{
#   forcast_emissions_plef(lsoa_emissions_all, PLEF)
# }),

# tar_target(lsoa_emissions_all_forcasts,{
#   dplyr::left_join(lsoa_emissions_all, PLEF_emissions, by = "LSOA21CD")
# }),

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
  zoomstack_buildings_lsoa(buildings_heights, dl_os_zoomstack, bounds_lsoa_GB_full, bounds_lsoa_GB_generalised, bounds_lsoa_GB_super_generalised)
}),

# Scenarios

# LA Summaries

# Build GeoJSON
tar_target(lsoa_map_data,{
  select_map_outputs(lsoa_emissions_all, year = 2019)
}),

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

# Load Survey Data
tar_target(nts,{
  load_NTS(path = file.path(parameters$path_secure_data,"National Travel Survey/Safeguarded/"))
}),

# tar_target(sipher,{
#   load_SIPHER(path = file.path(parameters$path_secure_data,"SIPHER Syntheic Population"))
# }),

tar_target(synth_pop_seed,{
  build_synth_pop_seed(file.path(parameters$path_data,"population"))
}),

tar_target(synth_pop_seed_scotland,{
  build_synth_pop_seed_scotland(file.path(parameters$path_data,"population_scotland"))
}),

tar_target(census21_synth_households,{
  sythetic_census(path = file.path(parameters$path_data,"population"), synth_pop_seed) # Long running ~ 3.5 days
}),

tar_target(scot_synth_households,{
  sythetic_census_scot(path_data = file.path(parameters$path_data,"population_scotland"), synth_pop_seed_scotland) # Long running ~ 3.5 days
}),

tar_target(lcfs,{
  load_LCFS(path = file.path(parameters$path_secure_data,"Living Costs and Food Survey/Safeguarded"))
}),

tar_target(lcfs_clean,{
  selected_lcfs(lcfs)
}),

tar_target(synth_households_lcfs_2020,{
  match_LCFS_synth_pop(census21_synth_households,lcfs_clean,oac11lsoa21,income_lsoa_msoa,
                       population, dwellings_type_backcast, base_year = "2020/21")
}),

tar_target(synth_households_lcfs_2018,{
  match_LCFS_synth_pop(census21_synth_households,lcfs_clean,oac11lsoa21,income_lsoa_msoa,
                       population, dwellings_type_backcast, base_year = "2018/19")
}),

tar_target(synth_households_lcfs_2016,{
  match_LCFS_synth_pop(census21_synth_households,lcfs_clean,oac11lsoa21,income_lsoa_msoa,
                       population, dwellings_type_backcast, base_year = "2016/17")
}),

tar_target(synth_households_lcfs_2014,{
  match_LCFS_synth_pop(census21_synth_households,lcfs_clean,oac11lsoa21,income_lsoa_msoa,
                       population, dwellings_type_backcast, base_year = "2014/15")
}),

tar_target(synth_households_lcfs_2012,{
  match_LCFS_synth_pop(census21_synth_households,lcfs_clean,oac01lsoa21,income_lsoa_msoa,
                       population, dwellings_type_backcast, base_year = "2012/13")
}),

tar_target(synth_households_lcfs_2010,{
  match_LCFS_synth_pop(census21_synth_households,lcfs_clean,oac01lsoa21,income_lsoa_msoa,
                       population, dwellings_type_backcast, base_year = "2010/11")
}),

# tar_target(us,{
#   load_US(path = file.path(parameters$path_secure_data,"Understanding Society/Safeguarded"))
# }),


# Build PMTiles
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


tar_target(pmtiles_postcode,{
  make_pmtiles(geojson_postcode, "postcodes.geojson","postcodes.pmtiles",
               name = "postcodes", shared_borders = TRUE, extend_zoom = TRUE,
               coalesce = TRUE, min_zoom = 6, max_zoom = 14)
}, format = "file"),


tar_target(pmtiles_epc_dom,{
  make_pmtiles(geojson_epc_dom, "epc_dom.geojson","epc_dom.pmtiles",
               name = "epc_dom", extend_zoom = TRUE,
               drop = TRUE, min_zoom = 6, max_zoom = 14)
}, format = "file"),


tar_target(pmtiles_epc_nondom,{
  make_pmtiles(geojson_epc_nondom, "epc_nondom.geojson","epc_nondom.pmtiles",
               name = "epc_nondom", shared_borders = TRUE, extend_zoom = TRUE,
               drop = TRUE, min_zoom = 6, max_zoom = 14)
}, format = "file"),

# Build JSON -------------------------------------------------------
tar_target(build_lsoa_jsons,{
  export_zone_json(lsoa_emissions_all, path = "outputdata/json/zones")
}),

tar_target(build_population_jsons,{
  export_zone_json(population_summary, path = "outputdata/json/population", dataframe = "columns")
}),

tar_target(build_access_jsons,{
  sub = access_proximity[,c("LSOA21CD","categoryname","classname","access_15",
                            "proximity_15","access_30","proximity_30","access_45",
                            "proximity_45","access_60","proximity_60")]
  export_zone_json(sub, idcol = "LSOA21CD", rounddp = 2, path = "outputdata/json/access", dataframe = "columns")
}),

tar_target(build_postcode_jsons,{
  export_zone_json(postcode_gas_electricity_emissions, idcol = "postcode",
                   path = "outputdata/json/postcode", rounddp = 0, dataframe = "columns")
}),


# Build pmtiles -------------------------------------------------------

tar_target(pmtiles_retrofit,{
  make_pmtiles_stack(retrofit_lsoa_data,
                     bounds_lsoa_GB_full,
                     bounds_lsoa_GB_generalised,
                     bounds_lsoa_GB_super_generalised,
                     zoomstack_buildings_lst_4326,
                     name = "retrofit",
                     output_path = "outputdata/retrofit")
}, format = "file"),


tar_target(pmtiles_transport,{
  make_pmtiles_stack(transport_lsoa_data,
                     bounds_lsoa_GB_full,
                     bounds_lsoa_GB_generalised,
                     bounds_lsoa_GB_super_generalised,
                     zoomstack_buildings_lst_4326,
                     name = "transport",
                     output_path = "outputdata/transport")
}, format = "file"),

tar_target(pmtiles_pbcc,{
  make_pmtiles_stack(lsoa_map_data,
                     bounds_lsoa_GB_full,
                     bounds_lsoa_GB_generalised,
                     bounds_lsoa_GB_super_generalised,
                     zoomstack_buildings_lst_4326,
                     name = "pbcc",
                     output_path = "outputdata/pbcc")
}, format = "file"),

# Build Bulk Exports -------------------------------------------------------

tar_target(bulk_pbcc,{
  bulk_export_pbcc(lsoa_emissions_all)
}, format = "file"),

tar_target(bulk_household_clusters,{
  bulk_export_household_clusters(household_clusters)
}, format = "file"),

tar_target(bulk_pt_frequency,{
  bulk_export_pt_frequency(pt_frequency)
}, format = "file"),

tar_target(bulk_access_proximity,{
  bulk_export_access_proximity(access_proximity)
}, format = "file"),

tar_target(bulk_epc_dom_summary,{
  bulk_export_epc_dom_summary(epc_dom_summary)
}, format = "file"),

# tar_target(bulk_epc_dom,{
#   bulk_export_epc_dom(geojson_epc_dom)
# }, format = "file"),

# tar_target(bulk_epc_nondom,{
#   bulk_export_epc_nondom(geojson_epc_nondom)
# }, format = "file"),
#
tar_target(bulk_buildings_heights,{
  bulk_export_buildings(buildings_heights)
}, format = "file")

)
