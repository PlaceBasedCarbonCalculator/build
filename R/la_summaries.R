#' Summarise per-capita emissions by local authority
#'
#' @description Aggregates the per-LSOA emissions totals to local
#'   authorities: for each LAD and year, total emissions per domain divided
#'   by total population. A GB-wide row (LAD25CD = "GB") is prepended for
#'   comparison. Used by the `la_emissions_all` target, exported by
#'   `la_emissions_summary_json`.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`).
#' @param lsoa_admin Zone-to-LA lookup (`lsoa_admin` target).
#' @param population GB population (`population` target).
#' @return A data frame per LAD25CD-year of per-capita emissions by domain.
#' @keywords internal
make_la_summary = function(lsoa_emissions_all, lsoa_admin, population){

  lsoa_admin = lsoa_admin[,c("LSOA21CD","LAD25CD")]

  population = population[,c("LSOA21CD","year","all_ages")]

  lsoa_emissions_all = lsoa_emissions_all[,!grepl("grade",names(lsoa_emissions_all))]
  lsoa_emissions_all = lsoa_emissions_all[,!grepl("kgco2e_percap",names(lsoa_emissions_all))]

  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, lsoa_admin, by = "LSOA21CD")
  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, population, by = c("LSOA21CD","year"))

  la_emissions = lsoa_emissions_all |>
    dplyr::group_by(LAD25CD, year) |>
    dplyr::summarise(
      dom_gas_kgco2e_percap = sum(dom_gas_total_emissions) / sum(all_ages),
      dom_elec_kgco2e_percap = sum(dom_elec_total_emissions) / sum(all_ages),
      car_kgco2e_percap = sum(car_emissions) / sum(all_ages),
      van_kgco2e_percap = sum(van_emissions) / sum(all_ages),
      company_bike_kgco2e_percap = sum(company_bike_emissions) / sum(all_ages),
      heating_other_kgco2e_percap = sum(heating_other_emissions_total) / sum(all_ages),
      food_kgco2e_percap = sum(emissions_food) / sum(all_ages),
      alcohol_kgco2e_percap = sum(emissions_alcohol) / sum(all_ages),
      clothing_kgco2e_percap = sum(emissions_clothing) / sum(all_ages),
      communication_kgco2e_percap = sum(emissions_communication) / sum(all_ages),
      housing_other_kgco2e_percap = sum(emissions_housing_other) / sum(all_ages),
      furnish_kgco2e_percap = sum(emissions_furnish) / sum(all_ages),
      recreation_kgco2e_percap = sum(emissions_recreation) / sum(all_ages),
      transport_optranequip_other_kgco2e_percap = sum(emissions_transport_optranequip_other) / sum(all_ages),
      transport_vehiclepurchase_kgco2e_percap = sum(emissions_transport_vehiclepurchase) / sum(all_ages),
      transport_pt_kgco2e_percap = sum(emissions_transport_pt) / sum(all_ages),
      health_kgco2e_percap = sum(emissions_health) / sum(all_ages),
      education_kgco2e_percap = sum(emissions_education) / sum(all_ages),
      restaurant_kgco2e_percap = sum(emissions_restaurant) / sum(all_ages),
      misc_kgco2e_percap = sum(emissions_misc) / sum(all_ages),
      flights_kgco2e_percap = sum(flights_emissions_total) / sum(all_ages),
      goods_services_combined_kgco2e_percap = sum(goods_services_combined_total) / sum(all_ages),
      total_kgco2e_percap = sum(emissions_total) / sum(all_ages)

    )

  national_emissions = lsoa_emissions_all |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      dom_gas_kgco2e_percap = sum(dom_gas_total_emissions) / sum(all_ages),
      dom_elec_kgco2e_percap = sum(dom_elec_total_emissions) / sum(all_ages),
      car_kgco2e_percap = sum(car_emissions) / sum(all_ages),
      van_kgco2e_percap = sum(van_emissions) / sum(all_ages),
      company_bike_kgco2e_percap = sum(company_bike_emissions) / sum(all_ages),
      heating_other_kgco2e_percap = sum(heating_other_emissions_total) / sum(all_ages),
      food_kgco2e_percap = sum(emissions_food) / sum(all_ages),
      alcohol_kgco2e_percap = sum(emissions_alcohol) / sum(all_ages),
      clothing_kgco2e_percap = sum(emissions_clothing) / sum(all_ages),
      communication_kgco2e_percap = sum(emissions_communication) / sum(all_ages),
      housing_other_kgco2e_percap = sum(emissions_housing_other) / sum(all_ages),
      furnish_kgco2e_percap = sum(emissions_furnish) / sum(all_ages),
      recreation_kgco2e_percap = sum(emissions_recreation) / sum(all_ages),
      transport_optranequip_other_kgco2e_percap = sum(emissions_transport_optranequip_other) / sum(all_ages),
      transport_vehiclepurchase_kgco2e_percap = sum(emissions_transport_vehiclepurchase) / sum(all_ages),
      transport_pt_kgco2e_percap = sum(emissions_transport_pt) / sum(all_ages),
      health_kgco2e_percap = sum(emissions_health) / sum(all_ages),
      education_kgco2e_percap = sum(emissions_education) / sum(all_ages),
      restaurant_kgco2e_percap = sum(emissions_restaurant) / sum(all_ages),
      misc_kgco2e_percap = sum(emissions_misc) / sum(all_ages),
      flights_kgco2e_percap = sum(flights_emissions_total) / sum(all_ages),
      goods_services_combined_kgco2e_percap = sum(goods_services_combined_total) / sum(all_ages),
      total_kgco2e_percap = sum(emissions_total) / sum(all_ages)

    )
  national_emissions$LAD25CD = "GB"

  la_emissions = rbind(national_emissions, la_emissions)

  la_emissions

}

#' Summarise per-capita emissions by Westminster constituency
#'
#' @description As `make_la_summary()` but grouped by parliamentary
#'   constituency (`PCON24CD`), without a GB row. Not currently wired to a
#'   target.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`).
#' @param lsoa_admin Zone-to-admin lookup (`lsoa_admin` target).
#' @param population GB population (`population` target).
#' @return A data frame per PCON24CD-year of per-capita emissions.
#' @keywords internal
make_westminter_summary = function(lsoa_emissions_all, lsoa_admin, population){

  lsoa_admin = lsoa_admin[,c("LSOA21CD","PCON24CD","PCON24NM")]

  population = population[,c("LSOA21CD","year","all_ages")]

  lsoa_emissions_all = lsoa_emissions_all[,!grepl("grade",names(lsoa_emissions_all))]
  lsoa_emissions_all = lsoa_emissions_all[,!grepl("kgco2e_percap",names(lsoa_emissions_all))]

  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, lsoa_admin, by = "LSOA21CD")
  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, population, by = c("LSOA21CD","year"))

  la_emissions = lsoa_emissions_all |>
    dplyr::group_by(PCON24CD, year) |>
    dplyr::summarise(
      dom_gas_kgco2e_percap = sum(dom_gas_total_emissions) / sum(all_ages),
      dom_elec_kgco2e_percap = sum(dom_elec_total_emissions) / sum(all_ages),
      car_kgco2e_percap = sum(car_emissions) / sum(all_ages),
      van_kgco2e_percap = sum(van_emissions) / sum(all_ages),
      company_bike_kgco2e_percap = sum(company_bike_emissions) / sum(all_ages),
      heating_other_kgco2e_percap = sum(heating_other_emissions_total) / sum(all_ages),
      food_kgco2e_percap = sum(emissions_food) / sum(all_ages),
      alcohol_kgco2e_percap = sum(emissions_alcohol) / sum(all_ages),
      clothing_kgco2e_percap = sum(emissions_clothing) / sum(all_ages),
      communication_kgco2e_percap = sum(emissions_communication) / sum(all_ages),
      housing_other_kgco2e_percap = sum(emissions_housing_other) / sum(all_ages),
      furnish_kgco2e_percap = sum(emissions_furnish) / sum(all_ages),
      recreation_kgco2e_percap = sum(emissions_recreation) / sum(all_ages),
      transport_optranequip_other_kgco2e_percap = sum(emissions_transport_optranequip_other) / sum(all_ages),
      transport_vehiclepurchase_kgco2e_percap = sum(emissions_transport_vehiclepurchase) / sum(all_ages),
      transport_pt_kgco2e_percap = sum(emissions_transport_pt) / sum(all_ages),
      health_kgco2e_percap = sum(emissions_health) / sum(all_ages),
      education_kgco2e_percap = sum(emissions_education) / sum(all_ages),
      restaurant_kgco2e_percap = sum(emissions_restaurant) / sum(all_ages),
      misc_kgco2e_percap = sum(emissions_misc) / sum(all_ages),
      flights_kgco2e_percap = sum(flights_emissions_total) / sum(all_ages),
      goods_services_combined_kgco2e_percap = sum(goods_services_combined_total) / sum(all_ages),
      total_kgco2e_percap = sum(emissions_total) / sum(all_ages)

    )


  la_emissions

}

#' Summarise per-capita emissions by parish
#'
#' @description As `make_la_summary()` but grouped by parish (`PAR23CD`),
#'   without a GB row. Not currently wired to a target.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`).
#' @param lsoa_admin Zone-to-admin lookup (`lsoa_admin` target).
#' @param population GB population (`population` target).
#' @return A data frame per PAR23CD-year of per-capita emissions.
#' @keywords internal
make_parish_summary = function(lsoa_emissions_all, lsoa_admin, population){

  lsoa_admin = lsoa_admin[,c("LSOA21CD","PAR23CD","PAR23NM")]

  population = population[,c("LSOA21CD","year","all_ages")]

  lsoa_emissions_all = lsoa_emissions_all[,!grepl("grade",names(lsoa_emissions_all))]
  lsoa_emissions_all = lsoa_emissions_all[,!grepl("kgco2e_percap",names(lsoa_emissions_all))]

  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, lsoa_admin, by = "LSOA21CD")
  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, population, by = c("LSOA21CD","year"))

  la_emissions = lsoa_emissions_all |>
    dplyr::group_by(PAR23CD, year) |>
    dplyr::summarise(
      dom_gas_kgco2e_percap = sum(dom_gas_total_emissions) / sum(all_ages),
      dom_elec_kgco2e_percap = sum(dom_elec_total_emissions) / sum(all_ages),
      car_kgco2e_percap = sum(car_emissions) / sum(all_ages),
      van_kgco2e_percap = sum(van_emissions) / sum(all_ages),
      company_bike_kgco2e_percap = sum(company_bike_emissions) / sum(all_ages),
      heating_other_kgco2e_percap = sum(heating_other_emissions_total) / sum(all_ages),
      food_kgco2e_percap = sum(emissions_food) / sum(all_ages),
      alcohol_kgco2e_percap = sum(emissions_alcohol) / sum(all_ages),
      clothing_kgco2e_percap = sum(emissions_clothing) / sum(all_ages),
      communication_kgco2e_percap = sum(emissions_communication) / sum(all_ages),
      housing_other_kgco2e_percap = sum(emissions_housing_other) / sum(all_ages),
      furnish_kgco2e_percap = sum(emissions_furnish) / sum(all_ages),
      recreation_kgco2e_percap = sum(emissions_recreation) / sum(all_ages),
      transport_optranequip_other_kgco2e_percap = sum(emissions_transport_optranequip_other) / sum(all_ages),
      transport_vehiclepurchase_kgco2e_percap = sum(emissions_transport_vehiclepurchase) / sum(all_ages),
      transport_pt_kgco2e_percap = sum(emissions_transport_pt) / sum(all_ages),
      health_kgco2e_percap = sum(emissions_health) / sum(all_ages),
      education_kgco2e_percap = sum(emissions_education) / sum(all_ages),
      restaurant_kgco2e_percap = sum(emissions_restaurant) / sum(all_ages),
      misc_kgco2e_percap = sum(emissions_misc) / sum(all_ages),
      flights_kgco2e_percap = sum(flights_emissions_total) / sum(all_ages),
      goods_services_combined_kgco2e_percap = sum(goods_services_combined_total) / sum(all_ages),
      total_kgco2e_percap = sum(emissions_total) / sum(all_ages)

    )


  la_emissions

}

#' Summarise per-capita emissions by electoral ward
#'
#' @description As `make_la_summary()` but grouped by ward (`WD25CD`),
#'   without a GB row. Not currently wired to a target.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`).
#' @param lsoa_admin Zone-to-admin lookup (`lsoa_admin` target).
#' @param population GB population (`population` target).
#' @return A data frame per WD25CD-year of per-capita emissions.
#' @keywords internal
make_ward_summary = function(lsoa_emissions_all, lsoa_admin, population){

  lsoa_admin = lsoa_admin[,c("LSOA21CD","WD25CD", "WD25NM")]

  population = population[,c("LSOA21CD","year","all_ages")]

  lsoa_emissions_all = lsoa_emissions_all[,!grepl("grade",names(lsoa_emissions_all))]
  lsoa_emissions_all = lsoa_emissions_all[,!grepl("kgco2e_percap",names(lsoa_emissions_all))]

  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, lsoa_admin, by = "LSOA21CD")
  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, population, by = c("LSOA21CD","year"))

  la_emissions = lsoa_emissions_all |>
    dplyr::group_by(WD25CD, year) |>
    dplyr::summarise(
      dom_gas_kgco2e_percap = sum(dom_gas_total_emissions) / sum(all_ages),
      dom_elec_kgco2e_percap = sum(dom_elec_total_emissions) / sum(all_ages),
      car_kgco2e_percap = sum(car_emissions) / sum(all_ages),
      van_kgco2e_percap = sum(van_emissions) / sum(all_ages),
      company_bike_kgco2e_percap = sum(company_bike_emissions) / sum(all_ages),
      heating_other_kgco2e_percap = sum(heating_other_emissions_total) / sum(all_ages),
      food_kgco2e_percap = sum(emissions_food) / sum(all_ages),
      alcohol_kgco2e_percap = sum(emissions_alcohol) / sum(all_ages),
      clothing_kgco2e_percap = sum(emissions_clothing) / sum(all_ages),
      communication_kgco2e_percap = sum(emissions_communication) / sum(all_ages),
      housing_other_kgco2e_percap = sum(emissions_housing_other) / sum(all_ages),
      furnish_kgco2e_percap = sum(emissions_furnish) / sum(all_ages),
      recreation_kgco2e_percap = sum(emissions_recreation) / sum(all_ages),
      transport_optranequip_other_kgco2e_percap = sum(emissions_transport_optranequip_other) / sum(all_ages),
      transport_vehiclepurchase_kgco2e_percap = sum(emissions_transport_vehiclepurchase) / sum(all_ages),
      transport_pt_kgco2e_percap = sum(emissions_transport_pt) / sum(all_ages),
      health_kgco2e_percap = sum(emissions_health) / sum(all_ages),
      education_kgco2e_percap = sum(emissions_education) / sum(all_ages),
      restaurant_kgco2e_percap = sum(emissions_restaurant) / sum(all_ages),
      misc_kgco2e_percap = sum(emissions_misc) / sum(all_ages),
      flights_kgco2e_percap = sum(flights_emissions_total) / sum(all_ages),
      goods_services_combined_kgco2e_percap = sum(goods_services_combined_total) / sum(all_ages),
      total_kgco2e_percap = sum(emissions_total) / sum(all_ages)

    )


  la_emissions

}

#' Summarise per-capita emissions by area classification group
#'
#' @description As `make_la_summary()` but grouped by the 2011 LSOA area
#'   classification group code, allowing "places like this" comparisons.
#'   Used by the `oac_emissions_all` target, exported by
#'   `oac_emissions_summary_json`.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`).
#' @param area_classifications_11_21 Classifications on 2021 zones.
#' @param population GB population (`population` target).
#' @return A data frame per `lsoa_class_code`-year of per-capita emissions.
#' @keywords internal
make_oac_summary = function(lsoa_emissions_all, area_classifications_11_21, population){

  area_classifications_11_21 = area_classifications_11_21[,c("LSOA21CD","lsoa_class_code")]

  population = population[,c("LSOA21CD","year","all_ages")]

  lsoa_emissions_all = lsoa_emissions_all[,!grepl("grade",names(lsoa_emissions_all))]
  lsoa_emissions_all = lsoa_emissions_all[,!grepl("kgco2e_percap",names(lsoa_emissions_all))]

  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, area_classifications_11_21, by = "LSOA21CD")
  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, population, by = c("LSOA21CD","year"))

  la_emissions = lsoa_emissions_all |>
    dplyr::group_by(lsoa_class_code, year) |>
    dplyr::summarise(
      dom_gas_kgco2e_percap = sum(dom_gas_total_emissions) / sum(all_ages),
      dom_elec_kgco2e_percap = sum(dom_elec_total_emissions) / sum(all_ages),
      car_kgco2e_percap = sum(car_emissions) / sum(all_ages),
      van_kgco2e_percap = sum(van_emissions) / sum(all_ages),
      company_bike_kgco2e_percap = sum(company_bike_emissions) / sum(all_ages),
      heating_other_kgco2e_percap = sum(heating_other_emissions_total) / sum(all_ages),
      food_kgco2e_percap = sum(emissions_food) / sum(all_ages),
      alcohol_kgco2e_percap = sum(emissions_alcohol) / sum(all_ages),
      clothing_kgco2e_percap = sum(emissions_clothing) / sum(all_ages),
      communication_kgco2e_percap = sum(emissions_communication) / sum(all_ages),
      housing_other_kgco2e_percap = sum(emissions_housing_other) / sum(all_ages),
      furnish_kgco2e_percap = sum(emissions_furnish) / sum(all_ages),
      recreation_kgco2e_percap = sum(emissions_recreation) / sum(all_ages),
      transport_optranequip_other_kgco2e_percap = sum(emissions_transport_optranequip_other) / sum(all_ages),
      transport_vehiclepurchase_kgco2e_percap = sum(emissions_transport_vehiclepurchase) / sum(all_ages),
      transport_pt_kgco2e_percap = sum(emissions_transport_pt) / sum(all_ages),
      health_kgco2e_percap = sum(emissions_health) / sum(all_ages),
      education_kgco2e_percap = sum(emissions_education) / sum(all_ages),
      restaurant_kgco2e_percap = sum(emissions_restaurant) / sum(all_ages),
      misc_kgco2e_percap = sum(emissions_misc) / sum(all_ages),
      flights_kgco2e_percap = sum(flights_emissions_total) / sum(all_ages),
      goods_services_combined_kgco2e_percap = sum(goods_services_combined_total) / sum(all_ages),
      total_kgco2e_percap = sum(emissions_total) / sum(all_ages)

    )

  la_emissions

}
