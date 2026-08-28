#' Build the master per-LSOA emissions table with grades
#'
#' @description Joins every emissions domain (domestic gas/electricity,
#'   other heating, car/van/company driving, flights and the consumption
#'   categories from the synthetic population) into one table per zone-year
#'   up to `max_year`, derives combined goods-and-services and grand totals
#'   (per-capita and absolute), and grades every per-capita measure A+ to
#'   F- within each year via `value2grade()`. Zone-years with implausible
#'   company car rates (>2000 kgCO2e/person, almost always a fleet registered
#'   at one address) have `company_bike_kgco2e_percap` and
#'   `company_bike_emissions` set to NA before the totals are built, so the
#'   totals, the grade and every downstream aggregate exclude them
#'   consistently; `company_bike_suppressed` flags those rows and drives
#'   warning code 6 in `make_lsoa_warnings()`. This is the
#'   `lsoa_emissions_all` target - the core output behind the map, JSONs,
#'   LA/OAC summaries and bulk download.
#' @param flights_lsoa_emissions `flights_lsoa_emissions` target.
#' @param consumption_emissions `consumption_emissions` target.
#' @param car_emissions `car_emissions` target.
#' @param domestic_electricity_emissions `domestic_electricity_emissions`.
#' @param domestic_gas_emissions `domestic_gas_emissions` target.
#' @param other_heating_emissions `other_heating_emissions` target.
#' @param max_year Latest year to include (2022 in `_targets.R`).
#' @return A data frame per LSOA-year with per-capita and total emissions
#'   for every domain, `*_grade` columns, and a logical
#'   `company_bike_suppressed`.
#' @keywords internal
combine_lsoa_emissions = function(flights_lsoa_emissions,
                                  consumption_emissions,
                                  car_emissions,
                                  domestic_electricity_emissions,
                                  domestic_gas_emissions,
                                  other_heating_emissions,
                                  max_year = 2020
                                  ) {


  other_heating_emissions = other_heating_emissions[,c("LSOA21CD","year","heating_other_emissions_total","heating_other_kgco2e_percap")]

  #TODO: some NA grades, check for consistency of 0 population zones.
  lsoa = dplyr::left_join(domestic_gas_emissions, domestic_electricity_emissions, by = c("LSOA21CD","year"))

  lsoa = lsoa[lsoa$year <= max_year,]

  names(car_emissions) = gsub("_emissions_percap","_kgco2e_percap",names(car_emissions))


  other_heating_emissions = other_heating_emissions[other_heating_emissions$year <= max_year,]

  
  consumption_emissions = consumption_emissions[,c("LSOA21CD","year",
                                                   "emissions_percap_food","emissions_percap_alcohol","emissions_percap_clothing",
                                                   "emissions_percap_communication","emissions_percap_housing_other",
                                                   "emissions_percap_furnish","emissions_percap_recreation",
                                                   "emissions_percap_transport_optranequip_other",
                                                   "emissions_percap_transport_vehiclepurchase","emissions_percap_transport_pt",
                                                   "emissions_percap_health",
                                                   "emissions_percap_education","emissions_percap_restaurant","emissions_percap_misc",
                                                   "emissions_food","emissions_alcohol","emissions_clothing","emissions_communication",
                                                   "emissions_housing_gaselecfuel","emissions_housing_other","emissions_furnish",
                                                   "emissions_recreation","emissions_transport_optranequip","emissions_transport_optranequip_other",
                                                   "emissions_transport_services","emissions_transport_pt","emissions_transport_vehiclepurchase",
                                                   "emissions_health","emissions_education","emissions_restaurant",
                                                   "emissions_misc"
                                                   )]

  names(consumption_emissions) = gsub("emissions_percap_","",names(consumption_emissions))
  names(consumption_emissions)[3:16] = paste0(names(consumption_emissions)[3:16],"_kgco2e_percap")


  flights_lsoa_emissions$flights_emissions_total = rowSums(flights_lsoa_emissions[,c("emissions_international","emissions_domestic")], na.rm = TRUE)
  flights_lsoa_emissions = flights_lsoa_emissions[,c("LSOA21CD","year","emissions_percap","flights_emissions_total")]
  names(flights_lsoa_emissions)[3] = "flights_kgco2e_percap"


  lsoa = dplyr::left_join(lsoa, car_emissions, by = c("LSOA21CD","year"))
  lsoa = dplyr::left_join(lsoa, other_heating_emissions, by = c("LSOA21CD","year"))
  lsoa = dplyr::left_join(lsoa, consumption_emissions, by = c("LSOA21CD","year"))
  lsoa = dplyr::left_join(lsoa, flights_lsoa_emissions, by = c("LSOA21CD","year"))

  # Total Goods and Services
  lsoa$goods_services_combined_kgco2e_percap = rowSums(lsoa[,c("food_kgco2e_percap",
                                                               "alcohol_kgco2e_percap",
                                                               "clothing_kgco2e_percap",
                                                               "communication_kgco2e_percap",
                                                               "housing_other_kgco2e_percap",
                                                               "furnish_kgco2e_percap",
                                                               "recreation_kgco2e_percap",
                                                               "health_kgco2e_percap",
                                                               "education_kgco2e_percap",
                                                               "restaurant_kgco2e_percap",
                                                               "misc_kgco2e_percap")], na.rm = TRUE)
  lsoa$goods_services_combined_total = rowSums(lsoa[,c("emissions_food",
                                                               "emissions_alcohol",
                                                               "emissions_clothing",
                                                               "emissions_communication",
                                                               "emissions_housing_other",
                                                               "emissions_furnish",
                                                               "emissions_recreation",
                                                               "emissions_health",
                                                               "emissions_education",
                                                               "emissions_restaurant",
                                                               "emissions_misc")], na.rm = TRUE)

  # Bad Data Checks
  # Company Cars
  # A few zones record a leasing company's or a large employer's whole fleet at
  # a single address, so their company/motorbike emissions have nothing to do
  # with the people who live there.
  #
  # Blank the component itself, before the totals are built, rather than
  # subtracting it from the total afterwards. Doing it here means everything
  # downstream agrees without further special-casing:
  #   * both totals drop it (the rowSums below use na.rm = TRUE);
  #   * value2grade() returns "NA" for the company/bike grade, and the zone no
  #     longer distorts the percentile breaks used to grade every other zone;
  #   * the website's stacked chart leaves a gap instead of drawing a bar that
  #     the headline total excludes;
  #   * the LA / ward / parish / constituency aggregates inherit the same
  #     suppression, because they are summed from these columns.
  # The estimated km driven (company_km) is left alone: it is what the source
  # data records, and only the attribution of the emissions to residents is
  # being rejected here.
  company_bike_max_kgco2e_percap = 2000 # 99.7% of zone-years are below this

  lsoa$company_bike_suppressed = !is.na(lsoa$company_bike_kgco2e_percap) &
    lsoa$company_bike_kgco2e_percap > company_bike_max_kgco2e_percap

  lsoa$company_bike_kgco2e_percap[lsoa$company_bike_suppressed] = NA_real_
  lsoa$company_bike_emissions[lsoa$company_bike_suppressed] = NA_real_

  lsoa$total_kgco2e_percap = rowSums(lsoa[,c("dom_gas_kgco2e_percap",
                                             "dom_elec_kgco2e_percap",
                                             "car_kgco2e_percap",
                                             "van_kgco2e_percap",
                                             "company_bike_kgco2e_percap",
                                             "flights_kgco2e_percap",
                                             "heating_other_kgco2e_percap",
                                             "transport_vehiclepurchase_kgco2e_percap",
                                             "transport_pt_kgco2e_percap",
                                             "transport_optranequip_other_kgco2e_percap",
                                             "goods_services_combined_kgco2e_percap")], na.rm = TRUE)

  lsoa$emissions_total = rowSums(lsoa[,c("dom_gas_total_emissions",
                                         "dom_elec_total_emissions",
                                         "car_emissions",
                                         "van_emissions",
                                         "company_bike_emissions",
                                         "flights_emissions_total",
                                         "heating_other_emissions_total",
                                         "emissions_transport_vehiclepurchase",
                                         "emissions_transport_pt",
                                         "emissions_transport_optranequip_other",
                                         "goods_services_combined_total")], na.rm = TRUE)

  lsoa = lsoa |>
    dplyr::group_by(year) |>
    dplyr::mutate(

      dom_gas_grade = value2grade(dom_gas_kgco2e_percap),
      dom_elec_grade = value2grade(dom_elec_kgco2e_percap),
      heating_other_grade = value2grade(heating_other_kgco2e_percap),
      car_grade = value2grade(car_kgco2e_percap),
      van_grade = value2grade(van_kgco2e_percap),
      company_bike_grade = value2grade(company_bike_kgco2e_percap),
      transport_optranequip_other_grade = value2grade(transport_optranequip_other_kgco2e_percap),
      transport_vehiclepurchase_grade = value2grade(transport_vehiclepurchase_kgco2e_percap),
      transport_pt_grade = value2grade(transport_pt_kgco2e_percap),
      flights_grade = value2grade(flights_kgco2e_percap),
      food_grade = value2grade(food_kgco2e_percap),
      alcohol_grade = value2grade(alcohol_kgco2e_percap),
      clothing_grade = value2grade(clothing_kgco2e_percap),
      communication_grade = value2grade(communication_kgco2e_percap),
      housing_other_grade = value2grade(housing_other_kgco2e_percap),
      furnish_grade = value2grade(furnish_kgco2e_percap),
      recreation_grade = value2grade(recreation_kgco2e_percap),
      health_grade = value2grade(health_kgco2e_percap),
      education_grade = value2grade(education_kgco2e_percap),
      restaurant_grade = value2grade(restaurant_kgco2e_percap),
      misc_grade = value2grade(misc_kgco2e_percap),
      total_grade = value2grade(total_kgco2e_percap),
      goods_services_combined_grade = value2grade(goods_services_combined_kgco2e_percap),

    ) |>
    dplyr::ungroup()






  lsoa
}
