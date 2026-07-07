#' Assign MSOA income estimates to their constituent 2021 LSOAs
#'
#' @description Gives every 2021 LSOA the income estimate of its parent
#'   MSOA (2011 MSOAs bridged via the 2011-to-2021 best-fit lookup for the
#'   pre-2023 releases). Used by the `income_lsoa_msoa` target, feeding the
#'   retrofit map and the E&W synthetic-population income matching.
#' @param income_msoa ONS MSOA income estimates (`income_msoa` target).
#' @param lookup_MSOA_2011_21 MSOA best-fit lookup (`lookup_MSOA_2011_21`).
#' @param lookup_OA_LSOA_MSOA_2021 2021 geography lookup.
#' @return A data frame with `LSOA21CD`, `year`, `total_annual_income` and
#'   the confidence limits.
#' @keywords internal
match_income_lsoa_msoa = function(income_msoa,
                                  lookup_MSOA_2011_21,
                                  lookup_OA_LSOA_MSOA_2021) {

  lookup_OA_LSOA_MSOA_2021 = lookup_OA_LSOA_MSOA_2021[,c("LSOA21CD","MSOA21CD")]
  lookup_OA_LSOA_MSOA_2021 = lookup_OA_LSOA_MSOA_2021[!duplicated(lookup_OA_LSOA_MSOA_2021$LSOA21CD),]

  lookup_MSOA_2011_21 = lookup_MSOA_2011_21[,c("MSOA11CD","CHNGIND","MSOA21CD" )]
  lookup_MSOA_2011_21 = lookup_MSOA_2011_21[!duplicated(lookup_MSOA_2011_21$MSOA21CD),]

  lookup_OA_LSOA_MSOA_2021 = dplyr::left_join(lookup_OA_LSOA_MSOA_2021, lookup_MSOA_2011_21, by = c("MSOA21CD"))

  #income_msoa = income_msoa[income_msoa$year == income_year,]

  income_msoa_11 = income_msoa[!is.na(income_msoa$MSOA11),]
  income_msoa_21 = income_msoa[!is.na(income_msoa$MSOA21),]

  income_msoa_11$MSOA21 = NULL
  income_msoa_21$MSOA11 = NULL

  income_lsoa_11 = dplyr::left_join(lookup_OA_LSOA_MSOA_2021, income_msoa_11, by = c("MSOA11CD" = "MSOA11"),
                                    relationship = "many-to-many")
  income_lsoa_21 = dplyr::left_join(lookup_OA_LSOA_MSOA_2021, income_msoa_21, by = c("MSOA21CD" = "MSOA21"))

  income_lsoa = rbind(income_lsoa_11, income_lsoa_21)

  income_lsoa = income_lsoa[order(income_lsoa$LSOA21CD, income_lsoa$year),]
  income_lsoa = income_lsoa[,c("LSOA21CD","year","upper_limit" ,"lower_limit","total_annual_income")]

  income_lsoa
}


#' Pick one matched LCFS household, weighted towards the zone's income
#'
#' @description From the candidate LCFS household IDs matched to a
#'   synthetic household, samples one with probability weighted by a normal
#'   density centred on the zone's mean income (falling back to uniform if
#'   all weights are zero).
#' @param lst Candidate LCFS `household_id`s.
#' @param mean_income Zone mean annual income.
#' @param sd_income Zone income standard deviation (from the confidence
#'   limits).
#' @param hh LCFS household table with `annual_income`.
#' @return A single selected household ID.
#' @keywords internal
select_id_income = function(lst, mean_income, sd_income, hh){
  lst = unlist(lst)
  if(length(lst) == 1){
    return(lst)
  }
  inc = hh$annual_income[match(lst, hh$household_id)]
  weights <- dnorm(inc, mean = mean_income, sd = sd_income)
  if(sum(weights) > 0){
    res = try(sample(lst,1, prob = weights), silent = TRUE)
  } else {
    res = try(sample(lst,1), silent = TRUE)
  }
  # res = try(sample(lst,1, prob = weights), silent = TRUE)
  if(inherits(res,"try-error")){
    stop(paste(lst, collapse = " "))
  } else {
    return(res)
  }

}

#' Attach LCFS spending to every E&W synthetic household
#'
#' @description The heart of the consumption model for England & Wales: the
#'   census-2021 synthetic households are rescaled to the target year's
#'   household counts and dwelling mix (`select_synth_pop_year()` with the
#'   dwelling-type backcast), then each household is matched to an LCFS
#'   household with the same tenure, composition, size, car ownership and
#'   OAC area type (with similarity fallbacks via `match_hh_census3()`),
#'   picking among candidates by zone income (`select_id_income()`). Used by
#'   the seven `synth_households_lcfs_*` targets (base years 2010/11 to
#'   2022/23).
#' @param census21_synth_households `census21_synth_households` target.
#' @param lcfs_clean Pooled LCFS datasets (`lcfs_clean` target).
#' @param oac11lsoa21 OAC mix per zone (`oac11lsoa21` or `oac01lsoa21`).
#' @param income_lsoa_msoa Zone income estimates (`income_lsoa_msoa`).
#' @param population GB population/households (`population` target).
#' @param dwellings_type_backcast Dwelling types by year
#'   (`dwellings_type_backcast` target).
#' @param base_year LCFS base year, e.g. "2020/21".
#' @return A data frame with one row per synthetic household including the
#'   matched LCFS spending, income and flight variables.
#' @keywords internal
match_LCFS_synth_pop = function(census21_synth_households,
                                lcfs_clean,
                                oac11lsoa21,
                                income_lsoa_msoa,
                                population,
                                dwellings_type_backcast,
                                base_year = "2020/21"){


  inc_year = as.numeric(substr(base_year,1,4))
  if(inc_year < 2012){
    inc_year = 2012
  } else if(inc_year == 2022){
    inc_year = 2023 # Skipped a year from 2020 to 2023
  }
  if(inc_year == 2012){
    oac_year = 2001
  } else if(inc_year >= 2024) {
    oac_year = 2021
  } else {
    oac_year = 2011
  }


  census21_synth_households$conv = NULL
  census21_synth_households$pValue = NULL

  population = population[,c("LSOA21CD","year","households_est","all_properties")]
  population = population[population$year == as.numeric(substr(base_year,1,4)),]

  population = population[population$LSOA21CD %in% unique(census21_synth_households$LSOA21CD),] #TODO: Scotland

  dwellings_type_backcast = dwellings_type_backcast[dwellings_type_backcast$year == as.numeric(substr(base_year,1,4)),]

  dwellings_type_backcast$detached = dwellings_type_backcast$house_detached + dwellings_type_backcast$unknown
  dwellings_type_backcast$semidetached = dwellings_type_backcast$house_semi + dwellings_type_backcast$bungalow
  dwellings_type_backcast$terraced = dwellings_type_backcast$house_terraced
  dwellings_type_backcast$flat = dwellings_type_backcast$flat_mais + dwellings_type_backcast$annexe
  dwellings_type_backcast$caravan = dwellings_type_backcast$caravan_houseboat_mobilehome

  dwellings_type_backcast = dwellings_type_backcast[,c("year","lsoa21cd","detached","semidetached","terraced","flat","caravan")]

  hh = lcfs_clean[[base_year]]

  income_lsoa_msoa = income_lsoa_msoa[income_lsoa_msoa$year == inc_year,]

  oac11lsoa21$OAC11combine = sapply(oac11lsoa21$OAC, function(x){
    # x = x[order(x$Freq, decreasing = TRUE),]
    # x = paste(x$OAC, collapse = " ")
    x = as.character(x$OAC)
    x = x[order(x)]
    x = paste(x, collapse = " ")
    x
  })
  oac11lsoa21$OAC = NULL

  census21_synth_households = dplyr::left_join(census21_synth_households, oac11lsoa21, by = c("LSOA21CD" = "LSOA21CD"))

  similarity_table = make_similarity_table(hh, oac_year)

  census21_synth_households = dplyr::left_join(census21_synth_households, income_lsoa_msoa, by = c("LSOA21CD" = "LSOA21CD"))
  census21_synth_households$sd_income = (census21_synth_households$upper_limit - census21_synth_households$lower_limit) / 3.92

  # Expand Census
  cenus_long = census21_synth_households[rep(1:nrow(census21_synth_households), times = census21_synth_households$households),]
  cenus_long$households = NULL

  cenus_long = cenus_long[order(cenus_long$LSOA21CD),]
  dwellings_type_backcast = dwellings_type_backcast[order(dwellings_type_backcast$lsoa21cd),]
  population = population[order(population$LSOA21CD),]

  cenus_long = dplyr::ungroup(cenus_long)
  cenus_long = dplyr::group_split(cenus_long, LSOA21CD)
  dwellings_type_backcast = dplyr::group_split(dwellings_type_backcast, lsoa21cd)
  population = dplyr::group_split(population, LSOA21CD)

  # cenus_long2 = purrr::pmap(.l = list(
  #   cenus_long,
  #   population,
  #   dwellings_type_backcast
  # ),
  # .f = select_synth_pop_year,
  # .progress = TRUE
  # )

  # cen = cenus_long[[31882]]
  # pop = population[[31882]]
  # bk = dwellings_type_backcast[[31882]]


  t1 = Sys.time()
  future::plan("multisession")
  cenus_long2 = furrr::future_pmap(.l = list(
    cen = cenus_long,
    pop = population,
    bk = dwellings_type_backcast
  ),

  .f = select_synth_pop_year,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = 1234L,
                                  scheduling  = 1))
  future::plan("sequential")
  t2 = Sys.time()
  message(round(difftime(t2,t1, units = "mins"),2), " min")

  cenus_long2 = data.table::rbindlist(cenus_long2)
  cenus_long2 = as.data.frame(cenus_long2)

  # Census Unique Combinations
  census_unique =  cenus_long2 |>
    dplyr::group_by(hhComp15, Tenure5, hhSize5, CarVan5, OAC11combine) |>
    dplyr::summarise(households = dplyr::n())


  hh$annual_income = hh$incanon * (365/7)

  t1 = Sys.time()
  future::plan("multisession")
  x = furrr::future_pmap(.l = list(
    Tenure5 = census_unique$Tenure5,
    hhComp15 = census_unique$hhComp15,
    hhSize5 = census_unique$hhSize5,
    CarVan5 = census_unique$CarVan5,
    OACs = census_unique$OAC11combine
  ),

  .f = match_hh_census3,
  hh = hh[,c("household_id","Tenure5","hhComp15","hhSize5","CarVan5","OAC")],
  similarity_table = similarity_table,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = 1234L,
                                  scheduling  = 1))
  future::plan("sequential")
  t2 = Sys.time()
  message(round(difftime(t2,t1, units = "mins"),2), " min")

  x = data.table::rbindlist(x)

  cenus_long2 = dplyr::left_join(cenus_long2, x,
                                by = c("hhComp15", "Tenure5", "hhSize5", "CarVan5",
                                       "OAC11combine" = "OACs"))

  cenus_long2$sd_income = (cenus_long2$upper_limit - cenus_long2$lower_limit) / 3.92


  future::plan("multisession")
  cenus_long2$household_id_single = furrr::future_pmap_int(.l = list(lst = cenus_long2$household_id,
                                                                    mean_income = cenus_long2$total_annual_income,
                                                                    sd_income = cenus_long2$sd_income),
                                                          .f = select_id_income,
                                                          hh = hh[,c("household_id","annual_income")],
                                                          .options = furrr::furrr_options(seed = 1234L,
                                                                                          scheduling  = 1))
  future::plan("sequential")

  hh = hh[,!names(hh) %in% c("Tenure5","CarVan5","hhSize5","hhComp15")]

  cenus_long2 = dplyr::left_join(cenus_long2, hh, by = c("household_id_single" = "household_id"))

  cenus_long2

}

#' Attach LCFS spending to every Scottish synthetic household
#'
#' @description Scottish counterpart of `match_LCFS_synth_pop()`: the
#'   Scotland-2022 synthetic households are rescaled to the target year via
#'   `select_synth_pop_year_scot()` (no dwelling-type backcast available),
#'   then matched to LCFS households on tenure, 10-category composition,
#'   size, cars and OAC via the similarity-table approach
#'   (`match_hh_census3()`), sampling by Data Zone income. Scottish income
#'   estimates only exist for 2014-2020 (2016 substitutes 2017). Used by
#'   the `synth_households_lcfs_*_scotland` targets.
#' @param scot_synth_households `scot_synth_households` target.
#' @param lcfs_clean Pooled LCFS datasets (`lcfs_clean` target).
#' @param oac11dz22 OAC mix per DZ (`oac11dz22` or `oac01dz22`).
#' @param income_scot_dz22 DZ income estimates (`income_scot_dz22`).
#' @param population GB population/households (`population` target).
#' @param base_year LCFS base year, e.g. "2020/21".
#' @return A data frame with one row per synthetic household including the
#'   matched LCFS spending, income and flight variables.
#' @keywords internal
match_LCFS_synth_pop_scotland = function(scot_synth_households,
                                         lcfs_clean,
                                         oac11dz22,
                                         income_scot_dz22,
                                         population,
                                         base_year = "2020/21"){


  inc_year = as.numeric(substr(base_year,1,4))

  if(inc_year <= 2012){
    oac_year = 2001
  } else if(inc_year >= 2024) {
    oac_year = 2021
  } else {
    oac_year = 2011
  }


  if(inc_year < 2014){
    inc_year = 2014
  }
  if(inc_year > 2020){
    inc_year = 2020
  }
  if(inc_year == 2016){ # No 2016 data so use 2017
    inc_year = 2017
  }

  scot_synth_households$conv = NULL
  scot_synth_households$error_margins = NULL
  scot_synth_households$MAE = NULL

  population = population[,c("LSOA21CD","year","households_est","all_properties")]
  population = population[population$year == as.numeric(substr(base_year,1,4)),]

  population = population[population$LSOA21CD %in% unique(scot_synth_households$LSOA21CD),]

  hh = lcfs_clean[[base_year]]

  income_scot_dz22 = income_scot_dz22[income_scot_dz22$year == inc_year,]

  oac11dz22$OAC11combine = sapply(oac11dz22$OAC, function(x){
    x = as.character(x$OAC)
    x = x[order(x)]
    x = paste(x, collapse = " ")
    x
  })
  oac11dz22$OAC = NULL

  scot_synth_households = dplyr::left_join(scot_synth_households, oac11dz22, by = c("LSOA21CD" = "LSOA21CD"))

  # Match LCFS categories
  scot_synth_households$Tenure5[scot_synth_households$Tenure5 == "rentfree"] = "privaterented"
  scot_synth_households$hhSize5 = as.character(scot_synth_households$hhSize5)
  scot_synth_households$CarVan5 = as.character(scot_synth_households$CarVan5)
  scot_synth_households$hhSize5[scot_synth_households$hhSize5 %in% c("p4","p5+")] = "p4+"
  scot_synth_households$CarVan5[scot_synth_households$CarVan5 %in% c("car3","car4+")] = "car3+"

  similarity_table = make_similarity_table(hh, oac_year)

  scot_synth_households = dplyr::left_join(scot_synth_households, income_scot_dz22, by = c("LSOA21CD" = "DataZone22"))
  scot_synth_households$sd_income = (scot_synth_households$upper_limit - scot_synth_households$lower_limit) / 3.92

  # Expand Census
  cenus_long = scot_synth_households[rep(1:nrow(scot_synth_households), times = scot_synth_households$households),]
  cenus_long$households = NULL

  cenus_long = cenus_long[order(cenus_long$LSOA21CD),]
  population = population[order(population$LSOA21CD),]

  cenus_long = dplyr::ungroup(cenus_long)
  cenus_long = dplyr::group_split(cenus_long, LSOA21CD)
  population = dplyr::group_split(population, LSOA21CD)


  t1 = Sys.time()
  future::plan("multisession")
  cenus_long2 = furrr::future_pmap(.l = list(
    cen = cenus_long,
    pop = population
  ),
  .f = select_synth_pop_year_scot,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = 1234L,
                                  scheduling  = 1))
  future::plan("sequential")
  t2 = Sys.time()
  message(round(difftime(t2,t1, units = "mins"),2), " min")

  cenus_long2 = data.table::rbindlist(cenus_long2)
  cenus_long2 = as.data.frame(cenus_long2)

  # Census Unique Combinations
  census_unique =  cenus_long2 |>
    dplyr::group_by(householdComp10, Tenure5, hhSize5, CarVan5, OAC11combine) |>
    dplyr::summarise(households = dplyr::n())


  hh$annual_income = hh$incanon * (365/7)

  t1 = Sys.time()
  future::plan("multisession")
  x = furrr::future_pmap(.l = list(
    Tenure5 = census_unique$Tenure5,
    hhComp15 = census_unique$householdComp10 ,
    hhSize5 = census_unique$hhSize5,
    CarVan5 = census_unique$CarVan5,
    OACs = census_unique$OAC11combine
  ),

  .f = match_hh_census3,
  hh = hh[,c("household_id","Tenure5","hhComp15","hhSize5","CarVan5","OAC")],
  similarity_table = similarity_table,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = 1234L,
                                  scheduling  = 1))
  future::plan("sequential")
  t2 = Sys.time()
  message(round(difftime(t2,t1, units = "mins"),2), " min")

  x = data.table::rbindlist(x)

  cenus_long2 = dplyr::left_join(cenus_long2, x,
                                 by = c("householdComp10" = "hhComp15",
                                        "Tenure5"= "Tenure5",
                                        "hhSize5" = "hhSize5",
                                        "CarVan5" = "CarVan5",
                                        "OAC11combine" = "OACs"))

  cenus_long2$sd_income = (cenus_long2$upper_limit - cenus_long2$lower_limit) / 3.92

  future::plan("multisession")
  cenus_long2$household_id_single = furrr::future_pmap_int(.l = list(lst = cenus_long2$household_id,
                                                                     mean_income = cenus_long2$total_annual_income,
                                                                     sd_income = cenus_long2$sd_income),
                                                           .f = select_id_income,
                                                           hh = hh[,c("household_id","annual_income")],
                                                           .options = furrr::furrr_options(seed = 1234L,
                                                                                           scheduling  = 1))
  future::plan("sequential")

  hh = hh[,!names(hh) %in% c("Tenure5","CarVan5","hhSize5","hhComp15")]
  cenus_long2 = dplyr::left_join(cenus_long2, hh, by = c("household_id_single" = "household_id"))
  cenus_long2

}

#' Map LCFS tenure labels to the census Tenure5 categories
#'
#' @description Converts the LCFS tenure descriptions to the pipeline's
#'   five tenure categories (outright, mortgage, socialrented,
#'   privaterented; rent-free counts as private rented).
#' @param housing_tenure Character/factor vector of LCFS tenure labels.
#' @return A character vector of Tenure5 codes (NA if unmatched).
#' @keywords internal
convert_housing_tenure <- function(housing_tenure) {
  # Define a named vector for mapping
  mapping <- c(
    "Owned outright" = "outright",
    "Priv. rented (unfurn)" = "privaterented",
    "LA (furnished unfurnished)" = "socialrented",
    "Owned with mortgage" = "mortgage",
    "Hsng Assn (furnished unfrnish)" = "socialrented",
    "Priv. rented (furnished)" = "privaterented",
    "Rent free" = "privaterented",
    "Owned by rental purchase" = "mortgage"
  )

  # Convert the input vector using the mapping
  converted_tenure <- mapping[as.character(housing_tenure)]
  converted_tenure <- unname(converted_tenure)

  # Return the converted vector
  return(converted_tenure)
}

#' Map LCFS NS-SEC labels to census NS-SEC codes
#'
#' @description Converts the LCFS NS-SEC descriptions to the shortened
#'   census codes (L1L2L3 ... L15, DNA) used elsewhere in the pipeline.
#' @param ns_sec Character vector of LCFS NS-SEC labels.
#' @return A named character vector of codes.
#' @keywords internal
convert_NSSEC <- function(ns_sec) {
  # Create a named vector for mapping
  mapping <- c(
    "Large employers and higher managerial occupations" = "L1L2L3",
    "Higher Professional occupations" = "L1L2L3",
    "Lower managerial and professional occupations" = "L4L5L6",
    "Intermediate occupations" = "L7",
    "Small employers and own account workers" = "L8L9",
    "Lower supervisory and technical occupations" = "L10L11",
    "Semi-routine occupations" = "L12",
    "Routine occupations" = "L13",
    "Never worked and long term unemployed" = "L14",
    "Students" = "L15",
    "Occupation not stated" = "DNA",
    "Not classified for other reasons" = "DNA"
  )

  # Convert the input NS-SEC classifications to the new classifications
  new_classification <- mapping[ns_sec]
  #new_classification[is.na(new_classification)] = "DNA"
  #new_classification = unname(new_classification)

  return(new_classification)
}

#' Band household sizes into the census hhSize5 categories
#'
#' @description Converts numeric household sizes to the p0/p1/p2/p3/p4+
#'   bands used for census matching.
#' @param hhsize Integer vector of household sizes.
#' @return A character vector of size bands (NA preserved).
#' @keywords internal
convert_household_size <- function(hhsize) {
  # classify one household size into a band
  classify_hh <- function(n) {
    if (is.na(n)) {
      return(NA)
    } else if (n == 0) {
      return("p0")
    } else if (n == 1) {
      return("p1")
    } else if (n == 2) {
      return("p2")
    } else if (n == 3) {
      return("p3")
    } else {
      return("p4+")
    }
  }

  # Apply the classification function to the input vector
  classified_hh <- sapply(hhsize, classify_hh)

  # Return the classified vector
  return(classified_hh)
}

#' Band car counts into the census CarVan5 categories
#'
#' @description Converts numeric car/van counts to the car0/car1/car2/car3+
#'   bands used for census matching.
#' @param num_cars Integer vector of cars/vans per household.
#' @return A character vector of car bands (NA preserved).
#' @keywords internal
convert_car_ownership <- function(num_cars) {
  # classify one car count into a band
  classify_cars <- function(n) {
    if (is.na(n)) {
      return(NA)
    } else if (n == 0) {
      return("car0")
    } else if (n == 1) {
      return("car1")
    } else if (n == 2) {
      return("car2")
    } else {
      return("car3+")
    }
  }

  # Apply the classification function to the input vector
  classified_cars <- sapply(num_cars, classify_cars)

  # Return the classified vector
  return(classified_cars)
}


# Accommodation Type
# TODO: Missing in 2020 and 2021 data. - Removed by ONS
#
# convert_housing_type <- function(housing_type) {
#   # Define a named vector for mapping
#   mapping <- c(
#     "Not Recorded" = NA,
#     "Whole house,bungalow-detached" = "Detached",
#     "Whole hse,bungalow-semi-dtchd" = "Semi",
#     "Whole house,bungalow-terraced" = "Terraced",
#     "Purpose-built flat maisonette" = "Flat",
#     "Part of house converted flat" = "Flat",
#     "Others" = "caravan"
#   )
#
#   # Convert the input vector using the mapping
#   converted_type <- mapping[housing_type]
#
#   # Return the converted vector
#   return(converted_type)
# }








#' Resample one LSOA's synthetic households to a target year (E&W)
#'
#' @description Adjusts the census-2021 synthetic households of one zone to
#'   represent an earlier/later year: the backcast dwelling-type counts are
#'   scaled by the year's occupancy rate (households / dwellings), then the
#'   right number of households of each accommodation type is sampled from
#'   the 2021 synthetic set (with replacement if more are needed than
#'   exist).
#' @param cen Synthetic households for one zone.
#' @param pop One row of household/dwelling counts for the target year.
#' @param bk One row of backcast dwelling-type counts for the target year.
#' @return The resampled synthetic household data frame.
#' @keywords internal
select_synth_pop_year = function(cen, pop, bk){
  if(!all(unique(c(cen$LSOA21CD,pop$LSOA21CD)) %in% unique(bk$lsoa21cd))){
    stop("LSOA don't match")
  }
  #cen_long = cen[rep(1:nrow(cen), times = cen$households),]
  #cen_long$households = NULL
  cen = dplyr::group_split(cen, AccType5)

  if(pop$all_properties == 0){
    weight =  0
  } else {
    weight =  pop$households_est / pop$all_properties
  }

  bk$detached = round(bk$detached * weight)
  bk$semidetached = round(bk$semidetached * weight)
  bk$terraced = round(bk$terraced * weight)
  bk$flat     = round(bk$flat * weight)
  bk$caravan  = round(bk$caravan * weight)

  cen_long2 = list()
  for(j in seq(1, length(cen))){
    cen_sub = cen[[j]]
    cnt = bk[[as.character(cen_sub$AccType5[1])]]
    if(cnt > 0){
      if(cnt <= nrow(cen_sub)){
        cen_long2[[j]] = cen_sub[sample(seq(1, nrow(cen_sub)), cnt),]
      } else {
        if(cnt - nrow(cen_sub) > nrow(cen_sub)){
          replace = TRUE
        } else {
          replace = FALSE
        }
        cen_long2[[j]] = rbind(cen_sub, cen_sub[sample(seq(1, nrow(cen_sub)), cnt - nrow(cen_sub), replace = replace),])
      }

    } else {
      cen_long2[[j]] = NULL
    }
  }

  cen_long2 = data.table::rbindlist(cen_long2)
  cen_long2 = as.data.frame(cen_long2)
  cen_long2

}


#' Resample one Data Zone's synthetic households to a target year (Scotland)
#'
#' @description Scottish version of `select_synth_pop_year()`: without a
#'   dwelling-type backcast, simply samples the target year's household
#'   count from the 2022 synthetic set (topping up with replacement when
#'   more households are needed than exist).
#' @param cen Synthetic households for one Data Zone.
#' @param pop One row of household counts for the target year.
#' @return The resampled synthetic household data frame.
#' @keywords internal
select_synth_pop_year_scot = function(cen, pop){
  if(length(unique(c(cen$LSOA21CD,pop$LSOA21CD))) != 1){
    stop("LSOA don't match")
  }

  if(is.na(pop$households_est)){
    weight =  0
  } else {
    weight =  pop$households_est
  }

  replace = FALSE
  if(weight > nrow(cen)){
    if((weight - nrow(cen)) > nrow(cen)){
      cen_long2 = cen[sample(seq_len(nrow(cen)), size = weight - nrow(cen), replace = TRUE),]
    } else {
      cen_long2 = cen[sample(seq_len(nrow(cen)), size = weight - nrow(cen), replace = FALSE),]
    }
    cen_long2 = rbind(cen_long2,cen)
  } else {
    cen_long2 = cen[sample(seq_len(nrow(cen)), size = weight, replace = FALSE),]
  }


  cen_long2

}
