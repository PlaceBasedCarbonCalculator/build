#' Load the airport-to-airport flight emissions dataset
#'
#' @description Reads the pre-built GeoPackage of passenger flows and annual
#'   emissions between airport pairs (from the CREDS long-distance travel
#'   work). Used by the `flights_od` target.
#' @param path Path to `od_emissions_<year>.gpkg`.
#' @return An sf data frame of airport-pair lines with `emissions_YYYY`
#'   columns.
#' @keywords internal
load_flights_od = function(path = "../../creds2/LDT/data/clean/od_emissions_2021.gpkg") {
  pass_od <- sf::read_sf(path)
  pass_od
}

#' Load airports and tag them with their UK home nation
#'
#' @description Reads the cleaned airports GeoPackage and spatially joins each
#'   airport to a home nation (E/W/S/N, derived by dissolving local authority
#'   boundaries on the first letter of their code); airports outside the UK
#'   become "Other Country". Used by the `flights_airports` target.
#' @param path Path to the cleaned airports GeoPackage.
#' @param bounds_la Local authority boundaries (`bounds_la` target).
#' @return An sf POINT data frame of airports with a `country_uk` column.
#' @keywords internal
load_flights_airports = function(path = "../../creds2/LDT/data/clean/airports_clean_second_pass_2021.gpkg", bounds_la) {
  airports <- sf::read_sf(path)

  # Add home nations to airports
  bounds_la$country_uk = substr(bounds_la$LAD25CD, 1, 1)
  bounds_la = dplyr::group_by(bounds_la, country_uk)
  bounds_la = dplyr::summarise(bounds_la)
  bounds_la = sf::st_transform(bounds_la, 4326)
  airports = sf::st_join(airports, bounds_la)
  airports$country_uk[is.na(airports$country_uk)] = "Other Country"


  airports

}

#' Total annual flight emissions attributable to each home nation
#'
#' @description Aggregates airport-pair emissions to national totals for
#'   2010 onwards. Domestic (UK-UK) emissions are split 50:50 between the
#'   origin and destination nations. International emissions are assigned to
#'   the UK end of the route, scaled by 0.64 (about 34% of passengers are
#'   foreign residents, whose emissions are not counted), pooled across
#'   England/Scotland/Wales ("ESW") because passengers freely cross borders
#'   to fly, and Northern Ireland's international flights are excluded from
#'   the GB total. Used by the `flights_total_emissions` target.
#' @param flights_od Airport-pair emissions (`flights_od` target).
#' @param flights_airports Airports with `country_uk` (`flights_airports`).
#' @param max_year Latest emissions year column to use.
#' @return A data frame with `country_uk`, `type`
#'   ("domestic"/"international") and `emissions_YYYY` columns.
#' @keywords internal
get_flights_total_emissions = function(flights_od, flights_airports, max_year = 2024) {

  flights_airports = sf::st_drop_geometry(flights_airports)
  flights_airports = flights_airports[!duplicated(flights_airports),]
  flights_od = sf::st_drop_geometry(flights_od)

  names(flights_airports)[3] = "fromclass"
  flights_od = dplyr::left_join(flights_od, flights_airports,
                                by = c("airport1" = "airport",
                                       "airport1_country" = "country"))
  names(flights_airports)[3] = "toclass"
  flights_od = dplyr::left_join(flights_od, flights_airports,
                                by = c("airport2" = "airport",
                                       "airport2_country" = "country"))


  flights_summary = flights_od[,c("fromclass","toclass",paste0("emissions_",2010:max_year))]
  flights_summary = dplyr::group_by(flights_summary, fromclass, toclass)
  flights_summary = dplyr::summarise_all(flights_summary, sum, na.rm = TRUE)
  flights_summary = dplyr::ungroup(flights_summary)

  flights_summary = flights_summary[!(flights_summary$fromclass == "Other Country" &
                                      flights_summary$toclass == "Other Country"),]

  # Split International and Domestic
  summary_dom = flights_summary[!(flights_summary$fromclass == "Other Country" |
                                    flights_summary$toclass == "Other Country"), ]
  summary_int = flights_summary[(flights_summary$fromclass == "Other Country" |
                                    flights_summary$toclass == "Other Country"), ]

  # Split Domestic emissions 50:50 between nations
  # (local helper: sum then take a fraction, e.g. half to each nation)
  part_sum = function(x, frac = 0.5){
    sum(x, na.rm = TRUE) * frac
  }

  dom_from = dplyr::select(summary_dom, -toclass)
  dom_from = dplyr::group_by(dom_from, fromclass)
  dom_from = dplyr::summarise_all(dom_from, part_sum)

  dom_to = dplyr::select(summary_dom, -fromclass)
  dom_to = dplyr::group_by(dom_to, toclass)
  dom_to = dplyr::summarise_all(dom_to, part_sum)

  names(dom_from)[1] = "country_uk"
  names(dom_to)[1] = "country_uk"
  emissions_dom = rbind(dom_from, dom_to)
  emissions_dom = dplyr::group_by(emissions_dom, country_uk)
  emissions_dom = dplyr::summarise_all(emissions_dom, sum, na.rm = TRUE)
  emissions_dom = dplyr::ungroup(emissions_dom)

  # International Emissions
  #34% of passengers are foreign residents (2016)
  #TODO: Get time series of British vs foreign residents
  for(i in 1:nrow(summary_int)){
    if(summary_int$fromclass[i] == "Other Country"){
      summary_int$fromclass[i] = summary_int$toclass[i]
    }
  }

  summary_int = dplyr::select(summary_int, -toclass)
  summary_int = dplyr::group_by(summary_int, fromclass)
  summary_int = dplyr::summarise_all(summary_int, part_sum, frac = 0.64)

  # No way to split international flights between nation as people easily can
  # cross boarders for flights Assume international flights from NI only belong
  # to NI people, check on Google Flights Belfast airports currently only serve
  # Europe/near east. So seems unlikely many GB people will connect through Belfast
  # TODO: Better way to split emissions between nations.
  summary_int = summary_int[summary_int$fromclass != "N",]

  summary_int = as.data.frame(t(colSums(summary_int[grepl("emissions_",names(summary_int))])))
  summary_int$country_uk = "ESW"
  summary_int$type = "international"

  emissions_dom$type = "domestic"

  summary_all = rbind(summary_int, emissions_dom)
  # summary_all = dplyr::group_by(summary_all, country_uk)
  # summary_all = dplyr::summarise_all(summary_all, sum, na.rm = TRUE)

  # Check 99.98219% of emissions (some channel island to Europe)
  # for(i in 1990:2021){
  #   print(sum(summary_all[paste0("emissions_",i)]) / sum(flights_od[paste0("emissions_",i)], na.rm = TRUE))
  # }

  summary_all

}

#' Distribute national flight emissions to zones by estimated flying habits
#'
#' @description Downscales the national flight emissions totals to LSOAs.
#'   Each zone's share of the domestic total (within its nation) and the GB
#'   international total is weighted by the flight counts estimated from the
#'   synthetic population/LCFS consumption data (returns counted twice). A
#'   consistency check verifies the 2022 zone totals sum back to the national
#'   figure. Used by the `flights_lsoa_emissions` target, feeding
#'   `combine_lsoa_emissions()`.
#' @param flights_total_emissions National totals
#'   (`flights_total_emissions` target).
#' @param consumption_emissions Per-zone consumption results including
#'   flight counts (`consumption_emissions` target).
#' @return A data frame per zone-year with flight counts,
#'   `emissions_international`, `emissions_domestic` and `emissions_percap`.
#' @keywords internal
get_flights_lsoa_emissions = function(flights_total_emissions, consumption_emissions){

  # Consistency Checks
  chk_total = sum(flights_total_emissions$emissions_2022[flights_total_emissions$country_uk != "N"])


  consumption_emissions = consumption_emissions[,c("LSOA21CD","year","all_ages",
                                                   "flight_international_return",
                                                   "flight_other","flight_domestic_return",
                                                   "flight_domestic_single")]

  flights_total_emissions = tidyr::pivot_longer(flights_total_emissions,
                                                cols = names(flights_total_emissions)[grepl("emissions_",names(flights_total_emissions))],
                                                names_sep = "_",
                                                names_to = c("dud","year"),
                                                values_to = "emissions")

  flights_total_emissions$dud = NULL

  flights_total_emissions = tidyr::pivot_wider(flights_total_emissions,
                                               values_from  = "emissions",
                                               names_from = "type")

  consumption_emissions$weight_international = (consumption_emissions$flight_international_return * 2) +
    consumption_emissions$flight_other
  consumption_emissions$weight_domestic = (consumption_emissions$flight_domestic_return * 2) +
    consumption_emissions$flight_domestic_single

  consumption_emissions$country_uk = substr(consumption_emissions$LSOA21CD,1,1)

  flights_total_emissions$year = as.numeric(flights_total_emissions$year)

  # Domestic Join
  consumption_emissions = dplyr::left_join(consumption_emissions,
                                           flights_total_emissions[,c("country_uk","year","domestic")],
                                           by = c("year","country_uk"))
  # Interntational Join
  consumption_emissions = dplyr::left_join(consumption_emissions,
                                           flights_total_emissions[flights_total_emissions$country_uk == "ESW",c("year","international")],
                                           by = c("year"))

  emissions_summary = consumption_emissions |>
    dplyr::group_by(year, country_uk) |>
    dplyr::mutate(weight_domestic = (weight_domestic / sum(weight_domestic))) |>
    dplyr::ungroup(year, country_uk)

  emissions_summary = emissions_summary |>
    dplyr::group_by(year) |>
    dplyr::mutate(weight_international = weight_international / sum(weight_international)) |>
    dplyr::ungroup(year)

  # sum(emissions_summary$weight_international[emissions_summary$year == 2019]) # 1
  # sum(emissions_summary$weight_domestic[emissions_summary$year == 2019 & emissions_summary$country_uk == "E"]) # 1
  # sum(emissions_summary$weight_domestic[emissions_summary$year == 2019 & emissions_summary$country_uk == "W"]) # 1

  emissions_summary$emissions_international = emissions_summary$international * emissions_summary$weight_international
  emissions_summary$emissions_domestic = emissions_summary$domestic * emissions_summary$weight_domestic

  # Check
  if(abs(sum(c(emissions_summary$emissions_international[emissions_summary$year == 2022],
           emissions_summary$emissions_domestic[emissions_summary$year == 2022])) - chk_total) > 1){
    stop("Flight emissission check failed: Total ",round(sum(c(emissions_summary$emissions_international[emissions_summary$year == 2022],
                                                         emissions_summary$emissions_domestic[emissions_summary$year == 2022])))," not equalt to ",round(chk_total))
  }


  emissions_summary$emissions_percap = remove_inf((emissions_summary$emissions_international + emissions_summary$emissions_domestic) / emissions_summary$all_ages)

  emissions_summary = emissions_summary[,c("LSOA21CD","year",
                                           "flight_international_return","flight_other",
                                           "flight_domestic_return","flight_domestic_single",
                                           "emissions_international","emissions_domestic","emissions_percap")]
  emissions_summary

}





#' Assign each value its percentile rank (0-100)
#'
#' @description Computes the 0-100 percentile breaks of `dat` and returns,
#'   for each value, the percentile band it falls in. Duplicate break values
#'   (common with many zeros) are collapsed so ties share the same
#'   percentile. Used by `value2grade()` in grades.R to convert emissions to
#'   A+ to F- grades.
#' @param dat Numeric vector.
#' @param zeroNA If TRUE, zeros are treated as NA (excluded from break
#'   calculation and returned as NA).
#' @return An integer vector of percentile bands (0-99, each one hundredth of
#'   the distribution), NA where `dat` is NA.
#' @keywords internal
percentile <- function(dat, zeroNA = FALSE){
  if(zeroNA){
    dat[dat == 0] = NA
  }
  pt1 <- quantile(dat, probs = seq(0, 1, by = 0.01), type = 7, na.rm = TRUE)
  pt2 <- unique(as.data.frame(pt1), fromLast = TRUE)
  pt3 <- rownames(pt2)
  pt4 <- as.integer(strsplit(pt3, "%"))

  # 101 quantile breaks bound 100 intervals, so there are 100 bands, numbered
  # 0-99: band j is the slice (q_j, q_j+1], with include.lowest putting the
  # minimum in band 0.
  #
  # This used to prepend an extra break - hard-coded 0 - to make a 101st band,
  # which caused two problems. Band 0 then spanned (0, min] and so held only
  # the single lowest zone rather than a percent of them, and the 0 is only
  # below the data for a non-negative variable: for one that can go negative
  # (preduction, the fall in emissions since 2010, negative wherever emissions
  # rose) it sorted into the middle of the quantiles, displacing every value
  # below it by a band and leaving a step discontinuity at zero.
  datp <- pt4[-length(pt4)][as.integer(
    cut(dat, pt2$pt1, labels = seq_len(length(pt3) - 1), include.lowest = TRUE))]
  datp
}
