#' Make Transport LA Summary
#'
#' @description Summarise LSOA transport data to local authority scale using
#'   population-weighted means.
#' @param transport_lsoa_data Input object or parameter named `transport_lsoa_data`.
#' @param lsoa_admin Input object or parameter named `lsoa_admin`.
#' @param population Input object or parameter named `population`.
#' @param pop_year Year to use for population weights.
#' @return A data frame with transport variables summarised at LAD25CD level.
#' @keywords internal
make_transport_la_summary = function(transport_lsoa_data, lsoa_admin, population, pop_year = 2021){

  lsoa_admin = lsoa_admin[, c("LSOA21CD", "LAD25CD")]

  population = population[population$year == pop_year, c("LSOA21CD", "all_ages")]

  dat = dplyr::left_join(transport_lsoa_data, lsoa_admin, by = "LSOA21CD")
  dat = dplyr::left_join(dat, population, by = "LSOA21CD")

  wpop_mean = function(x, w) {
    keep = !is.na(x)
    sum(x[keep] * w[keep], na.rm = TRUE) / sum(w[keep], na.rm = TRUE)
  }

  la_transport = dat |>
    dplyr::group_by(LAD25CD) |>
    dplyr::summarise(
      pBEV_COMPANY        = wpop_mean(pBEV_COMPANY,        all_ages),
      pBEV_PRIVATE        = wpop_mean(pBEV_PRIVATE,        all_ages),
      pULEV_COMPANY       = wpop_mean(pULEV_COMPANY,       all_ages),
      pULEV_PRIVATE       = wpop_mean(pULEV_PRIVATE,       all_ages),
      vehiclesPHousehold  = wpop_mean(vehiclesPHousehold,  all_ages),
      change_bus_2008_2023       = wpop_mean(change_bus_2008_2023,       all_ages),
      tph_daytime_avg_2023_0     = wpop_mean(tph_daytime_avg_2023_0,     all_ages),
      tph_daytime_avg_2023_1     = wpop_mean(tph_daytime_avg_2023_1,     all_ages),
      tph_daytime_avg_2023_2     = wpop_mean(tph_daytime_avg_2023_2,     all_ages),
      tph_daytime_avg_2023_3     = wpop_mean(tph_daytime_avg_2023_3,     all_ages),
      tph_daytime_avg_2023_4     = wpop_mean(tph_daytime_avg_2023_4,     all_ages)
    )

  national_transport = dat |>
    dplyr::summarise(
      pBEV_COMPANY        = wpop_mean(pBEV_COMPANY,        all_ages),
      pBEV_PRIVATE        = wpop_mean(pBEV_PRIVATE,        all_ages),
      pULEV_COMPANY       = wpop_mean(pULEV_COMPANY,       all_ages),
      pULEV_PRIVATE       = wpop_mean(pULEV_PRIVATE,       all_ages),
      vehiclesPHousehold  = wpop_mean(vehiclesPHousehold,  all_ages),
      change_bus_2008_2023       = wpop_mean(change_bus_2008_2023,       all_ages),
      tph_daytime_avg_2023_0     = wpop_mean(tph_daytime_avg_2023_0,     all_ages),
      tph_daytime_avg_2023_1     = wpop_mean(tph_daytime_avg_2023_1,     all_ages),
      tph_daytime_avg_2023_2     = wpop_mean(tph_daytime_avg_2023_2,     all_ages),
      tph_daytime_avg_2023_3     = wpop_mean(tph_daytime_avg_2023_3,     all_ages),
      tph_daytime_avg_2023_4     = wpop_mean(tph_daytime_avg_2023_4,     all_ages)
    )
  national_transport$LAD25CD = "GB"

  la_transport = rbind(national_transport, la_transport)

  la_transport

}
