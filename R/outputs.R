#' Select the emissions grade columns shown on the main map
#'
#' @description Builds the attribute table for the main PBCC map tiles
#'   (`lsoa_map_data` target, consumed by `pmtiles_pbcc`). Takes the per-LSOA
#'   emissions grades for `year`, computes the percentage reduction in total
#'   per-capita emissions between 2010 and `year` (graded via `value2grade()`),
#'   and joins on the LSOA area classification code.
#' @param lsoa_emissions_all Output of `combine_lsoa_emissions()`: per-LSOA,
#'   per-year emissions with grade columns.
#' @param area_classifications_11_21 Output of
#'   `match_2011_classifications_2021()`, with `LSOA21CD` and `lsoa_class_code`.
#' @param year The year of emissions data to map (2019 in `_targets.R`).
#' @return A data frame with one row per LSOA: grade columns for each emissions
#'   domain, a `reduction_grade`, and the `lsoa_class_code`.
#' @keywords internal
select_map_outputs = function(lsoa_emissions_all, area_classifications_11_21, year = 2020) {
  if(year == 2010){
    stop("year must differ from the 2010 baseline used for the reduction grade")
  }
  lsoa_savings = lsoa_emissions_all[lsoa_emissions_all$year %in% c(2010,year),]
  lsoa_savings = lsoa_savings[,c("LSOA21CD","total_kgco2e_percap","year")]
  lsoa_savings = lsoa_savings |>
    tidyr::pivot_wider(values_from = total_kgco2e_percap, names_from = year)
  lsoa_savings$preduction = (lsoa_savings[["2010"]] - lsoa_savings[[as.character(year)]]) / lsoa_savings[["2010"]] * 100
  lsoa_savings$preduction[is.infinite(lsoa_savings$preduction)] = NA
  lsoa_savings$reduction_grade = value2grade(lsoa_savings$preduction, high_good = TRUE, zeroNA = FALSE)

  lsoa = lsoa_emissions_all[lsoa_emissions_all$year == year,]
  lsoa = lsoa[,c("LSOA21CD","dom_gas_grade","dom_elec_grade","car_grade",
                 "van_grade","flights_grade","total_grade","goods_services_combined_grade")]

  lsoa = dplyr::left_join(lsoa, lsoa_savings[,c("LSOA21CD","reduction_grade")], by = "LSOA21CD")

  lsoa = dplyr::left_join(lsoa, area_classifications_11_21[,c("LSOA21CD","lsoa_class_code")], by = "LSOA21CD")

  lsoa
}


#' Join map attribute data onto LSOA boundaries
#'
#' @description Left-joins per-LSOA map data (e.g. from
#'   `select_map_outputs()`) onto an sf boundary layer by `LSOA21CD`, ready for
#'   GeoJSON/PMTiles export.
#' @param lsoa_map_data Data frame with an `LSOA21CD` column.
#' @param bounds sf data frame of LSOA boundaries with an `LSOA21CD` column.
#' @return The `bounds` sf data frame with the attribute columns joined on.
#' @keywords internal
join_for_geojson = function(lsoa_map_data, bounds) {
  bounds = dplyr::left_join(bounds, lsoa_map_data, by = "LSOA21CD")
  bounds
}
