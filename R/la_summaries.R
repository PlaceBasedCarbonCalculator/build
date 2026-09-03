# Per-capita emissions summaries for administrative areas (LA, ward, parish,
# constituency) and OAC groups, aggregated from the per-LSOA emissions table.

# Output per-capita column name -> source column in lsoa_emissions_all.
# Shared by all the area summary functions below; the order sets the output
# column order and matches the per-LSOA table.
percap_source_cols = c(
  dom_gas_kgco2e_percap = "dom_gas_total_emissions",
  dom_elec_kgco2e_percap = "dom_elec_total_emissions",
  car_kgco2e_percap = "car_emissions",
  van_kgco2e_percap = "van_emissions",
  company_bike_kgco2e_percap = "company_bike_emissions",
  heating_other_kgco2e_percap = "heating_other_emissions_total",
  food_kgco2e_percap = "emissions_food",
  alcohol_kgco2e_percap = "emissions_alcohol",
  clothing_kgco2e_percap = "emissions_clothing",
  communication_kgco2e_percap = "emissions_communication",
  housing_other_kgco2e_percap = "emissions_housing_other",
  furnish_kgco2e_percap = "emissions_furnish",
  recreation_kgco2e_percap = "emissions_recreation",
  transport_optranequip_other_kgco2e_percap = "emissions_transport_optranequip_other",
  transport_vehiclepurchase_kgco2e_percap = "emissions_transport_vehiclepurchase",
  transport_pt_kgco2e_percap = "emissions_transport_pt",
  health_kgco2e_percap = "emissions_health",
  education_kgco2e_percap = "emissions_education",
  restaurant_kgco2e_percap = "emissions_restaurant",
  misc_kgco2e_percap = "emissions_misc",
  flights_kgco2e_percap = "flights_emissions_total",
  goods_services_combined_kgco2e_percap = "goods_services_combined_total",
  total_kgco2e_percap = "emissions_total"
)

#' Aggregate per-LSOA emissions to per-capita summaries for a set of areas
#'
#' @description Shared worker behind `make_la_summary()`,
#'   `make_westminter_summary()`, `make_parish_summary()`,
#'   `make_ward_summary()` and `make_oac_summary()`. Joins the area lookup and
#'   population onto the per-LSOA emissions, then for each area and year
#'   returns total emissions per domain divided by total population. Area-years
#'   with zero or unknown population get NA (never Inf/NaN) so zero-population
#'   LSOAs (e.g. new developments in early years) can't corrupt the output.
#'   Optionally adds `*_grade` columns (A+ to F- via `value2grade()`, as for
#'   LSOAs) ranked within each year across the areas in `summ`, i.e. relative
#'   to other areas of the same type.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`).
#' @param lookup Data frame with `LSOA21CD` plus the area code column(s) named
#'   in `by`.
#' @param population GB population (`population` target).
#' @param by Character, the area code column to group by (e.g. "LAD25CD");
#'   `character(0)` gives a single national row per year.
#' @param grades Logical, add `*_grade` columns (default TRUE).
#' @param weight Name of a column in `lookup` holding the share of each LSOA
#'   belonging to the area, or NULL for a whole-LSOA lookup. Where given,
#'   `lookup` may hold several rows per LSOA (see `lsoa_area_weights()`) and
#'   both the emissions and the population of each LSOA are split between them
#'   in proportion to where its residents live. Per-capita figures are then the
#'   population-weighted mean of the LSOA values, which is what lets small
#'   areas such as parishes have their own numbers.
#' @return A data frame per area-year of per-capita emissions by domain,
#'   plus grades if requested.
#' @keywords internal
summarise_emissions_by = function(lsoa_emissions_all, lookup, population, by,
                                  grades = TRUE, weight = NULL){

  population = population[,c("LSOA21CD","year","all_ages")]

  lsoa_emissions_all = lsoa_emissions_all[,!grepl("grade",names(lsoa_emissions_all))]
  lsoa_emissions_all = lsoa_emissions_all[,!grepl("kgco2e_percap",names(lsoa_emissions_all))]

  # A weighted lookup holds one row per (LSOA, area) pair and the emissions
  # table one per (LSOA, year), so the join is deliberately many-to-many
  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, lookup, by = "LSOA21CD",
                                        relationship = "many-to-many")
  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, population, by = c("LSOA21CD","year"))

  # Drop LSOAs with no code for this area type (e.g. missing from the lookup):
  # they would otherwise form an NA group, which export_zone_bin() rejects
  if(length(by) > 0){
    lsoa_emissions_all = lsoa_emissions_all[!is.na(lsoa_emissions_all[[by]]), ]
  }

  # Split each LSOA's totals between the areas it overlaps. Both the emissions
  # and the population are scaled by the same share, so an area's per-capita
  # figure is unaffected by the size of the share and only its contribution to
  # the area total changes.
  if(!is.null(weight)){
    shares = lsoa_emissions_all[[weight]]
    for(col in c(unname(percap_source_cols), "all_ages")){
      lsoa_emissions_all[[col]] = lsoa_emissions_all[[col]] * shares
    }
  }

  # na.rm = TRUE: combine_lsoa_emissions() NAs the company/bike columns of the
  # handful of zones whose fleet registrations are implausible, and a plain
  # sum() would propagate that single NA to the whole area's figure. Dropping
  # it instead carries the same suppression up to the area, which is what the
  # per-LSOA and per-area numbers have to agree on.
  summ = lsoa_emissions_all |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "year")))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(c(unname(percap_source_cols), "all_ages")),
                    \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )

  # Per-capita, but NA rather than Inf/NaN where an area-year has no population
  pop = summ$all_ages
  no_pop = is.na(pop) | pop == 0
  for(i in seq_along(percap_source_cols)){
    vals = summ[[percap_source_cols[i]]] / pop
    vals[no_pop] = NA_real_
    summ[[names(percap_source_cols)[i]]] = vals
  }
  summ = summ[, c(by, "year", names(percap_source_cols))]

  if(grades){
    summ = grade_percap_by_year(summ)
  }

  summ
}

#' Add A+ to F- grade columns to a per-capita emissions summary
#'
#' @description For each `*_kgco2e_percap` column in `percap_source_cols`,
#'   adds a `*_grade` column (named as in the per-LSOA table, e.g.
#'   `dom_gas_grade`) grading each area against the other areas in `summ` for
#'   the same year via `value2grade()`. NA values (including zero-population
#'   area-years) grade as "NA".
#' @param summ Output of `summarise_emissions_by()`, with a `year` column.
#' @return `summ` with the grade columns appended.
#' @keywords internal
grade_percap_by_year = function(summ){
  for(nm in names(percap_source_cols)){
    gname = sub("_kgco2e_percap$", "_grade", nm)
    summ[[gname]] = NA_character_
    for(yr in unique(summ$year)){
      sel = summ$year == yr
      summ[[gname]][sel] = value2grade(summ[[nm]][sel])
    }
  }
  summ
}

#' Summarise per-capita emissions by local authority
#'
#' @description Aggregates the per-LSOA emissions totals to local
#'   authorities: for each LAD and year, total emissions per domain divided
#'   by total population, with grades relative to other LAs in the same year
#'   (see `summarise_emissions_by()`). A GB-wide row (LAD25CD = "GB") is
#'   prepended for comparison; it is not an LA so its grades are "NA". Used by
#'   the `la_emissions_all` target, exported by `la_emissions_summary_json`.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`).
#' @param lsoa_admin Zone-to-LA lookup (`lsoa_admin` target).
#' @param population GB population (`population` target).
#' @return A data frame per LAD25CD-year of per-capita emissions by domain
#'   plus `*_grade` columns.
#' @keywords internal
make_la_summary = function(lsoa_emissions_all, lsoa_admin, population){

  lsoa_admin = lsoa_admin[,c("LSOA21CD","LAD25CD")]

  la_emissions = summarise_emissions_by(lsoa_emissions_all, lsoa_admin, population,
                                        by = "LAD25CD")

  national_emissions = summarise_emissions_by(lsoa_emissions_all, lsoa_admin, population,
                                              by = character(0), grades = FALSE)
  national_emissions$LAD25CD = "GB"
  # GB isn't an LA so isn't graded
  for(gname in grep("_grade$", names(la_emissions), value = TRUE)){
    national_emissions[[gname]] = "NA"
  }

  la_emissions = rbind(national_emissions[, names(la_emissions)], la_emissions)

  la_emissions

}

#' Summarise per-capita emissions by Westminster constituency
#'
#' @description As `make_la_summary()` but grouped by parliamentary
#'   constituency (`PCON24CD`), without a GB row. Grades are relative to other
#'   constituencies in the same year. Used by the `constituency_emissions_all`
#'   target, exported by `constituency_emissions_summary_json`.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`).
#' @param lsoa_admin Zone-to-admin lookup (`lsoa_admin` target).
#' @param population GB population (`population` target).
#' @return A data frame per PCON24CD-year of per-capita emissions plus grades.
#' @keywords internal
make_westminter_summary = function(lsoa_emissions_all, lsoa_admin, population){

  summarise_emissions_by(lsoa_emissions_all, lsoa_admin[,c("LSOA21CD","PCON24CD")],
                         population, by = "PCON24CD")

}

#' Summarise per-capita emissions by parish
#'
#' @description As `make_la_summary()` but grouped by parish (`PAR23CD`),
#'   without a GB row. Grades are relative to other parishes in the same year.
#'   A parish is usually much smaller than an LSOA, so this uses the population
#'   weighted lookup (`area_weights$parish`) rather than assigning each LSOA
#'   wholly to one parish: see `lsoa_area_weights()` for why. Used by the
#'   `parish_emissions_all` target, exported by `parish_emissions_summary_json`.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`).
#' @param area_weights Weighted LSOA-to-area lookups (`area_weights` target).
#' @param population GB population (`population` target).
#' @return A data frame per PAR23CD-year of per-capita emissions plus grades.
#' @keywords internal
make_parish_summary = function(lsoa_emissions_all, area_weights, population){

  summarise_emissions_by(lsoa_emissions_all, area_weights$parish,
                         population, by = "PAR23CD", weight = "weight")

}

#' Summarise per-capita emissions by electoral ward
#'
#' @description As `make_la_summary()` but grouped by ward (`WD25CD`),
#'   without a GB row. Grades are relative to other wards in the same year.
#'   Uses the population weighted lookup (`area_weights$ward`), which splits
#'   LSOAs between the wards they straddle and covers the small wards no LSOA
#'   centroid falls inside (see `lsoa_area_weights()`).
#'   Used by the `ward_emissions_all` target, exported by
#'   `ward_emissions_summary_json`.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`).
#' @param area_weights Weighted LSOA-to-area lookups (`area_weights` target).
#' @param population GB population (`population` target).
#' @return A data frame per WD25CD-year of per-capita emissions plus grades.
#' @keywords internal
make_ward_summary = function(lsoa_emissions_all, area_weights, population){

  summarise_emissions_by(lsoa_emissions_all, area_weights$ward,
                         population, by = "WD25CD", weight = "weight")

}

#' Summarise per-capita emissions by area classification group
#'
#' @description As `make_la_summary()` but grouped by the 2011 LSOA area
#'   classification group code, allowing "places like this" comparisons.
#'   No grade columns (grading a couple of dozen OAC groups against each other
#'   adds little). Used by the `oac_emissions_all` target, exported by
#'   `oac_emissions_summary_json`.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`).
#' @param area_classifications_11_21 Classifications on 2021 zones.
#' @param population GB population (`population` target).
#' @return A data frame per `lsoa_class_code`-year of per-capita emissions.
#' @keywords internal
make_oac_summary = function(lsoa_emissions_all, area_classifications_11_21, population){

  summarise_emissions_by(lsoa_emissions_all,
                         area_classifications_11_21[,c("LSOA21CD","lsoa_class_code")],
                         population, by = "lsoa_class_code", grades = FALSE)

}
