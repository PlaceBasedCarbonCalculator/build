# Aggregate the per-LSOA tool datasets (transport & retrofit) to administrative
# areas (local authority, ward, parish, Westminster constituency), so the
# website's area report pages can show the same report cards as the LSOA
# report. Output JSONs deliberately keep the SAME column names as the per-LSOA
# exports, so the website's generated report-card modules work unchanged with
# only an endpoint override.
#
# Aggregation methods (see also website reports/README.md):
#  * counts            -> summed
#  * means / averages  -> recomputed from summed totals where possible,
#                         otherwise weighted means
#  * rates / percents  -> population-weighted means
#  * medians           -> WEIGHTED medians - an approximation (a true area
#                         median requires unit-level data); flagged on the
#                         website wherever shown
#  * isochrones        -> excluded (point-based; not meaningful for areas)

# The four supported area levels: level name -> lsoa_admin code column
area_levels <- function() {
  c(la = "LAD25CD", ward = "WD25CD", parish = "PAR23CD", constituency = "PCON24CD")
}

#' Weighted median
#'
#' @description Median of `x` where each observation carries weight `w`
#'   (e.g. a per-LSOA median weighted by that LSOA's meter count). This is an
#'   approximation of the true unit-level median and is documented as such.
#' @param x Numeric values (e.g. per-LSOA medians).
#' @param w Non-negative weights (e.g. meter counts).
#' @return The weighted median, or NA if no usable data.
#' @keywords internal
weighted_median <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  if (length(x) == 0) {
    return(NA_real_)
  }
  o <- order(x)
  x <- x[o]
  w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

#' Attach an area code and a static population weight to an LSOA dataset
#'
#' @description Joins the `lsoa_admin` area code column and a time-invariant
#'   population weight (the mean of `all_ages` across available years) onto a
#'   per-LSOA data frame. A static weight is used because the tool datasets
#'   span different year ranges than the population series.
#' @keywords internal
join_area_and_weight <- function(x, lsoa_admin, population, area_col) {
  admin <- lsoa_admin[, c("LSOA21CD", area_col)]
  x <- dplyr::left_join(x, admin, by = "LSOA21CD")
  popw <- population |>
    dplyr::group_by(LSOA21CD) |>
    dplyr::summarise(pop_weight = mean(all_ages, na.rm = TRUE))
  x <- dplyr::left_join(x, popw, by = "LSOA21CD")
  x$pop_weight[is.na(x$pop_weight) | x$pop_weight <= 0] <- 1
  # Drop LSOAs whose centroid fell outside any area of this type, and the
  # catch-all "Unparished" pseudo-parish (aggregating all unparished LSOAs
  # nationally would be meaningless)
  x <- x[!is.na(x[[area_col]]) & x[[area_col]] != "Unparished", ]
  x
}

#' Aggregate the vehicle summary (transport tool) to an area level
#'
#' @description Counts (vehicles by body/fuel/licence) are summed; rate and
#'   percentage columns (`p...`, `vehiclesP...`) are population-weighted means.
#' @return Data frame per (area, year) with the same columns as `vehicle_summary`.
#' @keywords internal
agg_area_vehicle_summary <- function(vehicle_summary, lsoa_admin, population, area_col) {
  x <- join_area_and_weight(vehicle_summary, lsoa_admin, population, area_col)
  num_cols <- setdiff(names(x)[sapply(x, is.numeric)], c("year", "pop_weight"))
  rate_cols <- grep("^p[A-Z]|^vehiclesP", num_cols, value = TRUE)
  count_cols <- setdiff(num_cols, rate_cols)
  x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(area_col, "year")))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(rate_cols), ~ stats::weighted.mean(.x, w = pop_weight, na.rm = TRUE)),
      .groups = "drop"
    )
}

#' Aggregate public transport frequency (transport tool) to an area level
#'
#' @description Reproduces the long->wide pivot used by the `pt_json` target,
#'   then takes the population-weighted mean of every frequency column: the
#'   result is the service frequency experienced by the average resident.
#' @return Data frame per (area, year) matching the per-LSOA PTfrequency JSON.
#' @keywords internal
agg_area_pt_frequency <- function(pt_frequency, lsoa_admin, population, area_col) {
  ptf <- pt_frequency[!is.na(pt_frequency$zone_id), ]
  names(ptf) <- gsub("Morning_Peak", "MorningPeak", names(ptf))
  names(ptf) <- gsub("Afternoon_Peak", "AfternoonPeak", names(ptf))
  ptf <- tidyr::pivot_longer(ptf,
    cols = tph_weekday_MorningPeak_2004_2:tph_daytime_avg_2023_4,
    names_prefix = "tph_", names_sep = "_",
    names_to = c("day", "time", "year", "mode"))
  ptf <- tidyr::pivot_wider(ptf,
    names_from = c("day", "time", "mode"), values_from = "value",
    id_cols = c("zone_id", "year"))
  names(ptf)[names(ptf) == "zone_id"] <- "LSOA21CD"
  x <- join_area_and_weight(ptf, lsoa_admin, population, area_col)
  freq_cols <- setdiff(names(x)[sapply(x, is.numeric)], c("pop_weight"))
  freq_cols <- setdiff(freq_cols, "year")
  x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(area_col, "year")))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(freq_cols), ~ stats::weighted.mean(.x, w = pop_weight, na.rm = TRUE)),
      .groups = "drop"
    )
}

#' Aggregate accessibility/proximity scores (transport tool) to an area level
#'
#' @description Population-weighted mean of each z-score per service class:
#'   the provision experienced by the average resident of the area. Scotland
#'   has no access data, so Scottish-only areas produce NA rows.
#' @return Data frame per (area, categoryname, classname).
#' @keywords internal
agg_area_access <- function(access_proximity, lsoa_admin, population, area_col) {
  keep <- c("LSOA21CD", "categoryname", "classname",
            "access_15", "proximity_15", "access_30", "proximity_30",
            "access_45", "proximity_45", "access_60", "proximity_60")
  x <- access_proximity[, keep]
  x <- join_area_and_weight(x, lsoa_admin, population, area_col)
  score_cols <- grep("^(access|proximity)_", names(x), value = TRUE)
  x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(area_col, "categoryname", "classname")))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(score_cols), ~ stats::weighted.mean(.x, w = pop_weight, na.rm = TRUE)),
      .groups = "drop"
    )
}

#' Aggregate the domestic EPC summary (retrofit tool) to an area level
#'
#' @description All dwelling counts are summed; `epc_score_avg` and
#'   `floor_area_avg` are recomputed as EPC-count-weighted means.
#' @return Data frame per area with the same columns as `epc_dom_summary`.
#' @keywords internal
agg_area_epc <- function(epc_dom_summary, lsoa_admin, population, area_col) {
  x <- join_area_and_weight(epc_dom_summary, lsoa_admin, population, area_col)
  num_cols <- setdiff(names(x)[sapply(x, is.numeric)], "pop_weight")
  avg_cols <- intersect(c("epc_score_avg", "floor_area_avg"), num_cols)
  count_cols <- setdiff(num_cols, avg_cols)
  # summarise() evaluates sequentially: the weighted means must be computed
  # BEFORE the counts are summed, or `w = epc_total` would see the summed
  # scalar rather than the per-LSOA vector (and error on length mismatch)
  out <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(area_col))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(avg_cols), ~ stats::weighted.mean(.x, w = epc_total, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  # Restore the per-LSOA export's column order
  out[, c(area_col, num_cols)]
}

#' Aggregate domestic gas/electricity consumption (retrofit tool) to an area level
#'
#' @description Meter counts and totals are summed; means are recomputed from
#'   the summed totals; medians are METER-WEIGHTED medians of the per-LSOA
#'   medians (approximate - flagged on the website); bills are meter-weighted
#'   means.
#' @return Data frame per (area, year) with the same columns as the per-LSOA
#'   gas/electric JSON export.
#' @keywords internal
agg_area_gas_electric <- function(gas_electric_lsoa, lsoa_admin, population, area_col) {
  x <- join_area_and_weight(gas_electric_lsoa, lsoa_admin, population, area_col)
  # summarise() evaluates sequentially, so everything weighted by the per-LSOA
  # meter counts (medians, bills) must be computed BEFORE meters_gas/meters_elec
  # are replaced by their sums; the means come last, derived from summed totals
  x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(area_col, "year")))) |>
    dplyr::summarise(
      median_gas_kwh = weighted_median(median_gas_kwh, meters_gas),
      median_elec_kwh = weighted_median(median_elec_kwh, meters_elec),
      median_gas_kgco2e = weighted_median(median_gas_kgco2e, meters_gas),
      median_elec_kgco2e = weighted_median(median_elec_kgco2e, meters_elec),
      gas_average_bill = stats::weighted.mean(gas_average_bill, w = meters_gas, na.rm = TRUE),
      elec_average_bill = stats::weighted.mean(elec_average_bill, w = meters_elec, na.rm = TRUE),
      energy_average_bill = stats::weighted.mean(energy_average_bill, w = meters_elec, na.rm = TRUE),
      otherheating_average_bill = stats::weighted.mean(otherheating_average_bill, w = pop_weight, na.rm = TRUE),
      mean_other_kgco2e = stats::weighted.mean(mean_other_kgco2e, w = pop_weight, na.rm = TRUE),
      meters_gas = sum(meters_gas, na.rm = TRUE),
      meters_elec = sum(meters_elec, na.rm = TRUE),
      total_gas_kwh = sum(total_gas_kwh, na.rm = TRUE),
      total_elec_kwh = sum(total_elec_kwh, na.rm = TRUE),
      total_gas_kgco2e = sum(total_gas_kgco2e, na.rm = TRUE),
      total_elec_kgco2e = sum(total_elec_kgco2e, na.rm = TRUE),
      mean_gas_kwh = total_gas_kwh / meters_gas,
      mean_elec_kwh = total_elec_kwh / meters_elec,
      mean_gas_kgco2e = total_gas_kgco2e / meters_gas,
      mean_elec_kgco2e = total_elec_kgco2e / meters_elec,
      .groups = "drop"
    ) |>
    # Restore the per-LSOA export's column order
    dplyr::select(dplyr::all_of(c(area_col, "year",
      "meters_gas", "meters_elec", "total_gas_kwh", "total_elec_kwh",
      "total_gas_kgco2e", "total_elec_kgco2e",
      "mean_gas_kwh", "mean_elec_kwh", "mean_gas_kgco2e", "mean_elec_kgco2e",
      "median_gas_kwh", "median_elec_kwh", "median_gas_kgco2e", "median_elec_kgco2e",
      "gas_average_bill", "elec_average_bill", "energy_average_bill",
      "otherheating_average_bill", "mean_other_kgco2e")))
}

#' Aggregate house prices (retrofit tool) to an area level
#'
#' @description Transaction counts are summed; `price_min`/`price_max` are the
#'   true min/max of the per-LSOA values; the quartile columns (`price_25`,
#'   `price_median`, `price_75`) are TRANSACTION-WEIGHTED medians of the
#'   per-LSOA quantiles (approximate - flagged on the website). Land Registry
#'   data covers England & Wales only, so Scottish-only areas produce no rows.
#' @return Data frame per (area, year) with the same columns as the per-LSOA
#'   prices export (`house_prices_lsoa`).
#' @keywords internal
agg_area_prices <- function(house_prices_lsoa, lsoa_admin, population, area_col) {
  x <- join_area_and_weight(house_prices_lsoa, lsoa_admin, population, area_col)
  # summarise() evaluates sequentially: the weighted quantiles must come
  # BEFORE transactions is replaced by its sum (see agg_area_gas_electric)
  x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(area_col, "year")))) |>
    dplyr::summarise(
      price_25 = weighted_median(price_25, transactions),
      price_median = weighted_median(price_median, transactions),
      price_75 = weighted_median(price_75, transactions),
      transactions = sum(transactions, na.rm = TRUE),
      price_min = min(price_min, na.rm = TRUE),
      price_max = max(price_max, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Restore the per-LSOA export's column order
    dplyr::select(dplyr::all_of(c(area_col, "year", "transactions",
      "price_min", "price_25", "price_median", "price_75", "price_max")))
}

#' Aggregate the population estimates (PBCC tool) to an area level
#'
#' @description Every column of the population summary is a count (residents by
#'   age band, estimated households, dwellings), so aggregation is a plain sum
#'   by area and year. Unparished LSOAs are dropped (see
#'   `join_area_and_weight()`); the population weight it attaches is unused here
#'   as nothing is averaged.
#' @return Data frame per (area, year) with the same columns as the per-LSOA
#'   population export (`population_summary`).
#' @keywords internal
agg_area_population <- function(population_summary, lsoa_admin, population, area_col) {
  x <- join_area_and_weight(population_summary, lsoa_admin, population, area_col)
  count_cols <- setdiff(names(x)[sapply(x, is.numeric)], c("year", "pop_weight"))
  x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(area_col, "year")))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    # Restore the per-LSOA export's column order
    dplyr::select(dplyr::all_of(c(area_col, "year", count_cols)))
}

#' Run one dataset's aggregation for all four area levels
#'
#' @param agg_fun One of the agg_area_* functions above.
#' @param ... Passed through to `agg_fun` (dataset, lsoa_admin, population).
#' @return Named list of data frames: la, ward, parish, constituency.
#' @keywords internal
agg_all_levels <- function(agg_fun, ...) {
  lapply(area_levels(), function(area_col) agg_fun(..., area_col = area_col))
}

#' Export a list of per-level aggregations as website binary files
#'
#' @description Writes a date-stamped `data_{level}_{name}_*.bin` plus index
#'   into `outputdata/jsonbin` for each level, using the same
#'   `export_zone_bin()` machinery as the per-LSOA exports. Deploy the bin and
#'   index files to the blob store.
#' @param lst Named list from `agg_all_levels()`.
#' @param name Dataset name used in the output file names (e.g.
#'   "vehicle_summary"), prefixed with the area level.
#' @param dataframe Passed to `export_zone_bin()`. MUST match the orientation
#'   of the corresponding per-LSOA export, or the website's report cards will
#'   not be able to read the area records (epc_dom is "rows", the rest are
#'   "columns").
#' @keywords internal
export_area_bins <- function(lst, name, rounddp = 2, dataframe = "columns") {
  levels <- area_levels()
  for (level in names(levels)) {
    export_zone_bin(lst[[level]],
      idcol = levels[[level]],
      name = paste0(level, "_", name),
      dataframe = dataframe, rounddp = rounddp)
  }
  invisible(NULL)
}
