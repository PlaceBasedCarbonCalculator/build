# Summarise Land Registry price-paid transactions per LSOA.
#
# The matching of transactions to UPRNs (land_registry_add_uprn()) and the
# 2025 nowcast (house_price_extrapolate()) have moved to the LandOwnership
# repo, which owns all UPRN / address work as of July 2026 - see
# LandOwnership/pipeline/R/price_paid.R. The `house_price_lr_uprn` and
# `house_prices_nowcast` targets here read its outputs
# (R/landownership_resources.R). This summary was never ported, so it stays.

#' Summarise house prices per LSOA per year
#'
#' @description Aggregates the geocoded price-paid transactions to
#'   transaction counts and price quantiles (min/25/median/75/max) per 2021
#'   LSOA per year. Used by the `house_prices_lsoa` target, exported by
#'   `prices_json`.
#' @param house_price_lr_uprn Geocoded transactions
#'   (`house_price_lr_uprn` target).
#' @return A data frame per LSOA-year of transaction counts and price
#'   quantiles.
#' @keywords internal
house_price_lsoa_summary = function(house_price_lr_uprn){

  house_price_lr_uprn$year = lubridate::year(house_price_lr_uprn$date)

  house_price_lsoa = house_price_lr_uprn |>
    dplyr::group_by(LSOA21CD, year) |>
    dplyr::summarise(transactions = dplyr::n(),
                     price_min = min(price),
                     price_25 = quantile(price, 0.25),
                     price_median = median(price),
                     price_75 = quantile(price, 0.75),
                     price_max = max(price)
    )

  house_price_lsoa = house_price_lsoa[!is.na(house_price_lsoa$LSOA21CD),]

  house_price_lsoa
}
