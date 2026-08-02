# Read UPRN / address / Price Paid / INSPIRE objects from the sibling
# PlaceBasedCarbonCalculator/LandOwnership `targets` store - no rebuilding
# multi-GB objects that another pipeline already produces.
#
# Division of responsibility (July 2026): the LandOwnership repo owns
# everything UPRN / address related. It builds uprn_historical,
# house_price_lr, house_prices_ubdc, house_price_lr_uprn,
# house_prices_nowcast, uprn_historical_epc_lr and the cleaned INSPIRE
# parcels from the raw inputs and the EPC repo's cleaned registers (see
# LandOwnership/pipeline/R/{uprn_historical,price_paid,inspire_uprn_lookup}.R,
# which were ported from this repo). This repo consumes them rather than
# recomputing them, and remains the source of the generic statistical
# geographies (lookup_postcode_OA_LSOA_MSOA_2021, bounds_lsoa_GB_full,
# lsoa_admin, bounds_postcodes_2015/2020/2024) that LandOwnership reads back
# from our store - see LandOwnership/pipeline/R/external_resources.R for the
# mirror image of this file. Do not delete those targets from this pipeline
# even where nothing here consumes them.
#
# We take the `_final` variants where LandOwnership publishes one: those
# include its fuzzy / postcode-changed rematch recoveries, so they match
# more Price Paid transactions to UPRNs than the plain targets of the same
# name that this repo used to build itself.
#
# Caveat: tar_read_raw() gives `targets` nothing to hash, so this pipeline
# will NOT notice when LandOwnership rebuilds. After a LandOwnership run,
# invalidate the affected targets here by hand (tar_invalidate()). This is
# the same trade-off LandOwnership already accepts reading from us.

landownership_store <- "F:/GitHub/PlaceBasedCarbonCalculator/LandOwnership/_targets"

load_landownership_target <- function(name, store = landownership_store) {
  if (!dir.exists(store)) {
    stop(
      "LandOwnership targets store not found at ", store, ". ",
      "This target reads the UPRN / Price Paid / INSPIRE objects from the ",
      "PlaceBasedCarbonCalculator/LandOwnership repo - check it's still at ",
      "that path, and that its pipeline has been run."
    )
  }
  targets::tar_read_raw(name, store = store)
}

#' Land Registry Price Paid transactions with UPRN and LSOA attached
#'
#' @description LandOwnership's `house_price_lr_final`: every Price Paid
#'   transaction with a `uprn` and `LSOA21CD` where one could be found, after
#'   both the original matching cascade and the fuzzy rematch pass. Carries
#'   two extra columns this repo ignores (`match_source`, `match_quality`).
#'   Backs the `house_price_lr_uprn` target, which feeds
#'   `house_price_lsoa_summary()`.
#' @return A data frame of transactions with `date`, `price`, `uprn`,
#'   `LSOA21CD`.
#' @keywords internal
load_lo_house_price_lr_uprn <- function() {
  load_landownership_target("house_price_lr_final")
}

#' Per-property 2025 nowcast values
#'
#' @description LandOwnership's `house_prices_nowcast_final`: one row per
#'   property (its latest sale) with the local-authority growth multiple and
#'   `price_2025`. Backs the `house_prices_nowcast` target, which the
#'   retrofit map uses for the median property value per LSOA.
#' @return A data frame with `LSOA21CD`, `LAD25CD`, `price_2025`.
#' @keywords internal
load_lo_house_prices_nowcast <- function() {
  load_landownership_target("house_prices_nowcast_final")
}

#' UPRNs classified against the EPC registers and Land Registry
#'
#' @description LandOwnership's `uprn_historical_epc_lr_final`: every
#'   historical UPRN classified as domestic / non-domestic / unknown, with
#'   its newest EPC record, latest sale and 2025 value attached. Backs the
#'   `uprn_historical_epc_lr` target, which feeds the three published UPRN
#'   point layers.
#' @return A list of three data frames: `domestic`, `nondomestic`, `unknown`.
#' @keywords internal
load_lo_uprn_historical_epc_lr <- function() {
  load_landownership_target("uprn_historical_epc_lr_final")
}

#' Cleaned INSPIRE cadastral parcels for England & Wales
#'
#' @description LandOwnership's `inspire_clean`: the 2026 INSPIRE release,
#'   loaded in parallel and repaired for the 500m grid split artefact (the
#'   same cleaning this repo's `load_inspire()` used to do, on older data).
#'   Backs the `inspire` target, which `combine_os_osm_buildings()` uses to
#'   split building footprints along parcel boundaries. Scotland is not
#'   covered by LandOwnership - `inspire_scotland` is still built here.
#' @return An sf data frame in EPSG:27700 with `local_authority`,
#'   `INSPIREID`, `area` and a `GEOMETRY` column.
#' @keywords internal
load_lo_inspire <- function() {
  load_landownership_target("inspire_clean")
}
