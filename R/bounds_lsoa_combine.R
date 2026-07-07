#' Combine E&W LSOA and Scottish Data Zone boundaries into one GB layer
#'
#' @description Renames the Data Zone code column to `LSOA21CD` and appends
#'   the Scottish zones to the England & Wales LSOAs, giving a single GB zone
#'   layer. Scottish Data Zones are only published at full resolution, so when
#'   `keep < 1` they are simplified with `rmapshaper::ms_simplify()` to match
#'   the generalisation of the E&W layer. Used by the `bounds_lsoa_GB_full`
#'   (keep = 1), `bounds_lsoa_GB_generalised` (keep = 0.2) and
#'   `bounds_lsoa_GB_super_generalised` (keep = 0.05) targets.
#' @param bounds_lsoa sf 2021 LSOA boundaries for England & Wales.
#' @param bounds_dz11 sf 2022 Scottish Data Zone boundaries (`bounds_dz22`
#'   target; the parameter name is historical).
#' @param keep Proportion of vertices to retain when simplifying the Data
#'   Zones; 1 means no simplification.
#' @return An sf data frame of GB zones with a single `LSOA21CD` column.
#' @keywords internal
combine_lsoa_bounds = function(bounds_lsoa, bounds_dz11, keep = 1){
  names(bounds_dz11)[1] = "LSOA21CD"
  if(keep < 1){
    bounds_dz11 = rmapshaper::ms_simplify(bounds_dz11, keep = keep, keep_shapes = TRUE)
  }

  bounds_lsoa$LSOA21NM = NULL
  bounds_lsoa = rbind(bounds_lsoa, bounds_dz11)
  bounds_lsoa
}

