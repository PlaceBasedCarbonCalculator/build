#' Combine LSOA Bounds
#'
#' @description Combine LSOA bounds inputs into a single consolidated result.
#' @details Merge LSOA and Data Zone boundaries, and simplify Data Zones for
#'   low-res equivalents.
#' @param bounds_lsoa Input object or parameter named `bounds_lsoa`.
#' @param bounds_dz11 Input object or parameter named `bounds_dz11`.
#' @param keep Input object or parameter named `keep`.
#' @return A combined data frame or table merging the provided inputs.
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

