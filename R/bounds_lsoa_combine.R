combine_lsoa_bounds = function(bounds_lsoa, bounds_dz11, keep = 1){
  names(bounds_dz11)[1] = "LSOA21CD"
  if(keep < 1){
    bounds_dz11 = rmapshaper::ms_simplify(bounds_dz11, keep = keep, keep_shapes = TRUE)
  }

  bounds_lsoa$LSOA21NM = NULL
  bounds_lsoa = rbind(bounds_lsoa, bounds_dz11)
  bounds_lsoa
}

