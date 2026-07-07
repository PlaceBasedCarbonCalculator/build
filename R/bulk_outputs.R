# Functions to Save Outputs For Bulk Download

#PBCC
# PT over time
# AccessProximity

# Cleanred INSIORE Polygons

# EPC Summary
# Domecsitec EPC Clenad
# Non-Dmoestic EPC Cleaned

# terrain map
# DSM
# 3d buildings


#' Export a data frame as a zipped CSV for bulk download
#'
#' @description Writes a data frame to a dated CSV in a temp folder, zips it
#'   into `path` as `<name>_<yyyymmdd>.zip`, and returns the zip path. Any sf
#'   geometry is dropped and numeric columns are rounded. This is the generic
#'   used by the `bulk_*` CSV export targets (PBCC emissions, household
#'   clusters, PT frequency, access/proximity, EPC summary).
#' @param x Data frame (may be sf or tibble) to export.
#' @param name Base name for the output file (required).
#' @param date Date stamp used in the file name; defaults to today.
#' @param path Output directory, created if missing.
#' @param rounddp Decimal places to round numeric columns to.
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_csv_generic = function(x, name = NULL, date = Sys.Date(), path = "outputdata/bulk", rounddp = 2){

  if(!dir.exists(path)){
    dir.create(path)
  }

  if(is.null(name)){
    stop("name not specified")
  }

  dir.create(file.path(tempdir(),"bulkexport"))

  if(!inherits(x, "data.frame")){
    stop("x is not a data.frame")
  }

  if(inherits(x, "sf")){
    x <- sf::st_drop_geometry(x)
  }

  if(inherits(x, "tibble") | inherits(x, "tbl")){
    x <- as.data.frame(x)
  }

  # Round to 2DP
  for(i in seq_len(ncol(x))){
    if(inherits(x[[i]],"numeric")){
      x[[i]] = round(x[[i]], rounddp)
    }
  }

  date = gsub("-","",as.character(date))

  path_temp_out = file.path(tempdir(),"bulkexport",paste0(name,"_",date,".csv"))
  path_final_out = file.path(path,paste0(name,"_",date,".zip"))

  data.table::fwrite(x, file = path_temp_out)

  message("Zipping CSV")
  my_wd <- getwd()
  setwd(file.path(tempdir(),"bulkexport"))

  if(file.exists(path_final_out)){
    unlink(path_final_out)
  }

  utils::zip(file.path(my_wd,path_final_out),
             paste0(name,"_",date,".csv"),
             flags="-q")
  setwd(my_wd)


  return(path_final_out)

}


#' Convert a GeoJSON file to a zipped GeoPackage for bulk download
#'
#' @description Reads a GeoJSON file from disk, rounds numeric columns, writes
#'   it as a GeoPackage and zips it into `path` as `<name>_<yyyymmdd>.zip`.
#'   Used by the EPC point-data bulk export targets.
#' @param x Path to an existing GeoJSON file.
#' @param name Base name for the output file (required).
#' @param date Date stamp used in the file name; defaults to today.
#' @param path Output directory, created if missing.
#' @param rounddp Decimal places to round numeric columns to.
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_geojson_generic = function(x, name = NULL, date = Sys.Date(), path = "outputdata/bulk", rounddp = 2){

  if(!dir.exists(path)){
    dir.create(path)
  }

  if(is.null(name)){
    stop("name not specified")
  }

  dir.create(file.path(tempdir(),"bulkexport"))

  if(!file.exists(x)){
    stop(x," does not exist")
  }

  x = yyjsonr::read_geojson_file(x)

  if(inherits(x, "tibble") | inherits(x, "tbl")){
    crs <- sf::st_crs(x)
    x <- as.data.frame(x)
    x <- sf::st_as_sf(x, crs = crs)
  }

  # Round to 2DP
  for(i in seq_len(ncol(x))){
    if(inherits(x[[i]],"numeric")){
      x[[i]] = round(x[[i]], rounddp)
    }
  }

  date = gsub("-","",as.character(date))

  path_temp_out = file.path(tempdir(),"bulkexport",paste0(name,"_",date,".gpkg"))
  path_final_out = file.path(path,paste0(name,"_",date,".zip"))

  sf::write_sf(x, path_temp_out)

  message("Zipping gpkg")
  my_wd <- getwd()
  setwd(file.path(tempdir(),"bulkexport"))

  if(file.exists(path_final_out)){
    unlink(path_final_out)
  }

  utils::zip(file.path(my_wd,path_final_out),
             paste0(name,"_",date,".gpkg"),
             flags="-q")
  setwd(my_wd)

  unlink(file.path(tempdir(),"bulkexport"), recursive = TRUE)

  return(path_final_out)

}

#' Export an sf object as a zipped GeoPackage for bulk download
#'
#' @description Writes an in-memory sf data frame to a GeoPackage (numeric
#'   columns rounded) and zips it into `path` as `<name>_<yyyymmdd>.zip`. Used
#'   by `bulk_export_buildings()` for the building-heights layer.
#' @param x sf data frame to export.
#' @param name Base name for the output file (required).
#' @param date Date stamp used in the file name; defaults to today.
#' @param path Output directory, created if missing.
#' @param rounddp Decimal places to round numeric columns to.
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_sf_generic = function(x, name = NULL, date = Sys.Date(), path = "outputdata/bulk", rounddp = 2){

  if(!dir.exists(path)){
    dir.create(path)
  }

  if(is.null(name)){
    stop("name not specified")
  }

  dir.create(file.path(tempdir(),"bulkexport"))

  # Round to 2DP
  for(i in seq_len(ncol(x))){
    if(inherits(x[[i]],"numeric")){
      x[[i]] = round(x[[i]], rounddp)
    }
  }

  date = gsub("-","",as.character(date))

  path_temp_out = file.path(tempdir(),"bulkexport",paste0(name,"_",date,".gpkg"))
  path_final_out = file.path(path,paste0(name,"_",date,".zip"))

  sf::write_sf(x, path_temp_out)

  message("Zipping gpkg")
  my_wd <- getwd()
  setwd(file.path(tempdir(),"bulkexport"))

  if(file.exists(path_final_out)){
    unlink(path_final_out)
  }

  utils::zip(file.path(my_wd,path_final_out),
             paste0(name,"_",date,".gpkg"),
             flags="-q")
  setwd(my_wd)

  unlink(file.path(tempdir(),"bulkexport"), recursive = TRUE)


  return(path_final_out)

}

#' Bulk export the per-LSOA emissions table
#'
#' @description Wrapper for `bulk_export_csv_generic()` used by the
#'   `bulk_pbcc` target. Note the default value references a global
#'   (`lsoa_emissions_all_forcasts`) that no longer exists; the target passes
#'   `lsoa_emissions_all` explicitly.
#' @param x Per-LSOA emissions data frame (`lsoa_emissions_all` target).
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_pbcc = function(x = lsoa_emissions_all_forcasts){
  bulk_export_csv_generic(x, "pbcc_lsoa")
}

#' Bulk export the household clusters table
#'
#' @description Wrapper for `bulk_export_csv_generic()` used by the
#'   `bulk_household_clusters` target.
#' @param x Household clusters data frame (`household_clusters` target).
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_household_clusters = function(x = household_clusters){
  bulk_export_csv_generic(x, "household_clusters")
}

#' Bulk export the public transport frequency table
#'
#' @description Wrapper for `bulk_export_csv_generic()` used by the
#'   `bulk_pt_frequency` target.
#' @param x PT frequency data frame (`pt_frequency` target).
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_pt_frequency = function(x = pt_frequency){
  bulk_export_csv_generic(x, "pt_frequency")
}

#' Bulk export the access/proximity table
#'
#' @description Wrapper for `bulk_export_csv_generic()` used by the
#'   `bulk_access_proximity` target.
#' @param x Access/proximity data frame (`access_proximity` target).
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_access_proximity = function(x = access_proximity){
  bulk_export_csv_generic(x, "access_proximity")
}

#' Bulk export the domestic EPC LSOA summary
#'
#' @description Wrapper for `bulk_export_csv_generic()` used by the
#'   `bulk_epc_dom_summary` target.
#' @param x Domestic EPC summary data frame (`epc_dom_summary` target).
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_epc_dom_summary = function(x = epc_dom_summary){
  bulk_export_csv_generic(x, "epc_dom_summary")
}


#' Bulk export domestic EPC points as zipped GeoPackage
#'
#' @description Wrapper for `bulk_export_geojson_generic()`. The corresponding
#'   `bulk_epc_dom` target is currently commented out in `_targets.R` due to a
#'   JSON parsing error on the large epc_dom.geojson file.
#' @param geojson_epc_dom Path to the domestic EPC GeoJSON (`geojson_epc_dom`
#'   target).
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_epc_dom = function(geojson_epc_dom){
  bulk_export_geojson_generic(geojson_epc_dom, "epc_domestic")
}

#' Bulk export non-domestic EPC points as zipped GeoPackage
#'
#' @description Wrapper for `bulk_export_geojson_generic()` used by the
#'   `bulk_epc_nondom` target.
#' @param geojson_epc_nondom Path to the non-domestic EPC GeoJSON
#'   (`geojson_epc_nondom` target).
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_epc_nondom = function(geojson_epc_nondom){
  bulk_export_geojson_generic(geojson_epc_nondom, "epc_nondomestic")
}

#' Bulk export building footprints with heights as zipped GeoPackage
#'
#' @description Wrapper for `bulk_export_sf_generic()` used by the
#'   `bulk_buildings_heights` target.
#' @param buildings_heights sf building footprints with height attributes
#'   (`buildings_heights` target).
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_buildings = function(buildings_heights){
  bulk_export_sf_generic(buildings_heights, "buildings_heights")
}
