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

#' Convert a large GeoJSON file to a zipped GeoPackage via GDAL
#'
#' @description Streaming alternative to `bulk_export_geojson_generic()` for
#'   GeoJSON files that are too large to parse into memory. GDAL translates the
#'   GeoJSON straight to a GeoPackage without materialising it as an R object,
#'   then the result is zipped into `path` as `<name>_<yyyymmdd>.zip`.
#'
#'   `bulk_export_geojson_generic()` uses `yyjsonr::read_geojson_file()`, whose
#'   file reader truncates the file length to 32 bits on Windows. Any input
#'   larger than 4 GB is therefore read as only `size %% 2^32` bytes: when that
#'   remainder is under `INT_MAX` the parse fails with "unexpected end of data",
#'   and when it is over `INT_MAX` the length goes negative and the R process
#'   segfaults (killing the whole `tar_make()` run rather than one target).
#'   GDAL uses 64-bit file offsets throughout and has no such limit.
#'
#'   Unlike `bulk_export_geojson_generic()` this does not round numeric columns,
#'   as that would require reading the data into R. `make_geojson()` already
#'   caps coordinate precision at 6 decimal places via `sf::st_precision()`.
#' @param x Path to an existing GeoJSON file.
#' @param name Base name for the output file and the GeoPackage layer (required).
#' @param date Date stamp used in the file name; defaults to today.
#' @param path Output directory, created if missing.
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_geojson_gdal = function(x, name = NULL, date = Sys.Date(), path = "outputdata/bulk"){

  if(!dir.exists(path)){
    dir.create(path, recursive = TRUE)
  }

  if(is.null(name)){
    stop("name not specified")
  }

  if(!file.exists(x)){
    stop(x," does not exist")
  }

  dir_temp = file.path(tempdir(),"bulkexport")
  dir.create(dir_temp, showWarnings = FALSE)

  date = gsub("-","",as.character(date))

  path_temp_out = file.path(dir_temp, paste0(name,"_",date,".gpkg"))
  path_final_out = file.path(path, paste0(name,"_",date,".zip"))
  # Resolved before setwd() so `path` may be relative (as in the targets
  # pipeline) or absolute; the relative form is still what gets returned.
  path_final_abs = file.path(normalizePath(path, winslash = "/"),
                             paste0(name,"_",date,".zip"))

  if(file.exists(path_temp_out)){
    unlink(path_temp_out)
  }

  message("Translating GeoJSON to gpkg with GDAL")
  ok = sf::gdal_utils("vectortranslate",
                      source = x,
                      destination = path_temp_out,
                      options = c("-f","GPKG","-nln",name))

  if(!isTRUE(ok) || !file.exists(path_temp_out)){
    stop("GDAL failed to translate ",x," to ",path_temp_out)
  }

  message("Zipping gpkg")
  my_wd <- getwd()
  on.exit(setwd(my_wd), add = TRUE)
  setwd(dir_temp)

  if(file.exists(path_final_abs)){
    unlink(path_final_abs)
  }

  utils::zip(path_final_abs,
             paste0(name,"_",date,".gpkg"),
             flags="-q")
  setwd(my_wd)

  unlink(dir_temp, recursive = TRUE)

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
#' @description Wrapper for `bulk_export_geojson_gdal()` used by the
#'   `bulk_epc_dom` target. Uses the streaming GDAL exporter rather than
#'   `bulk_export_geojson_generic()` because epc_dom.geojson is over 4 GB (24 GB
#'   as of 2026-08) and trips the 32-bit file-size truncation in
#'   `yyjsonr::read_geojson_file()`.
#' @param geojson_epc_dom Path to the domestic EPC GeoJSON (`geojson_epc_dom`
#'   target).
#' @return The path of the zip file created.
#' @keywords internal
bulk_export_epc_dom = function(geojson_epc_dom){
  bulk_export_geojson_gdal(geojson_epc_dom, "epc_domestic")
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
