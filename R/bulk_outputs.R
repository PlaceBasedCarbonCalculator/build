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


#' Function to convert a data.frame into a zipped cSV
#' @param x data frame
#' @param name output file name
#' @param date date of output, defaults to today
#' @param path output directory
#' @param rounddp numeric, how many decimpla places to round to

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



bulk_export_pbcc = function(x = lsoa_emissions_all_forcasts){
  bulk_export_csv_generic(x, "pbcc_lsoa")
}

bulk_export_household_clusters = function(x = household_clusters){
  bulk_export_csv_generic(x, "household_clusters")
}

bulk_export_pt_frequency = function(x = pt_frequency){
  bulk_export_csv_generic(x, "pt_frequency")
}

bulk_export_access_proximity = function(x = access_proximity){
  bulk_export_csv_generic(x, "access_proximity")
}

bulk_export_inspire = function(){

}

bulk_export_epc_dom_summary = function(x = epc_dom_summary){
  bulk_export_csv_generic(x, "epc_dom_summary")
}


bulk_export_epc_dom = function(geojson_epc_dom){
  bulk_export_geojson_generic(geojson_epc_dom, "epc_domestic")
}

bulk_export_epc_nondom = function(geojson_epc_nondom){
  bulk_export_geojson_generic(geojson_epc_nondom, "epc_nondomestic")
}

bulk_export_buildings = function(buildings_heights){
  bulk_export_geojson_generic(buildings_heights, "buildings_heights")
}
