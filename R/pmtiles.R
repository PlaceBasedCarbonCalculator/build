#' Make Pmtiles
#'
#' @description Build pmtiles and return the generated output.
#' @param input Input object or parameter named `input`.
#' @param geojson Input object or parameter named `geojson`.
#' @param pmtiles Input object or parameter named `pmtiles`.
#' @param name Name or label used to identify output content.
#' @param layer Input object or parameter named `layer`.
#' @param output_path A file or directory path used by the function.
#' @param attribution Input object or parameter named `attribution`.
#' @param min_zoom Input object or parameter named `min_zoom`.
#' @param max_zoom Input object or parameter named `max_zoom`.
#' @param extend_zoom Input object or parameter named `extend_zoom`.
#' @param coalesce Input object or parameter named `coalesce`.
#' @param drop Input object or parameter named `drop`.
#' @param shared_borders Input object or parameter named `shared_borders`.
#' @param max_tile_bytes Input object or parameter named `max_tile_bytes`.
#' @param simplification Input object or parameter named `simplification`.
#' @param buffer Input object or parameter named `buffer`.
#' @param drop_rate Input object or parameter named `drop_rate`.
#' @param force Input object or parameter named `force`.
#' @param new_line_delim Input object or parameter named `new_line_delim`.
#' @return A generated data object, usually a data frame or spatial feature collection.
#' @keywords internal
make_pmtiles = function(input = NULL,
                        geojson = "school_locations.geojson",
                        pmtiles = "schools.pmtiles",
                        name = "schools", layer = name,
                        output_path = "outputdata",
                        attribution = "UniverstyofLeeds",
                        min_zoom = 6,
                        max_zoom = NA,
                        extend_zoom = FALSE,
                        coalesce = FALSE,
                        drop = FALSE,
                        shared_borders = FALSE,
                        max_tile_bytes = 5000000,
                        simplification = 10,
                        buffer = 5,
                        drop_rate = NA,
                        force = TRUE,
                        new_line_delim = TRUE
                        ){

  # Check input
  if(file.path(output_path, geojson) != file.path(output_path, geojson)){
    stop("input does not match")
  }

  if(!dir.exists(output_path)){
    stop("'",output_path, "' does not exist as a writeable folder in ",getwd())
  }

  if(!file.exists(file.path(output_path, geojson))){
    stop("'",geojson, "' does not exist")
  }

  if(file.exists(file.path(output_path,pmtiles))){
    unlink(file.path(output_path,pmtiles))
  }

  # TODO: check if extra spaces caused by collapse matter
  command_tippecanoe = paste('tippecanoe -o',pmtiles,
                             paste0('--name=',name),
                             paste0('--layer=',layer),
                             paste0('--attribution=',attribution),
                             paste0('--minimum-zoom=',min_zoom),
                             ifelse(is.na(max_zoom),'-zg',paste0('--maximum-zoom=',max_zoom)),
                             paste0('--maximum-tile-bytes=',format(max_tile_bytes, scientific = FALSE)),
                             ifelse(coalesce,'--coalesce-smallest-as-needed',''),
                             ifelse(drop,'--drop-densest-as-needed',''),
                             ifelse(shared_borders,'--detect-shared-borders',''),
                             ifelse(extend_zoom,'--extend-zooms-if-still-dropping',''),
                             paste0('--simplification=',simplification),
                             paste0('--buffer=',buffer),
                             ifelse(is.na(drop_rate),'',paste0('--drop-rate=',drop_rate)),
                             ifelse(force,'--force',''),
                             ifelse(new_line_delim,'-P',''),
                             geojson,collapse = " ")


  if(.Platform$OS.type == "unix") {
    command_cd = paste0('cd ',outputdata)
    command_all = paste(c(command_cd, command_tippecanoe), collapse = "; ")
  } else {
    # Using WSL
    dir = getwd()
    command_start = 'bash -c '
    command_cd = paste0('cd /mnt/',tolower(substr(dir,1,1)),substr(dir,3,nchar(dir)),'/',output_path)
    command_all = paste(c(command_cd, command_tippecanoe), collapse = "; ")
    command_all = paste0(command_start,'"',command_all,'"')
  }
  responce = system(command_all, intern = TRUE)

  if(file.exists(file.path(output_path,pmtiles))){
    return(file.path(output_path,pmtiles))
  } else {
    stop(responce)
  }

}


#' Join Pmtiles
#'
#' @description Combine pmtiles inputs into a single consolidated result.
#' @param output Input object or parameter named `output`.
#' @param inputs Input object or parameter named `inputs`.
#' @param output_path A file or directory path used by the function.
#' @return A combined data frame or table merging the provided inputs.
#' @keywords internal
join_pmtiles = function(output = 'dasymetric.pmtiles',
                        inputs = c('dasymetric_verylow.pmtiles',
                                   'dasymetric_low.pmtiles',
                                   'dasymetric_med.pmtiles',
                                   'dasymetric_high.pmtiles'),
                        output_path = "outputdata"
                        ){

  if(!dir.exists(output_path)){
    stop("'",output_path, "' does not exist as a writeable folder in ",getwd())
  }

  for(i in seq_along(inputs)){
    if(!file.exists(file.path(output_path, inputs[i]))){
      stop("'",inputs[i], "' does not exist")
    }
  }

  if(file.exists(file.path(output_path,output))){
    unlink(file.path(output_path,output))
  }


  command_tippecanoe = paste('tile-join -o',output,'-pk --force',
                          paste(inputs, collapse = " "),
                          collapse = " ")

  if(.Platform$OS.type == "unix") {
    command_cd = paste0('cd ',output_path)
    command_all = paste(c(command_cd, command_tippecanoe), collapse = "; ")
  } else {
    # Using WSL
    dir = getwd()
    command_start = 'bash -c '
    command_cd = paste0('cd /mnt/',tolower(substr(dir,1,1)),substr(dir,3,nchar(dir)),'/',output_path)
    command_all = paste(c(command_cd, command_tippecanoe), collapse = "; ")
    command_all = paste0(command_start,'"',command_all,'"')
  }
  responce = system(command_all, intern = TRUE)

  if(file.exists(file.path(output_path,output))){
    return(file.path(output_path,output))
  } else {
    stop(responce)
  }


}


# Common need is to low/med/high zooms
#' Make Pmtiles Stack
#'
#' @description Build pmtiles stack and return the generated output.
#' @param lsoa_data Input object or parameter named `lsoa_data`.
#' @param bounds_lsoa_GB_full Input object or parameter named `bounds_lsoa_GB_full`.
#' @param bounds_lsoa_GB_generalised Input object or parameter named `bounds_lsoa_GB_generalised`.
#' @param bounds_lsoa_GB_super_generalised Input object or parameter named `bounds_lsoa_GB_super_generalised`.
#' @param zoomstack_buildings_lst_4326 Input object or parameter named `zoomstack_buildings_lst_4326`.
#' @param name Name or label used to identify output content.
#' @param output_path A file or directory path used by the function.
#' @param rounddp Input object or parameter named `rounddp`.
#' @return A generated data object, usually a data frame or spatial feature collection.
#' @keywords internal
make_pmtiles_stack = function(lsoa_data,
                              bounds_lsoa_GB_full,
                              bounds_lsoa_GB_generalised,
                              bounds_lsoa_GB_super_generalised,
                              zoomstack_buildings_lst_4326 = NULL,
                              name = "",
                              output_path = "outputdata/retofit",
                              rounddp = 3
                              ){

  if(!dir.exists(output_path)){
    stop("'",output_path, "' does not exist as a writeable folder in ",getwd())
  }

  # Round to 3DP
  for(i in seq_len(ncol(lsoa_data))){
    if(inherits(lsoa_data[[i]],"numeric")){
      if(rounddp == 0){
        lsoa_data[[i]] = as.integer(lsoa_data[[i]])
      } else {
        lsoa_data[[i]] = round(lsoa_data[[i]], rounddp)
      }

    }
  }


  # Make GeoJSON
  zones_high =  join_for_geojson(lsoa_data, bounds_lsoa_GB_full)
  make_geojson(zones_high, file.path(output_path,paste0("zones_",name,"_high.geojson")))
  rm(zones_high)

  zones_medium =  join_for_geojson(lsoa_data, bounds_lsoa_GB_generalised)
  make_geojson(zones_medium, file.path(output_path,paste0("zones_",name,"_medium.geojson")))
  rm(zones_medium)

  zones_low =  join_for_geojson(lsoa_data, bounds_lsoa_GB_super_generalised)
  make_geojson(zones_low, file.path(output_path,paste0("zones_",name,"_low.geojson")))
  rm(zones_low)

  # Make pmtiles
  make_pmtiles(NULL,
               paste0("zones_",name,"_high.geojson"),
               paste0("zones_",name,"_high.pmtiles"),
               name = "zones", shared_borders = TRUE, extend_zoom = TRUE,
               coalesce = TRUE, min_zoom = 12, max_zoom = 13, output_path = output_path)


  make_pmtiles(NULL,
               paste0("zones_",name,"_medium.geojson"),
               paste0("zones_",name,"_medium.pmtiles"),
               name = "zones", shared_borders = TRUE,
               coalesce = TRUE, min_zoom = 9, max_zoom = 11, output_path = output_path)

  make_pmtiles(NULL,
               paste0("zones_",name,"_low.geojson"),
               paste0("zones_",name,"_low.pmtiles"),
               name = "zones", shared_borders = TRUE,
               coalesce = TRUE, min_zoom = 4, max_zoom = 8, output_path = output_path)

  # Join pmtiles
  join_pmtiles(paste0("zones_",name,".pmtiles"),
               c(paste0("zones_",name,"_high.pmtiles"),
                 paste0("zones_",name,"_medium.pmtiles"),
                 paste0("zones_",name,"_low.pmtiles")),
               output_path = output_path)


  if(file.exists(file.path(output_path, paste0("zones_",name,".pmtiles")))){
    res = file.path(output_path, paste0("zones_",name,".pmtiles"))
  } else {
    stop("Output failed to create", file.path(output_path, paste0("zones_",name,".pmtiles")))
  }



  if(!is.null(zoomstack_buildings_lst_4326)){
    buildings_high = join_for_geojson(lsoa_data, zoomstack_buildings_lst_4326$high)
    make_geojson(buildings_high, file.path(output_path,paste0("buildings_",name,"_high.geojson")))
    rm(buildings_high)

    buildings_medium = join_for_geojson(lsoa_data, zoomstack_buildings_lst_4326$medium)
    make_geojson(buildings_medium, file.path(output_path,paste0("buildings_",name,"_medium.geojson")))
    rm(buildings_medium)

    buildings_low = join_for_geojson(lsoa_data, zoomstack_buildings_lst_4326$low)
    make_geojson(buildings_low, file.path(output_path,paste0("buildings_",name,"_low.geojson")))
    rm(buildings_low)

    buildings_verylow = join_for_geojson(lsoa_data, zoomstack_buildings_lst_4326$verylow)
    make_geojson(buildings_verylow, file.path(output_path,paste0("buildings_",name,"_verylow.geojson")))
    rm(buildings_verylow)


    # Make pmtiles
    make_pmtiles(NULL,
                 paste0("buildings_",name,"_high.geojson"),
                 paste0("buildings_",name,"_high.pmtiles"),
                 name = "buildings", shared_borders = TRUE, extend_zoom = TRUE,
                 coalesce = TRUE, min_zoom = 14, max_zoom = 15, output_path = output_path)


    make_pmtiles(NULL,
                 paste0("buildings_",name,"_medium.geojson"),
                 paste0("buildings_",name,"_medium.pmtiles"),
                 name = "buildings", shared_borders = TRUE,
                 coalesce = TRUE, min_zoom = 12, max_zoom = 13, output_path = output_path)

    make_pmtiles(NULL,
                 paste0("buildings_",name,"_low.geojson"),
                 paste0("buildings_",name,"_low.pmtiles"),
                 name = "buildings", shared_borders = TRUE,
                 coalesce = TRUE, min_zoom = 8, max_zoom = 11, output_path = output_path)

    make_pmtiles(NULL,
                 paste0("buildings_",name,"_verylow.geojson"),
                 paste0("buildings_",name,"_verylow.pmtiles"),
                 name = "buildings", shared_borders = TRUE,
                 coalesce = TRUE, min_zoom = 4, max_zoom = 7, output_path = output_path)

    # Join pmtiles
    join_pmtiles(paste0("buildings_",name,".pmtiles"),
                 c(paste0("buildings_",name,"_high.pmtiles"),
                   paste0("buildings_",name,"_medium.pmtiles"),
                   paste0("buildings_",name,"_low.pmtiles"),
                   paste0("buildings_",name,"_verylow.pmtiles")),
                 output_path = output_path)


    if(file.exists(file.path(output_path, paste0("buildings_",name,".pmtiles")))){
      res2 = file.path(output_path, paste0("buildings_",name,".pmtiles"))
    } else {
      stop("Output failed to create", file.path(output_path, paste0("zones_",name,".pmtiles")))
    }

    #res = c(res, res2)

  }


  res


}
