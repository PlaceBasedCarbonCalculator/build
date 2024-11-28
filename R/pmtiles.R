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
