#' Convert a GeoJSON file to PMTiles using tippecanoe
#'
#' @description Shells out to `tippecanoe` to build a PMTiles vector tileset
#'   from a GeoJSON file inside `output_path`. On Windows the command is run
#'   through WSL (so tippecanoe must be installed in WSL); on unix it runs
#'   directly. Used by all the `pmtiles_*` targets and by
#'   `make_pmtiles_stack()`.
#' @param input Unused; kept so targets can declare a dependency on the
#'   GeoJSON-producing target.
#' @param geojson File name of the input GeoJSON, relative to `output_path`.
#' @param pmtiles File name of the output PMTiles, relative to `output_path`.
#' @param name Tileset name passed to tippecanoe `--name`.
#' @param layer Layer name passed to tippecanoe `--layer`; defaults to `name`.
#' @param output_path Folder containing the GeoJSON, where the PMTiles is
#'   written; the tippecanoe command is run from this folder.
#' @param attribution Attribution string embedded in the tileset.
#' @param min_zoom Minimum zoom level to tile.
#' @param max_zoom Maximum zoom level; NA uses tippecanoe's `-zg` (guess).
#' @param extend_zoom If TRUE, adds `--extend-zooms-if-still-dropping`.
#' @param coalesce If TRUE, adds `--coalesce-smallest-as-needed`.
#' @param drop If TRUE, adds `--drop-densest-as-needed`.
#' @param shared_borders If TRUE, adds `--detect-shared-borders` (recommended
#'   for zone polygons so simplified borders still align).
#' @param max_tile_bytes Maximum bytes per tile (`--maximum-tile-bytes`).
#' @param simplification Simplification factor at max zoom (`--simplification`).
#' @param buffer Tile buffer in screen pixels (`--buffer`).
#' @param drop_rate Point drop rate between zooms (`--drop-rate`); NA omits.
#' @param force If TRUE, adds `--force` to overwrite existing output.
#' @param new_line_delim If TRUE, adds `-P` to read the GeoJSON in parallel
#'   (requires newline-delimited features).
#' @return The path to the PMTiles file (`output_path/pmtiles`); errors with
#'   the tippecanoe output if the file was not created.
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

  if(file.exists(file.path(output_path,pmtiles))){
    return(file.path(output_path,pmtiles))
  } else {
    stop(responce)
  }

}


#' Merge several PMTiles files into one with tile-join
#'
#' @description Shells out to `tile-join` (part of tippecanoe, via WSL on
#'   Windows) to combine multiple PMTiles files - typically the high/medium/low
#'   zoom-range tilesets built by `make_pmtiles_stack()` - into a single
#'   tileset covering all zoom levels.
#' @param output File name of the merged PMTiles, relative to `output_path`.
#' @param inputs Character vector of input PMTiles file names, relative to
#'   `output_path`.
#' @param output_path Folder containing the inputs and receiving the output.
#' @return The path to the merged PMTiles file; errors with the tile-join
#'   output if the file was not created.
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


#' Build a multi-resolution PMTiles stack for LSOA data
#'
#' @description Joins per-LSOA attribute data onto full, generalised and
#'   super-generalised boundary layers, writes each as GeoJSON, tiles each at
#'   an appropriate zoom range, and merges them into a single
#'   `zones_<name>.pmtiles`. If building footprints are supplied, an analogous
#'   four-level `buildings_<name>.pmtiles` (dasymetric) stack is also built.
#'   Used by the `pmtiles_retrofit`, `pmtiles_transport` and `pmtiles_pbcc`
#'   targets.
#' @param lsoa_data Data frame with `LSOA21CD` plus the attribute columns to
#'   put in the tiles. Numeric columns are rounded to `rounddp`.
#' @param bounds_lsoa_GB_full sf LSOA/DZ boundaries, full resolution (zooms 12-13).
#' @param bounds_lsoa_GB_generalised sf boundaries, generalised (zooms 9-11).
#' @param bounds_lsoa_GB_super_generalised sf boundaries, super-generalised
#'   (zooms 4-8).
#' @param zoomstack_buildings_lst_4326 Optional list with elements `high`,
#'   `medium`, `low`, `verylow`: building footprints tagged with `LSOA21CD`,
#'   used for the dasymetric building-level tiles.
#' @param name Suffix used in all output file names, e.g. "retrofit".
#' @param output_path Existing folder in which all GeoJSON/PMTiles are written.
#' @param rounddp Decimal places to round numeric attribute columns to; 0
#'   converts to integer.
#' @return The path to `zones_<name>.pmtiles`, plus the path to
#'   `buildings_<name>.pmtiles` when building footprints are supplied.
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
        lsoa_data[[i]] = as.integer(round(lsoa_data[[i]]))
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
      stop("Output failed to create", file.path(output_path, paste0("buildings_",name,".pmtiles")))
    }

    res = c(res, res2)

  }


  res


}
