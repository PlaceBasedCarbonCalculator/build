#' Download the OS Open Zoomstack archive.
#'
#' @param path Directory to save the OS Zoomstack download.
#' @return The full path to the downloaded Open Zoomstack ZIP file.
#' @keywords internal
download_os_zoomstack = function(path = file.path(parameters$path_data,"os_zoomstack")) {
  url = "https://api.os.uk/downloads/v1/products/OpenZoomstack/downloads?area=GB&format=GeoPackage&redirect"

  if(!dir.exists(path)){
    dir.create(path)
  }

  if(!file.exists(file.path(path,"OS_Open_Zoomstack.zip"))){
    download.file(url, destfile = file.path(path,"OS_Open_Zoomstack.zip"), mode = "wb")
  }



  file.path(path,"OS_Open_Zoomstack.zip")

}

#' Read Open Zoomstack building geometries for high-detail building tiles.
#'
#' @param dl_os_zoomstack Path to the downloaded OS Zoomstack zip archive.
#' @return An `sf` object containing high-detail building geometries.
#' @keywords internal
zoomstack_buildings_high = function(dl_os_zoomstack) {
  # TODO: Finish this function

  sf::sf_use_s2(FALSE)

  dir.create(file.path(tempdir(),"zoomstack"))
  unzip(dl_os_zoomstack, exdir = file.path(tempdir(),"zoomstack"))

  b_high = sf::st_read(file.path(tempdir(),"zoomstack","OS_Open_Zoomstack.gpkg"), layer = "local_buildings")

  unlink(file.path(tempdir(),"zoomstack"), recursive = TRUE)

  b_high$uuid = NULL

  b_high = change_geom_name(b_high)

  b_high

}





#' Split buildings by overlapping zone geometries.
#'
#' @param b An `sf` object representing building geometries.
#' @param z An `sf` object representing zone geometries.
#' @return A combined `sf` object containing split building geometries aligned to zones.
#' @keywords internal
split_buildings = function(b,z){
  if(nrow(z) > 2){
    suppressWarnings(suppressMessages(wth <- sf::st_within(z, b[1,])))
    wth <- as.logical(lengths(wth))
    zin <- z[wth,]
    zout <- z[!wth,]
    #qtm(b[1,]) + qtm(zin, fill = "red") + qtm(zout, fill = "blue")
    suppressWarnings(suppressMessages(zout2 <- sf::st_intersection(b[1,], zout)))
    zout2$LSOA21CD = NULL
    names(zout2)[names(zout2) == "LSOA21CD.1"] = "LSOA21CD"
    b <- sf::st_drop_geometry(b)
    zin <- dplyr::left_join(zin, b, by = "LSOA21CD")
    zin <- zin[,names(zout2)]
    fin <- rbind(zin, zout2)
    return(fin)
  } else {
    suppressWarnings(suppressMessages(b2 <- sf::st_intersection(b, z)))
    b2 <- b2[!duplicated(b2$geometry),]
    b2$LSOA21CD = NULL
    names(b2)[names(b2) == "LSOA21CD.1"] = "LSOA21CD"
    return(b2)
  }


}


#' Merge duplicate building features after zone splitting.
#'
#' @param build An `sf` object containing building geometries with duplicate ids.
#' @param bounds Zone boundaries used to guide the merge operation.
#' @return An `sf` object with duplicate buildings merged.
#' @keywords internal
split_merge = function(build, bounds) {

  dups <- unique(build$id[duplicated(build$id)])
  dup   <- build[ build$id %in% dups,]
  nodup <- build[!build$id %in% dups,]

  dup <- dplyr::group_by(dup, id)
  dup <- dplyr::group_split(dup)

  zone_list <- lapply(dup, function(x){unique(x$LSOA21CD)})
  zone_list <- lapply(zone_list, function(x){bounds[bounds$LSOA21CD %in% x,]})

  dup = purrr::map2(dup, zone_list, split_buildings, .progress = "Splitting buildings by zone")
  dup = dplyr::bind_rows(dup)
  res = rbind(dup, nodup)
  res
}


#' Rename `geom` column to `geometry` in an sf object.
#'
#' @param build An `sf` object with a `geom` geometry column.
#' @return The same `sf` object with a standardized geometry column name.
#' @keywords internal
change_geom_name = function(build){
  names(build)[names(build) == "geom"] = "geometry"
  sf::st_geometry(build) = "geometry"
  build
}


#' Read Open Zoomstack feature layers for sites, water, woodland, and greenspace.
#'
#' @param dl_os_zoomstack Path to the downloaded OS Zoomstack zip archive.
#' @return An `sf` object containing a combined set of spatial feature geometries.
#' @keywords internal
zoomstack_sites = function(dl_os_zoomstack) {
  # TODO: Finish this function

  sf::sf_use_s2(FALSE)

  dir.create(file.path(tempdir(),"zoomstack"))
  unzip(dl_os_zoomstack, exdir = file.path(tempdir(),"zoomstack"))

  sites = sf::st_read(file.path(tempdir(),"zoomstack","OS_Open_Zoomstack.gpkg"), layer = "sites")
  water = sf::st_read(file.path(tempdir(),"zoomstack","OS_Open_Zoomstack.gpkg"), layer = "surfacewater")
  woods = sf::st_read(file.path(tempdir(),"zoomstack","OS_Open_Zoomstack.gpkg"), layer = "woodland")
  greenspace = sf::st_read(file.path(tempdir(),"zoomstack","OS_Open_Zoomstack.gpkg"), layer = "greenspace")

  unlink(file.path(tempdir(),"zoomstack"), recursive = TRUE)

  # Water local is over detailed (small rivers)
  water_nat = water[water$type == "National",]
  water_reg = water[water$type == "Regional",]
  water_local = water[water$type == "Local",]

  water_local$area = as.numeric(sf::st_area(water_local))
  water_local = water_local[water_local$area > 100, ]

  water_local$perimeter = as.numeric(lwgeom::st_perimeter_2d(water_local))
  water_local$ap_ratio = water_local$area / water_local$perimeter
  water_local = water_local[water_local$ap_ratio > 1.2,]
  water_local$type = "water"
  water_local = water_local[,c("type","geom")]
  names(water_local) = c("type","geometry")
  sf::st_geometry(water_local) = "geometry"

  # Greenspace has site within sites
  gs_inter = sf::st_intersects(greenspace)
  greenspace_solo = greenspace[lengths(gs_inter) == 1,]
  greenspace_inter = greenspace[lengths(gs_inter) > 1,]

  greenspace_inter = sf::st_union(greenspace_inter)
  greenspace_inter = sf::st_cast(greenspace_inter, "POLYGON")

  greenspace_inter = sf::st_as_sf(greenspace_inter)
  greenspace_solo$type = NULL

  names(greenspace_solo) = "geometry"
  sf::st_geometry(greenspace_solo) = "geometry"
  names(greenspace_inter) = "geometry"
  sf::st_geometry(greenspace_inter) = "geometry"

  greenspace = rbind(greenspace_solo, greenspace_inter)
  greenspace$type = "greenspace"

  # Sites (Airports, Education, Medical, Service Stations, Ports)
  # Some overlap (e.g. ajacent schools)

  sites = dplyr::group_split(sites, type)
  sites_union = list()
  for(i in 1:length(sites)){
    message(i)
    sub = sites[[i]]
    nm = sub$type[1]
    sub_inter = sf::st_intersects(sub)
    sub_solo = sub[lengths(sub_inter) == 1,]
    sub_inter = sub[lengths(sub_inter) > 1,]
    sub_inter = sf::st_union(sub_inter)
    sub_inter = sf::st_cast(sub_inter, "POLYGON")
    sub_inter = sf::st_as_sf(sub_inter)
    sub_inter$type = nm

    names(sub_solo) = c("type","geometry")
    sf::st_geometry(sub_solo) = "geometry"
    names(sub_inter) = c("geometry","type")
    sf::st_geometry(sub_inter) = "geometry"
    sub_inter = sub_inter[,c("type","geometry")]
    sites_union[[i]] = rbind(sub_solo, sub_inter)
  }
  sites = dplyr::bind_rows(sites_union)

  # Local Woodland includes small cluster of trees
  # Regional and national woodland polygons are over-simplified for this output.
  wood_local = woods[woods$type == "Local",]
  wood_local$type = "woodland"
  wood_local = wood_local[,c("type","geom")]
  names(wood_local) = c("type","geometry")
  sf::st_geometry(wood_local) = "geometry"

  res = rbind(sites, water_local, greenspace, wood_local)

  res

}


#' Load OS greenspace data from an Open Greenspace archive.
#'
#' @param path Directory containing `opgrsp_gpkg_gb.zip`.
#' @return An `sf` object containing greenspace polygons.
#' @keywords internal
load_os_greenspace = function(path = file.path(parameters$path_data,"os_greenspace")) {
  # TODO: Finish this function

  sf::sf_use_s2(FALSE)

  dir.create(file.path(tempdir(),"greenspace"))
  unzip(file.path(path,"opgrsp_gpkg_gb.zip"), exdir = file.path(tempdir(),"greenspace"))

  greenspace = sf::st_read(file.path(tempdir(),"greenspace","Data","opgrsp_gb.gpkg"), layer = "greenspace_site")

  unlink(file.path(tempdir(),"greenspace"), recursive = TRUE)

  greenspace = greenspace[,"function."]

  # Greenspace has site within sites
  gs_inter = sf::st_intersects(greenspace)
  greenspace_solo = greenspace[lengths(gs_inter) == 1,]
  greenspace_inter = greenspace[lengths(gs_inter) > 1,]

  greenspace_inter = sf::st_union(greenspace_inter)
  greenspace_inter = sf::st_cast(greenspace_inter, "POLYGON")

  greenspace_inter = sf::st_as_sf(greenspace_inter)
  greenspace_solo$`function.` = NULL

  names(greenspace_solo) = "geometry"
  sf::st_geometry(greenspace_solo) = "geometry"
  names(greenspace_inter) = "geometry"
  sf::st_geometry(greenspace_inter) = "geometry"

  greenspace = rbind(greenspace_solo, greenspace_inter)


  greenspace$type = "greenspace"

  greenspace

}







#' Process medium-detail zoomstack buildings
#'
#' @param path Path to the downloaded OS Zoomstack .
#' @param bounds LSOA boundaries.
#' @param scale which case to process
#' @return An `sf` object with medium-detail buildings joined to LSOA boundaries.
#' @keywords internal
process_buildings_generic = function(path = "../inputdata/os_zoomstack/OS_Open_Zoomstack/OS_Open_Zoomstack.gpkg", bounds,
                                     scale = "med") {

  if(scale == "med"){
    layer = "district_buildings"
  } else if (scale == "low") {
    layer = "urban_areas"
  } else if (scale == "verylow"){
    layer = "urban_areas"
  } else {
    stop("Unknown scale")
  }

  sf::sf_use_s2(FALSE)

  b <- sf::st_read(
    path,
    layer = layer,
    quiet = TRUE
  )

  if(scale == "low"){
    b = b[b$type == "Regional",]
    b$type = NULL
  }
  if(scale == "verylow"){
    b = b[b$type == "National",]
    b$type = NULL
  }

  b <- change_geom_name(b)
  b$id <- 1:nrow(b)

  # Spatial join
  b <- sf::st_join(b, bounds)

  # Split duplicates
  b <- split_merge(b, bounds)

  # Transform and validate
  b <- sf::st_transform(b, 4326)
  b <- sf::st_make_valid(b)

  b
}


#' Process high-detail zoomstack buildings
#'
#' @param buildings_heights An `sf` object with building heights and geometry.
#' @param bounds_lsoa_GB_full Full-resolution LSOA boundaries.
#' @return An `sf` object with high-detail buildings joined to LSOA boundaries.
#' @keywords internal
process_buildings_high = function(buildings_heights, bounds_lsoa_GB_full) {
  sf::sf_use_s2(FALSE)

  buildings_heights <- buildings_heights[, c("height_max", "geometry")]
  names(buildings_heights)[names(buildings_heights) == "height_max"] <- "height"

  buildings_heights$id <- 1:nrow(buildings_heights)

  # DUCK DB doesn't do mixed geometry
  bounds_lsoa_GB_full <- sf::st_cast(bounds_lsoa_GB_full,"MULTIPOLYGON", warn = FALSE)

  message(Sys.time()," starting spatial join, nrow = ",nrow(buildings_heights))

  # Use duckspatial for faster spatial join
  # duckspatial can't do large datasets
  # buildings_heights <- duckspatial::ddbs_join(buildings_heights, bounds_lsoa_GB_full)
  # buildings_heights <- duckspatial::ddbs_collect(buildings_heights)

  chunk_size <- 2e6
  idx <- split(seq_len(nrow(buildings_heights)),
               ceiling(seq_len(nrow(buildings_heights)) / chunk_size))

  result <- pbapply::pblapply(idx, function(i) {
    duckspatial::ddbs_collect(duckspatial::ddbs_join(
      buildings_heights[i, ],
      bounds_lsoa_GB_full
    ))
  })

  buildings_heights <- dplyr::bind_rows(result)

  message(Sys.time()," spatial join complete nrow = ",nrow(buildings_heights))

  # Split duplicates
  buildings_heights <- split_merge(buildings_heights, bounds_lsoa_GB_full)

  # Transform and validate
  buildings_heights <- sf::st_transform(buildings_heights, 4326)
  buildings_heights <- sf::st_make_valid(buildings_heights)

  buildings_heights
}
