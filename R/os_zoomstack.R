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



#' Build LSOA-level zoomstack building tile layers.
#'
#' @param buildings_heights An `sf` object with building heights and geometry.
#' @param dl_os_zoomstack Path to the downloaded OS Zoomstack zip archive.
#' @param bounds_lsoa_GB_full Full-resolution LSOA boundaries.
#' @param bounds_lsoa_GB_generalised Generalised LSOA boundaries.
#' @param bounds_lsoa_GB_super_generalised Super-generalised LSOA boundaries.
#' @return A named list of `sf` objects for high, medium, low, and verylow zoomstack layers.
#' @keywords internal
zoomstack_buildings_lsoa = function(buildings_heights, dl_os_zoomstack, bounds_lsoa_GB_full, bounds_lsoa_GB_generalised, bounds_lsoa_GB_super_generalised) {
  # TODO: Finish this function

  sf::sf_use_s2(FALSE)

  b_high = buildings_heights[,c("height_max","geometry")]
  rm(buildings_heights)

  dir.create(file.path(tempdir(),"zoomstack"))
  unzip(dl_os_zoomstack, exdir = file.path(tempdir(),"zoomstack"))

  #b_high = sf::st_read(file.path(tempdir(),"zoomstack","OS_Open_Zoomstack.gpkg"), layer = "local_buildings")
  b_med = sf::st_read(file.path(tempdir(),"zoomstack","OS_Open_Zoomstack.gpkg"), layer = "district_buildings")
  b_low = sf::st_read(file.path(tempdir(),"zoomstack","OS_Open_Zoomstack.gpkg"), layer = "urban_areas")
  b_verylow = b_low[b_low$type == "National",]
  b_low = b_low[b_low$type == "Regional",]

  # b_high = gsub("OS_Open_Zoomstack.zip","",dl_os_zoomstack)
  # b_high = sf::st_read(file.path(b_high,"building_heights_gb.gpkg"))
  # b_high = b_high[,c("height_max")]
  names(b_high)[names(b_high) == "height_max"] = "height"

  unlink(file.path(tempdir(),"zoomstack"), recursive = TRUE)

  #b_high$uuid = NULL
  b_low$type = NULL
  b_verylow$type = NULL

  #b_high = change_geom_name(b_high)
  b_med = change_geom_name(b_med)
  b_low = change_geom_name(b_low)
  b_verylow = change_geom_name(b_verylow)

  b_high$id <- 1:nrow(b_high)
  b_med$id <- 1:nrow(b_med)
  b_low$id <- 1:nrow(b_low)
  b_verylow$id <- 1:nrow(b_verylow)

  b_high <- sf::st_join(b_high, bounds_lsoa_GB_full)
  b_med <- sf::st_join(b_med, bounds_lsoa_GB_full)
  b_low <- sf::st_join(b_low, bounds_lsoa_GB_generalised)
  b_verylow <- sf::st_join(b_verylow, bounds_lsoa_GB_super_generalised)

  # Split Duplicates
  b_high = split_merge(b_high, bounds_lsoa_GB_full)
  b_med = split_merge(b_med, bounds_lsoa_GB_full)
  b_low = split_merge(b_low, bounds_lsoa_GB_generalised)
  b_verylow = split_merge(b_verylow, bounds_lsoa_GB_super_generalised)


  b_high = sf::st_transform(b_high, 4326)
  b_med = sf::st_transform(b_med, 4326)
  b_low = sf::st_transform(b_low, 4326)
  b_verylow = sf::st_transform(b_verylow, 4326)

  b_high = sf::st_make_valid(b_high)
  b_med = sf::st_make_valid(b_med)
  b_low = sf::st_make_valid(b_low)
  b_verylow = sf::st_make_valid(b_verylow)


  res = list(high = b_high,
             medium = b_med,
             low = b_low,
             verylow = b_verylow)


  res


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





#' Merge Woods
#'
#' @description Combine woods inputs into a single consolidated result.
#' @param poly){ Input object or parameter named `poly){`.
#' @return A combined data frame or table merging the provided inputs.
#' @keywords internal
merge_woods = function(poly){

  # Buffer out and then in
  bout = sf::st_union(sf::st_buffer(poly, 10))
  bin = sf::st_buffer(bout, -30)

  poly$id = seq(1, nrow(poly))

  # Make points
  pts = suppressWarnings(sf::st_segmentize(sf::st_cast(poly, "LINESTRING"), dfMaxLength = 10))
  pts = suppressWarnings(sf::st_cast(pts, "POINT"))
  pts$pid = 1:nrow(pts)

  # Points on the edge
  pids = pts[bin, ]
  pts = pts[!pts$pid %in% pids$pid,]

  #qtm(poly, fill = "green") + qtm(pts)

  pts = dplyr::group_split(pts, id)
  pts = lapply(pts, function(x){
    x = sf::st_coordinates(x)[,1:2]
    x = sf::st_as_sf(sf::st_sfc(sf::st_linestring(x), crs = 27700))
    x
  })
  pts = dplyr::bind_rows(pts)
  pts = sf::st_difference(pts, bin)

  #Clip off the start and end of each line
  pts = sf::st_collection_extract(pts, "LINESTRING")
  pts = sf::st_cast(pts, "LINESTRING")
  pts = lapply(pts$x, function(x){
    x = sf::st_coordinates(x)[,1:2]
    if(nrow(x) < 4){
      return(NULL)
    }
    x = x[seq(2,nrow(x) - 1),]
    x = sf::st_as_sf(sf::st_sfc(sf::st_linestring(x), crs = 27700))
    x
  })
  pts = dplyr::bind_rows(pts)
  pts$id = seq(1,nrow(pts))

  pts_start = sf::st_as_sf(lwgeom::st_startpoint(pts))
  pts_end = sf::st_as_sf(lwgeom::st_endpoint(pts))
  pts_start$id = pts$id
  pts_end$id = pts$id

  # Pair up start and end
  nn = suppressMessages(nngeo::st_nn(pts_end, pts_start, k = 1, maxdist = 100, progress = FALSE))
  nn = unlist(nn)
  ordering = list()
  nnn = seq_along(nn)
  for(i in nnn){
    if(i == 1){
      ordering[[i]] = nnn[i]
    } else {
      ordering[[i]] = nn[nnn == ordering[[i-1]]]
    }
  }
  ordering = unlist(ordering)
  summary(nnn %in% ordering) # some lines lost

  pts$order = seq_along(ordering)[match(pts$id, ordering)]

  pts_na = pts[is.na(pts$order),]
  pts = pts[!is.na(pts$order),]
  pts = pts[order(pts$order),]

  pts = suppressWarnings(sf::st_cast(pts, "POINT"))
  pts = sf::st_coordinates(pts[,1:2])
  pts = rbind(pts, pts[1,])
  pts = sf::st_polygon(list(pts))
  pts = sf::st_sfc(pts, crs = 27700)

  pts

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
  buildings_heights <- duckspatial::ddbs_join(buildings_heights, bounds_lsoa_GB_full)
  buildings_heights <- duckspatial::ddbs_collect(buildings_heights)

  message(Sys.time()," spatial join complete nrow = ",nrow(buildings_heights))

  # Split duplicates
  buildings_heights <- split_merge(buildings_heights, bounds_lsoa_GB_full)

  # Transform and validate
  buildings_heights <- sf::st_transform(buildings_heights, 4326)
  buildings_heights <- sf::st_make_valid(buildings_heights)

  buildings_heights
}
