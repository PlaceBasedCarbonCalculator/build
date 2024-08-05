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


split_buildings = function(b,z){
  if(nrow(z) > 2){
    suppressWarnings(suppressMessages(wth <- sf::st_within(z, b[1,])))
    wth <- as.logical(lengths(wth))
    zin <- z[wth,]
    zout <- z[!wth,]
    #qtm(b[1,]) + qtm(zin, fill = "red") + qtm(zout, fill = "blue")
    suppressWarnings(suppressMessages(zout2 <- sf::st_intersection(b[1,], zout)))
    zout2$LSOA21CD = zout2$LSOA21CD.1
    zout2$LSOA21CD.1 <- NULL
    b <- sf::st_drop_geometry(b)
    zin <- dplyr::left_join(zin, b, by = "LSOA21CD")
    zin <- zin[,names(zout2)]
    fin <- rbind(zin, zout2)
    return(fin)
  } else {
    suppressWarnings(suppressMessages(b2 <- sf::st_intersection(b, z)))
    b2 <- b2[!duplicated(b2$geometry),]
    b2$LSOA21CD = b2$LSOA21CD.1
    b2$LSOA21CD.1 <- NULL
    return(b2)
  }


}


split_merge = function(build, bounds) {

  dups <- unique(build$id[duplicated(build$id)])
  dup   <- build[ build$id %in% dups,]
  nodup <- build[!build$id %in% dups,]

  dup <- dplyr::group_by(dup, id)
  dup <- dplyr::group_split(dup)

  zone_list <- lapply(dup, function(x){unique(x$LSOA21CD)})
  zone_list <- lapply(zone_list, function(x){bounds[bounds$LSOA21CD %in% x,]})

  dup = purrr::map2(dup, zone_list, split_buildings, .progress = "Splitting low")
  dup = dplyr::bind_rows(dup)
  res = rbind(dup, nodup)
  res
}


change_geom_name = function(build){
  names(build)[names(build) == "geom"] = "geometry"
  sf::st_geometry(build) = "geometry"
  build
}


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
  # Regional national are over simplified
  #wood_nat = woods[woods$type == "National",]
  #wood_reg = woods[woods$type == "Regional",]
  wood_local = woods[woods$type == "Local",]
  wood_local$type = "woodland"
  wood_local = wood_local[,c("type","geom")]
  names(wood_local) = c("type","geometry")
  sf::st_geometry(wood_local) = "geometry"

  #
  #
  # wlri = sf::st_intersects(wood_local, wood_reg)
  # wood_local_reg = wood_local[lengths(wlri) > 0,]
  # wood_local = wood_local[lengths(wlri) == 0,]
  # wood_local$area = as.numeric(sf::st_area(wood_local))
  # wood_local = wood_local[wood_local$area > 100000, ] # Only keep the large isolated woods

  # wood_local_reg are parts of woodland clusters (often split by small paths)

  #foo = wood_local_reg[wood_reg[4,],] # 3 fails
  #bar = merge_woods(foo)
  #qtm(bar, fill= "red") + qtm(foo)

  res = rbind(sites, water_local, greenspace, wood_local)

  res

}


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



# # Merge small clusters of polygons while maintaing outer boundary
# # Very concave areas may be infilled
# merge_woods = function(poly){
#
#   # Buffer out and then in
#   bout = sf::st_union(sf::st_buffer(poly, 10))
#   bin = sf::st_buffer(bout, -30)
#
#   poly$id = seq(1, nrow(poly))
#
#   # Make points
#   pts = suppressWarnings(sf::st_segmentize(sf::st_cast(poly, "LINESTRING"), dfMaxLength = 10))
#   pts = suppressWarnings(sf::st_cast(pts, "POINT"))
#   pts$pid = 1:nrow(pts)
#
#   # Points on the edge
#   pids = pts[bin, ]
#   pts = pts[!pts$pid %in% pids$pid,]
#
#   #qtm(poly, fill = "green") + qtm(pts)
#
#   pts = dplyr::group_split(pts, id)
#   pts = lapply(pts, function(x){
#     x = sf::st_coordinates(x)[,1:2]
#     x = sf::st_as_sf(sf::st_sfc(sf::st_linestring(x), crs = 27700))
#     x
#   })
#   pts = dplyr::bind_rows(pts)
#   pts = sf::st_difference(pts, bin)
#
#   #Clip off the start and end of each line
#   pts = sf::st_collection_extract(pts, "LINESTRING")
#   pts = sf::st_cast(pts, "LINESTRING")
#   pts = lapply(pts$x, function(x){
#     x = sf::st_coordinates(x)[,1:2]
#     if(nrow(x) < 4){
#       return(NULL)
#     }
#     x = x[seq(2,nrow(x) - 1),]
#     x = sf::st_as_sf(sf::st_sfc(sf::st_linestring(x), crs = 27700))
#     x
#   })
#   pts = dplyr::bind_rows(pts)
#   pts$id = seq(1,nrow(pts))
#
#   pts_start = sf::st_as_sf(lwgeom::st_startpoint(pts))
#   pts_end = sf::st_as_sf(lwgeom::st_endpoint(pts))
#   pts_start$id = pts$id
#   pts_end$id = pts$id
#
#   qtm(pts[98,], lines.lwd = 2, lines.col = "red") + qtm(pts) + qtm(pts_start, dots.col = "red") + qtm(pts_end, dots.col = "blue")
#
#   pts_all = rbind(pts_end, pts_start)
#
#   # Pair up start and end
#   nn = suppressMessages(nngeo::st_nn(pts_all, pts_all, k = 2, maxdist = 100, progress = FALSE))
#   nn = lapply(nn, function(x){
#     if(length(x) < 2){
#       return(NA)
#     } else {
#       x[2]
#     }
#   })
#   nn = unlist(nn)
#   nnn = seq_along(nn)
#
#   # Graph analysis identifies closed loops when multiple areas
#   edges <- c(rbind(nnn, nn))
#   edges_int <-
#   g = igraph::make_graph(edges = edges, n = length(nn), directed = FALSE)
#   #g = igraph::simplify(g)
#   comps <- igraph::components(g)
#   pts_all$membership = comps$membership
#
#   lookup = sf::st_drop_geometry(pts_all)
#
#
#   igraph::V(g)$subgraph_id <- comps$membership
#
#   # for(j in unique(comps$membership)){
#   #   subgraph <- igraph::induced_subgraph(g, which(comps$membership == j))
#   #   subgraph <- igraph::as_data_frame(subgraph)
#   #   subgraph <-
#   #
#   # }
#
#   #pts = pts[order(pts$membership),]
#   pts_list = dplyr::group_split(pts, membership, .keep = TRUE)
#
#   for(j in seq(1, length(pts_list))){
#     sub = pts_list[[j]]
#     nn_sub = nn[comps$membership == j]
#     nnn_sub = nnn[comps$membership == j]
#     qtm(pts[nnn_sub,])
#
#     ordering = list()
#     ordering[[1]] = nnn_sub[1]
#     for(i in seq(2,length(nnn_sub))){
#       ordering[[i]] = nn_sub[nnn_sub == ordering[[i-1]]]
#     }
#     ordering = unlist(ordering)
#     summary(nnn %in% ordering)
#
#
#   }
#
#
#    # some lines lost
#
#
#
#   pts$order = seq_along(ordering)[match(pts$id, ordering)]
#
#   pts_na = pts[is.na(pts$order),]
#   pts = pts[!is.na(pts$order),]
#   pts = pts[order(pts$order),]
#
#   pts = suppressWarnings(sf::st_cast(pts, "POINT"))
#   pts = sf::st_coordinates(pts[,1:2])
#   pts = rbind(pts, pts[1,])
#   pts = sf::st_polygon(list(pts))
#   pts = sf::st_sfc(pts, crs = 27700)
#
#   pts
#
# }
#
#
# # foo = wood_local_reg[wood_reg[1,],]
# # foob = sf::st_union(sf::st_buffer(foo, 5))
# # foou = sf::st_union(foo)
# # foo$id = 1:nrow(foo)
# #
# # foop = sf::st_cast(foo, "POINT")
# #
# # id = 400
# # x = nn[[id]]
# # qtm(foop) + qtm(foop[x,], dots.col = "red") + qtm(foop[id,], dots.col = "blue")
# # near_not_smame(x, id)
# #
# # near_not_smame = function(x, id, zids = foop$id){
# #   zid = zids[id]
# #   if(length(x) == 1){
# #     return(zid)
# #   }
# #   zids_near = zids[x]
# #   x = x[zids_near != zid]
# #   if(length(x) == 0){
# #     return(zid)
# #   }
# #   return(zids[x[1]])
# # }
# #
# # nn = nngeo::st_nn(foop, foop, k = 5, maxdist = 50)
# # nn2 = purrr::map2(nn, seq_along(nn), near_not_smame, zids = foop$id)
# # nn2 = unlist(nn2)
# #
# # foop$nn_id = nn2
# # foop$diff = ifelse(foop$id != foop$nn_id,"y","n")
# # foop$pid = 1:nrow(foop)
# # qtm(foo) + qtm(foop, dots.col = "diff")
# #
# # bar = foop[foop$diff == "n",]
# #
# # convex = sf::st_convex_hull(sf::st_union(bar))
# # convave = sf::st_concave_hull(sf::st_union(bar), 0, allow_holes = FALSE)
# # convave2 = sf::st_concave_hull(sf::st_union(bar[sample(1:nrow(bar), size = nrow(bar)),]), 0, allow_holes = FALSE)
# #
# # qtm(foo) + qtm(convex, fill = NULL, borders = "red") + qtm(convave, fill = NULL, borders = "blue") + qtm(bar, dots.col = "green")
# #
# # qtm(convave, fill = NULL, borders = "blue") + qtm(convave2, fill = NULL, borders = "red")
# #
# #
# #
# #
# #
# # qtm(fizz5, fill = "blue") + qtm(foo)
# #
# #
# # qtm(foo) + qtm(foobout, fill = NULL, borders = "red") + qtm(foobin, fill = NULL, borders = "blue") + qtm(fizz)
# #
# # qtm(foobin, fill = "blue") + qtm(foobin2, fill = "red") +qtm(foo) + qtm(fizz2)


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
