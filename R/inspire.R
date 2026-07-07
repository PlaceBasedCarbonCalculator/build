#' Load Scottish INSPIRE cadastral parcels
#'
#' @description Reads every zipped INSPIRE cadastral parcel shapefile (one per
#'   local authority) in `path` and binds them into one layer tagged with the
#'   source zip name as `local_authority`. Unlike the England & Wales loader,
#'   no grid-artefact cleaning is applied. Used by the `inspire_scotland`
#'   target, which feeds `combine_os_osm_buildings()`.
#' @param path Folder of INSPIRE Scotland zip files.
#' @return An sf data frame with `local_authority`, `inspireid` and geometry.
#' @keywords internal
load_inspire_scotland = function(path = file.path(parameters$path_data,"INSPIRE_scotland")) {
  zips = list.files(path, pattern = ".zip", full.names = TRUE)
  polys <- list()
  for(i in 1:length(zips)){
    dir.create(file.path(tempdir(),"inspire"))
    unzip(zips[i], exdir = file.path(tempdir(),"inspire"))
    fl = list.files(file.path(tempdir(),"inspire"), pattern = "_bng.shp$")
    poly <- sf::read_sf(file.path(tempdir(),"inspire",fl))
    poly <- poly[,c("inspireid")]
    message(Sys.time()," ",zips[i]," ",nrow(poly)," polygons")
    unlink(file.path(tempdir(),"inspire"), recursive = TRUE)
    polys[[i]] = poly
  }
  names(polys) = gsub(".zip","",zips)

  polys = dplyr::bind_rows(polys, .id = "local_authority")
  polys


}


#' Load and clean England & Wales INSPIRE cadastral parcels
#'
#' @description Reads every zipped INSPIRE GML (one per local authority) and
#'   repairs an artefact of the INSPIRE publication process: parcels are
#'   arbitrarily split along a 500 m national grid. For each LA the function
#'   rebuilds the 500 m grid, finds parcels whose edges lie on it, and merges
#'   the two polygons either side of each shared grid-line segment back
#'   together; a final pass merges any remaining exact 500 m squares into
#'   their neighbours. LA names are cleaned ("...Borough_Council" etc.
#'   stripped) and stored as `local_authority`. Used by the `inspire` target,
#'   which feeds `combine_os_osm_buildings()`. Slow, and known to fail on some
#'   inputs (e.g. City of London: "Splitting a Line by a GeometryCollection is
#'   unsupported").
#' @param path Folder of INSPIRE zip files, each containing
#'   `Land_Registry_Cadastral_Parcels.gml`.
#' @return An sf data frame with `local_authority`, `INSPIREID`, `area` (m2)
#'   and polygon geometry.
#' @keywords internal
load_inspire = function(path = file.path(parameters$path_data,"INSPIRE")){

  zips = list.files(path, pattern = ".zip", full.names = TRUE)

  polys <- list()
  # fails at 68 city of london
  # Splitting a Line by a GeometryCollection is unsupported

  for(i in 1:length(zips)){
    dir.create(file.path(tempdir(),"inspire"))

    unzip(zips[i], exdir = file.path(tempdir(),"inspire"))
    poly <- sf::read_sf(file.path(tempdir(),"inspire","Land_Registry_Cadastral_Parcels.gml"))
    poly <- poly[,c("INSPIREID")]
    message(Sys.time()," ",zips[i]," ",nrow(poly)," polygons")
    unlink(file.path(tempdir(),"inspire"), recursive = TRUE)

    # Make a grid
    grd <- sf::st_bbox(poly)
    grd[1] <- plyr::round_any(grd[1], 500, f = floor)
    grd[2] <- plyr::round_any(grd[2], 500, f = floor)
    grd[3] <- plyr::round_any(grd[3], 500, f = ceiling)
    grd[4] <- plyr::round_any(grd[4], 500, f = ceiling)
    grd <- sf::st_as_sfc(grd)
    grd <- sf::st_make_grid(grd, c(500,500))

    # Select polys that may be split by the grid
    sub <- poly[grd, , op = sf::st_touches]

    # Make the grid any polys into lines
    grd_line <- sf::st_cast(grd,"LINESTRING")
    suppressWarnings(sub_line <- sf::st_cast(sub,"LINESTRING"))

    # Convert Grid to Lines
    grd_line <- sf::st_as_sf(grd_line)
    grd_line$id <- 1
    grd_line <- stplanr::overline2(grd_line, "x", simplify = FALSE, quiet = TRUE)
    sub2 <- sub_line[grd_line,,op=sf::st_overlaps]

    #Problem with square boxes, so capture them
    sub3 <- sub_line[grd_line,,op=sf::st_covers]
    sub3 <- sub3[!sub3$INSPIREID %in% sub2$INSPIREID,]
    if(nrow(sub3) > 0){
      sub2 <- rbind(sub2, sub3)
    }
    rm(sub3)



    if(nrow(sub2) > 0){
      # Idea: Split the grid_line2 at the polygon boundaires so each line is only the join between two polygons
      # Then in a loop for each line select the two polygons an merge
      suppressWarnings(sub2_pt <- sf::st_cast(sub2,"POINT"))
      sub2_pt <- sub2_pt[grd_line, ]
      sub2_pt <- sub2_pt[!duplicated(sub2_pt$GEOMETRY),]
      sub2_pt <- sf::st_combine(sub2_pt)

      grd_line <- lwgeom::st_split(grd_line, sub2_pt)
      grd_line <- sf::st_collection_extract(grd_line, "LINESTRING")
      grd_line <- sf::st_as_sf(grd_line)
      grd_line$id <- as.character(sample(1:nrow(grd_line), nrow(grd_line)))
      grd_line <- grd_line[sub2, , op = sf::st_covered_by]

      sub2 <- poly[poly$INSPIREID %in% sub2$INSPIREID,]
      sub2_new <- sub2

      for(j in seq_len(nrow(grd_line))){
        #message(Sys.time()," ",j)
        lin <- grd_line[j,]
        sub2_sel <- sub2_new[lin,, op = sf::st_covers]

        # Should only have two polygons
        if(nrow(sub2_sel) > 2){
          sub2_sel <- sub2_new[lin,, op = sf::st_intersects]
        }

        if(nrow(sub2_sel) <= 1){
          #message("only one geom")
          next
        } else if(nrow(sub2_sel) > 2){
          #message("more than two geom")
          next
        }

        sub2_new <- sub2_new[!sub2_new$INSPIREID %in% sub2_sel$INSPIREID,]
        sub2_sel_geom <- sf::st_union(sub2_sel)
        sub2_sel <- sub2_sel[1,]
        sub2_sel$GEOMETRY <- sub2_sel_geom
        sub2_new <- rbind(sub2_new, sub2_sel)
      }

      poly_new <- poly[!poly$INSPIREID %in% sub2$INSPIREID, ]
      poly_new <- rbind(poly_new, sub2_new)
    } else {
      poly_new <- poly
    }

    poly_new$area <- as.numeric(sf::st_area(poly_new))
    poly_new$perimiter <- as.numeric(sf::st_perimeter(poly_new))

    # Final Pass for any perfect squares (500m x 500m: area and perimeter must both match)
    poly_squares <- poly_new[poly_new$area == 250000 & poly_new$perimiter == 2000,]

    if(nrow(poly_squares) > 0){
      poly_new <- poly_new[!poly_new$INSPIREID %in% poly_squares$INSPIREID,]

      for(j in seq_len(nrow(poly_squares))){
        #message(Sys.time()," ",j)
        sqr <- poly_squares[j,]
        poly_sel <- poly_new[sqr,, op = sf::st_intersects]
        poly_new <- poly_new[!poly_new$INSPIREID %in% poly_sel$INSPIREID,]
        poly_sel <- rbind(poly_sel, sqr)
        poly_sel_geom <- sf::st_union(poly_sel)
        poly_sel <- poly_sel[1,]
        poly_sel$GEOMETRY <- poly_sel_geom
        poly_new <- rbind(poly_new, poly_sel)
      }
    }

    # check class
    if("sfc_POLYGON" %in% class(poly_new$GEOMETRY)){
      # DO nothing
    } else {
      poly_mp <- poly_new[sf::st_geometry_type(poly_new) == "MULTIPOLYGON",]
      poly_new <- poly_new[sf::st_geometry_type(poly_new) == "POLYGON",]
      poly_mp <- sf::st_cast(poly_mp, "POLYGON")
      poly_new <- rbind(poly_new, poly_mp)
      rm(poly_mp)
    }

    poly_new$area <- round(as.numeric(sf::st_area(poly_new)))
    poly_new <- sf::st_make_valid(poly_new)
    polys[[i]] <- poly_new

    # plot(poly$GEOMETRY)
    # plot(st_transform(poly_new$GEOMETRY, 27700), add = T, border = "red")

    rm(poly, poly_new, grd, grd_line, lin, poly_sel, poly_sel_geom, poly_squares,
       sqr, sub,sub_line, sub2, sub2_new, sub2_pt, sub2_sel, sub2_sel_geom, j)
  }

  nms = gsub(".zip","",gsub(paste0(path,"/"),"",zips))

  nms = gsub("Metropolitan_District_Council","",nms)
  nms = gsub("Borough_Council","",nms)
  nms = gsub("District_Council","",nms)
  nms = gsub("Council","",nms)

  nms = gsub("_"," ",nms)

  names(polys) = nms

  polys = dplyr::bind_rows(polys, .id = "local_authority")
  polys


}



