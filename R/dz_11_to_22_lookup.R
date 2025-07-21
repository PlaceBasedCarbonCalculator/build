make_dz_11_22_lookup = function(bounds_dz11, bounds_dz22){

  over = sf::st_intersects(bounds_dz22, bounds_dz11)

  bounds_dz22$area22 = as.numeric(sf::st_area(bounds_dz22))
  bounds_dz11$area11 = as.numeric(sf::st_area(bounds_dz11))

  res = list()

  #Intresting examples
  exe = c("S01014682", "S01014688", "S01014832", "S01013485", "S01018390", "S01017990", "S01018879", "S01015643")
  exe_i = seq_along(over)[bounds_dz22$DataZone22 %in% exe]

  #Check i = 3 as next problem to work on
  for(i in seq_along(over)) {
    if(i %% 100 == 0) {
      message(paste("Processing DataZone22:", i, "of", length(over)))
    }
    if(length(over[[i]]) == 0) {
      stop(paste("No match found for DataZone22:", bounds_dz22$DataZone22[i]))
    }
    if(length(over[[i]]) == 1) {
      # Special case, 2022 zone is completely inside 2011 zone
      sub_intersection = sf::st_intersection(bounds_dz22[i,], bounds_dz11[over[[i]],])
      sub_intersection$areaInter = as.numeric(sf::st_area(sub_intersection))
      sub_intersection$pInter = sub_intersection$areaInter / sub_intersection$area11
      sub_intersection = sub_intersection[,c("DataZone","DataZone22","area11", "area22", "areaInter", "pInter")]
      res[[i]] = sub_intersection
    } else if(length(over[[i]]) > 1) {
      sub_intersection = sf::st_intersection(bounds_dz22[i,], bounds_dz11[over[[i]],])
      sub_intersection = slither_detection(sub_intersection) # Remove Slithers
      # Remove small areas, less than 1% of the 2022 area
      #sub_intersection = sub_intersection[sub_intersection$area_inter > (bounds_dz22$area22[i]/100),]

      # Remove small areas, less than 1% of the 2011 area
      sub_intersection$p_change = round(sub_intersection$area_inter / sub_intersection$area11,3)
      sub_intersection = sub_intersection[sub_intersection$p_change > 0.01,]


      # Also look for 2011 area larger than 2021

      # sub_difference = sf::st_difference(bounds_dz11[over[[i]],],bounds_dz22[i,])
      # sub_difference = slither_detection(sub_difference) # Remove Slithers
      # sub_difference$area_diff = as.numeric(sf::st_area(sub_difference))
      # sub_difference$area_change = round(sub_difference$area11 - sub_difference$area_diff)
      # sub_difference$p_change = sub_difference$area_change / sub_difference$area11
      # # Remove small areas, less than 1% of the 2011 area
      # sub_difference = sub_difference[sub_difference$p_change > 0.01,]

      # Summarise what makes up the area of the 2022 zone
      sel_11 = bounds_dz11[bounds_dz11$DataZone %in% unique(c(sub_intersection$DataZone, sub_difference$DataZone)),]
      inter2 = sf::st_intersection(sel_11, bounds_dz22[i,])
      inter2$areaInter = as.numeric(sf::st_area(inter2))
      inter2 = sf::st_collection_extract(inter2, "POLYGON")
      inter2 = inter2 |>
        dplyr::group_by(DataZone) |>
        dplyr::summarise(DataZone22 = DataZone22[1],
                         area11 = area11[1],
                         area22 = area22[1],
                         areaInter = areaInter[1])
      inter2$pInter = inter2$areaInter / inter2$area11

      # output = data.frame(DataZone22 = bounds_dz22$DataZone22[i],
      #                     overlaps_with = I(list(inter2)), #22 is bigger than 11
      #                     overlapped_by = I(list(sub_difference$DataZone)), #11 is bigger than 22
      #                     stringsAsFactors = FALSE
      #                     )

      res[[i]] = inter2



    }


  }

  res = dplyr::bind_rows(res)
  # res$overlaps_with_count = lengths(res$overlaps_with)
  # res$overlapped_by_count = lengths(res$overlapped_by)

}

# library(tmap)
# tmap_mode("view")

# qtm(bounds_dz22[i,], lines.col = "red", fill = NULL) +
#   qtm(bounds_dz11[over[[i]],], lines.col = "blue", fill = NULL)
#
#
# qtm(bounds_dz11[over[[i]],], lines.col = "blue", fill = NULL) +
#   qtm(inter2, lines.col = "green", fill = NULL) +
#   qtm(bounds_dz22[i,], lines.col = "red", fill = NULL)
#
#   # qtm(sub_intersection, lines.col = "black", fill = NULL) +
#   # qtm(sub_difference, lines.col = "green", fill = NULL)
#
# qtm(bounds_dz11[over[[i]],], lines.col = "blue", fill = NULL) +
#   qtm(sub_intersection, lines.col = "green", fill = NULL) +
#   qtm(bounds_dz22[i,], lines.col = "red", fill = NULL)
#
#
# qtm(bounds_dz22[i,], lines.col = "red", fill = NULL) +
#   qtm(sel_11, lines.col = "blue", fill = NULL)


slither_detection = function(x, apratio = 0.5, min_area = 100){
  x = sf::st_collection_extract(x, "POLYGON")
  x = sf::st_cast(x, "POLYGON")
  x$area_inter <- as.numeric(sf::st_area(x))
  x <- x[x$area_inter > min_area,]
  x$perimiter_inter <- as.numeric(sf::st_perimeter(x))
  x$apratio <- x$perimiter_inter / x$area_inter
  x <- x[x$apratio < apratio,]
  x
}
