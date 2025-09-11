make_dz_11_22_lookup = function(bounds_dz11, bounds_dz22, uprn_bng){

  over = sf::st_intersects(bounds_dz22, bounds_dz11)

  bounds_dz22$area22 = as.numeric(sf::st_area(bounds_dz22))
  bounds_dz11$area11 = as.numeric(sf::st_area(bounds_dz11))

  res = list()

  #Intresting examples
  exe = c("S01013531","S01014682", "S01014688", "S01014832", "S01013485", "S01018390", "S01017990", "S01018879", "S01015643")
  exe_i = seq_along(over)[bounds_dz22$DataZone22 %in% exe]

  #Check i = 3 as next problem to work on
  for(i in seq_along(over)) {
  #for(i in 1:100) {
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
      sub_intersection = suppressWarnings(sf::st_intersection(bounds_dz22[i,], bounds_dz11[over[[i]],]))
      sub_intersection = slither_detection(sub_intersection) # Remove Slithers
      # Remove small areas, less than 1% of the 2022 area
      #sub_intersection = sub_intersection[sub_intersection$area_inter > (bounds_dz22$area22[i]/100),]

      # Remove small areas, less than 1% of the 2011 area
      sub_intersection$p_change = round(sub_intersection$area_inter / sub_intersection$area11,3)
      sub_intersection = sub_intersection[sub_intersection$p_change > 0.01 | sub_intersection$area_inter > 25000,] #Keep big or significant areas


      # Also look for 2011 area larger than 2021

      # sub_difference = sf::st_difference(bounds_dz11[over[[i]],],bounds_dz22[i,])
      # sub_difference = slither_detection(sub_difference) # Remove Slithers
      # sub_difference$area_diff = as.numeric(sf::st_area(sub_difference))
      # sub_difference$area_change = round(sub_difference$area11 - sub_difference$area_diff)
      # sub_difference$p_change = sub_difference$area_change / sub_difference$area11
      # # Remove small areas, less than 1% of the 2011 area
      # sub_difference = sub_difference[sub_difference$p_change > 0.01,]

      # Summarise what makes up the area of the 2022 zone
      #sel_11 = bounds_dz11[bounds_dz11$DataZone %in% unique(c(sub_intersection$DataZone, sub_difference$DataZone)),]
      sel_11 = bounds_dz11[bounds_dz11$DataZone %in% unique(c(sub_intersection$DataZone)),]
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
  res$areaID = seq_len(nrow(res))

  uprn_bng = sf::st_join(uprn_bng, res[,"areaID"])

  #Start 15:28:45 UK laptop time, finished 11 by 16:25
  uprn_summary = uprn_bng |>
    sf::st_drop_geometry() |>
    dplyr::group_by(areaID) |>
    dplyr::summarise(count = dplyr::n())

  res = dplyr::left_join(res, uprn_summary, by = "areaID")

  res$count[is.na(res$count)] = 0

  res
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


make_dz_11_22_lookup_simple = function(lookup_dz_2011_22_pre){
  # Scotland
  lookup_dz_2011_22_pre = sf::st_drop_geometry(lookup_dz_2011_22_pre)
  # Share of the 2011 households
  lookup_dz_2011_22_pre = lookup_dz_2011_22_pre |>
    dplyr::group_by(DataZone) |>
    dplyr::mutate(splitshare = count / sum(count)) |>
    dplyr::ungroup()

  lookup_dz_2011_22_pre = lookup_dz_2011_22_pre[,c("DataZone","DataZone22","splitshare")]
  names(lookup_dz_2011_22_pre) = c("LSOA11CD","LSOA21CD","splitshare")
  lookup_dz_2011_22_pre
}

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


# Convert Population 2011 to Population 22

interpolate_population_dz11_dz22 = function(lookup_dz_2011_22_pre, households_scotland, population_scot, dwellings_tax_band_scotland){

  lookup_dz_2011_22_pre = sf::st_drop_geometry(lookup_dz_2011_22_pre)

  dwellings_tax_band_scotland = dwellings_tax_band_scotland[,c("LSOA11CD","year","all_properties")]
  #dwellings_tax_band_scotland = dwellings_tax_band_scotland[dwellings_tax_band_scotland$year >= 2010,]

  # Share of the 2011 households
  lookup_dz_2011_22_pre = lookup_dz_2011_22_pre |>
    dplyr::group_by(DataZone) |>
    dplyr::mutate(splitshare = count / sum(count)) |>
    dplyr::ungroup()


  lookup_dz_2011_22_pre = dplyr::group_split(lookup_dz_2011_22_pre, DataZone22)

  res = list()

  for(i in seq_along(lookup_dz_2011_22_pre)){
    sub = lookup_dz_2011_22_pre[[i]]
    pop_sub = population_scot[population_scot$LSOA11CD %in% sub$DataZone,]
    dwel_sub = households_scotland[households_scotland$dz11cd %in% sub$DataZone,]
    tax_sub = dwellings_tax_band_scotland[dwellings_tax_band_scotland$LSOA11CD %in% sub$DataZone,]

    dwel_sub = dwel_sub[,c("dz11cd","dwellings_total","occupied","year")]
    names(dwel_sub) = c("dz11cd","all_properties","households","year")

    tax_sub = tax_sub[tax_sub$year < 2014,]
    tax_sub$households = tax_sub$all_properties #TODO better estimate of households pre-2014

    names(tax_sub)[1] = "dz11cd"

    dwel_sub = rbind(dwel_sub, tax_sub)

    pop_sub = dplyr::left_join(pop_sub,
                               sub[,c("DataZone","DataZone22","splitshare")],
                               by = c("LSOA11CD" = "DataZone"))

    dwel_sub = dplyr::left_join(dwel_sub,
                               sub[,c("DataZone","DataZone22","splitshare")],
                               by = c("dz11cd" = "DataZone"))

    bands = c("all_ages","90+","0-4","5-9","10-14","15-19","20-24","25-29","30-34",
              "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79",
              "80-84","85-89")

    for(j in bands){
      pop_sub[[j]] = pop_sub[[j]] * pop_sub$splitshare
    }

    bands = c("all_properties","households")

    for(j in bands){
      dwel_sub[[j]] = dwel_sub[[j]] * dwel_sub$splitshare
    }

    pop_sub = pop_sub |>
      dplyr::group_by(DataZone22, year) |>
      dplyr::summarise(`all_ages` = round(sum(`all_ages`)),
                       `0-4` = round(sum(`0-4`)),
                       `5-9` = round(sum(`5-9`)),
                       `10-14` = round(sum(`10-14`)),
                       `15-19` = round(sum(`15-19`)),
                       `20-24` = round(sum(`20-24`)),
                       `25-29` = round(sum(`25-29`)),
                       `30-34` = round(sum(`30-34`)),
                       `35-39` = round(sum(`35-39`)),
                       `40-44` = round(sum(`40-44`)),
                       `45-49` = round(sum(`45-49`)),
                       `50-54` = round(sum(`50-54`)),
                       `55-59` = round(sum(`55-59`)),
                       `60-64` = round(sum(`60-64`)),
                       `65-69` = round(sum(`65-69`)),
                       `70-74` = round(sum(`70-74`)),
                       `75-79` = round(sum(`75-79`)),
                       `80-84` = round(sum(`80-84`)),
                       `85-89` = round(sum(`85-89`)),
                       `90+` = round(sum(`90+`)))


    dwel_sub = dwel_sub |>
      dplyr::group_by(DataZone22, year) |>
      dplyr::summarise(all_properties = round(sum(all_properties)),
                       households = round(sum(households)))

    pop_sub = dplyr::left_join(pop_sub, dwel_sub, by = c("DataZone22", "year"))

    pop_sub = pop_sub[pop_sub$year >= 2005, ]

    if(anyNA(pop_sub)){
      stop("NAs in",i)
    }


    res[[i]] = pop_sub

  }

  res = dplyr::bind_rows(res)
  res

}


