access_counts = function(zones, poi, oa, pop, lookup_oa2021_lsoa2021, gb_pop = 67.33e6){
  poi = poi[poi$measure_access,]
  # Calculate national rates
  poi = poi[,c("groupname","categoryname","classname","count")]
  poi_nat = sf::st_drop_geometry(poi)
  poi_nat = unique(poi_nat)
  poi_nat$nat_pps = gb_pop / poi_nat$count
  poi_nat$count = NULL
  poi_nat$nat_sp10kp = 10000/poi_nat$nat_pps

  pop = pop[,c("OA21CD","total_pop")]
  oa = dplyr::left_join(oa, pop, by = "OA21CD")

  total_pop = sum(pop$total_pop)

  inter_poi = sf::st_intersects(zones, poi)
  inter_pop = sf::st_intersects(zones, oa)

  poi = sf::st_drop_geometry(poi[,c("groupname","categoryname","classname")])

  #Fill in Missing Services
  poi_unique = poi[!duplicated(poi$classname),]
  poi_unique$count = 0

  summary_poi = pbapply::pblapply(inter_poi, summarise_poi, poi = poi, poi_unique = poi_unique)
  names(summary_poi) = zones[[1]]
  summary_poi = dplyr::bind_rows(summary_poi, .id = names(zones)[1])

  oa = sf::st_drop_geometry(oa)


  summary_pop = pbapply::pbsapply(inter_pop, summarise_pop, oa = oa)
  summary_pop = data.frame(population = summary_pop)
  summary_pop$zone = zones[[1]]

  # Check for missing population
  # Happens when snapping to road network further than travel time back to centroid.
  if(all(summary_pop$zone %in% pop$OA21CD)){
    summary_pop = dplyr::left_join(summary_pop, pop, by = c("zone" = "OA21CD"))
    summary_pop$population = dplyr::if_else(summary_pop$population == 0,
                                            summary_pop$total_pop,
                                            summary_pop$population)
    summary_pop$total_pop = NULL
  } else if (all(summary_pop$zone %in% lookup_oa2021_lsoa2021$LSOA21CD)) {
    pop_lsoa = dplyr::left_join(lookup_oa2021_lsoa2021, pop, by = c("nearest_OA2021" = "OA21CD"))
    pop_lsoa = pop_lsoa[,c("LSOA21CD","total_pop")]
    summary_pop = dplyr::left_join(summary_pop, pop_lsoa, by = c("zone" = "LSOA21CD"))
    summary_pop$population = dplyr::if_else(summary_pop$population == 0,
                                            summary_pop$total_pop,
                                            summary_pop$population)
    summary_pop$total_pop = NULL
  }


  colname = names(summary_poi)[1]
  summary_poi = dplyr::left_join(summary_poi, summary_pop, by = setNames("zone", colname))
  summary_poi$local_pps = summary_poi$population / summary_poi$count
  summary_poi = dplyr::left_join(summary_poi,poi_nat, by = c("groupname","categoryname","classname"))
  summary_poi$pps_diff = summary_poi$local_pps - summary_poi$nat_pps
  summary_poi$pps_diff_std = summary_poi$pps_diff / summary_poi$nat_pps
  summary_poi$local_sp10kp = 10000/summary_poi$local_pps
  summary_poi$local_sp10kp[summary_poi$local_sp10kp == 0] = NA
  #summary_poi$local_sp10kp[is.infinite(summary_poi$local_sp10kp)] = NA
  summary_poi$sp10kp_diff = summary_poi$local_sp10kp - summary_poi$nat_sp10kp
  summary_poi$sp10kp_diff_std = summary_poi$sp10kp_diff / summary_poi$nat_sp10kp

  summary_poi = dplyr::group_by(summary_poi, groupname, categoryname, classname)
  summary_poi = dplyr::mutate(summary_poi, sp10kp_SD = sd(local_sp10kp, na.rm = TRUE))
  summary_poi = dplyr::ungroup(summary_poi)
  summary_poi$sp10kp_diff_SD = summary_poi$sp10kp_diff / summary_poi$sp10kp_SD

  # hist(summary_poi$sp10kp_diff_std)
  #
  # foo = summary_poi[summary_poi$LSOA11CD == "E01012007",]
  #
  # library(ggplot2)
  # ggplot(foo, aes(x = sp10kp_diff_std, y = count, col = categoryname)) +
  #   geom_point() +
  #   theme(legend.position = "none") +
  #   xlim(-1,1)
  #
  # foo = summary_poi[summary_poi$classname == "Bed and Breakfast and Backpacker Accommodation", ]
  # hist(foo$local_pps, breaks = seq(0, 1130000, 100))
  # abline(v = mean(foo$local_pps), col = "red")
  # foo$diff = foo$local_pps - foo$nat_pps
  # hist(foo$diff)
  # sd(foo$local_pps)
  summary_poi

}

summarise_poi = function(x, poi, poi_unique){
  poi_sub = poi[x,]
  poi_sub = dplyr::group_by(poi_sub, groupname, categoryname, classname)
  poi_sub = dplyr::summarise(poi_sub, count = dplyr::n(), .groups= 'drop')
  # Add Missing
  poi_unique = poi_unique[!poi_unique$classname %in% poi_sub$classname,]
  poi_sub = rbind(poi_sub, poi_unique)
  poi_sub
}

summarise_pop = function(x, oa){
  pop_sub = oa[x,]
  pop_total = sum(pop_sub$total_pop)
  pop_total
}

#TODO: Some places have inf accessibility e.g. E01027752 to bus stops
