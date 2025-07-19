library(dplyr)

pt = list()

for(i in c(2004:2011,2014:2024)){
  sub = readRDS(paste0("../inputdata/pt_frequency/trips_per_lsoa21_by_mode_",i,".Rds"))
  sub$year = i
  pt[[i]] = sub
}
zone_service = dplyr::bind_rows(pt)
zone_service = zone_service[zone_service$route_type == 3,]# Bus Only

zone_service$tph_weekday_Night <- (zone_service$tph_Mon_Night +
                                     zone_service$tph_Tue_Night +
                                     zone_service$tph_Wed_Night +
                                     zone_service$tph_Thu_Night +
                                     zone_service$tph_Fri_Night) / 5

zone_service$tph_weekday_Morning_Peak <- (zone_service$`tph_Mon_Morning Peak` +
                                            zone_service$`tph_Tue_Morning Peak` +
                                            zone_service$`tph_Wed_Morning Peak` +
                                            zone_service$`tph_Thu_Morning Peak` +
                                            zone_service$`tph_Fri_Morning Peak`) / 5

zone_service$tph_weekday_Afternoon_Peak <- (zone_service$`tph_Mon_Afternoon Peak` +
                                              zone_service$`tph_Tue_Afternoon Peak` +
                                              zone_service$`tph_Wed_Afternoon Peak` +
                                              zone_service$`tph_Thu_Afternoon Peak` +
                                              zone_service$`tph_Fri_Afternoon Peak`) / 5

zone_service$tph_weekday_Midday <- (zone_service$tph_Mon_Midday +
                                      zone_service$tph_Tue_Midday +
                                      zone_service$tph_Wed_Midday +
                                      zone_service$tph_Thu_Midday +
                                      zone_service$tph_Fri_Midday) / 5

zone_service$tph_weekday_Evening <- (zone_service$tph_Mon_Evening +
                                       zone_service$tph_Tue_Evening +
                                       zone_service$tph_Wed_Evening +
                                       zone_service$tph_Thu_Evening +
                                       zone_service$tph_Fri_Evening) / 5

zone_service$tph_weekday_Night <- (zone_service$tph_Mon_Night +
                                     zone_service$tph_Tue_Night +
                                     zone_service$tph_Wed_Night +
                                     zone_service$tph_Thu_Night +
                                     zone_service$tph_Fri_Night) / 5

zone_service <- zone_service[,c("zone_id",
                                "route_type",
                                "year",
                                "tph_weekday_Morning_Peak",
                                "tph_weekday_Midday",
                                "tph_weekday_Afternoon_Peak",
                                "tph_weekday_Evening",
                                "tph_weekday_Night",
                                "tph_Sat_Morning Peak",
                                "tph_Sat_Midday",
                                "tph_Sat_Afternoon Peak",
                                "tph_Sat_Evening",
                                "tph_Sat_Night",
                                "tph_Sun_Morning Peak",
                                "tph_Sun_Midday",
                                "tph_Sun_Afternoon Peak",
                                "tph_Sun_Evening",
                                "tph_Sun_Night")]
names(zone_service) <- gsub(" ","_",names(zone_service))

zone_service$tph_daytime_avg =  (zone_service$tph_weekday_Morning_Peak * 5 * 4 +
                                   zone_service$tph_weekday_Midday * 5 * 5 +
                                   zone_service$tph_weekday_Afternoon_Peak * 5 * 3 +
                                   zone_service$tph_weekday_Evening * 5 * 4 +
                                   zone_service$tph_Sat_Morning_Peak * 4 +
                                   zone_service$tph_Sat_Midday * 5 +
                                   zone_service$tph_Sat_Afternoon_Peak  * 3 +
                                   zone_service$tph_Sat_Evening * 4 +
                                   zone_service$tph_Sun_Morning_Peak  * 4 +
                                   zone_service$tph_Sun_Midday * 5 +
                                   zone_service$tph_Sun_Afternoon_Peak * 3 +
                                   zone_service$tph_Sun_Evening * 4) / (7 * 16)


zone_service = zone_service[,c("zone_id","route_type","year","tph_daytime_avg")]

zone_summary = zone_service |>
  group_by(zone_id) |>
  summarise(tph_early = mean(tph_daytime_avg[year %in% c(2007:2009)]),
            tph_late = mean(tph_daytime_avg[year %in% c(2021:2023)])
            )



zone_summary$change = (zone_summary$tph_late - zone_summary$tph_early)/(zone_summary$tph_early) * 100

regions = read.csv("../inputdata/boundaries/LSOA_(2021)_to_Built_Up_Area_to_Local_Authority_District_to_Region_(December_2022)_Lookup_in_England_and_Wales_v2.csv")
regions = regions[,c("LSOA21CD","RGN22NM")]


zone_summary = left_join(zone_summary,regions, by = c("zone_id" = "LSOA21CD") )
zone_summary$RGN22NM = ifelse(is.na(zone_summary$RGN22NM) & substr(zone_summary$zone_id,1,1) == "S","Scotland",zone_summary$RGN22NM)

zone_summary = zone_summary[!is.na(zone_summary$zone_id),]

region_summary = zone_summary |>
  group_by(RGN22NM) |>
  summarise(min_change = min(change, na.rm = TRUE),
            max_change = max(change, na.rm = TRUE),
            mean_change = mean(change, na.rm = TRUE),
            median_change = median(change, na.rm = TRUE),
            p10_change = quantile(change, 0.1, na.rm = TRUE),
            p20_change = quantile(change, 0.2, na.rm = TRUE),
            p30_change = quantile(change, 0.3, na.rm = TRUE),
            p40_change = quantile(change, 0.4, na.rm = TRUE),
            p50_change = quantile(change, 0.5, na.rm = TRUE),
            p60_change = quantile(change, 0.6, na.rm = TRUE),
            p70_change = quantile(change, 0.7, na.rm = TRUE),
            p80_change = quantile(change, 0.8, na.rm = TRUE),
            p90_change = quantile(change, 0.9, na.rm = TRUE)
            )


write.csv(region_summary,"data/region_bus_change.csv", row.names = FALSE)
