#' Load public transport frequency per zone, 2004-2025
#'
#' @description Reads the pre-computed trips-per-zone-by-mode Rds files (from
#'   the UK2GTFS timetable analysis; 2012-2013 are unavailable), averages
#'   Monday-Friday into weekday figures per time period, computes a
#'   time-weighted average daytime trips-per-hour (`tph_daytime_avg`), and
#'   pivots wide to one row per zone with `<measure>_<year>_<route_type>`
#'   columns (route type 1100/air is dropped). Used by the `pt_frequency`
#'   target, feeding the PT JSONs, transport map data and bulk export.
#'
#'   Route types follow GTFS: 0 tram, 1 metro, 2 rail, 3 bus, 4 ferry. The
#'   analysis outputs also carry the extended type **200 coach**, which this
#'   function folds back into bus (`_3`) — the sources disagree about where
#'   coach belongs and which years have it at all, so only the combined total
#'   is consistent across the series. See the note at the fold for detail.
#'
#'   2024 and 2025 were rebuilt in August 2026. They previously summed the
#'   BODS national GTFS feed and the TNDS conversion, which both cover the
#'   whole GB bus network, and so counted nearly every journey twice; those two
#'   years now use TNDS alone, like 2018-2023. See
#'   `../PublicTransportAnalysis/reports/foe_bus_decline_comparison.md`.
#' @param path Folder holding the per-year `trips_per_lsoa21_22_by_mode_*.Rds`
#'   files. The `pt_frequency` target passes the analysis repo's output folder
#'   directly. A `pt_frequency/` subfolder is used if the files are not in
#'   `path` itself, which is how `inputdata/pt_frequency/` is laid out.
#' @return A wide data frame with `zone_id` and rounded frequency columns.
#' @keywords internal
load_pt_frequency = function(path = parameters$path_data){

  zone_service = list()

  # The analysis repo writes these straight into its data/ folder; the
  # inputdata release keeps them under pt_frequency/. Accept either, rather
  # than requiring a duplicate copy of 111 MB of Rds files that can silently
  # fall out of step with the originals.
  fname = function(i) paste0("trips_per_lsoa21_22_by_mode_",i,".Rds")
  dir = if(file.exists(file.path(path, fname(2023)))) path else file.path(path,"pt_frequency")
  if(!file.exists(file.path(dir, fname(2023)))){
    stop("No trips_per_lsoa21_22_by_mode_*.Rds found in ", path,
         " or its pt_frequency/ subfolder")
  }

  for(i in c(2004:2011,2014:2025)){
    sub = readRDS(file.path(dir, fname(i)))
    sub$year = i
    zone_service[[i]] = sub
  }

  zone_service = dplyr::bind_rows(zone_service)

  # Fold coach (GTFS extended type 200) back into bus (3).
  #
  # Sources and eras split coach from bus inconsistently: NPTDR marks it with
  # an ATCO-CIF COACH vehicle type, TNDS carries it in a separate national
  # archive (NCSD) that disappears from the snapshots after February 2025, and
  # the BODS GTFS feed codes it 200 throughout. Reporting the two apart would
  # therefore show coach appearing and vanishing for reasons that are entirely
  # about which archive a year came from. The combined total is consistent
  # across every year, so that is what is published.
  #
  # Every column here is either a count of departures or a rate per hour, and
  # a route is bus or coach but never both, so summing is exact.
  zone_service$route_type[zone_service$route_type == 200] = 3
  zone_service = dplyr::summarise(
    dplyr::group_by(zone_service, zone_id, route_type, year),
    dplyr::across(dplyr::everything(), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop")

  # Combine Weekdays
  zone_service$runs_weekday_Night <- (zone_service$runs_Mon_Night +
                                        zone_service$runs_Tue_Night +
                                        zone_service$runs_Wed_Night +
                                        zone_service$runs_Thu_Night +
                                        zone_service$runs_Fri_Night) / 5

  zone_service$runs_weekday_Morning_Peak <- (zone_service$`runs_Mon_Morning Peak` +
                                               zone_service$`runs_Tue_Morning Peak` +
                                               zone_service$`runs_Wed_Morning Peak` +
                                               zone_service$`runs_Thu_Morning Peak` +
                                               zone_service$`runs_Fri_Morning Peak`) / 5

  zone_service$runs_weekday_Afternoon_Peak <- (zone_service$`runs_Mon_Afternoon Peak` +
                                                 zone_service$`runs_Tue_Afternoon Peak` +
                                                 zone_service$`runs_Wed_Afternoon Peak` +
                                                 zone_service$`runs_Thu_Afternoon Peak` +
                                                 zone_service$`runs_Fri_Afternoon Peak`) / 5

  zone_service$runs_weekday_Midday <- (zone_service$runs_Mon_Midday +
                                         zone_service$runs_Tue_Midday +
                                         zone_service$runs_Wed_Midday +
                                         zone_service$runs_Thu_Midday +
                                         zone_service$runs_Fri_Midday) / 5

  zone_service$runs_weekday_Evening <- (zone_service$runs_Mon_Evening +
                                          zone_service$runs_Tue_Evening +
                                          zone_service$runs_Wed_Evening +
                                          zone_service$runs_Thu_Evening +
                                          zone_service$runs_Fri_Evening) / 5

  zone_service$runs_weekday_Night <- (zone_service$runs_Mon_Night +
                                        zone_service$runs_Tue_Night +
                                        zone_service$runs_Wed_Night +
                                        zone_service$runs_Thu_Night +
                                        zone_service$runs_Fri_Night) / 5


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
                                  "routes_Morning Peak",
                                  "routes_Midday",
                                  "routes_Afternoon Peak",
                                  "routes_Evening",
                                  "routes_Night",
                                  "runs_weekday_Morning_Peak",
                                  "runs_weekday_Midday",
                                  "runs_weekday_Afternoon_Peak",
                                  "runs_weekday_Evening",
                                  "runs_weekday_Night",
                                  "runs_Sat_Morning Peak",
                                  "runs_Sat_Midday",
                                  "runs_Sat_Afternoon Peak",
                                  "runs_Sat_Evening",
                                  "runs_Sat_Night",
                                  "runs_Sun_Morning Peak",
                                  "runs_Sun_Midday",
                                  "runs_Sun_Afternoon Peak",
                                  "runs_Sun_Evening",
                                  "runs_Sun_Night",
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

  wide = tidyr::pivot_wider(zone_service,
                     names_from = c("year","route_type"),
                     values_from = tph_weekday_Morning_Peak:tph_daytime_avg,
                     id_cols = c("zone_id")
  )
  wide = wide[,!grepl("_1100$",names(wide))]

  wide[2:ncol(wide)] <- lapply(wide[2:ncol(wide)], round, digits = 1)
  wide


}



#' Build the per-LSOA attribute table for the transport map
#'
#' @description Assembles the variables shown on the transport map tiles
#'   (`transport_lsoa_data` target, consumed by `pmtiles_transport`): latest
#'   BEV/ULEV percentages and vehicles per household from the vehicle
#'   summary (Scotland uses `year_scot` as its registration data lags),
#'   daytime trips-per-hour by mode for each year in `tph_years`, and the
#'   percentage change in bus frequency from the 2006-08 maximum to each year
#'   in `change_years`.
#'
#'   `tph_years` used to be a single hard-coded 2023. The timetable series now
#'   runs to 2025, so it is a vector: every year listed here becomes five more
#'   columns in the tiles (one per GTFS route type) and five more options in
#'   the tool's layer menu. Keep it short - these are whole-GB polygon tiles
#'   and each added year costs tile size for every zone in the country,
#'   whether or not anyone selects it. The full 2004-2025 series is already
#'   available per-zone in the report card charts (`pt_json`), which is the
#'   right place to look at trends; the map is for comparing places in a given
#'   year.
#'
#'   `change_years` keeps 2023 alongside the latest year deliberately: 2008-2023
#'   is the window used in the published Friends of the Earth analysis, so
#'   removing it would silently change the number people cite.
#' @param pt_frequency Wide PT frequency table (`pt_frequency` target).
#' @param vehicle_summary Vehicle summary (`vehicle_summary` target).
#' @param year Year of vehicle data for England & Wales.
#' @param year_scot Year of vehicle data for Scotland.
#' @param tph_years Years to carry daytime trips-per-hour columns for.
#' @param change_years Years to compute a bus change-since-2006-08 column for;
#'   must all appear in `tph_years`.
#' @param rounddp Number of decimal places to round the final output to.
#' @return A data frame with one row per LSOA and the map attribute columns.
#' @keywords internal
select_transport_vars = function(pt_frequency, vehicle_summary, year = 2024, year_scot = 2022,
                                 tph_years = 2025,
                                 change_years = 2025,
                                 rounddp = 1){

  stopifnot(all(change_years %in% tph_years))

  vehicle_summary$country = substr(vehicle_summary$LSOA21CD,1,1)
  vehicle_summary = vehicle_summary[(vehicle_summary$year == year & vehicle_summary$country != "S") |
                                      (vehicle_summary$year == year_scot & vehicle_summary$country == "S")
                                      ,]

  vehicle_summary = vehicle_summary[,c("LSOA21CD","pBEV_COMPANY","pBEV_PRIVATE","pULEV_COMPANY","pULEV_PRIVATE","vehiclesPHousehold")]

  pt_frequency = pt_frequency[!is.na(pt_frequency$zone_id),]

  # The five GTFS route types the map shows: tram, subway, rail, bus, ferry.
  # 200 (coach) is already folded into 3 by load_pt_frequency(), and 1100 (air)
  # is dropped upstream.
  route_types = 0:4

  tph_cols = as.vector(t(outer(tph_years, route_types,
                               function(y, m) paste0("tph_daytime_avg_", y, "_", m))))
  missing = setdiff(tph_cols, names(pt_frequency))
  if(length(missing) > 0){
    stop("pt_frequency has no column(s): ", paste(missing, collapse = ", "),
         ". Check tph_years against the years actually in the timetable series.")
  }

  pt_frequency$maxbus_2006_2008 = pmax(pt_frequency$tph_daytime_avg_2006_3,
                                       pt_frequency$tph_daytime_avg_2007_3,
                                       pt_frequency$tph_daytime_avg_2008_3,
                                       na.rm = TRUE)

  change_cols = character(0)
  for(y in change_years){
    col = paste0("change_bus_2008_", y)
    change = round(((pt_frequency[[paste0("tph_daytime_avg_", y, "_3")]] - pt_frequency$maxbus_2006_2008)/
                      pt_frequency$maxbus_2006_2008) * 100, 1)
    change[is.infinite(change)] = NA #Handful of cases of 0 service in 2006-2008
    pt_frequency[[col]] = change
    change_cols = c(change_cols, col)
  }

  pt_frequency = pt_frequency[,c("zone_id", change_cols, tph_cols)]

  names(pt_frequency)[1] = "LSOA21CD"

  join = dplyr::left_join(vehicle_summary, pt_frequency, by = "LSOA21CD")

  # Round
  for(i in 1:ncol(join)){
    if(inherits(join[[i]], "numeric")){
      join[[i]] = round(join[[i]], rounddp)
    }
  }

  join

}
