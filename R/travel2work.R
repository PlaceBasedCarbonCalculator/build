#' Estimate commuting distance and emissions from the 2011 travel-to-work OD
#'
#' @description Reads the census 2011 method-of-travel-to-work origin-
#'   destination table (WM12EW CT0489), builds straight lines between LSOA
#'   centroids, scales length by a 1.2 circuity factor, converts commuter
#'   counts to annual km (220 working days x 1.9 trips/day DfT assumption)
#'   per mode, and applies DEFRA 2020 business-travel emissions factors.
#'   Flows to/from outside England & Wales are excluded. Used by the
#'   `travel2work` target.
#' @param path Path to the zipped OD CSV (secure data).
#' @param cents 2011 LSOA population-weighted centroids (`centroids_lsoa11`).
#' @return An sf LINESTRING data frame per OD pair with commuter counts,
#'   `km_*` and `kgco2e_*` columns per mode.
#' @keywords internal
load_travel2work = function(path, cents){
  dir.create(file.path(tempdir(),"t2w"))
  unzip(path,
        exdir = file.path(tempdir(),"t2w"))
  od <- readr::read_csv(file.path(tempdir(),"t2w","WM12EW[CT0489]_lsoa.csv"))
  unlink(file.path(tempdir(),"t2w"), recursive = TRUE)

  od <- od[,c("Area of usual residence","Area of Workplace",names(od)[grepl("AllSexes_Age16Plus",names(od))])]
  names(od) <- gsub("_AllSexes_Age16Plus","",names(od))
  names(od)[1:2] <- c("LSOA_from","LSOA_to")

  cents <- cents[,"LSOA11CD"]

  # Note commuters outside England and Wales are excluded
  od <- od[od$LSOA_from %in% cents$LSOA11CD,]
  od <- od[od$LSOA_to %in% cents$LSOA11CD,]

  od_line <- stplanr::od2line(od, cents)
  od_line$dist_km <- as.numeric(st_length(od_line)) / 1000
  od_line$dist_km <- od_line$dist_km * 1.2 # average circuity was estimated around 1.2 (Newell, 1980)

  od_line$km_Underground <- od_line$Underground * od_line$dist_km * 220 * 1.9 # DFT assumed commuting trips per year
  od_line$km_Train <- od_line$Train * od_line$dist_km * 220 * 1.9
  od_line$km_Bus <- od_line$Bus * od_line$dist_km * 220 * 1.9
  od_line$km_Taxi <- od_line$Taxi * od_line$dist_km * 220 * 1.9
  od_line$km_Motorcycle <- od_line$Motorcycle * od_line$dist_km * 220 * 1.9
  od_line$km_CarOrVan <- od_line$CarOrVan * od_line$dist_km * 220 * 1.9
  od_line$km_Passenger <- od_line$Passenger * od_line$dist_km * 220 * 1.9
  od_line$km_Bicycle <- od_line$Bicycle * od_line$dist_km * 220 * 1.9
  od_line$km_OnFoot <- od_line$OnFoot * od_line$dist_km * 220 * 1.9
  od_line$km_OtherMethod <- od_line$OtherMethod * od_line$dist_km * 220 * 1.9

  # DEFRA 2020 emissions factors for business travel
  od_line$kgco2e_Underground <- od_line$km_Underground * 0.0275
  od_line$kgco2e_Train <- od_line$km_Train * 0.03694
  od_line$kgco2e_Bus <- od_line$km_Bus * 0.10312
  od_line$kgco2e_Taxi <- od_line$km_Taxi * 0.20369
  od_line$kgco2e_Motorcycle <- od_line$km_Motorcycle * 0.11337
  od_line$kgco2e_CarOrVan <- od_line$km_CarOrVan * 0.16844  # could use local data
  od_line$kgco2e_OtherMethod <- od_line$km_OtherMethod  * 0.16844

  od_line

}
