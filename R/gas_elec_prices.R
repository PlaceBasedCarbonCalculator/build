#' Load DESNZ regional gas and electricity prices
#'
#' @description Reads the DESNZ quarterly energy prices tables 2.3.4 (gas)
#'   and 2.2.4 (electricity): average variable unit price and fixed cost per
#'   region (gas LDZ / electricity PES areas), harmonising the Scottish region
#'   names. Used by the `prices_gas_electric` target, which feeds
#'   `estimate_gas_electric_bills()`.
#' @param path Folder containing `table_234.xlsx` and `table_224.xlsx`.
#' @return A data frame with `year`, `region`, `gas_price_kwh`,
#'   `gas_price_fixed`, `elec_price_kwh`, `elec_price_fixed`.
#' @keywords internal
load_gas_elec_prices = function(path = "../inputdata/gas_electric/prices"){

  gas = readxl::read_excel(file.path(path,"table_234.xlsx"), sheet = "2.3.4")
  elec = readxl::read_excel(file.path(path,"table_224.xlsx"), sheet = "2.2.4")

  names(gas) = as.character(gas[10,])
  gas = gas[11:nrow(gas),]
  gas = gas[,c("Year","LDZ area","Overall: Average variable unit price (£/kWh)[Note 1]","Overall: Average fixed cost (£/year)[Note 2]")]
  names(gas) = c("year","region","gas_price_kwh","gas_price_fixed")

  names(elec) = as.character(elec[12,])
  elec = elec[13:nrow(elec),]
  elec = elec[,c("Year","PES area","Overall: Average variable unit price (£/kWh)[Note 2]","Overall: Average fixed cost (£/year)[Note 3]")]
  names(elec) = c("year","region","elec_price_kwh","elec_price_fixed")

  gas$region[gas$region == "South Scotland"] = "Southern Scotland"
  gas$region[gas$region == "North Scotland"] = "Northern Scotland"

  elec$region[elec$region == "South Scotland"] = "Southern Scotland"
  elec$region[elec$region == "North Scotland"] = "Northern Scotland"

  gas$gas_price_kwh = as.numeric(gas$gas_price_kwh)
  gas$gas_price_fixed = as.numeric(gas$gas_price_fixed)

  elec$elec_price_kwh = as.numeric(elec$elec_price_kwh)
  elec$elec_price_fixed = as.numeric(elec$elec_price_fixed)


  prices = dplyr::full_join(gas, elec, by = c("region","year"))

  prices

}

#DNO or PES Areas
#https://www.neso.energy/data-portal/gis-boundaries-gb-dno-license-areas

#' Load GB electricity DNO licence area boundaries
#'
#' @description Unzips and reads the NESO DNO licence area shapefile and adds
#'   a `region` column renamed to match the DESNZ price-table region names.
#'   Used by the `bounds_dno` target.
#' @param path Folder containing the DNO licence areas zip.
#' @return An sf data frame of the 14 DNO areas with a `region` column.
#' @keywords internal
load_dno_areas = function(path = "../inputdata/gas_electric/"){
  dir.create(file.path(tempdir(),"dno"))
  unzip(file.path(path,"gb-dno-license-areas-20240503-as-esri-shape-file.zip"), exdir = file.path(tempdir(),"dno"))

  dno = sf::read_sf(file.path(tempdir(),"dno","GB DNO License Areas 20240503 as ESRI Shape File.shp"))

  unlink(file.path(tempdir(),"dno"), recursive = TRUE)

  # Match DESNZ names
  dno$region = gsub(" England","",dno$Area)
  dno$region[dno$region == "East"] = "Eastern"
  dno$region[dno$region == "North Scotland"] = "Northern Scotland"
  dno$region[dno$region == "South and Central Scotland"] = "Southern Scotland"
  dno$region[dno$region == "North Wales, Merseyside and Cheshire"] = "Merseyside & North Wales"

  dno

}

#' Assign every GB zone to its DNO price region
#'
#' @description Spatially joins LSOA and Data Zone centroids to the DNO
#'   licence areas so regional energy prices can be applied per zone. One
#'   coastal LSOA (Barrow-in-Furness) misses the polygons and is manually
#'   assigned to "North West". Used by the `lsoa_dno_lookup_GB` target.
#' @param dno DNO areas with `region` (`bounds_dno` target).
#' @param centroids_lsoa21 E&W LSOA centroids (`centroids_lsoa21` target).
#' @param centroids_dz22 Scottish DZ centroids (`centroids_dz22` target).
#' @return A data frame with `LSOA21CD` and `region`.
#' @keywords internal
make_lsoa_to_dno_lookup = function(dno, centroids_lsoa21, centroids_dz22){
  dno = dno[,c("region")]

  centroids_lsoa21 = sf::st_join(centroids_lsoa21, dno)
  centroids_dz22 = sf::st_join(centroids_dz22, dno)

  # Error for Barrow in Furness
  centroids_lsoa21$region[is.na(centroids_lsoa21$region)] = "North West"

  centroids_lsoa21 = sf::st_drop_geometry(centroids_lsoa21)
  centroids_dz22 = sf::st_drop_geometry(centroids_dz22)
  centroids_dz22 = centroids_dz22[,c("LSOA21CD","region")]

  lsoa_dno_lookup_GB = rbind(centroids_lsoa21, centroids_dz22)

  lsoa_dno_lookup_GB

}


#' Estimate average household gas and electricity bills per LSOA
#'
#' @description Combines LSOA gas/electricity consumption with regional DESNZ
#'   prices (via the DNO lookup): total bill = meters x fixed cost + kWh x
#'   unit price, then averaged per meter. Used by the `bills_gas_electric`
#'   target, feeding the retrofit map data and energy JSONs.
#' @param domestic_gas LSOA gas consumption (`domestic_gas` target).
#' @param domestic_electricity LSOA electricity consumption
#'   (`domestic_electricity` target).
#' @param prices_gas_electric Regional prices (`prices_gas_electric` target).
#' @param lsoa_dno_lookup_GB Zone-to-region lookup (`lsoa_dno_lookup_GB`).
#' @return A data frame per LSOA-year with unit prices and `gas_average_bill`,
#'   `elec_average_bill` and `energy_average_bill` (pounds).
#' @keywords internal
estimate_gas_electric_bills = function(domestic_gas, domestic_electricity, prices_gas_electric, lsoa_dno_lookup_GB){

  names(domestic_gas)[names(domestic_gas) == "meters"] = "gas_meters"
  names(domestic_electricity)[names(domestic_electricity) == "meters"] = "elec_meters"

  prices_gas_electric$year = as.numeric(prices_gas_electric$year)
  prices_gas_electric = prices_gas_electric[prices_gas_electric$year %in% unique(domestic_gas$year),]

  lsoa_dno_lookup_GB = dplyr::left_join(lsoa_dno_lookup_GB, prices_gas_electric, by = "region", relationship = "many-to-many")

  bills = dplyr::left_join(lsoa_dno_lookup_GB, domestic_gas, by = c("LSOA21CD","year"))
  bills = dplyr::left_join(bills, domestic_electricity, by = c("LSOA21CD","year"))

  bills$gas_fixed_bill = bills$gas_meters * bills$gas_price_fixed
  bills$elec_fixed_bill = bills$elec_meters * bills$elec_price_fixed

  bills$gas_energy_bill = bills$total_gas_kwh * bills$gas_price_kwh
  bills$elec_energy_bill = bills$total_elec_kwh * bills$elec_price_kwh

  bills$gas_total_bill = bills$gas_fixed_bill + bills$gas_energy_bill
  bills$elec_total_bill = bills$elec_fixed_bill + bills$elec_energy_bill

  bills$gas_average_bill = bills$gas_total_bill / bills$elec_meters # Average bill over all households, not just those with gas meters, to avoid biasing the average upwards in areas with low gas penetration
  bills$elec_average_bill = bills$elec_total_bill / bills$elec_meters

  bills$gas_average_bill[is.na(bills$gas_average_bill)] = 0
  bills$gas_average_bill[is.infinite(bills$gas_average_bill)] = 0

  bills$elec_average_bill[is.na(bills$elec_average_bill)] = 0
  bills$elec_average_bill[is.infinite(bills$elec_average_bill)] = 0

  bills$energy_average_bill = bills$gas_average_bill + bills$elec_average_bill

  bills = bills[,c("LSOA21CD","year","gas_price_kwh","gas_price_fixed","elec_price_kwh",
                   "elec_price_fixed","gas_average_bill","elec_average_bill","energy_average_bill")]

  bills
}


