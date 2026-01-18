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

  bills$gas_average_bill = bills$gas_total_bill / bills$elec_meters
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


