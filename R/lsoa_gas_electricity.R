dowload_lsoa_electric <- function(){

  url_elec = "https://assets.publishing.service.gov.uk/media/63a2ea3fd3bf7f375b61c12d/LSOA_domestic_elec_2010-21.xlsx"

  dir.create(file.path(tempdir(),"gaselec"))
  download.file(url_elec, file.path(tempdir(),"gaselec","lsoa_elec.xlsx"), mode = "wb")

  elec = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(tempdir(),"gaselec","lsoa_elec.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]
    names(sub) <- c("LAcode","LAname",
                    "MSOA","MSOAname",
                    "LSOA","LSOAname",
                    "metres","total_elec_kwh",
                    "mean_elec_kwh","median_elec_kwh")

    sub$year <- i
    elec[[i]] <- sub
  }
  elec = dplyr::bind_rows(elec)
  unlink(file.path(tempdir(),"gaselec"), recursive = TRUE)

  elec


}



dowload_msoa_electric_nondom <- function(){

  url_elec = "https://assets.publishing.service.gov.uk/media/63a2e9e58fa8f5390dfdf56b/MSOA_non-domestic_elec_2010-21.xlsx"

  dir.create(file.path(tempdir(),"gaselec"))
  download.file(url_elec, file.path(tempdir(),"gaselec","msoa_elec.xlsx"), mode = "wb")

  elec = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(tempdir(),"gaselec","msoa_elec.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]
    names(sub) <- c("LAcode","LAname",
                    "MSOA","MSOAname",
                    "metre_type",
                    "metres","total_elec_kwh",
                    "mean_elec_kwh","median_elec_kwh")

    sub$year <- i
    elec[[i]] <- sub
  }
  elec = dplyr::bind_rows(elec)
  unlink(file.path(tempdir(),"gaselec"), recursive = TRUE)

  elec

}



dowload_lsoa_gas <- function(){

  url_gas = "https://assets.publishing.service.gov.uk/media/63a2ef098fa8f53913e8071b/LSOA_domestic_gas_2010-21.xlsx"

  dir.create(file.path(tempdir(),"gaselec"))
  download.file(url_gas, file.path(tempdir(),"gaselec","lsoa_gas.xlsx"), mode = "wb")

  gas = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(tempdir(),"gaselec","lsoa_gas.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]
    names(sub) <- c("LAcode","LAname",
                    "MSOA","MSOAname",
                    "LSOA","LSOAname",
                    "metres","total_gas_kwh",
                    "mean_gas_kwh","median_gas_kwh")

    sub$year <- i
    gas[[i]] <- sub
  }
  gas = dplyr::bind_rows(gas)
  unlink(file.path(tempdir(),"gaselec"), recursive = TRUE)

  gas


}



dowload_msoa_gas_nondom <- function(){

  url_gas = "https://assets.publishing.service.gov.uk/media/63a31f44e90e075870e2cf0f/MSOA_non-domestic_gas_2010-21.xlsx"

  dir.create(file.path(tempdir(),"gaselec"))
  download.file(url_gas, file.path(tempdir(),"gaselec","msoa_gas.xlsx"), mode = "wb")

  gas = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(tempdir(),"gaselec","msoa_elec.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]
    names(sub) <- c("LAcode","LAname",
                    "MSOA","MSOAname",
                    "metre_type",
                    "metres","total_gas_kwh",
                    "mean_gas_kwh","median_gas_kwh")

    sub$year <- i
    gas[[i]] <- sub
  }
  gas = dplyr::bind_rows(gas)
  unlink(file.path(tempdir(),"gaselec"), recursive = TRUE)

  gas

}






