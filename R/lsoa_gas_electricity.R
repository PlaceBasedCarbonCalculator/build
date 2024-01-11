dowload_gas_electric <- function(path = file.path(data_path(),"gas_electric")){
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    fls = list.files(path, pattern = "xlsx")
    if(length(fls) > 3){
      return(path)
    }
  }

  url_elec = "https://assets.publishing.service.gov.uk/media/63a2ea3fd3bf7f375b61c12d/LSOA_domestic_elec_2010-21.xlsx"
  download.file(url_elec, file.path(path,"lsoa_elec_dom.xlsx"), mode = "wb")

  url_elec = "https://assets.publishing.service.gov.uk/media/63a2e9e58fa8f5390dfdf56b/MSOA_non-domestic_elec_2010-21.xlsx"
  download.file(url_elec, file.path(path,"msoa_elec_nondom.xlsx"), mode = "wb")

  url_gas = "https://assets.publishing.service.gov.uk/media/63a2ef098fa8f53913e8071b/LSOA_domestic_gas_2010-21.xlsx"
  download.file(url_gas, file.path(path,"lsoa_gas_dom.xlsx"), mode = "wb")

  url_gas = "https://assets.publishing.service.gov.uk/media/63a31f44e90e075870e2cf0f/MSOA_non-domestic_gas_2010-21.xlsx"
  download.file(url_gas, file.path(path,"msoa_gas_nondom.xlsx"), mode = "wb")

  return(path)

}



load_lsoa_electric <- function(path){

  elec = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(path,"lsoa_elec_dom.xlsx"),
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



load_msoa_electric_nondom <- function(path){



  elec = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(path,"msoa_elec_nondom.xlsx"),
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



load_lsoa_gas <- function(path){



  gas = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(path,"lsoa_gas_dom.xlsx"),
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



load_msoa_gas_nondom <- function(path){

  gas = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(path,"msoa_elec_nondom.xlsx"),
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






