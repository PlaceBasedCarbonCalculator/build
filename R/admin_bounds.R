read_bounds <- function(path = file.path(data_path(),"boundaries/Local_Authority_Districts_May_2023_UK_BFC_V2_179125415192200502.gpkg")){
  bounds <- sf::read_sf(path)
  bounds$geometry <- sf::st_geometry(bounds)
  bounds[ncol(bounds)-1] <- NULL
  sf::st_geometry(bounds) <- "geometry"
  bounds
}


read_bounds_la <- function(path = file.path(data_path(),"boundaries/Local_Authority_Districts_May_2023_UK_BFC_V2_179125415192200502.gpkg")){
  bounds <- read_bounds(path)
  bounds <- bounds[,c("LAD23CD","LAD23NM")]
  bounds
}


read_bounds_wards <- function(path = file.path(data_path(),"boundaries/Wards_December_2022_Boundaries_GB_BFC_-6255559218859347997.gpkg")){
  bounds <- read_bounds(path)
  bounds <- bounds[,c("WD22CD","WD22NM")]
  bounds
}

read_bounds_parish <- function(path = file.path(data_path(),"boundaries/Parishes_May_2023_Boundaries_EW_BFC_5274983877488017783.gpkg")){
  bounds <- read_bounds(path)
  bounds <- bounds[,c("PAR23CD","PAR23NM")]
  bounds
}

read_bounds_westminster <- function(path = file.path(data_path(),"boundaries/Westminster_Parliamentary_Constituencies_Dec_2022_UK_BFC_8233416892985902015.gpkg")){
  bounds <- read_bounds(path)
  bounds <- bounds[,c("PCON22CD","PCON22NM")]
  bounds
}

read_bounds_lsoa_full <- function(path = file.path(data_path(),"boundaries/Lower_layer_Super_Output_Areas_2021_EW_BFC_V8_4078143405809415814.gpkg")){
  bounds <- read_bounds(path)
  bounds <- bounds[,c("LSOA21CD","LSOA21NM")]
  bounds
}

read_bounds_lsoa_generalised <- function(path = file.path(data_path(),"boundaries/Lower_layer_Super_Output_Areas_2021_EW_BGC_V3_2542665517405622314.gpkg")){
  bounds <- read_bounds(path)
  bounds <- bounds[,c("LSOA21CD","LSOA21NM")]
  bounds
}

read_bounds_lsoa_super_generalised <- function(path = file.path(data_path(),"boundaries/Lower_layer_Super_Output_Areas_2021_EW_BSC_v2_8443070537763669663.gpkg")){
  bounds <- read_bounds(path)
  bounds <- bounds[,c("LSOA21CD","LSOA21NM")]
  bounds
}
