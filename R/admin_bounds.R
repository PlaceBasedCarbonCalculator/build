read_bounds <- function(path = file.path(data_path(),"boundaries/Local_Authority_Districts_May_2023_UK_BFC_V2_179125415192200502.gpkg")){
  bounds <- sf::read_sf(path)
  bounds$geometry <- sf::st_geometry(bounds)
  bounds[ncol(bounds)-1] <- NULL
  sf::st_geometry(bounds) <- "geometry"
  bounds
}

download_boundaries <- function(path = file.path(data_path(),"boundaries")){
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    fls = list.files(path, pattern = "gpkg")
    if(length(fls) > 9){
      return(path)
    }
  }
  download.file("https://github.com/PlaceBasedCarbonCalculator/inputdata/releases/download/boundaries/Boundaries.zip",
                destfile = file.path(tempdir(),"Boundaries.zip"), mode = "wb")
  unzip(file.path(tempdir(),"Boundaries.zip"), exdir = path)
  path
}

read_bounds_la <- function(path){
  file_path = file.path(path, "Local_Authority_Districts_May_2023_UK_BFC_V2_179125415192200502.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("LAD23CD","LAD23NM")]
  bounds
}


read_bounds_wards <- function(path){
  file_path = file.path(path, "Wards_December_2022_Boundaries_GB_BFC_-6255559218859347997.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("WD22CD","WD22NM")]
  bounds
}

read_bounds_parish <- function(path){
  file_path = file.path(path, "Parishes_May_2023_Boundaries_EW_BFC_5274983877488017783.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("PAR23CD","PAR23NM")]
  bounds
}

read_bounds_westminster <- function(path){
  file_path = file.path(path, "Westminster_Parliamentary_Constituencies_Dec_2022_UK_BFC_8233416892985902015.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("PCON22CD","PCON22NM")]
  bounds
}

read_bounds_lsoa_full <- function(path){
  file_path = file.path(path, "Lower_layer_Super_Output_Areas_2021_EW_BFC_V8_4078143405809415814.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("LSOA21CD","LSOA21NM")]
  bounds <- sf::st_make_valid(bounds)
  bounds
}

read_bounds_lsoa_generalised <- function(path){
  file_path = file.path(path, "Lower_layer_Super_Output_Areas_2021_EW_BGC_V3_2542665517405622314.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("LSOA21CD","LSOA21NM")]
  bounds <- sf::st_make_valid(bounds)
  bounds
}

read_bounds_lsoa_super_generalised <- function(path){
  file_path = file.path(path, "Lower_layer_Super_Output_Areas_2021_EW_BSC_v2_8443070537763669663.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("LSOA21CD","LSOA21NM")]
  bounds <- sf::st_make_valid(bounds)
  bounds
}

read_centroids <- function(path){
  file_path = file.path(path, "LSOA_Dec_2011_PWC_in_England_and_Wales_2022_4940074699479565285.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("lsoa11cd","lsoa11nm")]
  names(bounds)[1:2] = c("LSOA11CD","LSOA11NM")
  bounds
}

read_centroids_dz11 <- function(path){
  dir.create(file.path(tempdir(),"dz"))
  unzip(file.path(path,"SG_DataZoneCent_2011.zip"), exdir = file.path(tempdir(),"dz"))
  file_path = file.path(tempdir(),"dz", "SG_DataZone_Cent_2011.shp")
  cents <- sf::read_sf(file_path)
  unlink(file.path(tempdir(),"dz"), recursive = TRUE)
  cents <- cents[,c("DataZone","Name")]
  names(cents)[1:2] = c("LSOA11CD","LSOA11NM")
  cents
}

read_centroids_oa21 <- function(path){
  file_path = file.path(path, "Output_Areas_2021_PWC_V3_-4067204786746319875.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("OA21CD")]
  bounds
}

read_centroids_lsoa21 <- function(path){
  file_path = file.path(path, "LSOA_Dec_2021_PWC_for_England_and_Wales_2022_-7410472461544737417.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("LSOA21CD")]
  bounds
}

read_postcodes <- function(path){
  dir.create(file.path(tempdir(),"postcodes"))
  unzip(path, exdir = file.path(tempdir(),"postcodes"))

  zips <- list.files(file.path(tempdir(),"postcodes"),
                      recursive = TRUE, pattern = ".zip", full.names = TRUE)
  postcode_areas <- list()
  postcodes <- list()
  for(i in 1:length(zips)){
    dir.create(file.path(tempdir(),"postcodes2"))
    unzip(zips[i], exdir = file.path(tempdir(),"postcodes2"))
    fl <- list.files(file.path(tempdir(),"postcodes2"), full.names = TRUE, pattern = ".shp")
    if(length(fl) != 1){
      stop("Multiple files")
    }
    pc <- sf::read_sf(fl)
    pc <- pc[,c("POSTCODE","PC_AREA","geometry")]
    postcodes[[i]] <- pc
    rm(pc, fl)
    unlink(file.path(tempdir(),"postcodes2"), recursive = TRUE)
  }

  postcodes <- bind_sf(postcodes)
  postcodes
}

make_postcode_areas <- function(postcodes){
  postcodes <- dplyr::group_by(postcodes, PC_AREA)
  postcodes <- dplyr::summarise(postcodes)
  postcodes
}

load_LSOA_2011_2021_lookup <- function(path){
  file_path = file.path(path, "LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2).csv")
  lookup = readr::read_csv(file_path)
  lookup = lookup[,c("LSOA11CD","LSOA21CD","LSOA21NM","CHGIND","LAD22CD","LAD22NM")]
  lookup
}

load_LSOA_2001_2011_lookup <- function(path){
  file_path = file.path(path, "Lower_Layer_Super_Output_Area_(2001)_to_Lower_Layer_Super_Output_Area_(2011)_to_Local_Authority_District_(2011)_Lookup_in_England_and_Wales.csv")
  lookup = readr::read_csv(file_path)
  lookup = lookup[,c("LSOA01CD","LSOA11CD","CHGIND","LAD11CD","LAD11NM")]
  lookup
}

load_OA_LSOA_MSOA_class_2011_lookup <- function(path){
  file_path = file.path(path, "GB_OA_LSOA_MSOA_LAD_Classifications_2017.csv")
  lookup = readr::read_csv(file_path)
  lookup = lookup[,c("OA11CD","OAC11CD", "OAC11NM", "LSOA11CD", "SOAC11CD", "SOAC11NM", "MSOA11CD", "LAD17CD", "LAD17NM", "LACCD", "LACNM")]
  lookup
}

load_postcode_OA_LSOA_MSOA_class_2021_lookup <- function(path){
  dir.create(file.path(tempdir(),"lookup"))
  unzip(file.path(path,"PCD_OA21_LSOA21_MSOA21_LAD_FEB24_UK_LU.zip"), exdir = file.path(tempdir(),"lookup"))
  lookup = readr::read_csv(file.path(tempdir(),"lookup","PCD_OA21_LSOA21_MSOA21_LAD_FEB24_UK_LU.csv"))
  lookup = lookup[,c("pcds","oa21cd","lsoa21cd","msoa21cd","ladcd","ladnm")]
  lookup
}


# Bind list of SF data frames together using faster data.table::rbindlist
bind_sf = function(x, idcol = NULL) {
  if (length(x) == 0) stop("Empty list")
  geom_name = attr(x[[1]], "sf_column")
  x = data.table::rbindlist(x, use.names = FALSE, idcol = idcol)
  x[[geom_name]] = sf::st_sfc(x[[geom_name]], recompute_bbox = TRUE)
  x = sf::st_as_sf(x)
  x
}

read_bounds_lsoa11_full <- function(path){
  file_path = file.path(path, "Lower_layer_Super_Output_Areas_Dec_2011_Boundaries_Full_Clipped_BFC_EW_V3_2022_3969098746815328641.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("LSOA11CD","LSOA11NM")]
  bounds
}


read_bounds_dz11 <- function(path){
  dir.create(file.path(tempdir(),"dz"))
  unzip(file.path(path, "SG_DataZoneBdry_2011.zip"), exdir = file.path(tempdir(),"dz"))
  bounds <- sf::read_sf(file.path(file.path(tempdir(),"dz","SG_DataZone_Bdry_2011.shp")))
  unlink(file.path(tempdir(),"dz"), recursive = TRUE)
  bounds <- bounds[,c("DataZone","geometry")]
  bounds <- sf::st_make_valid(bounds)
  bounds
}
