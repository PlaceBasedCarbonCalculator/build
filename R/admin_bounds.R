#' Read a boundary GeoPackage and standardise the geometry column
#'
#' @description Reads a GeoPackage of boundaries and renames whatever the
#'   geometry column is called (e.g. "SHAPE" in ONS Open Geography downloads)
#'   to "geometry", dropping the original. Shared helper for the various
#'   `read_bounds_*` / `read_centroids_*` functions in this file.
#' @param path Path to a GeoPackage file.
#' @return An sf data frame with its geometry column named "geometry".
#' @keywords internal
read_bounds <- function(path = file.path(data_path(),"boundaries/Local_Authority_Districts_May_2023_UK_BFC_V2_179125415192200502.gpkg")){
  bounds <- sf::read_sf(path)
  bounds$geometry <- sf::st_geometry(bounds)
  bounds[ncol(bounds)-1] <- NULL
  sf::st_geometry(bounds) <- "geometry"
  bounds
}

#' Read a zipped shapefile of boundaries
#'
#' @description Unzips a boundary shapefile to a temp folder, reads it, and
#'   drops the ONS bookkeeping columns (BNG_E, BNG_N, LONG, LAT, GlobalID).
#'   Errors if the zip contains more than one .shp file.
#' @param path Path to a zip file containing exactly one shapefile.
#' @return An sf data frame of boundaries.
#' @keywords internal
read_bounds_shp <- function(path = file.path("../inputdata/","boundaries/LAD_MAY_2025_UK_BFC_V2_1170922526770375649.zip")){
  dir.create(file.path(tempdir(),"bounds"))
  unzip(path, exdir = file.path(tempdir(),"bounds"))
  fls = list.files(file.path(tempdir(),"bounds"), pattern = ".shp$", full.names = TRUE)
  if(length(fls) > 1){
    stop("muliple shape files in ",path)
  }
  bounds <- sf::read_sf(fls)
  unlink(file.path(tempdir(),"bounds"), recursive = TRUE)
  bounds <- bounds[,!names(bounds) %in% c("NG_E","BNG_N","BNG_E","LONG","LAT","GlobalID")]
  bounds
}


#' Download the bundled boundaries release from GitHub
#'
#' @description Downloads and unzips the `Boundaries.zip` release from the
#'   PlaceBasedCarbonCalculator/inputdata GitHub repo into `path`. Skipped if
#'   the folder already contains more than 9 GeoPackages. This is the
#'   `dl_boundaries` target that almost all boundary/lookup targets depend on.
#' @param path Folder to store the boundary files in; created if missing.
#' @return `path`, for use as the input to the `read_bounds_*` functions.
#' @keywords internal
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
                destfile = file.path(tempdir(),"Boundaries.zip"), mode = "wb", )
  unzip(file.path(tempdir(),"Boundaries.zip"), exdir = path)
  path
}

#' Read Local Authority District boundaries (May 2025, UK, full clipped)
#'
#' @description Reads the LAD May 2025 BFC boundaries from the downloaded
#'   boundaries folder. Used by the `bounds_la` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf data frame with `LAD25CD` and `LAD25NM`.
#' @keywords internal
read_bounds_la <- function(path){
  file_path = file.path(path, "LAD_MAY_2025_UK_BFC_V2_1170922526770375649.zip")
  bounds <- read_bounds_shp(file_path)
  bounds <- bounds[,c("LAD25CD","LAD25NM")]
  bounds
}


#' Read electoral ward boundaries (May 2025, UK, full clipped)
#'
#' @description Reads the ward May 2025 BFC boundaries from the downloaded
#'   boundaries folder. Used by the `bounds_wards` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf data frame with `WD25CD` and `WD25NM`.
#' @keywords internal
read_bounds_wards <- function(path){
  file_path = file.path(path, "Wards_(May_2025)_Boundaries_UK_BFC_(V2).zip")
  bounds <- read_bounds_shp(file_path)
  bounds <- bounds[,c("WD25CD","WD25NM")]
  bounds
}

#' Read parish boundaries (May 2023, England & Wales, full clipped)
#'
#' @description Reads the parish May 2023 BFC boundaries from the downloaded
#'   boundaries folder. Used by the `bounds_parish` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf data frame with `PAR23CD` and `PAR23NM`.
#' @keywords internal
read_bounds_parish <- function(path){
  file_path = file.path(path, "Parishes_May_2023_Boundaries_EW_BFC_5274983877488017783.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("PAR23CD","PAR23NM")]
  bounds
}

#' Read Westminster parliamentary constituency boundaries (July 2024, UK)
#'
#' @description Reads the constituency July 2024 BFC boundaries from the
#'   downloaded boundaries folder. Used by the `bounds_westminster` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf data frame with `PCON24CD` and `PCON24NM`.
#' @keywords internal
read_bounds_westminster <- function(path){
  file_path = file.path(path, "Westminster_Parliamentary_Constituencies_July_2024_Boundaries_UK_BFC_-6236279356162627018.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("PCON24CD","PCON24NM")]
  bounds
}

#' Read 2021 LSOA boundaries, full resolution (BFC)
#'
#' @description Reads the full-clipped 2021 LSOA boundaries for England &
#'   Wales and makes the geometries valid. Used by the `bounds_lsoa21_full`
#'   target and combined with Scottish Data Zones in `combine_lsoa_bounds()`.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf data frame with `LSOA21CD` and `LSOA21NM`.
#' @keywords internal
read_bounds_lsoa_full <- function(path){
  file_path = file.path(path, "Lower_layer_Super_Output_Areas_2021_EW_BFC_V8_4078143405809415814.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("LSOA21CD","LSOA21NM")]
  bounds <- sf::st_make_valid(bounds)
  bounds
}

#' Read 2021 LSOA boundaries, generalised (BGC, 20m)
#'
#' @description Reads the generalised 2021 LSOA boundaries for England &
#'   Wales and makes the geometries valid. Used by the
#'   `bounds_lsoa21_generalised` target for mid-zoom map tiles.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf data frame with `LSOA21CD` and `LSOA21NM`.
#' @keywords internal
read_bounds_lsoa_generalised <- function(path){
  file_path = file.path(path, "Lower_layer_Super_Output_Areas_2021_EW_BGC_V3_2542665517405622314.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("LSOA21CD","LSOA21NM")]
  bounds <- sf::st_make_valid(bounds)
  bounds
}

#' Read 2021 LSOA boundaries, super-generalised (BSC, 200m)
#'
#' @description Reads the super-generalised 2021 LSOA boundaries for England &
#'   Wales and makes the geometries valid. Used by the
#'   `bounds_lsoa21_super_generalised` target for low-zoom map tiles.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf data frame with `LSOA21CD` and `LSOA21NM`.
#' @keywords internal
read_bounds_lsoa_super_generalised <- function(path){
  file_path = file.path(path, "Lower_layer_Super_Output_Areas_2021_EW_BSC_v2_8443070537763669663.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("LSOA21CD","LSOA21NM")]
  bounds <- sf::st_make_valid(bounds)
  bounds
}

#' Read 2011 LSOA population-weighted centroids (England & Wales)
#'
#' @description Reads the 2011 LSOA population-weighted centroids from the
#'   downloaded boundaries folder. Used by the `centroids_lsoa11` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf POINT data frame with `LSOA11CD` and `LSOA11NM`.
#' @keywords internal
read_centroids <- function(path){
  file_path = file.path(path, "LSOA_Dec_2011_PWC_in_England_and_Wales_2022_4940074699479565285.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("lsoa11cd","lsoa11nm")]
  names(bounds)[1:2] = c("LSOA11CD","LSOA11NM")
  bounds
}

#' Read 2011 Scottish Data Zone centroids
#'
#' @description Reads the 2011 Data Zone centroids from a zipped shapefile in
#'   the boundaries folder. Columns are renamed to `LSOA11CD`/`LSOA11NM` so
#'   Scottish zones can be treated interchangeably with E&W LSOAs downstream.
#'   Used by the `centroids_dz11` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf POINT data frame with `LSOA11CD` and `LSOA11NM` (Data Zone
#'   codes/names).
#' @keywords internal
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

#' Read 2022 Scottish Data Zone centroids
#'
#' @description Reads the 2022 Data Zone centroids from a zipped shapefile in
#'   the boundaries folder. Columns are renamed to `LSOA21CD`/`LSOA21NM` so
#'   Scottish zones can be treated interchangeably with E&W 2021 LSOAs
#'   downstream. Used by the `centroids_dz22` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf POINT data frame with `LSOA21CD` and `LSOA21NM` (Data Zone
#'   codes/names).
#' @keywords internal
read_centroids_dz22 <- function(path){
  dir.create(file.path(tempdir(),"dz"))
  unzip(file.path(path,"SG_DataZoneCent_2022.zip"), exdir = file.path(tempdir(),"dz"))
  file_path = file.path(tempdir(),"dz", "SG_DataZoneCent_2022.shp")
  cents <- sf::read_sf(file_path)
  unlink(file.path(tempdir(),"dz"), recursive = TRUE)
  cents <- cents[,c("DZCode","DZName")]
  names(cents)[1:2] = c("LSOA21CD","LSOA21NM")
  cents
}

#' Read 2021 Output Area population-weighted centroids (England & Wales)
#'
#' @description Reads the 2021 OA population-weighted centroids from the
#'   downloaded boundaries folder. Used by the `centroids_oa21` target and the
#'   accessibility analysis.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf POINT data frame with `OA21CD`.
#' @keywords internal
read_centroids_oa21 <- function(path){
  file_path = file.path(path, "Output_Areas_2021_PWC_V3_-4067204786746319875.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("OA21CD")]
  bounds
}

#' Read 2021 LSOA population-weighted centroids (England & Wales)
#'
#' @description Reads the 2021 LSOA population-weighted centroids from the
#'   downloaded boundaries folder. Used by the `centroids_lsoa21` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf POINT data frame with `LSOA21CD`.
#' @keywords internal
read_centroids_lsoa21 <- function(path){
  file_path = file.path(path, "LSOA_Dec_2021_PWC_for_England_and_Wales_2022_-7410472461544737417.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("LSOA21CD")]
  bounds
}

#' Read postcode polygon boundaries from a nested zip
#'
#' @description Unzips the (secure) postcode polygons archive, which contains
#'   one zipped shapefile per postcode area, reads each and binds them into a
#'   single layer via `bind_sf()`. Used by the `bounds_postcodes_2015/2020/2024`
#'   targets.
#' @param path Path to the outer zip of postcode polygon shapefiles.
#' @return An sf data frame with `POSTCODE`, `PC_AREA` and geometry for every
#'   unit postcode.
#' @keywords internal
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

#' Dissolve unit postcodes into postcode areas
#'
#' @description Unions the unit postcode polygons by postcode area (e.g. "LS",
#'   "M") to give one polygon per area. Used by the `bounds_postcode_area`
#'   target.
#' @param postcodes sf data frame from `read_postcodes()` with a `PC_AREA`
#'   column.
#' @return An sf data frame with one dissolved polygon per `PC_AREA`.
#' @keywords internal
make_postcode_areas <- function(postcodes){
  postcodes <- dplyr::group_by(postcodes, PC_AREA)
  postcodes <- dplyr::summarise(postcodes)
  postcodes
}

#' Read OS Code-Point postcode centroids
#'
#' @description Unzips the OS Code-Point Open GeoPackage and reads the unit
#'   postcode points. Used by the `postcode_points` target.
#' @param path Path to the `codepo_gpkg_gb.zip` archive.
#' @return An sf POINT data frame with `postcode` and geometry.
#' @keywords internal
read_postcode_points = function(path = "D:/OneDrive - University of Leeds/Data/Postcodes/codepo_20251101/codepo_gpkg_gb.zip"){
  dir.create(file.path(tempdir(),"postcodes"))
  unzip(path, exdir = file.path(tempdir(),"postcodes"))
  points = sf::st_read(file.path(tempdir(),"postcodes","data/codepo_gb.gpkg"))
  unlink(file.path(tempdir(),"postcodes"), recursive = TRUE)
  points = points[,c("postcode","geometry")]
  points
}

#' Load the ONS 2011-to-2021 LSOA lookup
#'
#' @description Reads the ONS best-fit lookup between 2011 and 2021 LSOAs for
#'   England & Wales, including the change indicator (`CHGIND`: U unchanged,
#'   S split, M merged, X fragmented). Used by the `lookup_lsoa_2011_21`
#'   target, which underpins all 2011-to-2021 data conversions.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return A data frame with `LSOA11CD`, `LSOA21CD`, `LSOA21NM`, `CHGIND`,
#'   `LAD22CD`, `LAD22NM`.
#' @keywords internal
load_LSOA_2011_2021_lookup <- function(path){
  file_path = file.path(path, "LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2).csv")
  lookup = readr::read_csv(file_path)
  lookup = lookup[,c("LSOA11CD","LSOA21CD","LSOA21NM","CHGIND","LAD22CD","LAD22NM")]
  lookup
}

#' Load the ONS 2001-to-2011 LSOA lookup
#'
#' @description Reads the ONS lookup between 2001 and 2011 LSOAs for England &
#'   Wales, including the change indicator. Used by the `lookup_lsoa_2001_11`
#'   target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return A data frame with `LSOA01CD`, `LSOA11CD`, `CHGIND`, `LAD11CD`,
#'   `LAD11NM`.
#' @keywords internal
load_LSOA_2001_2011_lookup <- function(path){
  file_path = file.path(path, "Lower_Layer_Super_Output_Area_(2001)_to_Lower_Layer_Super_Output_Area_(2011)_to_Local_Authority_District_(2011)_Lookup_in_England_and_Wales.csv")
  lookup = readr::read_csv(file_path)
  lookup = lookup[,c("LSOA01CD","LSOA11CD","CHGIND","LAD11CD","LAD11NM")]
  lookup
}

#' Load the GB 2011 OA/LSOA/MSOA lookup with area classifications
#'
#' @description Reads the 2017 GB lookup linking 2011 Output Areas to LSOAs,
#'   MSOAs and LADs, including the 2011 Output Area Classification (OAC11) and
#'   LSOA classification (SOAC11) codes. Used by the
#'   `lookup_OA_LSOA_MSOA_classifications` target and the OAC-related targets.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return A data frame with OA/LSOA/MSOA/LAD codes and OAC/SOAC
#'   classification codes and names.
#' @keywords internal
load_OA_LSOA_MSOA_class_2011_lookup <- function(path){
  file_path = file.path(path, "GB_OA_LSOA_MSOA_LAD_Classifications_2017.csv")
  lookup = readr::read_csv(file_path)
  lookup = lookup[,c("OA11CD","OAC11CD", "OAC11NM", "LSOA11CD", "SOAC11CD", "SOAC11NM", "MSOA11CD", "LAD17CD", "LAD17NM", "LACCD", "LACNM")]
  lookup
}

#' Load the England & Wales 2021 OA/LSOA/MSOA/LAD lookup
#'
#' @description Reads the 2021 census geography lookup linking Output Areas to
#'   LSOAs, MSOAs and LADs. Used by the `lookup_OA_LSOA_MSOA_2021` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return A data frame with `OA21CD`, `LSOA21CD`, `MSOA21CD`, `LAD22CD`
#'   and `LAD22NM`.
#' @keywords internal
load_OA_LSOA_MSOA_2021_lookup <- function(path){
  file_path = file.path(path, "OA_LSOA_MSOA_LAD_2021_Lookup_EW_v3.csv")
  lookup = readr::read_csv(file_path)
  lookup = lookup[,c("OA21CD", "LSOA21CD", "MSOA21CD", "LAD22CD", "LAD22NM")]
  lookup
}

#' Load the UK postcode to 2021 census geography lookup
#'
#' @description Unzips and reads the ONS February 2024 postcode-to-OA/LSOA/
#'   MSOA/LAD lookup for the UK. Used by the `lookup_postcode_OA_LSOA_MSOA_2021`
#'   target (e.g. for geocoding EPC and house-price records).
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return A data frame with `pcds`, `oa21cd`, `lsoa21cd`, `msoa21cd`,
#'   `ladcd`, `ladnm`.
#' @keywords internal
load_postcode_OA_LSOA_MSOA_class_2021_lookup <- function(path){
  dir.create(file.path(tempdir(),"lookup"))
  unzip(file.path(path,"PCD_OA21_LSOA21_MSOA21_LAD_FEB24_UK_LU.zip"), exdir = file.path(tempdir(),"lookup"))
  lookup = readr::read_csv(file.path(tempdir(),"lookup","PCD_OA21_LSOA21_MSOA21_LAD_FEB24_UK_LU.csv"))
  lookup = lookup[,c("pcds","oa21cd","lsoa21cd","msoa21cd","ladcd","ladnm")]
  lookup
}

#' Load the ONS 2011-to-2021 MSOA best-fit lookup
#'
#' @description Reads the ONS best-fit lookup between 2011 and 2021 MSOAs for
#'   England & Wales. Used by the `lookup_MSOA_2011_21` target (income
#'   downscaling).
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return A data frame with all columns of the ONS lookup CSV.
#' @keywords internal
load_MSOA_2011_2021_lookup <- function(path){
  file_path = file.path(path, "MSOA_(2011)_to_MSOA_(2021)_to_Local_Authority_District_(2022)_Best_Fit_Lookup_for_EW_(V2).csv")
  lookup = readr::read_csv(file_path)
  lookup
}



#' Fast row-bind a list of sf data frames
#'
#' @description Binds a list of sf data frames using
#'   `data.table::rbindlist()`, which is much faster than `rbind()` for many
#'   parts, then restores the sfc geometry column and bbox. All elements must
#'   share the same column order (`use.names = FALSE`).
#' @param x List of sf data frames with identical columns.
#' @param idcol Optional name for an ID column recording which list element
#'   each row came from (passed to `rbindlist`).
#' @return A single sf data frame.
#' @keywords internal
bind_sf = function(x, idcol = NULL) {
  if (length(x) == 0) stop("Empty list")
  geom_name = attr(x[[1]], "sf_column")
  x = data.table::rbindlist(x, use.names = FALSE, idcol = idcol)
  x[[geom_name]] = sf::st_sfc(x[[geom_name]], recompute_bbox = TRUE)
  x = sf::st_as_sf(x)
  x
}

#' Read 2011 LSOA boundaries, full resolution (BFC)
#'
#' @description Reads the full-clipped 2011 LSOA boundaries for England &
#'   Wales. Used by the `bounds_lsoa11_full` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf data frame with `LSOA11CD` and `LSOA11NM`.
#' @keywords internal
read_bounds_lsoa11_full <- function(path){
  file_path = file.path(path, "Lower_layer_Super_Output_Areas_Dec_2011_Boundaries_Full_Clipped_BFC_EW_V3_2022_3969098746815328641.gpkg")
  bounds <- read_bounds(file_path)
  bounds <- bounds[,c("LSOA11CD","LSOA11NM")]
  bounds
}


#' Read 2011 Scottish Data Zone boundaries
#'
#' @description Unzips and reads the 2011 Data Zone boundary shapefile and
#'   makes the geometries valid. Used by the `bounds_dz11` target and the
#'   2011-to-2022 Data Zone lookup.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf data frame with `DataZone` and geometry.
#' @keywords internal
read_bounds_dz11 <- function(path){
  dir.create(file.path(tempdir(),"dz"))
  unzip(file.path(path, "SG_DataZoneBdry_2011.zip"), exdir = file.path(tempdir(),"dz"))
  bounds <- sf::read_sf(file.path(file.path(tempdir(),"dz","SG_DataZone_Bdry_2011.shp")))
  unlink(file.path(tempdir(),"dz"), recursive = TRUE)
  bounds <- bounds[,c("DataZone","geometry")]
  bounds <- sf::st_make_valid(bounds)
  bounds
}


#' Assign each LSOA/Data Zone to its administrative areas
#'
#' @description Spatially joins the GB LSOA/DZ population-weighted centroids
#'   to ward, parish, Westminster constituency and local authority boundaries,
#'   giving a lookup of which administrative areas each zone sits in. Zones
#'   outside any parish are labelled "Unparished". Used by the `lsoa_admin`
#'   target, which feeds the LSOA overview JSONs, LA summaries and house price
#'   extrapolation.
#' @param bounds_wards Ward boundaries (`bounds_wards` target).
#' @param bounds_parish Parish boundaries (`bounds_parish` target).
#' @param bounds_westminster Constituency boundaries (`bounds_westminster`).
#' @param bounds_la Local authority boundaries (`bounds_la` target).
#' @param centroids_lsoa21 E&W 2021 LSOA population-weighted centroids.
#' @param centroids_dz22 Scottish 2022 Data Zone centroids (with columns
#'   renamed to LSOA21CD/LSOA21NM).
#' @return A data frame (geometry dropped) with one row per zone and the
#'   codes/names of the administrative areas containing its centroid.
#' @keywords internal
lsoa_admin_summary = function(bounds_wards, bounds_parish, bounds_westminster, bounds_la, centroids_lsoa21,
                            centroids_dz22){

  centroids_dz22$LSOA21NM = NULL
  cents = rbind(centroids_lsoa21, centroids_dz22)
  #cents = sf::st_point_on_surface(bounds_lsoa_GB_full)

  cents = sf::st_join(cents, bounds_wards)
  cents = sf::st_join(cents, bounds_parish)
  cents = sf::st_join(cents, bounds_westminster)
  cents = sf::st_join(cents, bounds_la)
  #cents = cents[,c("LSOA21CD","WD25NM","PAR23NM","PCON24NM","LAD25NM","LAD25CD")]
  cents$PAR23NM[is.na(cents$PAR23NM)] = "Unparished"
  cents$PAR23CD[is.na(cents$PAR23CD)] = "Unparished"

  cents = sf::st_drop_geometry(cents)
  cents

}


#' Read 2011 Scottish Output Area population-weighted centroids
#'
#' @description Unzips and reads the 2011 Scottish OA population-weighted
#'   centroids. Used by the `centroids_oa11_scotland` target (OAC lookups for
#'   Scotland).
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf POINT data frame with `OA11`.
#' @keywords internal
read_cents_scotland_oa11 = function(path = "../inputdata/boundaries/"){
  dir.create(file.path(tempdir(),"dz"))
  unzip(file.path(path, "Scotland-output-area-2011-pwc.zip"), exdir = file.path(tempdir(),"dz"))
  bounds <- sf::read_sf(file.path(file.path(tempdir(),"dz","OutputArea2011_PWC.shp")))
  unlink(file.path(tempdir(),"dz"), recursive = TRUE)
  bounds = bounds[,c("code")]
  names(bounds)[1] = "OA11"
  bounds

}

#' Read 2001 Scottish Output Area household-weighted centroids
#'
#' @description Unzips and reads the 2001 Scottish OA household-weighted
#'   centroids. Used by the `centroids_oa01_scotland` target (2001 OAC lookups
#'   for Scotland).
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf POINT data frame with `OA01` and `NRSoldOutp`.
#' @keywords internal
read_cents_scotland_oa01 = function(path = "../inputdata/boundaries/"){
  dir.create(file.path(tempdir(),"dz"))
  unzip(file.path(path, "Scotland-OutputArea2001_HWC.zip"), exdir = file.path(tempdir(),"dz"))
  bounds <- sf::read_sf(file.path(file.path(tempdir(),"dz","OutputArea2001_HWC.shp")))
  unlink(file.path(tempdir(),"dz"), recursive = TRUE)
  bounds = bounds[,c("OutputArea","NRSoldOutp")]
  names(bounds)[1] = "OA01"
  bounds

}

