# The historical UPRN archive (osopenuprn_2020_2025_all.zip) is loaded by the
# LandOwnership repo, which owns all UPRN / address work as of July 2026 - see
# LandOwnership/pipeline/R/uprn_historical.R. The single-snapshot BNG loader
# below is a different product and stays here: `uprn_bng` feeds the Data Zone
# 2011-to-2022 lookup and, through it, the whole Scottish population chain.

#' Load OS Open UPRN points in British National Grid
#'
#' @description Unzips and reads the June 2024 OS Open UPRN CSV, keeping the
#'   easting/northing coordinates (EPSG:27700). Used by the `uprn_bng` target,
#'   which feeds the Data Zone 2011-to-2022 lookup. Note the full dataset is
#'   ~40M points, so this is memory-hungry.
#' @param path Folder containing `osopenuprn_202406_csv.zip`.
#' @return An sf POINT data frame (EPSG:27700) with `UPRN`.
#' @keywords internal
load_uprn_27700 = function(path = file.path(parameters$path_data,"os_uprn")) {

  dir.create(file.path(tempdir(),"uprn"))
  unzip(file.path(path,"osopenuprn_202406_csv.zip"), exdir = file.path(tempdir(),"uprn"))

  uprn = readr::read_csv(file.path(tempdir(),"uprn","osopenuprn_202406.csv"))
  unlink(file.path(tempdir(),"uprn"), recursive = TRUE)
  uprn = uprn[,c("UPRN","X_COORDINATE","Y_COORDINATE")]
  uprn = sf::st_as_sf(uprn, coords = c("X_COORDINATE","Y_COORDINATE"), crs = 27700)
  uprn
}
