#' Read the 2022 Scottish Data Zone lookup CSV
#'
#' @description Reads the lookup linking 2022 Data Zones to higher Scottish
#'   geographies (Intermediate Zones, councils, etc.). Used by the
#'   `lookup_DataZone_2022` target, which feeds the Scottish household
#'   clustering.
#' @param path Boundaries folder containing
#'   `DataZone2022lookup_2024-12-16.csv`.
#' @return A data frame with the full contents of the lookup CSV.
#' @keywords internal
read_datazone_lookup_2022 = function(path = "../inputdata/boundaries/"){

  lookup = readr::read_csv(file.path(path,"DataZone2022lookup_2024-12-16.csv"))

  lookup
}
