#' Read Datazone Lookup 2022
#'
#' @description Read datazone lookup 2022 from disk into an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_datazone_lookup_2022 = function(path = "../inputdata/boundaries/"){

  lookup = readr::read_csv(file.path(path,"DataZone2022lookup_2024-12-16.csv"))

  lookup
}
