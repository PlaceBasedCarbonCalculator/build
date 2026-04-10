#' Load Oa Population
#'
#' @description Load oa population data from the source path and return it as an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path){ Input object or parameter named `path){`.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
load_oa_population = function(path){
  pop = readr::read_csv(file.path(path, "nomis_RM011_OA2021_Age.csv"), skip = 8)
  pop$mnemonic = NULL
  names(pop) = c("OA21CD","total_pop","pop_U16","pop_16_24","pop_25_34",
                 "pop_35_49","pop_50_64","pop_O65")
  pop = pop[!is.na(pop$OA21CD),]
  pop
}
