#' Load 2021 census population by age for Output Areas
#'
#' @description Reads the Nomis RM011 extract of 2021 census population by
#'   broad age group for 2021 Output Areas. Used by the `population_oa21`
#'   target, which weights the accessibility analysis (`access_counts()`).
#' @param path Folder containing `nomis_RM011_OA2021_Age.csv`.
#' @return A data frame with `OA21CD`, `total_pop` and age-group columns
#'   (`pop_U16` ... `pop_O65`).
#' @keywords internal
load_oa_population = function(path){
  pop = readr::read_csv(file.path(path, "nomis_RM011_OA2021_Age.csv"), skip = 8)
  pop$mnemonic = NULL
  names(pop) = c("OA21CD","total_pop","pop_U16","pop_16_24","pop_25_34",
                 "pop_35_49","pop_50_64","pop_O65")
  pop = pop[!is.na(pop$OA21CD),]
  pop
}
