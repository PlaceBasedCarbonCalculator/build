#' Select and rename population columns for the website JSON export
#'
#' @description Filters the GB population series to 2010 onwards and shortens
#'   the age-band column names (e.g. "0-4" -> "a04") to reduce JSON size. Used
#'   by the `population_summary` target, exported per zone by
#'   `build_population_jsons`.
#' @param population GB population table (`population` target).
#' @return A data frame with `LSOA21CD`, `year`, short-named age bands,
#'   `households_est` and `all_properties`.
#' @keywords internal
summarise_population = function(population){

  population = population[population$year > 2009,]


  population = population[,c("LSOA21CD","year","0-4","5-9","10-14","15-19","20-24",
                             "25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                             "60-64","65-69","70-74","75-79","80-84","85+","households_est","all_properties")]

  names(population) = c("LSOA21CD","year","a04","a59","a1014","a1519","a2024",
          "a2529","a3034","a3539","a4044","a4549","a5054","a5559",
          "a6064","a6569","a7074","a7579","a8084","85+","households_est","all_properties")

  population

}
