#' Summarise Oac
#'
#' @description Summarise oac into a compact table suitable for analysis.
#' @details This function is used to prepare intermediate analysis tables for later pipeline targets.
#' @param lsoa_emissions_all Input object or parameter named `lsoa_emissions_all`.
#' @param oac21){ Input object or parameter named `oac21){`.
#' @return A summary data frame with aggregated metrics.
#' @keywords internal
summarise_oac = function(lsoa_emissions_all, oac21){
  #TODO: Scotland

  oac21 = oac21[,c("oa21cd","la23cd","supergroup","group","subgroup")]

}
