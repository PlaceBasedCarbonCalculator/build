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
