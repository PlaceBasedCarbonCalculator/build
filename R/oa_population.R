load_oa_population = function(path){
  pop = readr::read_csv(file.path(path, "nomis_RM011_OA2021_Age.csv"), skip = 8)
  pop$mnemonic = NULL
  names(pop) = c("OA21CD","total_pop","pop_U16","pop_16_24","pop_25_34",
                 "pop_35_49","pop_50_64","pop_O65")
  pop = pop[!is.na(pop$OA21CD),]
  pop
}
