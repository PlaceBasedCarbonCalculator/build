load_NTS = function(path = file.path(parameters$path_secure_data,"National Travel Survey/Safeguarded/")){

  dir = file.path(tempdir(),"NTS")
  dir.create(dir)
  unzip(file.path(path,"NTS_Safeguarded_2002_2023_SPSS_V1.zip"), exdir = dir)

  fls = list.files(file.path(dir,"UKDA-5340-spss","spss","spss28"), full.names = FALSE)
  nms = gsub("_eul_2002-2023.sav","",fls)

  nts = list()

  for(i in 1:length(fls)){
    nts[[i]] = foreign::read.spss(file.path(file.path(dir,"UKDA-5340-spss","spss","spss28",fls[i])),
                                  to.data.frame=TRUE)
  }

  lookup_tables = readxl::read_excel(file.path(dir,"UKDA-5340-spss","mrdoc","excel","5340_nts_lookup_table_eul_2002_to_2023.xlsx"),
                              sheet = "Multicoded Tables")

  lookup_cols = readxl::read_excel(file.path(dir,"UKDA-5340-spss","mrdoc","excel","5340_nts_lookup_table_response_levels_eul_2002_to_2023.xlsx"))
  lookup_cols = lookup_cols[,c("Table","Variable","Description","ID","Desc")]

  lookup_cols$Description = gsub(" - Created in SQL","",lookup_cols$Description)
  lookup_cols$Description = gsub(" - created in SQL","",lookup_cols$Description)
  lookup_cols$Description = gsub(",|\\(|\\)|<|=|>|?","",lookup_cols$Description)
  lookup_cols$Description = gsub("  regional / Metropolitan area breakdown","",lookup_cols$Description)
  lookup_cols$Description = gsub("/","",lookup_cols$Description)
  lookup_cols$Description = gsub("-","",lookup_cols$Description)
  lookup_cols$Description = gsub(" ","",lookup_cols$Description)

  lookup_tables$`Table Name` = tolower(lookup_tables$`Table Name`)

  list_names = data.frame(nms = nms)
  lookup_tables = dplyr::left_join(list_names,
                                   lookup_tables[,c("Table Name","Description")],
                                   by = c("nms" = "Table Name")
  )
  lookup_tables$Description = ifelse(is.na(lookup_tables$Description), lookup_tables$nms, lookup_tables$Description)



  lookup_tables$dup = duplicated(lookup_tables$Description)

  lookup_tables$occurrences <- ave(seq_along(lookup_tables$Description), lookup_tables$Description, FUN = seq_along)
  lookup_tables$Description = ifelse(lookup_tables$occurrences > 1,
                                     paste0(lookup_tables$Description," ",LETTERS[lookup_tables$occurrences]),
                                     lookup_tables$Description)

  names(nts) = lookup_tables$Description


  # Replace lookups with values
  for(i in 1:length(nts)){
    sub = nts[[i]]
    lookup_sub = lookup_cols[lookup_cols$Variable %in% names(sub),]
    for(j in names(sub)){
      col_sub = lookup_sub[lookup_sub$Variable == j,]
      col_sub$Desc = as.factor(col_sub$Desc)
      if(nrow(col_sub) > 0){
        names(col_sub)[5] = col_sub$Description[1]
        names(col_sub)[4] = col_sub$Variable[1]
        col_sub = col_sub[,c(4:5)]
        sub[[j]] = as.numeric(sub[[j]])
        sub = dplyr::left_join(sub, col_sub, by = j)
        sub[j] = NULL
      }
    }
    nts[[i]] = sub
  }












  # lookup_cols = readxl::read_excel(file.path(dir,"UKDA-5340-spss","mrdoc","excel","5340_nts_lookup_table_eul_2002_to_2023.xlsx"),
  #                                    sheet = "Main Table Variables")
  # lookup_cols = lookup_cols[,c("Variable","Description")]
  #
  # lookup_cols$clean = gsub(" - Created in SQL","",lookup_cols$Description)
  # lookup_cols$clean = gsub(" - created in SQL","",lookup_cols$clean)
  # lookup_cols$clean = gsub(",|\\(|\\)|<|=|>|?","",lookup_cols$clean)
  # lookup_cols$clean = gsub("  regional / Metropolitan area breakdown","",lookup_cols$clean)
  # lookup_cols$clean = gsub("/","",lookup_cols$clean)
  # lookup_cols$clean = gsub("-","",lookup_cols$clean)
  # lookup_cols$clean = gsub(" ","",lookup_cols$clean)
  #
  # for(i in 1:length(nts)){
  #   tab_nms = names(nts[[i]])
  #   tab_match = lookup_cols$clean[match(tab_nms, lookup_cols$Variable)]
  #   tab_match = ifelse(is.na(tab_match),tab_nms, tab_match)
  #
  #   names(nts[[i]]) = tab_match
  # }

  unlink(dir, recursive = TRUE)

  nts
}



build_nts_zones = function(nts){
  nts_household = nts$household
  #nts_ldj = nts$ldj
  nts_psu = nts$psu

  nts_household = nts_household[,c("HouseholdID", "PSUID","TWSYear",
                                   "Typeofpropertyfoundattheaddress",
                                   "HouseholdRegion","Countryofresidence",
                                   "IsLUlightrailmetrotramstopcloserthanrailwaystation",
                                   "ONSRuralUrbanClassificationofresidence2011Censussummary5categories",
                                   "2011CensusOutputAreaClassificationSupergroup8bands")]

  nts_household$zone = paste0(nts_household$HouseholdRegion," ",nts_household$ONSRuralUrbanClassificationofresidence2011Censussummary5categories)




}
