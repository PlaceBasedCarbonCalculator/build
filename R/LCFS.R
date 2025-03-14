# Living Costs and Food Survey

load_LCFS = function(path = file.path(parameters$path_secure_data,"Living Costs and Food Survey/Safeguarded")) {

  zips = list.files(path, pattern = ".zip")
  nms = gsub("_V1.zip","",gsub("LCFS_","",zips))

  lcfs_all = list()

  for(i in 1:length(zips)){
    lcfs_all[[i]] = load_LCFS_single(path = file.path(path, zips[i]))
  }

  names(lcfs_all) = nms


  lcfs_all

}


load_LCFS_single = function(path = file.path(parameters$path_secure_data,"Living Costs and Food Survey/Safeguarded","LCFS_20202021_V1.zip")){

  dir = file.path(tempdir(),"lcfs")
  dir.create(dir)
  unzip(file.path(path), exdir = dir)

  fls = list.files(file.path(dir,
                   list.files(file.path(dir), pattern = "spss")[1],
                   "spss"), full.names = TRUE)

  fls2 = list.files(fls, full.names = FALSE, pattern = ".sav")
  nms = gsub("%20","_",fls2)
  nms = gsub("_ukanon[0-9]*\\.sav","",nms)
  nms = gsub("_ukanon_final.sav","",nms)
  nms = gsub("^[0-9]*[-|_][0-9]*_","",nms)
  nms = gsub("^[0-9]*\\_","",nms)
  nms = gsub("^lcfs_[0-9]*_","",nms)

  lcfs = list()

  for(i in 1:length(fls2)){
    lcfs[[i]] = foreign::read.spss(file.path(fls,fls2[i]),
                                  to.data.frame=TRUE, )
  }
  names(lcfs) = nms

  # Load Expenditure Code Lookup
  #unzip(file.path(dir,"UKDA-8803-spss","mrdoc","ukda_data_dictionaries.zip"), exdir = dir)
  lookup_path = list.files(file.path(dir,list.files(file.path(dir), pattern = "spss")[1],"mrdoc","excel"),
                           pattern = "volume_d_expenditure_codes",
                           full.names = TRUE)


  lookup = readxl::read_excel(lookup_path[1], sheet = "Part 1")

  unlink(dir, recursive = TRUE)

  purchases = lcfs$dv_set89
  purchases$COI_PLUS = trimws(purchases$COI_PLUS, "both")

  # TODO: about 30 codes missing from lookup
  names(lookup) = gsub(" ","",names(lookup))

  lookup = lookup[,c("COIPLUSCode","COIPLUSDescription","DivisionalDescription")]
  lookup = lookup[!duplicated(lookup$`COIPLUSCode`),]

  purchases = dplyr::left_join(purchases, lookup, by = c("COI_PLUS" = "COIPLUSCode"))

  # TODO: 23405 purchases missing DivisionalDescription
  purchases$`DivisionalDescription` = ifelse(is.na(purchases$`DivisionalDescription`) &
                                                grepl("^11.1",purchases$COI_PLUS),
                                              "Food and non-alcoholic beverages",
                                              purchases$`DivisionalDescription`
                                              )
  purchases$`DivisionalDescription` = ifelse(is.na(purchases$`DivisionalDescription`) &
                                                grepl("^9.4",purchases$COI_PLUS),
                                              "Recreation and culture",
                                              purchases$`DivisionalDescription`
  )

  purchases$`DivisionalDescription` = ifelse(is.na(purchases$`DivisionalDescription`) &
                                                grepl("^1.2",purchases$COI_PLUS),
                                              "Food and non-alcoholic beverages",
                                              purchases$`DivisionalDescription`
  )

  purchases$`DivisionalDescription` = ifelse(is.na(purchases$`DivisionalDescription`) &
                                                grepl("^6.3",purchases$COI_PLUS),
                                              "Health",
                                              purchases$`DivisionalDescription`
  )

  purchases$`DivisionalDescription` = ifelse(is.na(purchases$`DivisionalDescription`) &
                                                grepl("^2.1",purchases$COI_PLUS),
                                              "Alcoholic beverages, tobacco and narcotics",
                                              purchases$`DivisionalDescription`
  )

  purchases$`DivisionalDescription` = ifelse(is.na(purchases$`DivisionalDescription`) &
                                               grepl("^3.11",purchases$COI_PLUS),
                                             "Alcoholic beverages, tobacco and narcotics",
                                             purchases$`DivisionalDescription`
  )

  #summary(purchases$COI_PLUS %in% lookup$`COIPLUSCode`)
  #summary(is.na(purchases$DivisionalDescription))
  # Mode   FALSE    TRUE
  # logical   23405  282877
  #foo = unique(purchases$COI_PLUS[is.na(purchases$DivisionalDescription)])

  if(any(grepl("urban",names(lcfs)))){
    households = lcfs$dvhh_urbanrural
  } else {
    households = lcfs$dvhh
  }

  households_raw = lcfs$rawhh

  flights = dplyr::select(households_raw, case, dplyr::matches("FlyDest|FlyAdult|FlyChild"))
  names(flights) = tolower(names(flights))
  flights = flights[!is.na(flights$flydest),]

  names(flights)[names(flights) == "flydest"] = "flydest1"
  names(flights)[names(flights) == "flyadult"] = "flyadult1"
  names(flights)[names(flights) == "flychild"] = "flychild1"


  flights = tidyr::pivot_longer(flights,
      cols = tidyr::starts_with("flydest") | tidyr::starts_with("flyadult") | tidyr::starts_with("flychild"),
      names_to = c(".value", "number"),
      names_pattern = "(flydest|flyadult|flychild)(\\d*)"
    )
  flights = flights[!is.na(flights$flydest),]

  # TODO: Domestic flights does not record under of passengers so assuming single adult
  flights$flyadult[is.na(flights$flyadult)] = 1
  flights$flychild[is.na(flights$flychild)] = 0

  flights$number = as.integer(flights$number)

  flights_summary = dplyr::group_by(flights, case, flydest)
  flights_summary = dplyr::summarise(flights_summary,
                             destinations_total = dplyr::n(),
                             pass_trips_total = sum(flyadult) + sum(flychild )
                             )

  flights_summary = tidyr::pivot_wider(flights_summary, id_cols = "case",
                                       names_from = "flydest",
                                       values_from = "pass_trips_total",
                                       values_fill = 0)

  # Household Variables - SIPER
  # Composition
  # (one_person_no_child	one_person_with_children	married_no_child	married_with_children	cohibiting_no_child	cohibiting_with_children	other_family	others_not_family_no_child	others_not_family_with_children)


  # Household size (1 - 8+)
  # Tenure (owned_outright	owned_mortgage	social_rented	private_rented	other)
  # Heating (not.centrally.heated	centrally.heated)
  # Deprivation (not.deprived	deprived.in.one	deprived.in.two	deprived.in.three	deprived.in.four)
  # Cars (0,1,2+)
  #

  # Mine
  # Income
  # Composition
  # OnePersonOver66, OnePersonOther, FamilyOver66, CoupleNoChildren, CoupleChildren, CoupleNonDepChildren, LoneParent,   # LoneParentNonDepChildren, OtherNoChildren, OtherChildren, OtherIncStudentOrOver66
  # Household size (1 - 8+)
  # Tenure
  # Housing Type
  # Cars
  # Rural Urban

  # Accommodation Alts

  accommodation = dplyr::select(households_raw, dplyr::any_of(c("case",
                                                         "AccOth", #Is the accommodation: a caravan; mobile home; homeboat; or some other kind of accommodation?
                                                         "AccOth2",
                                                        "CTBand", # Council Tax Band
                                                         "RoomShar", #	Are any rooms in your accommodation shared with anyone who is not a member of your household?
                                                         "Ten1",	#In which of these ways do you occupy this accommodation?
                                                        "P200p", #(number of rooms occupied - anonymised),
                                                        "A111p", #(rooms used solely by household - anonymised)
                                                        "A114p", #(rooms in accommodation - total - anonymised)
                                                        "p200p", #(number of rooms occupied - anonymised),
                                                        "a111p", #(rooms used solely by household - anonymised)
                                                        "a114p", #(rooms in accommodation - total - anonymised)
                                                        "Nrmsp",
                                                        "Nrms2p",
                                                        "Nrms3p",
                                                        "Nrms4p",
                                                        "Nrms5p",
                                                        "Nrms6p",
                                                        "DVrmsp",
                                                        "DVbedp",
                                                        "DVsharep",
                                                        "OAC"
                                                         )))





  columns_to_select <- c("case",
                         "Mscale", # McClements Scale factor
                         "OECDSc", # OECD Scale factor
                         "weightq",
                         "weighta",
                         "Gorx", # Government Office Region modified
                         "URGridEWp", # Rural Urban England
                         "URGridSCp", # Rural Scotland England
                         "OAC3D", # Output Area Classification -3D
                         "incanon", # Anonymised household income and allowances
                         "p344p", # Gross normal weekly household income - top-coded
                         "A122", # Tenure
                         "A124", # Cars and Vans
                         "a124p", # Cars and Vans top coded
                         "A094", # NS-SEC 12 Class of HRP,
                         "A091", # Socio-economic group HRP,
                         "A116", # Building Type - Removed in 2020
                         "A062", # Household Comp
                         "A138", # Second Dwelling
                         # Total consumption
                         "P600t",
                         "P601t",
                         "P602t",
                         "P603t",
                         "P604t",
                         "P605t",
                         "P606t",
                         "P607t",
                         "P608t",
                         "P609t",
                         "P610t",
                         "P611t",
                         "P612t",
                         "P620tp",
                         "P630tp",
                         # Other Vars
                         "p493p",
                         "p492p",
                         "a069p",
                         "P431p",
                         "p431", # Main source of income
                         "a117p",
                         "a118p",
                         "a143p",
                         "a162p",
                         "p200p",
                         "a111p",
                         "a114p")


  household_char = dplyr::select(households, dplyr::any_of(columns_to_select))

  missing_cols = columns_to_select[!columns_to_select %in% names(household_char)]
  if(length(missing_cols) > 0){
    warning("Ths columns are missing: ", paste(missing_cols, collapse = ", "))
  }

  # atttribs = attributes(households)
  # vars = data.frame(nms = names(atttribs$variable.labels), vals = unname(atttribs$variable.labels))
  # write.csv(vars, file.path(path,"hosuehold_variaibles.csv"), row.names = FALSE)

  # Check consistences

  # purchases_summary = dplyr::group_by(purchases, case, `DivisionalDescription`)
  # purchases_summary = dplyr::summarise(purchases_summary, pdamount = sum(pdamount, na.rm = TRUE))
  # purchases_summary = tidyr::pivot_wider(purchases_summary,
  #                                        values_from = "pdamount",
  #                                        names_from = "DivisionalDescription",
  #                                        values_fill = 0)
  #
  # purchases_summary = dplyr::left_join(purchases_summary, household_char, by = "case")
  #
  # purchases_summary = purchases_summary[,c("case","Clothing and footwear",
  #                                          "Food and non-alcoholic beverages",
  #                                          "Furnishings, household equipment and routine maintenance of house",
  #                                          "Health","Miscellaneous goods and services",
  #                                          "Non-consumption expenditure","Recreation and culture",
  #                                          "Transport","Alcoholic beverages, tobacco and narcotics",
  #                                          "Housing, water, electricity, gas and other fuels",
  #                                          "Communication","Education","Restaurants and hotels",
  #                                          "P600t","P601t","P602t","P603t",
  #                                          "P604t","P605t","P606t","P607t",
  #                                          "P608t","P609t","P610t","P611t",
  #                                          "P612t","P620tp","P630tp")]
  #
  # purchases_summary$d_food = round(abs(purchases_summary$`Food and non-alcoholic beverages` - purchases_summary$P601t))
  # purchases_summary$d_alc = round(abs(purchases_summary$`Alcoholic beverages, tobacco and narcotics` - purchases_summary$P602t))
  # purchases_summary$d_cloth = round(abs(purchases_summary$`Clothing and footwear` - purchases_summary$P603t))
  # purchases_summary$d_house = round(abs(purchases_summary$`Housing, water, electricity, gas and other fuels` - purchases_summary$P604t))
  # purchases_summary$d_furn = round(abs(purchases_summary$`Furnishings, household equipment and routine maintenance of house` - purchases_summary$P605t))
  # purchases_summary$d_health = round(abs(purchases_summary$`Health` - purchases_summary$P606t))
  # purchases_summary$d_trans = round(abs(purchases_summary$`Transport` - purchases_summary$P607t))
  # purchases_summary$d_comm = round(abs(purchases_summary$`Communication` - purchases_summary$P608t))
  # purchases_summary$d_rec = round(abs(purchases_summary$`Recreation and culture` - purchases_summary$P609t))
  # purchases_summary$d_edu = round(abs(purchases_summary$`Education` - purchases_summary$P610t))
  # purchases_summary$d_rest = round(abs(purchases_summary$`Restaurants and hotels` - purchases_summary$P611t))
  # purchases_summary$d_misc = round(abs(purchases_summary$`Miscellaneous goods and services` - purchases_summary$P612t))
  #
  # summary((round(purchases_summary$`Clothing and footwear`,2) - round(purchases_summary$P603t,2)))
  # summary((round(,2) - round(,2)))
  # summary((round(purchases_summary$`Health`,2) - round(purchases_summary$P606t,2)))
  # summary((round(purchases_summary$`Furnishings, household equipment and routine maintenance of house`,2) - round(purchases_summary$P605t,2)))
  # summary((round(purchases_summary$`Miscellaneous goods and services`,2) - round(purchases_summary$P612t,2)))
  # summary((round(purchases_summary$`Non-consumption expenditure`,2) - round(purchases_summary$P620tp,2)))

  people = lcfs$dvper
  people_summary = summarise_household_composition(people)



  lcfs_final = list(household = household_char,
                    people = people_summary,
                    purchases = purchases,
                    flights = flights_summary,
                    accommodation = accommodation)

  lcfs_final


}


summarise_household_composition = function(people){

  if(inherits(people$a005p, "factor")){
    people$a005p = as.character(people$a005p)
    people$a005p = ifelse(people$a005p %in% c("80 or older","80 and older"),
                          "81",people$a005p)
    people$a005p = as.integer(people$a005p)
  }

  if(is.null(people$a012p)){
    people$a012p = people$A012p
  }


  ppl_list = dplyr::group_split(people, case)

  ppl_summary = purrr::map(ppl_list, hh_comp, .progress = TRUE)
  ppl_summary = dplyr::bind_rows(ppl_summary)
  #foo = unique(ppl_summary[,2:3])
  ppl_summary
}

hh_comp = function(x){

  #TODO: Check that string are the same across years, which some are not
  if(nrow(x) == 1){
    # Single Person
    if(x$a005p > 65){
      return(data.frame(case = x$case, hhsize = nrow(x), hhcomp = "OnePersonOver66",
                        hhcomp5 = "Oneperson", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
    } else {
      return(data.frame(case = x$case, hhsize = nrow(x), hhcomp = "OnePersonOther",
                        hhcomp5 = "Oneperson", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
    }
  } else {
    # Check for Children
    if(any(x$A002 %in% c("Son or daughter","Grandson grndaughter"))){
      # Children
      x$dependancy = ifelse(x$A007 %in% c("Not recorded","University/polytechnic, any other higher education college"),
                            "non-dependant","dependant")
      x$dependancy = ifelse(x$A003 == "HRP", "non-dependant", x$dependancy) #HRP can't be dependant e.g. student parent
      x$dependancy = ifelse(x$a005p >= 18, "non-dependant", x$dependancy) #Adults can't be dependant
      x$child = ifelse(x$A002 %in% c("Son or daughter","Grandson grndaughter"),
                       "child", "adult")

      x$child_dep = ifelse(x$dependancy == "dependant", "dependant child", paste0(x$dependancy," ",x$child))
      x$child_dep[x$child_dep == "non-dependant adult"] = "adult"

      # Count adults
      n_adutls = sum(x$child_dep == "adult")

      if(any(x$child_dep == "dependant child")){
        # Dependant Children
        if(n_adutls == 1){
          # Check for Grandparent HRP - edge case couple/loneparent + grandparent and gradnparent is HRP



          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "LoneParent",
                            hhcomp5 = "Loneparent", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
        } else if (n_adutls == 2 & all(x$A002[x$child_dep == "adult"] %in% c("Person 1","Spouse cohabitee civil partner","Spouse/Cohabitee/Civil Partner"))){
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "CoupleChildren",
                            hhcomp5 = "Couple", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
        } else {
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "OtherChildren",
                            hhcomp5 = "Other", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
        }

      } else {
        # All non-dependant children
        if(n_adutls == 1){
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "LoneParentNonDepChildren",
                            hhcomp5 = "Loneparent", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
        } else if (n_adutls == 2 & all(x$A002[x$child_dep == "adult"] %in% c("Person 1","Spouse cohabitee civil partner","Spouse/Cohabitee/Civil Partner"))){
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "CoupleNonDepChildren",
                            hhcomp5 = "Couple", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
        } else {
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "OtherNoChildren - nondepchild",
                            hhcomp5 = "Other", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
        }
      }

    } else {
      # No Children
      if(all(x$A206 == "Retired/unoccupied and of minimum NI Pension age")){
        if(all(x$A002 != "Non-relative")){
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "FamilyOver66", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
        } else {
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "OtherIncStudentOrOver66 - retired", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
        }
      } else if (nrow(x) == 2 & all(x$A002 %in% c("Person 1","Spouse cohabitee civil partner","Spouse/Cohabitee/Civil Partner"))){
        return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "CoupleNoChildren", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
      } else if (all(x$A007 == "University/polytechnic, any other higher education college")){
        return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "OtherIncStudentOrOver66 - students", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
      } else {
        return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "OtherNoChildren", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$a012p[x$A003 == "HRP"]))
      }

    }
  }

  stop("Unknown example", x$case[1])
}

