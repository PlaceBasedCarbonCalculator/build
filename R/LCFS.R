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
                         "P389p", # Normal weekly disposable income
                         "P352p", # Gross current income of household - top-coded
                         "A122", # Tenure
                         "A124", # Cars and Vans
                         "a124p", # Cars and Vans top coded
                         "A094", # NS-SEC 12 Class of HRP,
                         "A091", # Socio-economic group HRP,
                         "A116", # Building Type - Removed in 2020
                         "A062", # Household Comp
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
                         # Component consumption
                         # Gas Electric and other fuels
                         "B017",#	Electricity - amount paid in last account, less rebates
                         "B018",#	Gas - amount paid in last account, less rebates
                         "B053u",#	Gas – combined account, amount
                         "B056u",#	Electricity – combined account, amount
                         "B221", # Unknown
                         "B222", # Unknown
                         #"B489",#	Bottled gas for central heating - amount
                         #"B490",#	Oil for central heating cost last quarter
                         #"B254",#	Electricity – (combined) prepayment meter, less rebate
                         #"B255",#	Gas – (combined) pre-payment meter less rebate
                         #"B226",#	Electricity – (separate) pre-payment meter, less rebate
                         #"B227",#	Gas – (separate) pre-payment meter less rebate
                         "C45112t",#	Second dwelling: electricity account payment
                         "C45114t",#	Electricity slot meter payment
                         "C45212t",#	Second dwelling: gas account payment
                         "C45214t",#	Gas slot meter payment
                         "C45222t",#	Bottled gas - other
                         "C45312t",#	Paraffin
                         "C45411t",#	Coal and coke
                         "C45412t",#	Wood and peat
                         "C45511t",#	Hot water, steam and ice
                         "B170", #  Gas - amount paid in last account
                         "B173", #  Gas - amount last rebate (to remove)
                         "B175", #  Electricity - amount paid in last account
                         "B178", #  Electricity - amount last rebate (to remove)

                         "P249t", # Gas - slot meter payments less rebate
                         "P250t", # Electricity - slot meter payments less rebate


                         ##Transport
                         # Purchase of Vehicles
                         "B244",#	Vehicle - cost of new car | van outright
                         "B245",#	Vehicle - cost of second hand car | van outright
                         "B247",#	Vehicle - cost of motorcycles outright
                         "C71111c",#	Outright purchase of new car/van
                         "C71112t",#	Loan / HP purchase of new car/van
                         "C71121c",#	Outright purchase of secondhand car/van
                         "C71122t",#	Loan / HP purchase of second-hand car/van
                         "C71211c",#	Outright purchase of new or second-hand motorcycle
                         "C71212t",#	Loan / HP purchase of new or second-hand motorcycle
                         "C71311t",#	Purchase of bicycle
                         "C71411t",#	Animal drawn vehicles

                         # Operation of personal transport equipment
                         "C72211t",#	Petrol
                         "C72212t",#	Diesel oil
                         "C72213t",#	Other motor oils
                         "B249",#	Car or van - servicing: amount paid
                         "B250",#	Car or van - other works, repairs: amount paid
                         "B252",#	Motor cycle - services, repairs: amount paid
                         "B248",#	Car leasing - expenditure on
                         "C72111t",#	Car / van accessories and fittings
                         "C72112t",#	Car / van spare parts
                         "C72113t",#	Motorcycle accessories and spare parts
                         "C72114t",#	Anti-freeze, battery water, cleaning materials
                         "C72115t",#	Bicycle accessories, repairs and other costs
                         "C72311c",#	Car or van repairs and servicing
                         "C72312c",#	Motor cycle repairs, service
                         "C72313t",#	Motoring organisation subscription (eg AA and RAC)
                         "C72314t",#	Car washing and breakdown services
                         "C72411t",#	Parking fees, tolls, and permits (excluding motoring fines)
                         "C72412t",#	Garage rent, MOT etc
                         "C72413t",#	Driving lessons
                         "C72414t",#	Hire of self-drive cars, vans, bicycles

                         # Transport services
                         "B218",#	Season ticket - rail / tube - total net amount
                         "B217",#	Season ticket - bus / coach - total net amount
                         "B219",#	Water travel season ticket
                         "B216",#	Bus + tube and/or rail season
                         "C73112t",#	Railway and tube fares other than season tickets
                         "C73212t",#	Bus and coach fares other than season tickets
                         "C73411t",#	Water travel
                         "C73512t",#	Combined fares other than season tickets
                         "C73213t",#	Taxis and hired cars with drivers
                         "C73214t",#	Other personal travel
                         "C73513t",#	School travel
                         "C73611t",#	Delivery charges and other transport services
                         "C73311t",#	Air fares(within UK) (alternate source?)
                         "C73312t",#	Air fares (international) (alternate source?)

                         "B487", #	Domestic flight expenditure
                         "B488", #	International flight expenditure


                         # Other Vars
                         "p493p",
                         "p492p",
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

  #Values that have to de deducted as rebates
  if("B173" %in% names(household_char)){
    household_char$B173 = - household_char$B173
  }

  if("B178" %in% names(household_char)){
    household_char$B178 = - household_char$B178
  }

  if("P249t" %in% names(household_char)){
    household_char$P249t = - household_char$P249t
  }

  if("P250t" %in% names(household_char)){
    household_char$P250t = - household_char$P250t
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

subset_lcfs = function(lcfs, yrs = c("20182019","20192020")){

  if(length(yrs) != 2){
    stop("Only two yrs values allowed")
  }

  if(!all(yrs %in% names(lcfs))){
    stop("yrs values not valid")
  }

  lcfs_2021 = lcfs[[yrs[1]]]
  lcfs_2022 = lcfs[[yrs[2]]]

  #Dwelling Type missing in 21/22 and 20/21 data (A116), emailed ONS - removed in 2020
  #TODO: OAC seem wrong in post 2020 data, emailed ONS

  hh_21 = lcfs_2021$household
  hh_22 = lcfs_2022$household

  pp_21 = lcfs_2021$people
  pp_22 = lcfs_2022$people

  fly_21 = lcfs_2021$flights
  fly_22 = lcfs_2022$flights

  names(fly_21)[names(fly_21) %in% c("Single return from UK to outside UK","To a country outside the UK")] = "flight_international_return"
  names(fly_22)[names(fly_22) %in% c("Single return from UK to outside UK","To a country outside the UK")] = "flight_international_return"

  names(fly_21)[names(fly_21) == "Other flight"] = "flight_other"
  names(fly_22)[names(fly_22) == "Other flight"] = "flight_other"

  names(fly_21)[names(fly_21) %in% c("Domestic UK return","A domestic UK return")] = "flight_domestic_return"
  names(fly_22)[names(fly_22) %in% c("Domestic UK return","A domestic UK return")] = "flight_domestic_return"

  names(fly_21)[names(fly_21) %in% c("Domestic UK one way","A domestic UK one way")] = "flight_domestic_single"
  names(fly_22)[names(fly_22) %in% c("Domestic UK one way","A domestic UK one way")] = "flight_domestic_single"

  hh_21$case = as.character(hh_21$case)
  hh_22$case = as.character(hh_22$case)

  pp_21$case = as.character(pp_21$case)
  pp_22$case = as.character(pp_22$case)

  fly_21$case = as.character(fly_21$case)
  fly_22$case = as.character(fly_22$case)

  hh_21 = dplyr::left_join(hh_21, pp_21, by = "case")
  hh_22 = dplyr::left_join(hh_22, pp_22, by = "case")

  hh_21 = dplyr::left_join(hh_21, fly_21, by = "case")
  hh_22 = dplyr::left_join(hh_22, fly_22, by = "case")

  # Fix for changeeing source of flight emissions
  # if("B487" %in% names(hh_21)){ #& !"C73311t" %in% names(hh_21)){
  #   hh_21$C73311t = hh_21$B487
  # }
  #
  # if("B487" %in% names(hh_22)){ # & !"C73311t" %in% names(hh_22)){
  #   hh_22$C73311t = hh_22$B487
  # }

  hh_21 = add_component_costs(hh_21, yrs[1])
  hh_22 = add_component_costs(hh_22, yrs[2])


  hh = dplyr::bind_rows(list(hh_21, hh_22))
  hh$household_id = 1:nrow(hh)

  hh$Tenure5 = convert_housing_tenure(hh$A122)
  if(is.null(hh$A124)){
    hh$CarVan5 = convert_car_ownership(hh$a124p)
  } else {
    hh$CarVan5 = convert_car_ownership(hh$A124)
  }
  hh$hhSize5 = convert_household_size(hh$hhsize)
  # hh$NSSEC10 = convert_NSSEC(hh$A094)

  hh$hhComp15 = as.character(hh$hhcomp)
  hh$hhComp15 = gsub(" - students","",hh$hhComp15)
  hh$hhComp15 = gsub(" - retired","",hh$hhComp15)
  hh$hhComp15 = gsub(" - nondepchild","",hh$hhComp15)

  # OAC
  hh$OAC = trimws(hh$OAC)
  if(any(grepl("Aspiring", hh$OAC, ignore.case = TRUE))){
    hh$OAC = clean_OAC_codes(hh$OAC)
  }

  hh$OAC = tolower(hh$OAC)
  hh$OAC[hh$OAC == ""] = NA
  hh$OAC[hh$OAC == "ni"] = NA
  hh = hh[!is.na(hh$OAC),]

  # Summarise Components







  columns_to_select <- c("household_id",
                         "case",
                         "Tenure5",
                         "CarVan5",
                         "hhSize5",
                         "hhComp15",
                         "OAC",
                         "Gorx",
                         "URGridEWp",
                         "URGridSCp",
                         "OAC3D",
                         "incanon",
                         "A122", # Tenure
                         "A094",
                         "A091",
                         "A116", # Dwelling Category
                         "p200p", # Number of rooms
                         "P600t", # Consumption Categories
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
                         "P431p",
                         "spend_housing_gaselecfuel",
                         "spend_housing_gaselec_rebates",
                         "spend_transport_vehiclepurchase",
                         "spend_transport_optranequip_fuel",
                         "spend_transport_optranequip_other",
                         "spend_transport_services_pt",
                         "spend_transport_services_air",
                        "a117p", # New Cars
                        "a118p", # Second Hand Cars
                        "a162p", # Motor Cycles
                        "HRPemploy",
                        "HRPethnic",
                        "flight_international_return",
                        "flight_other",
                        "flight_domestic_return",
                        "flight_domestic_single"
                        )


  hh = dplyr::select(hh, dplyr::any_of(columns_to_select))
  hh = dplyr::rename(hh,
                     tenure = A122,
                     NSSEC8_HRP = A094,
                     SEC_HRP = A091,
                     dwelling_type = A116,
                     rooms = p200p,
                     spend_totalconsump = P600t,
                     spend_food = P601t,
                     spend_alcohol = P602t,
                     spend_clothing = P603t,
                     spend_housing = P604t,
                     spend_furnish = P605t,
                     spend_health = P606t,
                     spend_transport = P607t,
                     spend_communication = P608t,
                     spend_recreation = P609t,
                     spend_education = P610t,
                     spend_restaurant = P611t,
                     spend_misc = P612t,
                     spend_nonconsump = P620tp,
                     spend_total = P630tp,
                     cars_new = a117p,
                     cars_secondhand = a118p,
                     motorcycles = a162p
                     )


  hh
}


selected_lcfs = function(lcfs){

  res = list("2010/11" = subset_lcfs(lcfs, yrs = c("2010","2011")),
             "2012/13" = subset_lcfs(lcfs, yrs = c("2012","2013")),
             "2014/15" = subset_lcfs(lcfs, yrs = c("2014","20152016")),
             "2016/17" = subset_lcfs(lcfs, yrs = c("20162017","20172018")),
             "2018/19" = subset_lcfs(lcfs, yrs = c("20182019","20192020")),
             "2020/21" = subset_lcfs(lcfs, yrs = c("20202021","20212022"))
  )

  res

}

add_component_costs = function(hh, yr = "2010"){
  hh$spend_housing_gaselecfuel = rowSums(dplyr::select(hh, dplyr::any_of(c(
    "B017",#	Electricity - amount paid in last account, less rebates
    "B018",#	Gas - amount paid in last account, less rebates
    "B053u",#	Gas – combined account, amount
    "B056u",#	Electricity – combined account, amount
    #"B221", # Unknown so excluding from energy
    #"B222", # Unknown
    "C45112t",#	Second dwelling: electricity account payment
    "C45114t",#	Electricity slot meter payment
    "C45212t",#	Second dwelling: gas account payment
    "C45214t",#	Gas slot meter payment
    "C45222t",#	Bottled gas - other
    "C45312t",#	Paraffin
    "C45411t",#	Coal and coke
    "C45412t",#	Wood and peat
    "C45511t",#	Hot water, steam and ice
    "B170", #  Gas - amount paid in last account
    "B173", #  Gas - amount last rebate (to remove)
    "B175", #  Electricity - amount paid in last account
    "B178", #  Electricity - amount last rebate (to remove)
    "P249t", # Gas - slot meter payments less rebate
    "P250t" # Electricity - slot meter payments less rebate

  ))), na.rm = TRUE)

  hh$spend_housing_gaselec_rebates = rowSums(dplyr::select(hh, dplyr::any_of(c(
    "B173", #  Gas - amount last rebate (to remove)
    "B178", #  Electricity - amount last rebate (to remove)
    "P249t", # Gas - slot meter payments less rebate
    "P250t" # Electricity - slot meter payments less rebate
  ))), na.rm = TRUE)

  hh$spend_transport_vehiclepurchase = rowSums(dplyr::select(hh, dplyr::any_of(c(
    "B244",#	Vehicle - cost of new car | van outright
    "B245",#	Vehicle - cost of second hand car | van outright
    "B247",#	Vehicle - cost of motorcycles outright
    "C71111c",#	Outright purchase of new car/van
    "C71112t",#	Loan / HP purchase of new car/van
    "C71121c",#	Outright purchase of secondhand car/van
    "C71122t",#	Loan / HP purchase of second-hand car/van
    "C71211c",#	Outright purchase of new or second-hand motorcycle
    "C71212t",#	Loan / HP purchase of new or second-hand motorcycle
    "C71311t",#	Purchase of bicycle
    "C71411t"#	Animal drawn vehicles
  ))), na.rm = TRUE)

  hh$spend_transport_optranequip_fuel = rowSums(dplyr::select(hh, dplyr::any_of(c(
    # Operation of personal transport equipment
    "C72211t",#	Petrol
    "C72212t",#	Diesel oil
    "C72213t"#	Other motor oils
  ))), na.rm = TRUE)

  hh$spend_transport_optranequip_other = rowSums(dplyr::select(hh, dplyr::any_of(c(
"B249",#	Car or van - servicing: amount paid
"B250",#	Car or van - other works, repairs: amount paid
"B252",#	Motor cycle - services, repairs: amount paid
"B248",#	Car leasing - expenditure on
"C72111t",#	Car / van accessories and fittings
"C72112t",#	Car / van spare parts
"C72113t",#	Motorcycle accessories and spare parts
"C72114t",#	Anti-freeze, battery water, cleaning materials
"C72115t",#	Bicycle accessories, repairs and other costs
"C72311c",#	Car or van repairs and servicing
"C72312c",#	Motor cycle repairs, service
"C72313t",#	Motoring organisation subscription (eg AA and RAC)
"C72314t",#	Car washing and breakdown services
"C72411t",#	Parking fees, tolls, and permits (excluding motoring fines)
"C72412t",#	Garage rent, MOT etc
"C72413t",#	Driving lessons
"C72414t"#	Hire of self-drive cars, vans, bicycles
  ))), na.rm = TRUE)

  hh$spend_transport_services_pt = rowSums(dplyr::select(hh, dplyr::any_of(c(
    # Transport services
"B218",#	Season ticket - rail / tube - total net amount
"B217",#	Season ticket - bus / coach - total net amount
"B219",#	Water travel season ticket
"B216",#	Bus + tube and/or rail season
"C73112t",#	Railway and tube fares other than season tickets
"C73212t",#	Bus and coach fares other than season tickets
"C73411t",#	Water travel
"C73512t",#	Combined fares other than season tickets
"C73213t",#	Taxis and hired cars with drivers
"C73214t",#	Other personal travel
"C73513t",#	School travel
"C73611t"#	Delivery charges and other transport services
  ))), na.rm = TRUE)


  if(yr %in% c("2010","2011","2012")){
    hh$spend_transport_services_air = rowSums(dplyr::select(hh, dplyr::any_of(c(
"C73311t",#	Air fares(within UK)
"C73312t" #	Air fares (international)
    ))), na.rm = TRUE)
  } else {
    hh$spend_transport_services_air = rowSums(dplyr::select(hh, dplyr::any_of(c(
      "B487",#	Air fares(within UK)
      "B488" #	Air fares (international)
    ))), na.rm = TRUE)
  }





  # Check for consistancy

  hh$diff = round(hh$P607t - (hh$spend_transport_vehiclepurchase + hh$spend_transport_optranequip_fuel + hh$spend_transport_optranequip_other + hh$spend_transport_services_air + hh$spend_transport_services_pt))

  if(!all(hh$diff == 0)){
    stop("Transport spending components don't add up, ",yr)
  } else {
    hh$diff = NULL
  }


  hh
}




clean_OAC_codes <- function(values) {
  # Create a mapping of descriptions to ONS codes
  mapping <- c(
    "Northern Ireland - not classified" = "NI",
    "Terraced Blue Collar (1)" = "1A1",
    "Terraced Blue Collar (2)" = "1A2",
    "Terraced Blue Collar (3)" = "1A3",
    "Younger Blue Collar (1)" = "1B1",
    "Younger Blue Collar (2)" = "1B2",
    "Older Blue Collar (1)" = "1C1",
    "Older Blue Collar (2)" = "1C2",
    "Older Blue Collar (3)" = "1C3",
    "Transient Communities (1)" = "2A1",
    "Transient Communities (2)" = "2A2",
    "Settled in the City (1)" = "2B1",
    "Settled in the City (2)" = "2B2",
    "Village Life (1)" = "3A1",
    "Village Life (2)" = "3A2",
    "Agricultural (1)" = "3B1",
    "Agricultural (2)" = "3B2",
    "Accessible Countryside (1)" = "3C1",
    "Accessible Countryside (2)" = "3C2",
    "Prospering Younger Families (1)" = "4A1",
    "Prospering Younger Families (2)" = "4A2",
    "Prospering Older Families (1)" = "4B1",
    "Prospering Older Families (2)" = "4B2",
    "Prospering Older Families (3)" = "4B3",
    "Prospering Older Families (4)" = "4B4",
    "Prospering Semis (1)" = "4C1",
    "Prospering Semis (2)" = "4C2",
    "Prospering Semis (3)" = "4C3",
    "Thriving Suburbs (1)" = "4D1",
    "Thriving Suburbs (2)" = "4D2",
    "Senior Communities (1)" = "5A1",
    "Senior Communities (2)" = "5A2",
    "Older Workers (1)" = "5B1",
    "Older Workers (2)" = "5B2",
    "Older Workers (3)" = "5B3",
    "Older Workers (4)" = "5B4",
    "Public Housing (1)" = "5C1",
    "Public Housing (2)" = "5C2",
    "Public Housing (3)" = "5C3",
    "Settled Households (1)" = "6A1",
    "Settled Households (2)" = "6A2",
    "Aspiring Households (1)" = "6B1",
    "Aspiring Households (2)" = "6B2",
    "Aspiring Households (3)" = "6B3",
    "Young Families in Terraced Homes (1)" = "6C1",
    "Young Families in Terraced Homes (2)" = "6C2",
    "Aspiring Households (1)" = "6D1",
    "Aspiring Households (1)_duplicated_6D1" = "6D1",
    "Aspiring Households (2)" = "6D2",
    "Aspiring Households (2)_duplicated_6D2" = "6D2",
    "Asian Communities (1)" = "7A1",
    "Asian Communities (2)" = "7A2",
    "Asian Communities (3)" = "7A3",
    "Afro-Caribbean Communities (1)" = "7B1",
    "Afro-Caribbean Communities (2)" = "7B2"
  )


  converted <- mapping[as.character(values)]
  converted <- unname(converted)
  converted[is.na(converted)] = values[is.na(converted)]


  return(converted)
}
