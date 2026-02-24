# Living Costs and Food Survey

load_LCFS = function(path = file.path(parameters$path_secure_data,"Living Costs and Food Survey/Safeguarded")) {

  zips = list.files(path, pattern = ".zip")
  nms = gsub("_V1.zip","",gsub("LCFS_","",zips))

  lcfs_all = list()

  for(i in 1:length(zips)){
    message(zips[i])
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
  nms = gsub("_ukanon_[0-9]*\\.sav","",nms)
  nms = gsub("_ukanon_v[0-9]*\\_[0-9]*\\.sav","",nms)
  nms = gsub("_ukanon_[0-9]*\\_[0-9]*\\.sav","",nms)
  nms = gsub("_ukanon_final.sav","",nms)
  nms = gsub("_ukanon_final_[0-9]*\\.sav","",nms)
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

  #2023-24 number not names
  if(inherits(flights$flydest1,"numeric")){
    FlyLookup = data.frame(id = 1:4,
                           names = c("Domestic UK one way","Domestic UK return",
                                     "Single return from UK to outside UK","Other flight"))
    flights$flydest1 = factor(FlyLookup$names[match(flights$flydest1,FlyLookup$id)], levels = FlyLookup$names)
  }

  # Sometimes levles missing
  if(inherits(flights$flydest1,"factor")){
    cols = grep("^flydest", names(flights))
    cols = cols[2:length(cols)]
    for(j in cols){
      if(inherits(flights[[j]],"numeric")){
        flights[[j]] = levels(flights$flydest1)[match(flights[[j]],seq(1, length(levels(flights$flydest1))))]
      }
    }
  }
  # Convert to character
  flights[grep("^flydest", names(flights))] <- lapply(flights[grep("^flydest", names(flights))], as.character)

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
                                                        "OAC",
                                                        "OAC3D"
                                                         )))





  columns_to_select <- c("case",
                         "Mscale", # McClements Scale factor
                         "OECDSc", # OECD Scale factor
                         "weightq",
                         "weighta",
                         "Gorx", # Government Office Region modified
                         "URGridEWp", # Rural Urban England
                         "URGridSCp", # Rural Scotland England
                         "oac3d", # OAC3D in 2022 version
                         "OAC3D", # Output Area Classification -3D
                         "incanon", # Anonymised household income and allowances
                         "p344p", # Gross normal weekly household income - top-coded
                         "P389p", # Normal weekly disposable income
                         "P352p", # Gross current income of household - top-coded
                         "A122", # Tenure
                         "A124", # Cars and Vans
                         "A124p", # Cars and Vans top coded
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
                         "B017",#	Electricity - amount paid in last account, less rebates
                         "B018",#	Gas - amount paid in last account, less rebates
                         "B053u",#	Gas – combined account, amount
                         "B056u",#	Electricity – combined account, amount
                         "B254",	#Electricity â€“ (combined) pre- payment meter, less rebate - ADDED 2021 data?
                         "B255",	#Gas â€“ (combined) pre-payment meter less rebate - ADDED 2021 data?
                         "B226",	#Electricity â€“  (separate) pre-payment meter, less rebate - ADDED 2021 data?
                         "B227",	#Gas â€“ (separate) pre-payment meter less rebate - ADDED 2021 data?
                         "B489",#	Bottled gas for central  heating - amount  - ADDED 2021 data?
                         "B490",#	Oil for central heating cost last quarter  - ADDED 2021 data?
                         "C44211t",	#Second dwelling: electricity account payment	gas/electric  - ADDED 2021 data?
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
                         "P493p",
                         "P492p",
                         "P431p",
                         "P431", # Main source of income
                         "A117p",
                         "A118p",
                         "A143p",
                         "A162p",
                         "P200p",
                         "A111p",
                         "A114p")


  columns_to_select2 = lowercase_CP(columns_to_select)

  household_char = dplyr::select(households, dplyr::any_of(columns_to_select2))

  missing_cols = columns_to_select[!columns_to_select %in% names(household_char)]
  if(length(missing_cols) > 0){
    warning("Ths columns are missing: ", paste(missing_cols, collapse = ", "))
  }

  #Fix OAC3D
  if(is.null(household_char$OAC3D)){
    household_char$OAC3D = household_char$oac3d
  }
  if(is.null(household_char$OAC)){
    household_char$OAC = household_char$OAC3D
  }

  #Fix Income
  if(is.null(household_char$incanon)){
    household_char$incanon = household_char$anon_income
  }

  #Fix Upper Case
  cols = names(household_char)[grepl("^[cpba]",names(household_char))]
  cols = cols[cols != "case"]

  for(j in 1:ncol(household_char)){
    nm = names(household_char)[j]
    if(nm %in% cols){
      names(household_char)[j] = paste0(toupper(substr(nm,1,1)),substr(nm,2,nchar(nm)))
      message(nm," renamed to ",names(household_char)[j])
    }
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

  # Fix Housing Type
  household_char$A116 = as.character(household_char$A116)
  if(any(as.character(0:6) %in% household_char$A116)){
    housing_types <- data.frame(
      id = 0:6,
      names = c(
        "Not Recorded",
        "Whole house,bungalow-detached",
        "Whole house,bungalow-semi-detached",
        "Whole house,bungalow-terraced",
        "Purpose-built flat maisonette",
        "Part of house converted flat",
        "Others"
      ),
      stringsAsFactors = FALSE
    )
    hh_match = housing_types$names[match(household_char$A116,housing_types$id)]
    household_char$A116 = ifelse(is.na(hh_match),household_char$A116,household_char$A116)
  }

  # Fix Tenures
  household_char$A122 = as.character(household_char$A122)
  if(any(as.character(0:8) %in% household_char$A122)){
    housing_types <- data.frame(
      id = 0:8,
      names = c(
        "Not Recorded",
        "LA (furnished unfurnished)",
        "Housing Assn (furnished unfurnished)",
        "Priv. rented (unfurnished)",
        "Priv. rented (furnished)",
        "Owned with mortgage",
        "Owned by rental purchase",
        "Owned outright",
        "Rent free"
      ),
      stringsAsFactors = FALSE
    )
    hh_match = housing_types$names[match(household_char$A122,housing_types$id)]
    household_char$A122 = ifelse(is.na(hh_match),household_char$A122,household_char$A122)
  }


  people = lcfs$dvper

  #Fix Upper Case
  cols = names(people)[grepl("^[cpba]",names(people))]
  cols = cols[cols != "case"]

  for(j in 1:ncol(people)){
    nm = names(people)[j]
    if(nm %in% cols){
      names(people)[j] = paste0(toupper(substr(nm,1,1)),substr(nm,2,nchar(nm)))
      message(nm," renamed to ",names(people)[j])
    }
  }



  # Still Null in 2022-23 data
  if(is.null(people$A012p)){
    people$A012p = NA
  }

  if(inherits(people$A003,"numeric")){
    PeopleLookup = data.frame(id = 1:2,
                           names = c("HRP",
                                     "Not HRP"))
    people$A003 = factor(PeopleLookup$names[match(people$A003,PeopleLookup$id)], levels = PeopleLookup$names)
  }

  people_summary = summarise_household_composition(people)

  # Check for extreme consumption
  household_char$P600t = remove_extreme_consumption(household_char$P600t, "total")
  household_char$P601t = remove_extreme_consumption(household_char$P601t, "Food")
  household_char$P602t = remove_extreme_consumption(household_char$P602t, "Achohol Tobacco")
  household_char$P603t = remove_extreme_consumption(household_char$P603t, "Clothes")
  household_char$P604t = remove_extreme_consumption(household_char$P604t, "Housing")
  household_char$P605t = remove_extreme_consumption(household_char$P605t, "Furnishings")
  household_char$P606t = remove_extreme_consumption(household_char$P606t, "Health")
  household_char$P607t = remove_extreme_consumption(household_char$P607t, "Transport")
  household_char$P608t = remove_extreme_consumption(household_char$P608t, "Communications")
  household_char$P609t = remove_extreme_consumption(household_char$P609t, "Recreation")
  household_char$P610t = remove_extreme_consumption(household_char$P610t, "Ediucation")
  household_char$P611t = remove_extreme_consumption(household_char$P611t, "Restaurant Hotels")
  household_char$P612t = remove_extreme_consumption(household_char$P612t, "Misc")

  household_char$P620tp = remove_extreme_consumption(household_char$P620tp, "Non-Consumption")
  household_char$P630tp = remove_extreme_consumption(household_char$P630tp, "Total Anoymised")
  household_char$A117p = remove_extreme_consumption(household_char$A117p, "new cars")
  household_char$A118p = remove_extreme_consumption(household_char$A118p, "second hand cars")


  lcfs_final = list(household = household_char,
                    people = people_summary,
                    purchases = purchases,
                    flights = flights_summary,
                    accommodation = accommodation)

  lcfs_final


}


lowercase_CP <- function(x) {
  x_orig = x
  is_cp <- grepl("^[CPBA]", x)
  # Only modify elements that start with C or P
  substr(x[is_cp], 1, 1) <- tolower(substr(x[is_cp], 1, 1))
  unique(c(x,x_orig))
}



remove_extreme_consumption = function(x, type = ""){
  #hist(x, seq(min(x)-10, max(x)+10, 10))

  # Reduce anything above the threshold
  threshold <- max(quantile(x, 0.9999), #99.99% percentile
                   2 * quantile(x, 0.995) - quantile(x, 0.005)) # Upper 0.5% + 99% range
  chk = x > threshold
  if(any(chk)){
    message("Reducing ",sum(chk)," values above £",round(threshold)," per week on ",type)
  }
  x[chk] = threshold
  x
}



summarise_household_composition = function(people){

  if(inherits(people$A005p, "factor")){
    people$A005p = as.character(people$A005p)
    people$A005p = ifelse(people$A005p %in% c("80 or older","80 and older"),
                          "81",people$A005p)
    people$A005p = as.integer(people$A005p)
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
    if(x$A005p > 65){
      return(data.frame(case = x$case, hhsize = nrow(x), hhcomp = "OnePersonOver66",
                        hhcomp5 = "Oneperson", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
    } else {
      return(data.frame(case = x$case, hhsize = nrow(x), hhcomp = "OnePersonOther",
                        hhcomp5 = "Oneperson", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
    }
  } else {
    # Check for Children
    if(any(x$A002 %in% c("Son or daughter","Grandson grndaughter"))){
      # Children
      x$dependancy = ifelse(x$A007 %in% c("Not recorded","University/polytechnic, any other higher education college"),
                            "non-dependant","dependant")
      x$dependancy = ifelse(x$A003 == "HRP", "non-dependant", x$dependancy) #HRP can't be dependant e.g. student parent
      x$dependancy = ifelse(x$A005p >= 18, "non-dependant", x$dependancy) #Adults can't be dependant
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
                            hhcomp5 = "Loneparent", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
        } else if (n_adutls == 2 & all(x$A002[x$child_dep == "adult"] %in% c("Person 1","Spouse cohabitee civil partner","Spouse/Cohabitee/Civil Partner"))){
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "CoupleChildren",
                            hhcomp5 = "Couple", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
        } else {
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "OtherChildren",
                            hhcomp5 = "Other", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
        }

      } else {
        # All non-dependant children
        if(n_adutls == 1){
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "LoneParentNonDepChildren",
                            hhcomp5 = "Loneparent", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
        } else if (n_adutls == 2 & all(x$A002[x$child_dep == "adult"] %in% c("Person 1","Spouse cohabitee civil partner","Spouse/Cohabitee/Civil Partner"))){
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "CoupleNonDepChildren",
                            hhcomp5 = "Couple", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
        } else {
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "OtherNoChildren - nondepchild",
                            hhcomp5 = "Other", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
        }
      }

    } else {
      # No Children
      if(all(x$A206 == "Retired/unoccupied and of minimum NI Pension age")){
        if(all(x$A002 != "Non-relative")){
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "FamilyOver66", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
        } else {
          return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "OtherIncStudentOrOver66 - retired", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
        }
      } else if (nrow(x) == 2 & all(x$A002 %in% c("Person 1","Spouse cohabitee civil partner","Spouse/Cohabitee/Civil Partner"))){
        return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "CoupleNoChildren", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
      } else if (all(x$A007 == "University/polytechnic, any other higher education college")){
        return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "OtherIncStudentOrOver66 - students", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
      } else {
        return(data.frame(case = x$case[1], hhsize = nrow(x), hhcomp = "OtherNoChildren", HRPemploy = x$A200[x$A003 == "HRP"], HRPethnic = x$A012p[x$A003 == "HRP"]))
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

  fly_21$case = as.character(fly_21$case)
  fly_22$case = as.character(fly_22$case)

  hh_21 = add_component_costs(hh_21, yrs[1])
  hh_22 = add_component_costs(hh_22, yrs[2])


  hh = dplyr::bind_rows(list(hh_21, hh_22))
  hh$household_id = 1:nrow(hh)

  hh$Tenure5 = convert_housing_tenure(hh$A122)
  if(is.null(hh$A124)){
    hh$CarVan5 = convert_car_ownership(hh$A124p)
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
                         "OAC3D",
                         "Gorx",
                         "URGridEWp",
                         "URGridSCp",
                         "incanon",
                         "A122", # Tenure
                         "A094",
                         "A091",
                         "A116", # Dwelling Category
                         "P200p", # Number of rooms
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
                         "spend_housing_gaselec",
                         "spend_housing_otherfuels",
                         "spend_housing_gaselec_seconddwelling",
                         "spend_housing_gaselec_rebates",
                         "spend_transport_vehiclepurchase",
                         "spend_transport_optranequip_fuel",
                         "spend_transport_optranequip_other",
                         "spend_transport_services_pt",
                         "spend_transport_services_air",
                        "A117p", # New Cars
                        "A118p", # Second Hand Cars
                        "A162p", # Motor Cycles
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
                     rooms = P200p,
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
                     cars_new = A117p,
                     cars_secondhand = A118p,
                     motorcycles = A162p
                     )


  hh
}


selected_lcfs = function(lcfs){

  res = list("2010/11" = subset_lcfs(lcfs, yrs = c("2010","2011")),
             "2012/13" = subset_lcfs(lcfs, yrs = c("2012","2013")),
             "2014/15" = subset_lcfs(lcfs, yrs = c("2014","20152016")),
             "2016/17" = subset_lcfs(lcfs, yrs = c("20162017","20172018")),
             "2018/19" = subset_lcfs(lcfs, yrs = c("20182019","20192020")),
             "2020/21" = subset_lcfs(lcfs, yrs = c("20202021","20212022")),
             "2022/23" = subset_lcfs(lcfs, yrs = c("20222023","20232024"))
  )

  res

}

add_component_costs = function(hh, yr = "2010"){

  hh$spend_housing_gaselec = rowSums(dplyr::select(hh, dplyr::any_of(c(
    "B017",#	Electricity - amount paid in last account, less rebates
    "B018",#	Gas - amount paid in last account, less rebates
    "B053u",#	Gas – combined account, amount
    "B056u",#	Electricity – combined account, amount
    #"B221", # Unknown so excluding from energy
    #"B222", # Unknown
    "B254",	#Electricity â€“ (combined) pre- payment meter, less rebate - ADDED 2021 data?
    "B255",	#Gas â€“ (combined) pre-payment meter less rebate - ADDED 2021 data?
    "B226",	#Electricity â€“  (separate) pre-payment meter, less rebate - ADDED 2021 data?
    "B227",	#Gas â€“ (separate) pre-payment meter less rebate - ADDED 2021 data?
    "C45114t",#	Electricity slot meter payment
    "B170", #  Gas - amount paid in last account
    "B173", #  Gas - amount last rebate (to remove)
    "B175", #  Electricity - amount paid in last account
    "B178", #  Electricity - amount last rebate (to remove)
    "P249t", # Gas - slot meter payments less rebate
    "P250t" # Electricity - slot meter payments less rebate
  ))), na.rm = TRUE)

  hh$spend_housing_otherfuels = rowSums(dplyr::select(hh, dplyr::any_of(c(
    "B489",#	Bottled gas for central  heating - amount  - ADDED 2021 data?
    "B490",#	Oil for central heating cost last quarter  - ADDED 2021 data?
    "C45222t",#	Bottled gas - other
    "C45312t",#	Paraffin
    "C45411t",#	Coal and coke
    "C45412t",#	Wood and peat
    "C45511t"#	Hot water, steam and ice
  ))), na.rm = TRUE)

  hh$spend_housing_gaselec_seconddwelling = rowSums(dplyr::select(hh, dplyr::any_of(c(
    "C44211t",	#Second dwelling: electricity account payment	gas/electric  - ADDED 2021 data?
    "C45112t",#	Second dwelling: electricity account payment
    "C45212t"#	Second dwelling: gas account payment
  ))), na.rm = TRUE)

  hh$spend_housing_gaselec_rebates = rowSums(dplyr::select(hh, dplyr::any_of(c(
    "B173", #  Gas - amount last rebate (to remove)
    "B178", #  Electricity - amount last rebate (to remove)
    "P249t", # Gas - slot meter payments less rebate
    "P250t" # Electricity - slot meter payments less rebate
  ))), na.rm = TRUE)

  hh$spend_housing_gaselecfuel = rowSums(dplyr::select(hh, dplyr::any_of(c(
    "spend_housing_gaselec",
    "spend_housing_otherfuels",
    "spend_housing_gaselec_seconddwelling"
  ))))

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

  #TODO: Documentation seems to be wrong for post 2022 as C73311t C73312t is specified but B487 B488 is in the data and makes the total numbers add up
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

  if(sum(hh$diff != 0) > 1){ # Allow one error for subpression of excessive consumption
    stop("Transport spending components don't add up in ",yr," ",sum(hh$diff == 0)," cases")
    foo = hh[,c("case","P607t","spend_transport_vehiclepurchase","spend_transport_optranequip_fuel","spend_transport_optranequip_other","spend_transport_services_air","spend_transport_services_pt","diff")]
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
