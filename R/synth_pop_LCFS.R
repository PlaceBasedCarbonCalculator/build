match_LCFS_synth_pop = function(census21_synth_households,
                                lcfs,
                                oac11lsoa21,
                                yrs = c("20182019","20192020"),
                                income_msoa,
                                lookup_MSOA_2011_21,
                                lookup_OA_LSOA_MSOA_2021,
                                income_year = 2018){

  lookup_OA_LSOA_MSOA_2021 = lookup_OA_LSOA_MSOA_2021[,c("LSOA21CD","MSOA21CD")]
  lookup_OA_LSOA_MSOA_2021 = lookup_OA_LSOA_MSOA_2021[!duplicated(lookup_OA_LSOA_MSOA_2021$LSOA21CD),]

  lookup_MSOA_2011_21 = lookup_MSOA_2011_21[,c("MSOA11CD","CHNGIND","MSOA21CD" )]
  lookup_MSOA_2011_21 = lookup_MSOA_2011_21[!duplicated(lookup_MSOA_2011_21$MSOA21CD),]

  lookup_OA_LSOA_MSOA_2021 = dplyr::left_join(lookup_OA_LSOA_MSOA_2021, lookup_MSOA_2011_21, by = c("MSOA21CD"))

  income_msoa = income_msoa[income_msoa$year == income_year,]

  income_lsoa = dplyr::left_join(lookup_OA_LSOA_MSOA_2021, income_msoa, by = c("MSOA11CD" = "MSOA11"))
  income_lsoa = income_lsoa[,c("LSOA21CD","upper_limit" ,"lower_limit","total_annual_income")]

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

  hh = rbind(hh_21, hh_22)
  hh$household_id = 1:nrow(hh)

  hh$Tenure5 = convert_housing_tenure(hh$A122)
  hh$CarVan5 = convert_car_ownership(hh$A124)
  hh$hhSize5 = convert_household_size(hh$hhsize)
 # hh$NSSEC10 = convert_NSSEC(hh$A094)

  hh$hhComp15 = as.character(hh$hhcomp)
  hh$hhComp15 = gsub(" - students","",hh$hhComp15)
  hh$hhComp15 = gsub(" - retired","",hh$hhComp15)
  hh$hhComp15 = gsub(" - nondepchild","",hh$hhComp15)

  # OAC
  hh$OAC = tolower(hh$OAC)
  hh$OAC = trimws(hh$OAC)
  hh$OAC[hh$OAC == ""] = NA
  hh = hh[!is.na(hh$OAC),]

  oac11lsoa21$OAC11combine = sapply(oac11lsoa21$OAC11CD, function(x){
    x = x[order(x$Freq, decreasing = TRUE),]
    x = paste(x$OAC11CD, collapse = " ")
    x
  })
  oac11lsoa21$OAC11CD = NULL

  census21_synth_households = dplyr::left_join(census21_synth_households, oac11lsoa21, by = c("LSOA" = "LSOA21CD"))

  nms_Tenure <- c("Outright", "Mortgage", "Social_rented", "Private_rented")
  nms_hhComp <- c("OnePersonOther", "OnePersonOver66", "CoupleNoChildren", "CoupleChildren",
                  "CoupleNonDepChildren", "FamilyOver66", "LoneParent", "LoneParentNonDepChildren",
                  "OtherChildren", "OtherIncStudentOrOver66", "OtherNoChildren")
  nms_hhSize <- c("p1", "p2", "p3", "p4+")
  nms_CarVan <- c("car0", "car1", "car2", "car3+")
  nms_NSSEC <- c("DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15")

  similarity_Tenure = matrix(c(
    1, 0.7, 0.5, 0.3,
    0.7, 1, 0.7, 0.5,
    0.5, 0.7, 1, 0.7,
    0.3, 0.5, 0.7, 1
  ), nrow = 4, dimnames = list(nms_Tenure, nms_Tenure))

  similarity_hhComp = matrix(c(
    1  , 0.9, 0.7, 0.5, 0.3, 0.1, 0.7, 0.7, 0.5, 0.3, 0.1,
    0.9, 1  , 0.9, 0.7, 0.5, 0.3, 0.9, 0.9, 0.7, 0.5, 0.3,
    0.7, 0.9, 1  , 0.9, 0.7, 0.5, 0.9, 0.5, 0.9, 0.7, 0.5,
    0.5, 0.7, 0.9, 1  , 0.9, 0.7, 0.7, 0.9, 0.9, 0.9, 0.7,
    0.3, 0.5, 0.7, 0.9, 1  , 0.9, 0.5, 0.7, 0.9, 0.2, 0.9,
    0.1, 0.3, 0.5, 0.7, 0.9, 1  , 0.3, 0.5, 0.7, 0.9, 0.7,
    0.7, 0.9, 0.9, 0.7, 0.5, 0.3, 1  , 0.9, 0.7, 0.5, 0.3,
    0.7, 0.9, 0.5, 0.9, 0.7, 0.5, 0.9, 1  , 0.9, 0.7, 0.5,
    0.5, 0.7, 0.9, 0.9, 0.9, 0.7, 0.7, 0.9, 1  , 0.9, 0.7,
    0.3, 0.5, 0.7, 0.9, 0.2, 0.9, 0.5, 0.7, 0.9, 1  , 0.9,
    0.1, 0.3, 0.5, 0.7, 0.9, 0.7, 0.3, 0.5, 0.7, 0.9, 1
  ), nrow = 11, dimnames = list(nms_hhComp, nms_hhComp))

  similarity_hhSize = matrix(c(
    1, 0.85, 0.65, 0.45,
    0.85, 1, 0.85, 0.65,
    0.65, 0.85, 1, 0.85,
    0.45, 0.65, 0.85, 1
  ), nrow = 4, dimnames = list(nms_hhSize, nms_hhSize))

  similarity_CarVan = matrix(c(
    1, 0.8, 0.6, 0.4,
    0.8, 1, 0.8, 0.6,
    0.6, 0.8, 1, 0.8,
    0.4, 0.6, 0.8, 1
  ), nrow = 4, dimnames = list(nms_CarVan, nms_CarVan))


  # Define the variables
  variables_21 <- c("1a1", "1a2", "1b1", "1b2", "1c1", "1c2", "2a1", "2a2", "2a3", "2b1", "2b2", "2c1", "2c2", "3a1", "3a2", "3a3", "3a4", "3b1", "3b2", "3c1", "3c2", "4a1", "4a2", "4a3", "4b1", "4b2", "4b3", "4b4", "4c1", "4c2", "5a1", "5a2", "5a3", "5b1", "5b2", "6a1", "6a2", "6a3", "6b1", "6b2", "6b3", "6c1", "6c2", "7a1", "7a2", "7b1", "7b2", "8a1", "8a2", "8b1", "8b2", "8b3")

  variables_11 <- c("1a1","1a2","1a3","1a4","1b1","1b2","1b3","1c1","1c2","1c3","2a1","2a2","2a3","2b1","2b2","2c1","2c2","2c3",
                    "2d1","2d2","2d3","3a1","3a2","3b1","3b2","3b3","3c1","3c2","3d1","3d2","3d3","4a1","4a2","4a3","4b1","4b2",
                    "4c1","4c2","4c3","5a1","5a2","5a3","5b1","5b2","5b3","6a1","6a2","6a3","6a4","6b1","6b2","6b3","6b4","7a1",
                    "7a2","7a3","7b1","7b2","7b3","7c1","7c2","7c3","7d1","7d2","7d3","7d4","8a1","8a2","8b1","8b2","8c1","8c2",
                    "8c3","8d1","8d2","8d3")

  variables = variables_11

  # Initialize the similarity matrix
  similarity_OAC <- matrix(0, nrow = length(variables), ncol = length(variables), dimnames = list(variables, variables))

  # Calculate the similarity matrix
  for (i in seq_along(variables)) {
    for (j in seq_along(variables)) {
      if (variables[i] == variables[j]) {
        similarity_OAC[i, j] <- 1
      } else if (substr(variables[i], 1, 2) == substr(variables[j], 1, 2)) {
        similarity_OAC[i, j] <- 0.8
      } else if (substr(variables[i], 1, 1) == substr(variables[j], 1, 1)) {
        similarity_OAC[i, j] <- 0.5
      } else {
        similarity_OAC[i, j] <- 0
      }
    }
  }


  # Similarity matrices can be precomputed and stored outside the function if they remain constant
  similarity_matrices <- list(
    Tenure5 = similarity_Tenure,
    hhComp15 = similarity_hhComp,
    hhSize5 = similarity_hhSize,
    CarVan5 = similarity_CarVan,
    OAC = similarity_OAC
  )

  # Census Unique Coombinations
  census21_synth_households = dplyr::left_join(census21_synth_households, income_lsoa, by = c("LSOA" = "LSOA21CD"))

  # Go from 95% CI to 99% CI
  census21_synth_households$standard_error = (census21_synth_households$upper_limit - census21_synth_households$lower_limit)/1.96
  census21_synth_households$upper_limit99 = census21_synth_households$total_annual_income + 2.576 * census21_synth_households$standard_error
  census21_synth_households$lower_limit99 = census21_synth_households$total_annual_income - 2.576 * census21_synth_households$standard_error
  census21_synth_households$lower_limit99[census21_synth_households$lower_limit99 < 0 ] =0

  census_unique =  census21_synth_households |>
    dplyr::group_by(hhComp15, Tenure5, hhSize5, CarVan5, OAC11combine, upper_limit99, lower_limit99) |>
    dplyr::summarise(households = sum(households))


  Tenure5 =  census_unique$Tenure5[6041]
  hhComp15 =  census_unique$hhComp15[6041]
  hhSize5 =  census_unique$hhSize5[6041]
  CarVan5 =  census_unique$CarVan5[6041]
  OACs =  census_unique$OAC11combine[6041]
  upper_limit = census_unique$upper_limit99[6041]
  lower_limit = census_unique$lower_limit99[6041]

  # 200 households too rich to be in any LSOA
  # 1550 hosuheoldsn too poor

  #
  #foo = match_hh_census(Tenure5,hhComp15,hhSize5,CarVan5,OACs,upper_limit, lower_limit, hh, similarity_matrices)

  hh$annual_income = hh$incanon * 52

  t1 = Sys.time()
  future::plan("multisession")
  x = furrr::future_pmap(.l = list(
    Tenure5 = census_unique$Tenure5,
    hhComp15 = census_unique$hhComp15,
    hhSize5 = census_unique$hhSize5,
    CarVan5 = census_unique$CarVan5,
    OACs = census_unique$OAC11combine,
    upper_limit = census_unique$upper_limit99,
    lower_limit = census_unique$lower_limit99
  ),

  .f = match_hh_census,
  hh = hh[,c("household_id","Tenure5","hhComp15","hhSize5","CarVan5","OAC","annual_income")],
  similarity_matrices = similarity_matrices,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE,
                                  scheduling  = 1))
  future::plan("sequential")
  t2 = Sys.time()
  message(round(difftime(t2,t1, units = "mins"),2), " min")

  #x = dplyr::bind_rows(x)
  x = data.table::rbindlist(x)
  #message(Sys.time())

  # Expand Census
  cenus_long = census21_synth_households[rep(1:nrow(census21_synth_households), times = census21_synth_households$households),]
  cenus_long$households = NULL

  cenus_long = dplyr::left_join(cenus_long, x,
                                by = c("hhComp15", "Tenure5", "hhSize5", "CarVan5",
                                       "OAC11combine" = "OACs", "upper_limit99" = "upper_limit",
                                       "lower_limit99" = "lower_limit"))
  future::plan("multisession")
  cenus_long$household_id = furrr::future_map_dbl(cenus_long$household_id, function(x){
       sample(x[[1]],1)
  }, .progress = TRUE)
  future::plan("sequential")
  cenus_long = dplyr::left_join(cenus_long, hh[,c("household_id","P600t","P601t","P602t",
                                           "P603t","P604t","P605t",
                                           "P606t","P607t","P608t",
                                           "P609t","P610t","P611t",
                                           "P612t","P620tp","P630tp",
                                           "incanon", # Anonymised household income and allowances
                                           "p344p", # Gross normal weekly household income - top-coded
                                           "p493p","p492p")], by = "household_id")

  cenus_long

}


convert_housing_tenure <- function(housing_tenure) {
  # Define a named vector for mapping
  mapping <- c(
    "Owned outright" = "Outright",
    "Priv. rented (unfurn)" = "Private_rented",
    "LA (furnished unfurnished)" = "Social_rented",
    "Owned with mortgage" = "Mortgage",
    "Hsng Assn (furnished unfrnish)" = "Social_rented",
    "Priv. rented (furnished)" = "Private_rented",
    "Rent free" = "Private_rented",
    "Owned by rental purchase" = "Mortgage"
  )

  # Convert the input vector using the mapping
  converted_tenure <- mapping[as.character(housing_tenure)]
  converted_tenure <- unname(converted_tenure)

  # Return the converted vector
  return(converted_tenure)
}

convert_NSSEC <- function(ns_sec) {
  # Create a named vector for mapping
  mapping <- c(
    "Large employers and higher managerial occupations" = "L1L2L3",
    "Higher Professional occupations" = "L1L2L3",
    "Lower managerial and professional occupations" = "L4L5L6",
    "Intermediate occupations" = "L7",
    "Small employers and own account workers" = "L8L9",
    "Lower supervisory and technical occupations" = "L10L11",
    "Semi-routine occupations" = "L12",
    "Routine occupations" = "L13",
    "Never worked and long term unemployed" = "L14",
    "Students" = "L15",
    "Occupation not stated" = "DNA",
    "Not classified for other reasons" = "DNA"
  )

  # Convert the input NS-SEC classifications to the new classifications
  new_classification <- mapping[ns_sec]
  #new_classification[is.na(new_classification)] = "DNA"
  #new_classification = unname(new_classification)

  return(new_classification)
}

convert_household_size <- function(hhsize) {
  # Define a function to classify the number of cars
  classify_hh <- function(n) {
    if (is.na(n)) {
      return(NA)
    } else if (n == 0) {
      return("p0")
    } else if (n == 1) {
      return("p1")
    } else if (n == 2) {
      return("p2")
    } else if (n == 3) {
      return("p3")
    } else {
      return("p4+")
    }
  }

  # Apply the classification function to the input vector
  classified_hh <- sapply(hhsize, classify_hh)

  # Return the classified vector
  return(classified_hh)
}

convert_car_ownership <- function(num_cars) {
  # Define a function to classify the number of cars
  classify_cars <- function(n) {
    if (is.na(n)) {
      return(NA)
    } else if (n == 0) {
      return("car0")
    } else if (n == 1) {
      return("car1")
    } else if (n == 2) {
      return("car2")
    } else {
      return("car3+")
    }
  }

  # Apply the classification function to the input vector
  classified_cars <- sapply(num_cars, classify_cars)

  # Return the classified vector
  return(classified_cars)
}


# Accommodation Type
# TODO: Missing in 2020 and 2021 data. - Removed by ONS
#
# convert_housing_type <- function(housing_type) {
#   # Define a named vector for mapping
#   mapping <- c(
#     "Not Recorded" = NA,
#     "Whole house,bungalow-detached" = "Detached",
#     "Whole hse,bungalow-semi-dtchd" = "Semi",
#     "Whole house,bungalow-terraced" = "Terraced",
#     "Purpose-built flat maisonette" = "Flat",
#     "Part of house converted flat" = "Flat",
#     "Others" = "caravan"
#   )
#
#   # Convert the input vector using the mapping
#   converted_type <- mapping[housing_type]
#
#   # Return the converted vector
#   return(converted_type)
# }


match_hh_census <- function(Tenure5,hhComp15,hhSize5,CarVan5,OACs, upper_limit, lower_limit, hh, similarity_matrices) {


  # Create named vectors for the input variables to match the dimension names in the similarity matrices
  input_vars <- list(
    Tenure5 = as.character(Tenure5),
    hhComp15 = as.character(hhComp15),
    hhSize5 = hhSize5,
    CarVan5 = CarVan5,
    OAC = unlist(strsplit(OACs," "))
  )

  # Initialize similarity scores as a numeric vector
  similarity_scores <- numeric(nrow(hh))

  # Calculate similarity scores using vectorized operations
  for (var in names(input_vars)) {
    sim_matrix <- similarity_matrices[[var]]
    input_value <- input_vars[[var]]
    hh_values <- hh[[var]]

    if(var == "OAC"){
      #Special case LSOAs can have multiple OACs,
      #input_value$subgroup = as.character(input_value$subgroup)
      #sim_matrix[,!colnames(sim_matrix) %in% input_value$subgroup] = 0
      #sim_matrix[,!colnames(sim_matrix) %in% input_value] = 0

      # Map the input value and household values to their corresponding indices
      input_index <- which(rownames(sim_matrix) %in% input_value)
      hh_indices <- match(hh_values, colnames(sim_matrix))

      # Extract the similarity scores for all households at once
      scores <- sim_matrix[input_index, hh_indices]
      if(inherits(scores,"matrix")){
        scores <- apply(scores, 2, max, na.rm = TRUE)
      }


    } else {
      # Map the input value and household values to their corresponding indices
      input_index <- which(rownames(sim_matrix) == input_value)
      hh_indices <- match(hh_values, colnames(sim_matrix))

      # Extract the similarity scores for all households at once
      scores <- sim_matrix[input_index, hh_indices]
    }

    similarity_scores <- similarity_scores + scores


  }

  # Find the maximum similarity score
  max_score <- max(similarity_scores, na.rm = TRUE)

  # Get all households with the maximum similarity score
  hh_sub <- hh[similarity_scores == max_score, ]

  # Multiple Options so check income
  if(any(hh_sub$annual_income  >= lower_limit)){
    hh_sub <- hh_sub[hh_sub$annual_income  >= lower_limit,]
    max_score = max_score + 1
  } else {
    hh_sub <- hh_sub[hh_sub$annual_income  == max(hh_sub$annual_income),]
    max_score = max_score + min(max(hh_sub$annual_income)/lower_limit,1)
  }

  if(any(hh_sub$annual_income  <= upper_limit)){
    hh_sub <- hh_sub[hh_sub$annual_income  <= upper_limit,]
    max_score = max_score + 1
  } else {
    hh_sub <- hh_sub[hh_sub$annual_income  == min(hh_sub$annual_income),]
    max_score = max_score + min(min(hh_sub$annual_income)/lower_limit,1)
  }


  if (nrow(hh_sub) > 0) {
    return(data.frame(
      Tenure5 = Tenure5,
      hhComp15 = hhComp15,
      hhSize5 = hhSize5,
      CarVan5 = CarVan5,
      OACs = OACs,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      n_match = nrow(hh_sub),
      match_score = max_score / 7,
      household_id = I(list(hh_sub$household_id))
    ))
  } else {
    message(unlist(input_vars))
    stop()
  }
}




