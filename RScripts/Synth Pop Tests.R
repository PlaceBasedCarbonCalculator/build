library(targets)
library(sf)
library(dplyr)
tar_load(lcfs)
tar_load(census21_synth_households)
tar_load(lsoa21_OAC21_summary)
tar_load(oac11lsoa21)

#census21_synth_households = res_com

# Check match quality

lcfs_2021 = lcfs$`20182019`
lcfs_2022 = lcfs$`20192020`

#Dwelling Type missing in 21/22 and 20/21 data (A116), emaield ONS - removed in 2020
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

#census_unique = unique(census21_synth_households[,c("hhComp15","hhSize5","Tenure5","CarVan5","NSSEC10")])

# 3769 unique househol types

# Match Laddifications

# Tenure
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


hh$Tenure5 = convert_housing_tenure(hh$A122)

# Car/Van
unique(census_unique$Car)
unique(hh$a143p)
unique(hh$A124)
# Also A124???

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

hh$CarVan5 = convert_car_ownership(hh$A124)
table(hh$CarVan5, useNA = "always")

#hhousehodl size

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

hh$hhSize5 = convert_household_size(hh$hhsize)
table(hh$hhSize5, useNA = "always")


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

hh$NSSEC10 = convert_NSSEC(hh$A094)
# Lot of NAs - mostly retired a few sick
table(hh$A091, useNA = "always")
table(as.character(hh$A091[is.na(hh$NSSEC10)]), useNA = "always")

hh$NSSEC10[is.na(hh$NSSEC10)] = "Retired/Sick"

table(hh$NSSEC10, useNA = "always")

# Accomdataion Type
# TODO: Missing in 2020 and 2021 data.
# table(hh$A116, useNA = "always")
# table(census_unique$Acc, useNA = "always")
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
#
# #
# hh$Acc <- convert_housing_type(hh$A116)
#
# table(hh$Acc, useNA = "always")

# Household compositon
hh$hhComp15 = as.character(hh$hhcomp)
hh$hhComp15 = gsub(" - students","",hh$hhComp15)
hh$hhComp15 = gsub(" - retired","",hh$hhComp15)
hh$hhComp15 = gsub(" - nondepchild","",hh$hhComp15)

hh_unique = hh[,c("NSSEC10","Tenure5","CarVan5","hhComp15","hhSize5")]
#names(hh_unique) = gsub("hhcomp","hhComp",names(hh_unique))


# OAC
hh$OAC = tolower(hh$OAC)
hh$OAC = trimws(hh$OAC)
hh$OAC[hh$OAC == ""] = NA
hh = hh[!is.na(hh$OAC),]


hh_unique = unique(hh_unique)
# 1164 unique housholds

#census_unique$Acc = as.character(census_unique$Acc)
#census_unique$hhComp = as.character(census_unique$hhComp)

# head(hh_unique)
# head(census_unique)

# census_unique$in_census = TRUE
# hh_unique$in_lcfs = TRUE
#
# foo = dplyr::full_join(hh_unique, census_unique, by = c("NSSEC10","Tenure5","CarVan5","hhComp15","hhSize5"))

# Too many mismatches, try without NSSEC

hh_unique = hh[,c("OAC","Tenure5","CarVan5","hhComp15","hhSize5")]
hh_unique = unique(hh_unique)

census_unique = unique(census21_synth_households[,c("hhComp15","hhSize5","Tenure5","CarVan5","LSOA")])

oac11lsoa21$OAC11combine = sapply(oac11lsoa21$OAC11CD, function(x){
  x = x[order(x$Freq, decreasing = TRUE),]
  x = paste(x$OAC11CD, collapse = " ")
  x
})
oac11lsoa21$OAC11CD = NULL

census_unique = dplyr::left_join(census_unique, oac11lsoa21, by = c("LSOA" = "LSOA21CD"))
census_unique$LSOA = NULL

census_unique = unique(census_unique)

#summary(unique(census_unique$Acc) %in% unique(hh_unique$Acc))
summary(unique(census_unique$Tenure5) %in% unique(hh_unique$Tenure5))
summary(unique(census_unique$CarVan5) %in% unique(hh_unique$CarVan5))
summary(unique(census_unique$hhComp15) %in% unique(hh_unique$hhComp15))
summary(unique(census_unique$NSSEC10) %in% unique(hh_unique$NSSEC10))

# Add OAC
#census21_synth_households = dplyr::left_join(census21_synth_households, lsoa21_OAC21_summary, by = c("LSOA" = "lsoa21cd"))
census21_synth_households = dplyr::left_join(census21_synth_households, oac11lsoa21, by = c("LSOA" = "LSOA21CD"))

# Expanc Cenus
cenus_long = census21_synth_households[rep(1:nrow(census21_synth_households), times = census21_synth_households$households),]
cenus_long$households = NULL


#nms_Acc <- c("Detached", "Semi", "Terraced", "Flat", "caravan")
nms_Tenure <- c("Outright", "Mortgage", "Social_rented", "Private_rented")
nms_hhComp <- c("OnePersonOther", "OnePersonOver66", "CoupleNoChildren", "CoupleChildren",
                "CoupleNonDepChildren", "FamilyOver66", "LoneParent", "LoneParentNonDepChildren",
                "OtherChildren", "OtherIncStudentOrOver66", "OtherNoChildren")
nms_hhSize <- c("p1", "p2", "p3", "p4+")
nms_CarVan <- c("car0", "car1", "car2", "car3+")
nms_NSSEC <- c("DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15")

# Example similarity matrices for each variable
# similarity_Acc = matrix(c(
#   1, 0.8, 0.6, 0.4, 0.2,
#   0.8, 1, 0.8, 0.6, 0.4,
#   0.6, 0.8, 1, 0.8, 0.6,
#   0.4, 0.6, 0.8, 1, 0.8,
#   0.2, 0.4, 0.6, 0.8, 1
# ), nrow = 5, dimnames = list(nms_Acc, nms_Acc))

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

similarity_NSSEC <- matrix(c(
  1,   0,   0,   0,     0,   0,   0,   0,   0,   0,
  0,   1,   0.8, 0.4,   0,   0,   0,   0,   0,   0,
  0,   0.8, 1,   0.8, 0.4,   0,   0,   0,   0,   0,
  0,   0.4, 0.8, 1,   0.8, 0.4,   0,   0,   0,   0,
  0,   0,   0.4, 0.8, 1,   0.8, 0.4,   0,   0,   0,
  0,   0,   0,   0.4, 0.8, 1,   0.8, 0.4,   0,   0,
  0,   0,   0,   0,   0.4, 0.8,   1, 0.8,   0,   0,
  0,   0,   0,   0,   0,   0.4, 0.8,   1,   0,   0,
  0,   0,   0,   0,   0,   0,   0,     0,   1,   0,
  0,   0,   0,   0,   0,   0,   0,     0,   0,   1
),  nrow = 10, byrow = TRUE, dimnames = list(nms_NSSEC, nms_NSSEC))

similarity_NSSEC_retired <- matrix(c(
    1,   0,   0,   0,     0,   0,   0,   0,   0,   0, 0,
    0,   1,   0.8, 0.4,   0,   0,   0,   0,   0,   0, 0.5,
    0,   0.8, 1,   0.8, 0.4,   0,   0,   0,   0,   0, 0.5,
    0,   0.4, 0.8, 1,   0.8, 0.4,   0,   0,   0,   0, 0.5,
    0,   0,   0.4, 0.8, 1,   0.8, 0.4,   0,   0,   0, 0.5,
    0,   0,   0,   0.4, 0.8, 1,   0.8, 0.4,   0,   0, 0.5,
    0,   0,   0,   0,   0.4, 0.8,   1, 0.8,   0,   0, 0.5,
    0,   0,   0,   0,   0,   0.4, 0.8,   1,   0,   0, 0.5,
    0,   0,   0,   0,   0,   0,   0,     0,   1,   0, 0.5,
    0,   0,   0,   0,   0,   0,   0,     0,   0,   1, 0
  ),  nrow = 10, byrow = TRUE, dimnames = list(nms_NSSEC, c(nms_NSSEC,"Retired/Sick")))


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



#hh$OAC = hh$OAC3D



# Similarity matrices can be precomputed and stored outside the function if they remain constant
similarity_matrices <- list(
  NSSEC10 = similarity_NSSEC,
  NSSEC10r = similarity_NSSEC_retired,
  Tenure5 = similarity_Tenure,
  hhComp15 = similarity_hhComp,
  hhSize5 = similarity_hhSize,
  CarVan5 = similarity_CarVan,
  OAC = similarity_OAC

)




Tenure5 =  cenus_long$Tenure5[6041]
hhComp15 =  cenus_long$hhComp15[6041]
hhSize5 =  cenus_long$hhSize5[6041]
CarVan5 =  cenus_long$CarVan5[6041]
OACs =  cenus_long$OAC11combine[6041]

match_hh_census5 <- function(Tenure5,hhComp15,hhSize5,CarVan5,OACs, hh, similarity_matrices) {


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

  if (nrow(hh_sub) > 0) {
    return(data.frame(
      n_match = nrow(hh_sub),
      match_score = max_score / 5,
      household_id = sample(hh_sub$household_id, 1)
    ))
  } else {
    message(unlist(input_vars))
    stop()
  }
}

# test run
match_hh_census5(Tenure5,hhComp15,hhSize5,CarVan5,OACs, hh, similarity_matrices)
#
# lsoa_test = sample(unique(census21_synth_households$LSOA),1)
# cenus_long_test = cenus_long[cenus_long$LSOA %in% lsoa_test,]


t1 = Sys.time()
future::plan("multisession")
x = furrr::future_pmap(.l = list(
  Tenure5 = cenus_long$Tenure5[1:100000],
  hhComp15 = cenus_long$hhComp15[1:100000],
  hhSize5 = cenus_long$hhSize5[1:100000],
  CarVan5 = cenus_long$CarVan5,
  OACs = cenus_long$OAC11combine
  ),

  .f = match_hh_census5,
  hh = hh[,c("household_id","Tenure5","hhComp15","hhSize5","CarVan5","OAC")],
  similarity_matrices = similarity_matrices,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE,
                                  scheduling  = 1))
future::plan("sequential")
t2 = Sys.time()
message(round(difftime(t2,t1, units = "mins"),2), " min")

x= bind_rows(x)

# r in (function (.l, .f, ..., .progress = FALSE)  :
#         ℹ In index: 6041.
#       Caused by error in `apply()`:
#         ! dim(X) must have a positive length

stop("End of test")

cenus_long = cbind(cenus_long, x)

cenus_long = left_join(cenus_long, hh[,c("household_id","P600t","P601t","P602t",
                                          "P603t","P604t","P605t",
                                          "P606t","P607t","P608t",
                                          "P609t","P610t","P611t",
                                          "P612t","P620tp","P630tp",
                                          "p493p","p492p")], by = "household_id")

cenus_long$food_co2 = cenus_long$P601t * 52 * 0.497  +#simple average of all food not weighted
  cenus_long$P602t * 52 * 0.628

lsoa_food = cenus_long %>%
  group_by(LSOA) %>%
  summarise(food_co2 = sum(food_co2, na.rm = TRUE))

tar_load(bounds_lsoa21_generalised)
tar_load(population)

bounds = bounds_lsoa21_generalised[bounds_lsoa21_generalised$LSOA21CD %in% lsoa_food$LSOA,]
population = population[population$LSOA21CD %in% lsoa_food$LSOA,]
population = population[population$year == 2021,]
population = population[,c("LSOA21CD","all_ages")]

bounds = left_join(bounds, population, by = "LSOA21CD")
bounds = left_join(bounds, lsoa_food, by = c("LSOA21CD" = "LSOA"))
bounds$co2_percap = bounds$food_co2 / bounds$all_ages

library(tmap)
tm_shape(bounds) +
  tm_fill("co2_percap", title = "Food and Drink\n kgCO2e per person",
          palette = "-RdYlBu",
          #midpoint =  800,
          style = "jenks") +
          #breaks = c(290,740,810,860,900,1030)) +
  tm_borders() +
  tm_scale_bar() +
  tm_compass(position = c("left","top"))


convert_nssec <- function(classifications) {
  # Define the mapping between original and new classifications
  mapping <- c(
    "Large employers and higher managerial occupations" = "L1L2L3",
    "Higher Professional occupations" = "L1L2L3",
    "Large Employers & Higher Managerial" = "L1L2L3",
    "Higher Professionals" = "L1L2L3",
    "Lower managerial and professional occupations" = "L4L5L6",
    "Lower Managerial & Professionals" = "L4L5L6",
    "Intermediate occupations" = "L7",
    "Intermediate" = "L7",
    "Small employers and own account workers" = "L8L9",
    "Small Employers and Own Account Workers" = "L8L9",
    "Lower supervisory and technical occupations" = "L10L11",
    "Lower Supervisory & Technical" = "L10L11",
    "Semi-routine occupations" = "L12",
    "Semi-Routine"  = "L12",
    "Routine occupations" = "L13",
    "Routine"  = "L13",
    "Never worked and long term unemployed" = "L14",
    "Never Worked and Long-Term Unemployed" = "L14",
    "Students" = "L15",
    "Not recorded" = "DNA",
    "Occupation not stated" = "DNA",
    "Not classified for other reasons" = "DNA",
    "Not classifiable for other reasons"  = "DNA"
  )

  # Map the classifications using the defined mapping
  new_classifications <- mapping[classifications]

  # Handle any classifications not found in the mapping
  new_classifications[is.na(new_classifications)] <- "Unknown classification"

  return(new_classifications)
}

match_hh_cenus = function(Acc,Tenure,hhComp,hhSize,Car, hh){
  hh_sub = hh[hh$Acc == Acc &
                hh$Tenure == Tenure &
                hh$hhComp == hhComp &
                hh$hhSize == hhSize &
                hh$Car == Car, ]
  if(nrow(hh_sub) > 0){
    return(data.frame(n_match = nrow(hh_sub),
                      match_values = 5,
                      household_id = hh_sub$household_id[sample(1:nrow(hh_sub), 1)]))
  } else {
    # Find next best match
    mthc = matrix(c(hh$Acc == Acc,
                    hh$Tenure == Tenure,
                    hh$hhComp == hhComp,
                    hh$hhSize == hhSize,
                    hh$Car == Car), ncol = 5)
    rsums = rowSums(mthc)
    maxsum = max(rsums)

    hh_sub = hh[rsums == maxsum,]
    if(nrow(hh_sub) > 0){
      return(data.frame(n_match = 0,
                        match_values = maxsum,
                        household_id = hh_sub$household_id[sample(1:nrow(hh_sub), 1)]))
    } else {
      stop("Error")
    }



  }


}


match_hh_census2 = function(Acc, Tenure, hhComp, hhSize, Car, hh) {


  nms_Acc = c("Detached","Semi","Terraced","Flat","caravan")
  nms_Tenure = c("Outright","Mortgage","Social_rented","Private_rented")
  nms_hhComp = c("OnePersonOther","OnePersonOver66","CoupleNoChildren","CoupleChildren",
                 "CoupleNonDepChildren","FamilyOver66","LoneParent","LoneParentNonDepChildren",
                 "OtherChildren","OtherIncStudentOrOver66","OtherNoChildren")
  nms_hhSize = c("p1","p2","p3","p4+")
  nms_CarVan = c("car0","car1","car2","car3+")

  # Example similarity matrices for each variable
  similarity_Acc = matrix(c(
    1, 0.8, 0.6, 0.4, 0.2,
    0.8, 1, 0.8, 0.6, 0.4,
    0.6, 0.8, 1, 0.8, 0.6,
    0.4, 0.6, 0.8, 1, 0.8,
    0.2, 0.4, 0.6, 0.8, 1
  ), nrow = 5, dimnames = list(nms_Acc, nms_Acc))

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



  # Calculate similarity scores for each row
  similarity_scores = rowSums(cbind(
    sapply(hh$Acc, function(x) similarity_Acc[Acc, x]),
    sapply(hh$Tenure, function(x) similarity_Tenure[Tenure, x]),
    sapply(hh$hhComp, function(x) similarity_hhComp[hhComp, x]),
    sapply(hh$hhSize, function(x) similarity_hhSize[hhSize, x]),
    sapply(hh$Car, function(x) similarity_CarVan[Car, x])
  ))

  # Find the row with the highest similarity score
  max_score = max(similarity_scores)
  hh_sub = hh[similarity_scores == max_score, ]

  if (nrow(hh_sub) > 0) {
    return(data.frame(
      n_match = sum(similarity_scores == max_score),
      match_values = max_score,
      household_id = hh_sub$household_id[sample(1:nrow(hh_sub), 1)]
    ))
  } else {
    return(NULL)
  }
}


match_hh_census2_optimized <- function(Acc, Tenure, hhComp, hhSize, Car, hh, similarity_matrices) {


  # Create named vectors for the input variables to match the dimension names in the similarity matrices
  input_vars <- list(
    Acc = Acc,
    Tenure = Tenure,
    hhComp = hhComp,
    hhSize = hhSize,
    Car = Car
  )

  # Initialize similarity scores as a numeric vector
  similarity_scores <- numeric(nrow(hh))

  # Calculate similarity scores using vectorized operations
  for (var in names(input_vars)) {
    sim_matrix <- similarity_matrices[[var]]
    input_value <- input_vars[[var]]
    hh_values <- hh[[var]]

    # Map the input value and household values to their corresponding indices
    input_index <- which(rownames(sim_matrix) == input_value)
    hh_indices <- match(hh_values, colnames(sim_matrix))

    # Extract the similarity scores for all households at once
    scores <- sim_matrix[input_index, hh_indices]
    similarity_scores <- similarity_scores + scores
  }

  # Find the maximum similarity score
  max_score <- max(similarity_scores, na.rm = TRUE)

  # Get all households with the maximum similarity score
  hh_sub <- hh[similarity_scores == max_score, ]

  if (nrow(hh_sub) > 0) {
    return(data.frame(
      n_match = nrow(hh_sub),
      match_values = max_score,
      household_id = sample(hh_sub$household_id, 1)
    ))
  } else {
    message(unlist(input_vars))
    stop()
  }
}

NSSEC10 = as.character(cenus_sub$NSSEC10[1])
Tenure5 = as.character(cenus_sub$Tenure5[1])
hhComp15 = as.character(cenus_sub$hhComp15[1])
hhSize5 = cenus_sub$hhSize5[1]
CarVan5 = cenus_sub$CarVan5[1]
subgroup = cenus_sub$subgroup[1]

match_hh_census4 <- function(NSSEC10,Tenure5,hhComp15,hhSize5,CarVan5,subgroup, hh, similarity_matrices) {



  # Create named vectors for the input variables to match the dimension names in the similarity matrices
  input_vars <- list(
    NSSEC10 = NSSEC10,
    NSSEC10r = NSSEC10,
    Tenure5 = Tenure5,
    hhComp15 = hhComp15,
    hhSize5 = hhSize5,
    CarVan5 = CarVan5,
    OAC = subgroup[[1]]
  )

  # Initialize similarity scores as a numeric vector
  similarity_scores <- numeric(nrow(hh))
  similarity_scores_r <- numeric(nrow(hh))

  # Calculate similarity scores using vectorized operations
  for (var in names(input_vars)[3:6]) {
    sim_matrix <- similarity_matrices[[var]]
    input_value <- input_vars[[var]]
    hh_values <- hh[[var]]

    if(var == "OAC"){
      #Special case LSOAs can have multiple OACs,
      input_value$subgroup = as.character(input_value$subgroup)
      sim_matrix[,!colnames(sim_matrix) %in% input_value$subgroup] = 0

      # Map the input value and household values to their corresponding indices
      input_index <- which(rownames(sim_matrix) %in% input_value$subgroup)
      hh_indices <- match(hh_values, colnames(sim_matrix))

      # Extract the similarity scores for all households at once
      scores <- sim_matrix[input_index, hh_indices]


    } else {
      # Map the input value and household values to their corresponding indices
      input_index <- which(rownames(sim_matrix) == input_value)
      hh_indices <- match(hh_values, colnames(sim_matrix))

      # Extract the similarity scores for all households at once
      scores <- sim_matrix[input_index, hh_indices]
    }


    if(var %in% c("NSSEC","NSSECr")){
      scores[is.na(scores)] = 0
    }


    # Survey has lots of missing NSSEC for retired/sick people, while Census classifies by previous job
    # Creat two sets of scores then select based on household composition
    if(var != "NSSECr"){
      similarity_scores <- similarity_scores + scores
      message(var," ",class(similarity_scores))
    }

    if(var != "NSSEC"){
      similarity_scores_r <- similarity_scores_r + scores
    }



  }

  # Find the maximum similarity score
  max_score <- max(similarity_scores, na.rm = TRUE)

  # Get all households with the maximum similarity score
  hh_sub <- hh[similarity_scores == max_score, ]

  if (nrow(hh_sub) > 0) {
    return(data.frame(
      n_match = nrow(hh_sub),
      match_values = max_score,
      household_id = sample(hh_sub$household_id, 1)
    ))
  } else {
    message(unlist(input_vars))
    stop()
  }
}
