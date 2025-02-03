library(targets)
library(sf)
library(dplyr)
tar_load(lcfs)
#tar_load(census21_synth_households)

census21_synth_households = res_com

# Check match quality

lcfs_2021 = lcfs$`20182019`
lcfs_2022 = lcfs$`20192020`

#TODO: Dwelling Type missing in 21/22 and 20/21 data (A116), emaield ONS


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

census_unique = unique(census21_synth_households[,c("Acc","Tenure","Car","hhComp","hhSize")])

# 1670 unique househol types

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


hh$Tenure = convert_housing_tenure(hh$A122)

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

hh$Car = convert_car_ownership(hh$A124)
table(hh$Car, useNA = "always")

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

hh$hhSize = convert_household_size(hh$hhsize)
table(hh$hhSize, useNA = "always")

# Accomdataion Type
# TODO: Missing in 2020 and 2021 data.
table(hh$A116, useNA = "always")
table(census_unique$Acc, useNA = "always")

convert_housing_type <- function(housing_type) {
  # Define a named vector for mapping
  mapping <- c(
    "Not Recorded" = NA,
    "Whole house,bungalow-detached" = "Detached",
    "Whole hse,bungalow-semi-dtchd" = "Semi",
    "Whole house,bungalow-terraced" = "Terraced",
    "Purpose-built flat maisonette" = "Flat",
    "Part of house converted flat" = "Flat",
    "Others" = "caravan"
  )

  # Convert the input vector using the mapping
  converted_type <- mapping[housing_type]

  # Return the converted vector
  return(converted_type)
}

#
hh$Acc <- convert_housing_type(hh$A116)

table(hh$Acc, useNA = "always")

# Household compositon
hh$hhComp = as.character(hh$hhcomp)
hh$hhComp = gsub(" - students","",hh$hhComp)
hh$hhComp = gsub(" - retired","",hh$hhComp)
hh$hhComp = gsub(" - nondepchild","",hh$hhComp)

hh_unique = hh[,c("Acc","Tenure","Car","hhComp","hhSize")]
#names(hh_unique) = gsub("hhcomp","hhComp",names(hh_unique))

hh_unique = unique(hh_unique)


census_unique$Acc = as.character(census_unique$Acc)
census_unique$hhComp = as.character(census_unique$hhComp)

head(hh_unique)
head(census_unique)

census_unique$in_census = TRUE
hh_unique$in_lcfs = TRUE

foo = dplyr::full_join(hh_unique, census_unique, by = c("Acc","Tenure","Car","hhComp","hhSize"))

summary(unique(census_unique$Acc) %in% unique(hh_unique$Acc))
summary(unique(census_unique$Tenure) %in% unique(hh_unique$Tenure))
summary(unique(census_unique$Car) %in% unique(hh_unique$Car))
summary(unique(census_unique$hhComp) %in% unique(hh_unique$hhComp))

# Expanc Cenus
cenus_long = census21_synth_households[rep(1:nrow(census21_synth_households), times = census21_synth_households$households),]
cenus_long$households = NULL

Acc = cenus_long$Acc[162]
Tenure = cenus_long$Tenure[162]
hhComp = cenus_long$hhComp[162]
hhSize = cenus_long$hhSize[162]
Car = cenus_long$Car[162]

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


nms_Acc <- c("Detached", "Semi", "Terraced", "Flat", "caravan")
nms_Tenure <- c("Outright", "Mortgage", "Social_rented", "Private_rented")
nms_hhComp <- c("OnePersonOther", "OnePersonOver66", "CoupleNoChildren", "CoupleChildren",
                "CoupleNonDepChildren", "FamilyOver66", "LoneParent", "LoneParentNonDepChildren",
                "OtherChildren", "OtherIncStudentOrOver66", "OtherNoChildren")
nms_hhSize <- c("p1", "p2", "p3", "p4+")
nms_CarVan <- c("car0", "car1", "car2", "car3+")

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

# Similarity matrices can be precomputed and stored outside the function if they remain constant
similarity_matrices <- list(
  Acc = similarity_Acc,
  Tenure = similarity_Tenure,
  hhComp = similarity_hhComp,
  hhSize = similarity_hhSize,
  Car = similarity_CarVan
)

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


future::plan("multisession")
x = furrr::future_pmap(.l = list(Acc = cenus_long$Acc,
                   Tenure = cenus_long$Tenure,
                   hhComp = cenus_long$hhComp,
                   hhSize = cenus_long$hhSize,
                   Car = cenus_long$Car), .f = match_hh_census2_optimized,
                hh = hh[,c("household_id","Acc","Tenure","hhComp","hhSize","Car")],
                similarity_matrices = similarity_matrices,
                .progress = TRUE, .options = furrr::furrr_options(seed = TRUE))
future::plan("sequential")

x= bind_rows(x)



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

