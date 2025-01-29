library(targets)
library(sf)
tar_load(lcfs)
tar_load(census21_synth_households)

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

hh_unique = hh[,c("Acc","Tenure","Car","hhComp","hhSize")]
#names(hh_unique) = gsub("hhcomp","hhComp",names(hh_unique))

hh_unique = unique(hh_unique)

head(hh_unique)
head(census_unique)
census_unique$Acc = as.character(census_unique$Acc)
census_unique$hhComp = as.character(census_unique$hhComp)

census_unique$in_census = TRUE
hh_unique$in_lcfs = TRUE

foo = dplyr::full_join(hh_unique, census_unique, by = c("Acc","Tenure","Car","hhComp","hhSize"))

summary(unique(census_unique$Acc) %in% unique(hh_unique$Acc))
summary(census_unique$Tenure %in% hh_unique$Tenure)
summary(census_unique$Car %in% hh_unique$Car)
summary(unique(census_unique$hhComp) %in% unique(hh_unique$hhComp))
