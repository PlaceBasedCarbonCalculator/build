library(ggplot2) # Graphs
library(dplyr) # Data manipulations
library(purrr) # Iteration
library(furrr) # Multi-core iteration
library(future) # Multi-core back end

# Load Data

path = "D:/OneDrive - University of Leeds/Share/Petar"

census = read.csv(file.path(path,"census.csv"))
hh = read.csv(file.path(path,"hh.csv"))
incomes = read.csv(file.path(path,"incomes.csv"))

# For quick running only analyse a single neighbourhood on a single core
single_core = TRUE

if(single_core) {
  census = census[census$msoa21cd == census$msoa21cd[1],] # comment out to do full dataset (slow)
}


# Load Code
source("R/synth_pop_LCFS.R")

# Make Similarity Matrix

nms_Tenure <- c("Outright", "Mortgage", "Social_rented", "Private_rented")
nms_hhComp <- c("OnePersonOther", "OnePersonOver66", "CoupleNoChildren", "CoupleChildren",
                "CoupleNonDepChildren", "FamilyOver66", "LoneParent", "LoneParentNonDepChildren",
                "OtherChildren", "OtherIncStudentOrOver66", "OtherNoChildren")
nms_hhSize <- c("p1", "p2", "p3", "p4+")
nms_CarVan <- c("car0", "car1", "car2", "car3+")

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


variables <- c("1a1","1a2","1a3","1a4","1b1","1b2","1b3","1c1","1c2","1c3","2a1","2a2","2a3","2b1","2b2","2c1","2c2","2c3",
                  "2d1","2d2","2d3","3a1","3a2","3b1","3b2","3b3","3c1","3c2","3d1","3d2","3d3","4a1","4a2","4a3","4b1","4b2",
                  "4c1","4c2","4c3","5a1","5a2","5a3","5b1","5b2","5b3","6a1","6a2","6a3","6a4","6b1","6b2","6b3","6b4","7a1",
                  "7a2","7a3","7b1","7b2","7b3","7c1","7c2","7c3","7d1","7d2","7d3","7d4","8a1","8a2","8b1","8b2","8c1","8c2",
                  "8c3","8d1","8d2","8d3")

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

# To save time only consider unique combinations of input variables
census_unique =  unique(census[,c("hhComp15", "Tenure5", "hhSize5", "CarVan5", "OAC11combine", "upper_limit99", "lower_limit99")])


# Match survey data to census data
# Single core version (for debugging) around 2 minutes for a single neighbourhood (540 rows)
# See furrr::future_pmap for multicore version


if(single_core) {
  x = purrr::pmap(.l = list(
    Tenure5 = census_unique$Tenure5,
    hhComp15 = census_unique$hhComp15,
    hhSize5 = census_unique$hhSize5,
    CarVan5 = census_unique$CarVan5,
    OACs = census_unique$OAC11combine,
    upper_limit = census_unique$upper_limit99,
    lower_limit = census_unique$lower_limit99
  ),

  .f = match_hh_census,
  hh = hh,
  similarity_matrices = similarity_matrices,
  .progress = TRUE)

} else {
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
  hh = hh,
  similarity_matrices = similarity_matrices,
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE,
                                  scheduling  = 1))
  future::plan("sequential")
}


x = dplyr::bind_rows(x)

# Convert to one row for each household
cenus_long = census[rep(1:nrow(census), times = census$households),]
cenus_long$households = NULL

# Join on survey data
cenus_long = dplyr::left_join(cenus_long, x,
                              by = c("hhComp15", "Tenure5", "hhSize5", "CarVan5",
                                     "OAC11combine" = "OACs", "upper_limit99" = "upper_limit",
                                     "lower_limit99" = "lower_limit"))

# Each hosuhde has  list of possible matches, pick one at random
select_id = function(lst){
  sample(unlist(lst),1)
}

if(single_core) {
  cenus_long$household_id_single = purrr::map_int(cenus_long$household_id, select_id, .progress = TRUE)
} else {
  future::plan("multisession")
  cenus_long$household_id_single = furrr::future_map_int(cenus_long$household_id, select_id, .progress = TRUE)
  future::plan("sequential")
}


# Join on hh.csv data
cenus_long = dplyr::left_join(cenus_long, hh[,c("household_id","annual_income")], by = c("household_id_single" = "household_id"))


if(single_core) {
  # Plots the results (this only works for a single neighbourhood)
  ggplot(cenus_long, aes(x = annual_income)) +
    geom_histogram() +
    scale_x_continuous(labels = scales::label_comma()) +
    xlab("Income") +
    geom_vline(xintercept = unique(cenus_long$lower_limit), colour = "#BB0000") +
    geom_vline(xintercept = unique(cenus_long$upper_limit), colour = "#BB0000") +
    geom_vline(xintercept = unique(cenus_long$upper_limit99), colour = "#000000") +
    geom_vline(xintercept = unique(cenus_long$lower_limit99), colour = "#000000")

} else {
  # Plot fo corrletaion for all neighbouhoods
  census_summary = cenus_long |>
    dplyr::group_by(msoa21cd) |>
    dplyr::summarise(annual_income = mean(annual_income, na.rm = TRUE))

  census_summary = dplyr::left_join(census_summary, incomes, by = c("msoa21cd" = "MSOA11"))

  ggplot(census_summary, aes(y = annual_income, x = total_annual_income)) +
    geom_point() +
    xlab("Observed average income") +
    ylab("Modeled average income") +
    geom_abline(intercept = 0, slope = 1, color = "red")
}
