generate_income_bands <- function(N, mean_income, lower_band, upper_band, id = "missing") {
  # Calculate the standard deviation from the 95% confidence interval
  sd_income <- (upper_band - lower_band) / 3.92

  # Generate normally distributed incomes
  incomes <- rnorm(N, mean = mean_income, sd = sd_income)

  #incomes[incomes < 0] = 0


    # Create income bands
  bands <- c(seq(0,        60000, by =   5000),
             seq( 70000,  100000, by =  10000),
             200000, 10000000)

  # Assign each income to a band
  income_bands <- cut(incomes, breaks = bands, include.lowest = TRUE, right = FALSE)

  income_table = as.data.frame(table(income_bands))

  income_table$income_bands = as.character(income_table$income_bands)
  income_table$income_bands = gsub("\\[|\\]|\\)","",income_table$income_bands)
  income_table$income_bands = gsub(",","_",income_table$income_bands)
  income_table$income_bands = gsub(".5e+04","5k",income_table$income_bands, fixed = TRUE)
  income_table$income_bands = gsub("e+04","0k",income_table$income_bands, fixed = TRUE)
  income_table$income_bands = gsub("e+03","k",income_table$income_bands, fixed = TRUE)
  income_table$income_bands = gsub("e+05","00k",income_table$income_bands, fixed = TRUE)
  income_table$income_bands = gsub("e+07","0m",income_table$income_bands, fixed = TRUE)

  income_table$income_bands = paste0("inc",income_table$income_bands)
  income_table$id = id

  result = tidyr::pivot_wider(income_table,
                              names_from = "income_bands",
                              values_from = "Freq",
                              id_cols = "id")


  return(result)
}

make_income_bands_lsoa = function(NSSEC_household,
                             income_msoa,
                             lookup_OA_LSOA_MSOA_classifications,
                             lookup_lsoa_2011_21){

  NSSEC_household$all_households = rowSums(NSSEC_household[,2:ncol(NSSEC_household)], na.rm = TRUE)
  NSSEC_household = NSSEC_household[,c("LSOA21CD","all_households")]

  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA11CD","LSOA21CD")]
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[!duplicated(lookup_lsoa_2011_21$LSOA21CD),]

  lookup_OA_LSOA_MSOA_classifications = lookup_OA_LSOA_MSOA_classifications[,c("LSOA11CD","MSOA11CD")]
  lookup_OA_LSOA_MSOA_classifications = lookup_OA_LSOA_MSOA_classifications[!duplicated(lookup_OA_LSOA_MSOA_classifications$LSOA11CD),]

  lookup = dplyr::left_join(lookup_lsoa_2011_21, lookup_OA_LSOA_MSOA_classifications, by = "LSOA11CD")
  lookup = lookup[,c("LSOA21CD","MSOA11CD")]

  income_msoa = income_msoa[income_msoa$year == 2020,]

  NSSEC_household = dplyr::left_join(NSSEC_household, lookup, by = "LSOA21CD")

  income_lsoa = dplyr::left_join(NSSEC_household, income_msoa, by = c("MSOA11CD" = "MSOA11"))


  income_bands_lsoa = purrr::pmap(.l = list(id = income_lsoa$LSOA21CD,
                                            N = income_lsoa$all_households,
                                            upper_band = income_lsoa$upper_limit,
                                            lower_band = income_lsoa$lower_limit,
                                            mean_income = income_lsoa$total_annual_income),
                                  .f = generate_income_bands,
                                  .progress = TRUE
                                  )

  income_bands_lsoa = dplyr::bind_rows(income_bands_lsoa)

  income_bands_lsoa

}



# # Example usage
# N <- 1000
# mean_income <- 108100
# lower_band <- 89400
# upper_band <- 130600
# #band_width <- 10000
#
# income_data <- generate_income_bands(N, mean_income, lower_band, upper_band)
# income_data <- generate_income_bands(N, 22200, 18200, 27100)
# head(income_data)
#
# hist(income_data$Income, breaks = c(seq(0,       100000, by =   5000),
#                                     seq(110000,  150000, by =  10000)))



#

# us_household = us$hhresp
#
# c("k_hrpid", # household reference person ID
#   "k_hrpno", # Household reference person PNO
#   "k_ncouple_dv", # Number of couples
#   "k_nonepar_dv", # Number of lone parents
#   "k_nkids_dv", # Number of Children
#
#   "k_nemp_dv", # Number employed in the hh
#   "k_nue_dv", # Number not in paid employment
#
#   "k_nkids015", # Number of Kids 0-15
#   "k_npensioner", # Number of pensioners
#   "k_npensioner_dv", # Number of pensionalbe age in hh
#   "k_hhsize", # Houshold size
#   "k_hhtype_dv", # Household compositon LFS-version
#   "k_hsbeds", # number of bedrooms
#   "k_hsrooms", # number of rooms
#   "k_hsowned", # House onwed, mortages, renteed
#   "k_tenure_dv", # Housing tenure
#   "k_xpmg", # Last month mortgage payment
#   "k_rent", # Net amount of last rent payment
#   "k_renhb", # Recived ren rebated or rent allowance
#   "k_rentg", # Gross rent including housing benefit
#   "k_rentgwc", # Weeks covered by gross rent
#   "k_fuelhave1", # Have electricity
#   "k_fuelhave2", # Have gas (inc calor)
#   "k_fuelhave3", # Have oil
#   "k_fuelhave4", # Have other fuel
#   "k_fuelhave69", # Have no fuel
#   "k_xpduely", # spend on gas + electric
#   "k_xpelecy", # spend on electricity
#   "k_xpgasy", # spend on gas
#   "k_xpoily", #spend on oil
#   "k_xpsfly", #spend on other fuel
#   "k_heatch", # has central heating
#   "k_xpfood1_g3", # amount spent on food from supermarket
#   "k_xpfdout_g3", # Amount spent on meals/snakcs outside home
#   "k_xpaltob_g3", # Amount spent alcohol
#   "k_ncars", # Number of cars
#   "k_carval", # Value of vehicles less amount outstadning
#   "k_fihhmngrs_dv", # Gross houhsold income (month)
#   "k_fihmnlabgrs_dv", # Gross houshold labout income
#   "k_fihhmnnet1_dv", # Net income no decudtions (month)
#   "k_fihhmlabnet_dv", # Net income from labour no deductions (month)
#   "k_fihhmngrs_dv", # Gross houshold income housing benefit asjusted
#   "k_rent_dv", # monthly rent
#   "k_rentgrs_dv", # montly rent including housing benefit
#   "k_xpmg_dv", # montlyh mortage inlcuding imputations
#   "k_ctband_dv", # council tax band
#   "k_gor_dv", # Region
#   "k_urban_dv", # Rural Urban
#
#   )
#
# foo = attributes(us_household)$variable.labels
# foo[grepl("de",foo, ignore.case = TRUE)]
