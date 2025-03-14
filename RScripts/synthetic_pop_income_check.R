head(cenus_long)

tar_load(lookup_postcode_OA_LSOA_MSOA_2021)

lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[,c("lsoa21cd","msoa21cd","ladnm")]
lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[!duplicated(lookup_postcode_OA_LSOA_MSOA_2021$lsoa21cd),]


cenus_long$annual_income = cenus_long$incanon * 52

cenus_long = dplyr::left_join(cenus_long, lookup_postcode_OA_LSOA_MSOA_2021, by = c("LSOA" = "lsoa21cd"))

msoa_income = cenus_long |>
  group_by(msoa21cd) |>
  summarise(income_min = min(annual_income, na.rm = TRUE),
            income_max = max(annual_income, na.rm = TRUE),
            income_mean = mean(annual_income, na.rm = TRUE),
            income_median = median(annual_income, na.rm = TRUE),
            )
summary(msoa_income)


tar_load(income_msoa)

incomes_2018 = income_msoa[income_msoa$year == 2018,]

#Note 2011 to 2021 join
joineds = left_join(incomes_2018, msoa_income, by = c("MSOA11" = "msoa21cd"))

plot(joineds$total_annual_income, joineds$income_median)
# COmplety bollocks
