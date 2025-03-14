lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[lookup_postcode_OA_LSOA_MSOA_2021$ladnm == "Leeds",]
cenus_leeds = census21_synth_households[census21_synth_households$LSOA %in% lookup_postcode_OA_LSOA_MSOA_2021$lsoa21cd,]

cenus_leeds = left_join(cenus_leeds, lookup_postcode_OA_LSOA_MSOA_2021, by = c("LSOA" = "lsoa21cd"))
head(cenus_leeds )

cenus_leeds = cenus_leeds[,c("LSOA","msoa21cd","households","hhComp15","Tenure5","NSSEC10",
                             "hhSize5","CarVan5","OAC11combine","upper_limit","lower_limit",
                             "upper_limit99","lower_limit99")]

cenus_leeds_unique = cenus_leeds |>
  group_by(LSOA,msoa21cd,households,hhComp15,Tenure5,NSSEC10,
           hhSize5,CarVan5,OAC11combine,upper_limit,lower_limit,
           upper_limit99,lower_limit99) |>
  summarise(households = sum(households))

write.csv(cenus_leeds_unique,"data/census_leeds.csv", row.names = FALSE)

hh_leeds = hh[,c("household_id","Tenure5","hhComp15","hhSize5","CarVan5","OAC","annual_income")]
hh_leeds$annual_income = round(hh_leeds$annual_income / 100) * 100

write.csv(hh_leeds,"data/hh_leeds.csv", row.names = FALSE)

incomes_2018_leeds = incomes_2018[incomes_2018$MSOA11 %in% cenus_leeds$msoa21cd,]
write.csv(incomes_2018_leeds,"data/incomes_leeds.csv", row.names = FALSE)

# Example E02002330
lsoa_example = unique(cenus_leeds$LSOA[cenus_leeds$msoa21cd == "E02002330"])
cenus_example = cenus_long[cenus_long$LSOA %in% lsoa_example,]
incomes_example = incomes_2018_leeds[incomes_2018_leeds$MSOA11 == "E02002330",]

library(ggplot2)

ggplot(cenus_example, aes(x = annual_income)) +
  geom_histogram() +
  scale_x_continuous(labels = scales::label_comma()) +
  xlab("Income") +
  geom_vline(xintercept = incomes_example$total_annual_income) +
  geom_vline(xintercept = incomes_example$lower_limit  ) +
  geom_vline(xintercept = incomes_example$upper_limit ) +
  geom_vline(xintercept = unique(cenus_example$upper_limit99) ) +
  geom_vline(xintercept = unique(cenus_example$lower_limit99) )


Tenure5 =  cenus_example$Tenure5[1]
hhComp15 =  cenus_example$hhComp15[1]
hhSize5 =  cenus_example$hhSize5[1]
CarVan5 =  cenus_example$CarVan5[1]
OACs =  cenus_example$OAC11combine[1]
upper_limit = cenus_example$upper_limit99[1]
lower_limit = cenus_example$lower_limit99[1]

test_match = match_hh_census(Tenure5,hhComp15,hhSize5,CarVan5,OACs, upper_limit, lower_limit, hh, similarity_matrices)
hh_match = hh[hh$household_id %in% unlist(test_match$household_id),]
