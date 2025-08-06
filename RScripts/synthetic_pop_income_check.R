library(dplyr)
library(targets)

tar_load(synth_households_lcfs_2020)
tar_load(income_msoa)
tar_load(lookup_postcode_OA_LSOA_MSOA_2021)

lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[,c("lsoa21cd","msoa21cd","ladnm")]
lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[!duplicated(lookup_postcode_OA_LSOA_MSOA_2021$lsoa21cd),]


synth_households_lcfs_2020$annual_income = synth_households_lcfs_2020$incanon * 52

synth_households_lcfs_2020 = dplyr::left_join(synth_households_lcfs_2020, lookup_postcode_OA_LSOA_MSOA_2021, by = c("LSOA21CD" = "lsoa21cd"))

msoa_income = synth_households_lcfs_2020 |>
  group_by(msoa21cd) |>
  summarise(#income_min = min(annual_income, na.rm = TRUE),
            #income_max = max(annual_income, na.rm = TRUE),
            #income_mean = mean(annual_income, na.rm = TRUE),
            income_median = median(annual_income, na.rm = TRUE),
            )
summary(msoa_income)


incomes_2020 = income_msoa[income_msoa$year == 2020,]

#Note 2011 to 2021 join
joineds = left_join(incomes_2020, msoa_income, by = c("MSOA11" = "msoa21cd"))

plot(joineds$total_annual_income, joineds$income_median)

library(ggplot2)
ggplot(joineds, aes(y = income_median, x = total_annual_income)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Observed average income") +
  ylab("Modeled average income") +
  scale_y_continuous(labels = scales::label_number(prefix = "£", big.mark = ","),
                     expand = c(0,0), limits = c(20000,110000)) +
  scale_x_continuous(labels = scales::label_number(prefix = "£", big.mark = ","),
                     expand = c(0,0),  limits = c(20000,110000)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme_minimal()

ggsave("plots/synth_pop_income_check_mipfp.png", dpi = 300, width = 10, height = 4)

# Check match scores
summary(synth_households_lcfs_2020$match_score)
message(round(sum(synth_households_lcfs_2020$match_score == 1) /
                nrow(synth_households_lcfs_2020) * 100,2),"% perfrect score")
message(round(sum(synth_households_lcfs_2020$match_score < 0.95) /
                nrow(synth_households_lcfs_2020) * 100,2),"% less than 0.95 score")
message(sum(synth_households_lcfs_2020$match_score == min(synth_households_lcfs_2020$match_score)),
        " have min score of ",
              min(synth_households_lcfs_2020$match_score))
