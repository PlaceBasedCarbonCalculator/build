library(targets)
library(ggplot2)
tar_load(lookup_OA_LSOA_MSOA_classifications)

dat = ""

tar_load(parameters)

tar_load(us)

us = load_US(wave = "l")

us_household = us$hhresp


path = file.path(parameters$path_data,"income")
income2020 = readxl::read_excel(file.path(path, "income2020.xlsx"), "Total annual income")

names(income2020) = c("MSOA11","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                      "total_annual_income","upper_limit","lower_limit","interval")
income2020 = income2020[5:nrow(income2020), ]
income2020 = income2020[!is.na(income2020$MSOAname),]
income2020$total_annual_income = as.numeric(income2020$total_annual_income)
income2020$upper_limit = as.numeric(income2020$upper_limit)
income2020$lower_limit = as.numeric(income2020$lower_limit)


us_household = us_household[,c("l_hidp","l_fihhmnnet1_dv","l_fihhmngrs_dv")]
us_household$l_fihhmnnet1_dv = as.numeric(as.character(us_household$l_fihhmnnet1_dv))
us_household$l_fihhmngrs_dv = as.numeric(as.character(us_household$l_fihhmngrs_dv))

quantile(us_household$l_fihhmngrs_dv * 12, seq(0,1,0.1), na.rm = TRUE)


sipher_household = read.csv("D:/OneDrive - University of Leeds/Data/SIPHER Syntheic Population/Household/20230413HHEW_population.csv")

sipher_household = dplyr::left_join(sipher_household, us_household, by = c("hidp" = "l_hidp"))


lookup = lookup_OA_LSOA_MSOA_classifications[,c("LSOA11CD","MSOA11CD")]
lookup = lookup[!duplicated(lookup$LSOA11CD),]

sipher2 = dplyr::left_join(sipher_household, lookup, by = c("ZoneID" = "LSOA11CD"))

sipher_summary = dplyr::group_by(sipher2, MSOA11CD)
sipher_summary = dplyr::summarise(sipher_summary,
                                  household_income_net_mean = mean(l_fihhmnnet1_dv * 12, na.rm = TRUE),
                                  household_income_net_median = median(l_fihhmnnet1_dv * 12, na.rm = TRUE),
                                  household_income_gross_mean = mean(l_fihhmngrs_dv * 12, na.rm = TRUE),
                                  household_income_gross_median = median(l_fihhmngrs_dv * 12, na.rm = TRUE),
)


check_msoa = dplyr::left_join(income2020, sipher_summary, by = c("MSOA11" = "MSOA11CD"))


ggplot(data = check_msoa, aes(y = total_annual_income, x = household_income_gross_mean)) +
  geom_point(size = 0.5) +
  geom_abline(slope=1, intercept=0, col = "red") +
  xlab("SIPHER/Understanding Society Mean Gross Income") +
  ylab("ONS Income estimates") +
  xlim(20000,100000) +
  ylim(20000,100000) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

check_msoa = check_msoa[order(check_msoa$total_annual_income),]
check_msoa$index = 1:nrow(check_msoa)

ggplot(data = check_msoa, aes(x = index)) +
  geom_line(aes(y = household_income_gross_mean), colour = "black") +
  geom_line(aes(y = upper_limit), colour = "red") +
  geom_line(aes(y = lower_limit), colour = "blue") +
  geom_line(aes(y = total_annual_income), colour = "green") +
  ylab("Annual Income Estimate") +
  xlab("MSOA Number") +
  ggtitle("England and Wales")

# SIPHER housheold has the same problem as SIPHER
