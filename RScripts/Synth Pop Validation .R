library(sf)
library(targets)
library(dplyr)
library(tmap)
library(tidyr)
library(ggplot2)
library(purrr)

tar_source()

tar_load(synth_households_lcfs_2020)
tar_load(synth_households_lcfs_2018)
tar_load(bounds_lsoa21_super_generalised)
tar_load(oac11lsoa21)

tar_load(domestic_gas)
tar_load(domestic_electricity)

domestic_gas = domestic_gas[domestic_gas$year == 2021, ]
domestic_electricity = domestic_electricity[domestic_electricity$year == 2021, ]

# Things to check HRPemploy HRPethnic NSSEC8_HRP dwelling_type rooms URGridEWp
# Mulivaraite check Household Composition (6 Variables)	Tenure	Cars & Vans	91.5%
collapsehhcomp = function(x){

  mattab = c("OnePersonOver66" = "OnePerson",
             "OnePersonOther" = "OnePerson",
             "FamilyOver66" = "FamilyOver66",
             "CoupleNoChildren" = "CoupleFamily",
             "CoupleChildren" = "CoupleFamily",
             "CoupleNonDepChildren" = "CoupleFamily",
             "LoneParent" = "LoneParent",
             "LoneParentNonDepChildren" = "LoneParent",
             "OtherChildren" = "Other6",
             "OtherNoChildren" = "Other6",
             "OtherIncStudentOrOver66" = "Other6")

  y = mattab[match(x, names(mattab))]
  y = unname(y)
  y

}



synth_households_lcfs_2020$hhComp6 <- collapsehhcomp(synth_households_lcfs_2020$hhComp15)

syth20_muti_summary = synth_households_lcfs_2020 |>
  group_by(LSOA21CD, hhComp6, Tenure5, CarVan5) |>
  summarise(households = n())

names(oac11lsoa21)[2] = "oac11_census"

synth_households_lcfs_2020 = left_join(synth_households_lcfs_2020, oac11lsoa21, by = c("LSOA21CD"))
# Check OAC
synth_households_lcfs_2020$OAC_valid = map2_lgl(synth_households_lcfs_2020$OAC3D,synth_households_lcfs_2020$oac11_census, .f = function(x,y){x %in% toupper(y$OAC11CD)}, .progress = TRUE )

summary(synth_households_lcfs_2020$OAC_valid)
# Mode    FALSE     TRUE
# logical   284398 24451016

mulicensus = read_Tenure_CarVan_hhComp6()
names(mulicensus) = paste0("census_",names(mulicensus))

mulicensus_join = left_join(mulicensus, syth20_muti_summary, by = c("census_LSOA21CD" = "LSOA21CD",
                                                                    "census_hhComp6" = "hhComp6",
                                                                    "census_Tenure5" = "Tenure5",
                                                                    "census_CarVan5" = "CarVan5"))
mulicensus_join$name = paste0(mulicensus_join$census_hhComp6," ",mulicensus_join$census_Tenure5," ",mulicensus_join$census_CarVan5)

mulicensus_join$households[is.na(mulicensus_join$households)] = 0

mulicensus_join = mulicensus_join[mulicensus_join$census_households > 0 & mulicensus_join$households > 0,]

mulicensus_join$census_hhComp6[mulicensus_join$census_hhComp6 == "Other6"] = "Other"
mulicensus_join$census_hhComp6[mulicensus_join$census_hhComp6 == "CoupleFamily"] = "Couple Family"
mulicensus_join$census_hhComp6[mulicensus_join$census_hhComp6 == "LoneParent"] = "Lone Parent"
mulicensus_join$census_hhComp6[mulicensus_join$census_hhComp6 == "FamilyOver66"] = "Family Over 66 yrs"
mulicensus_join$census_hhComp6[mulicensus_join$census_hhComp6 == "OnePerson"] = "One Person"
mulicensus_join$census_hhComp6[mulicensus_join$census_hhComp6 == "Other6"] = "Other"
mulicensus_join$census_Tenure5[mulicensus_join$census_Tenure5 == "privaterented"] = "Private Rented"
mulicensus_join$census_Tenure5[mulicensus_join$census_Tenure5 == "socialrented"] = "Social Rented"
mulicensus_join$census_Tenure5[mulicensus_join$census_Tenure5 == "mortgage"] = "Owned with mortgage"
mulicensus_join$census_Tenure5[mulicensus_join$census_Tenure5 == "outright"] = "Owned outright"

mod1 = lm(mulicensus_join$census_households ~ mulicensus_join$households)
summary(mod1)
rmse <- sqrt(mean((mulicensus_join$census_households - mulicensus_join$households)^2))
r2 <- summary(mod1)$r.squared

ggplot(mulicensus_join, aes(x = census_households, y = households, colour = census_hhComp6, shape = census_Tenure5)) +
  geom_point(size = 0.2) +
  annotate("text", x = 25, y = 290,
           label = paste("R² =", round(r2, 2))) +
  annotate("text", x = 30, y = 260,
           label = paste("RMSE =", round(rmse, 2))) +
  scale_x_continuous(limits=c(0, 300), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0, 300), expand = c(0, 0)) +
  xlab("Number of households 2021 Census") +
  ylab("Number of households Synthetic Population") +
  labs(colour='Household\nComposition') +
  labs(shape='Tenure') +
  geom_abline(slope=1, intercept=0, colour = "black") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  guides(shape = guide_legend(override.aes = list(size = 1)))

ggsave("plots/multivariate_sythpop.png", dpi = 600, width = 8, height = 4)

rural_urban = read.csv("../inputdata/rural_urban/Rural_Urban_Classification_(2021)_of_LSOAs_in_EW.csv")
rural_urban = rural_urban[,c("LSOA21CD","Urban_rural_flag")]

rooms = read.csv("../inputdata/population/census2021EW_Households_Rooms9_LSOA.csv")
rooms = rooms[,c("Lower.layer.Super.Output.Areas.Code",
                 "Number.of.rooms..Valuation.Office.Agency...9.categories..Code","Observation")]
names(rooms) = c("LSOA21CD","rooms","households")

rooms_summary = rooms |>
  group_by(LSOA21CD) |>
  summarise(total_households = sum(households),
            total_rooms = sum(households * rooms),
            mean_rooms = total_rooms / total_households
            )

ethonly = read.csv("../inputdata/population/census2021EW_Residents_Ethnicity_LSOA.csv")
names(ethonly) = c("LSOA21CD","LSOA21NM","ethnic6CD","ethnic6","residents")
ethonly = ethonly[,c("LSOA21CD","ethnic6","residents")]
ethonly$ethnic6 = simplify_ethnic6(ethonly$ethnic6)
ethonly = ethonly[!ethonly$ethnic6 == "DNA",]
ethonly = pivot_wider(ethonly, names_from = "ethnic6", values_from = "residents")
names(ethonly) = paste0("cenus_",names(ethonly))
ethonly$cenus_total = rowSums(ethonly[,2:6])



syth20_summary = synth_households_lcfs_2020 |>
  group_by(LSOA21CD) |>
  summarise(households = n(),
            rural = sum(URGridEWp == "Rural", na.rm = TRUE),
            urban = sum(URGridEWp == "Urban", na.rm = TRUE),
            ru_other = sum(is.na(URGridEWp)),
            white = sum(HRPethnic == "White", na.rm = TRUE),
            mixedrace = sum(HRPethnic == "Mixed race", na.rm = TRUE),
            asian = sum(HRPethnic == "Asian or Asian British", na.rm = TRUE),
            black = sum(HRPethnic == "Black or Black British", na.rm = TRUE),
            otherethnic = sum(HRPethnic == "Other ethnic group", na.rm = TRUE),
            ethinc_na = sum(is.na(HRPethnic)),
            Employee = sum(HRPemploy == "Employee"),
            Selfemployed = sum(HRPemploy == "Self-employed or employer"),
            Unemployed = sum(HRPemploy %in% c("Out of employment, seeking work within last 4 weeks","Out of employment, waiting to start a job already obtained","Unoccupied")),
            Sick = sum(HRPemploy == "Sick or injured"),
            Retired = sum(HRPemploy == "Retired including Job Release Scheme"),
            gaselec = sum(spend_housing_gaselec[spend_housing_gaselec > 0] + spend_housing_gaselec_rebates[spend_housing_gaselec > 0])


            )

syth18_summary = synth_households_lcfs_2018 |>
  group_by(LSOA21CD) |>
  summarise(households = n(),
            rooms_total = sum(rooms),
            rooms_mean = rooms_total / households)

syth18_summary = left_join(syth18_summary, rooms_summary, by = "LSOA21CD")

mod2 = lm(syth18_summary$rooms_mean ~ syth18_summary$mean_rooms)
rmse <- sqrt(mean((syth18_summary$rooms_mean - syth18_summary$mean_rooms)^2))
r2 <- summary(mod2)$r.squared

ggplot(syth18_summary) +
  geom_point(aes(x = mean_rooms, y = rooms_mean), colour = "black", shape = ".") +
  annotate("text", x = 1.85, y = 7.5,
           label = paste("R² =", round(r2, 2))) +
  annotate("text", x = 2, y = 7,
           label = paste("RMSE =", round(rmse, 2))) +
  scale_x_continuous(limits=c(1, 8), expand = c(0, 0)) +
  scale_y_continuous(limits=c(1, 8), expand = c(0, 0)) +
  geom_abline(slope=1, intercept=0, colour = "red") +
  xlab("Mean rooms per household 2021 Census") +
  ylab("Mean rooms per household Synthetic Population")

ggsave("plots/rooms_synthpop.png", dpi = 600, width = 4, height = 4)

syth20_summary = left_join(syth20_summary, rural_urban, by = "LSOA21CD")
syth20_summary = left_join(syth20_summary, ethonly, by = c("LSOA21CD" = "cenus_LSOA21CD"))


syth20_summary$p_urban = syth20_summary$urban / syth20_summary$households * 100
syth20_summary$p_rural = syth20_summary$rural / syth20_summary$households * 100

syth20_summary$p_white = syth20_summary$white / syth20_summary$households * 100
syth20_summary$p_white_census = syth20_summary$cenus_White / syth20_summary$cenus_total * 100

syth20_summary$p_black = syth20_summary$black / syth20_summary$households * 100
syth20_summary$p_black_census = syth20_summary$cenus_Black / syth20_summary$cenus_total * 100

syth20_summary$p_asian = syth20_summary$asian / syth20_summary$households * 100
syth20_summary$p_asian_census = syth20_summary$cenus_Asian / syth20_summary$cenus_total * 100

rmse_white <- sqrt(mean((syth20_summary$p_white_census - syth20_summary$p_white)^2))
r2_white <- summary(lm(syth20_summary$p_white ~ syth20_summary$p_white_census))$r.squared

rmse_black <- sqrt(mean((syth20_summary$p_black_census - syth20_summary$p_black)^2))
r2_black <- summary(lm(syth20_summary$p_black ~ syth20_summary$p_black_census))$r.squared

rmse_asian <- sqrt(mean((syth20_summary$p_asian_census - syth20_summary$p_asian)^2))
r2_asian <- summary(lm(syth20_summary$p_asian ~ syth20_summary$p_asian_census))$r.squared

ggplot(syth20_summary) +
  geom_point(aes(x = p_white_census, y = p_white), colour = "red", shape = ".") +
  geom_point(aes(x = p_asian_census, y = p_asian), colour = "blue", shape = ".") +
  geom_point(aes(x = p_black_census, y = p_black), colour = "green", shape = ".") +
  scale_x_continuous(limits=c(0, 101), expand = c(0, 0)) +
  scale_y_continuous(limits=c(0, 100), expand = c(0, 0)) +
  annotate("text", x = 33, y = 95, colour = "red",
           label = paste("White R² =", round(r2_white, 2),"  RMSE =", round(rmse_white, 2))) +
  annotate("text", x = 33, y = 90,  colour = "blue",
           label = paste("Asian R² =", round(r2_black, 2)," RMSE =", round(rmse_black, 2))) +
  annotate("text", x = 32, y = 85, colour = "green",
           label = paste("Black R² =", round(r2_asian, 2)," RMSE =", round(rmse_asian, 2))) +
  geom_abline(slope=1, intercept=0, colour = "black") +
  xlab("Percentage in 2021 Census") +
  ylab("Percentage in Synthetic Population")

ggsave("plots/ethnic_synthpop.png", dpi = 600, width = 4, height = 4)

summary(lm(syth20_summary$p_white_census ~ syth20_summary$p_white))
summary(lm(syth20_summary$p_white_census ~ syth20_summary$p_white))
cor(syth20_summary$p_white_census, syth20_summary$p_white)


bounds = left_join(bounds_lsoa21_super_generalised, syth20_summary, by = "LSOA21CD")

m1 = tm_shape(bounds[bounds$Urban_rural_flag == "Urban",]) +
  tm_fill("p_urban",
          fill.scale = tm_scale_intervals(values = "brewer.yl_or_rd",
                                          breaks = seq(0,100,10)),
          fill.legend = tm_legend(
            "% of Urban\nHouseholds",
            position = tm_pos_out(pos.h = "right", pos.v = "top"),
            bg.color = "white")

          ) +
  tm_shape(bounds[bounds$Urban_rural_flag == "Rural",]) +
  tm_fill("p_rural",
          fill.scale = tm_scale_intervals(values = "brewer.pu_bu_gn",
                                          breaks = seq(0,100,10)),
          fill.legend = tm_legend(
            "% of Rural\nHouseholds",
            position = tm_pos_out(pos.h = "right", pos.v = "bottom"),
            bg.color = "white")

  ) +
  tm_options(frame = FALSE)



tmap_save(m1,"plots/rural_urban_sythpop.png", dpi = 600, width = 4, height = 4)


econ_activity = read.csv("../inputdata/population/census2021EW_RefPerson_EconomicActivity12_LSOA.csv")
names(econ_activity) = c("LSOA21CD","dud1","dud2","econ_activity","households")
econ_activity = econ_activity[,c("LSOA21CD","econ_activity","households")]

econ_activity$econ_activity[econ_activity$econ_activity == "Economically active (excluding full-time students): In employment: Employee"] = "Employee"
econ_activity$econ_activity[econ_activity$econ_activity == "Economically active (excluding full-time students): In employment: Self-employed with employees"] = "Selfemployed"
econ_activity$econ_activity[econ_activity$econ_activity == "Economically active (excluding full-time students): In employment: Self-employed without employees"] = "Selfemployed"
econ_activity$econ_activity[econ_activity$econ_activity == "Economically active (excluding full-time students): Unemployed: Seeking work or waiting to start a job already obtained: Available to start working within 2 weeks"] = "Unemployed"
econ_activity$econ_activity[econ_activity$econ_activity == "Economically active and a full-time student: In employment"] = "Student"
econ_activity$econ_activity[econ_activity$econ_activity == "Economically active and a full-time student: Unemployed: Seeking work or waiting to start a job already obtained: Available to start working within 2 weeks"] = "Student"
econ_activity$econ_activity[econ_activity$econ_activity == "Economically inactive: Long-term sick or disabled"] = "sick"
econ_activity$econ_activity[econ_activity$econ_activity == "Economically inactive: Looking after home or family"] = "carer"
econ_activity$econ_activity[econ_activity$econ_activity == "Economically inactive: Other"] = "inactiveOther"
econ_activity$econ_activity[econ_activity$econ_activity == "Economically inactive: Retired"] = "Retired"
econ_activity$econ_activity[econ_activity$econ_activity == "Economically inactive: Student"] = "Student"
econ_activity$econ_activity[econ_activity$econ_activity == "Does not apply"] = "DNA"

econ_activity = econ_activity |>
  group_by(LSOA21CD, econ_activity) |>
  summarise(households = sum(households))

econ_activity = econ_activity |>
  pivot_wider(names_from = "econ_activity", values_from = "households")

econ_activity$totalecon = rowSums(econ_activity[,2:ncol(econ_activity)], na.rm = TRUE)
names(econ_activity) = paste0(names(econ_activity),"_census")

syth20_summary = left_join(syth20_summary, econ_activity, by = c("LSOA21CD" = "LSOA21CD_census"))

syth20_summary$p_Employee = syth20_summary$Employee / syth20_summary$households * 100
syth20_summary$p_Employee_census = syth20_summary$Employee_census / syth20_summary$totalecon_census * 100

syth20_summary$p_Selfemployed = syth20_summary$Selfemployed / syth20_summary$households * 100
syth20_summary$p_Selfemployed_census = syth20_summary$Selfemployed_census / syth20_summary$totalecon_census * 100

syth20_summary$p_Unemployed = syth20_summary$Unemployed / syth20_summary$households * 100
syth20_summary$p_Unemployed_census = syth20_summary$Unemployed_census / syth20_summary$totalecon_census * 100

syth20_summary$p_Retired = syth20_summary$Retired / syth20_summary$households * 100
syth20_summary$p_Retired_census = syth20_summary$Retired_census / syth20_summary$totalecon_census * 100

rmse_Employee <- sqrt(mean((syth20_summary$p_Employee_census - syth20_summary$p_Employee)^2))
r2_Employee <- summary(lm(syth20_summary$p_Employee ~ syth20_summary$p_Employee_census))$r.squared

rmse_Selfemployed <- sqrt(mean((syth20_summary$p_Selfemployed_census - syth20_summary$p_Selfemployed)^2))
r2_Selfemployed <- summary(lm(syth20_summary$p_Selfemployed ~ syth20_summary$p_Selfemployed_census))$r.squared

rmse_Unemployed <- sqrt(mean((syth20_summary$p_Unemployed_census - syth20_summary$p_Unemployed)^2))
r2_Unemployed <- summary(lm(syth20_summary$p_Unemployed ~ syth20_summary$p_Unemployed_census))$r.squared

rmse_Retired <- sqrt(mean((syth20_summary$p_Retired_census - syth20_summary$p_Retired)^2))
r2_Retired <- summary(lm(syth20_summary$p_Retired ~ syth20_summary$p_Retired_census))$r.squared


ggplot(syth20_summary) +
  geom_point(aes(x = p_Employee_census, y = p_Employee), colour = "red", shape = ".") +
  geom_point(aes(x = p_Retired_census, y = p_Retired), colour = "orange", shape = ".") +
  geom_point(aes(x = p_Selfemployed_census, y = p_Selfemployed), colour = "blue", shape = ".") +
  geom_point(aes(x = p_Unemployed_census, y = p_Unemployed), colour = "green", shape = ".") +

  scale_x_continuous(limits=c(0, 100), expand = c(0, 2)) +
  scale_y_continuous(limits=c(0, 100), expand = c(0, 0)) +
  annotate("text", x = 27 +10, y = 98, colour = "red",
           label = paste("Employee R² =", round(r2_Employee, 2),"  RMSE =", round(rmse_Employee, 2))) +
  annotate("text", x = 28 +10, y = 94,  colour = "blue",
           label = paste("Self employed R² =", round(r2_Selfemployed, 2)," RMSE =", round(rmse_Selfemployed, 2))) +
  annotate("text", x = 27 +10, y = 90, colour = "green",
           label = paste("Unemployed R² =", round(r2_Unemployed, 2)," RMSE =", round(rmse_Unemployed, 2))) +
  annotate("text", x = 23 +10, y = 86, colour = "orange",
           label = paste("Retired R² =", round(r2_Retired, 2)," RMSE =", round(rmse_Retired, 2))) +
  geom_abline(slope=1, intercept=0, colour = "black") +
  xlab("Percentage in 2021 Census") +
  ylab("Percentage in Synthetic Population")

ggsave("plots/employ_synthpop.png", dpi = 600, width = 4, height = 4)


# Check Gas / Electricity
names(domestic_gas)[names(domestic_gas) == "meters"] = "meters_gas"
names(domestic_electricity)[names(domestic_electricity) == "meters"] = "meters_elec"

syth20_summary = left_join(syth20_summary , domestic_gas[,c("LSOA21CD","total_gas_kwh","mean_gas_kwh","meters_gas")])
syth20_summary = left_join(syth20_summary , domestic_electricity[,c("LSOA21CD","total_elec_kwh","mean_elec_kwh","meters_elec")])

# Price Cap in 2021
#https://www.electricityprices.org.uk/history-of-the-energy-price-cap/
#October 2021 	£1,277 	20.80p 	24.89p 	4.07p 	26.12p

syth20_summary$gas_estimate_cost = (syth20_summary$total_gas_kwh * 0.0407) + (0.2612 * 7 * syth20_summary$households)
syth20_summary$elec_estimate_cost = (syth20_summary$total_elec_kwh * 0.2081) + (0.2489 * 7 * syth20_summary$households)

syth20_summary$gas_estimate_cost2 = (syth20_summary$mean_gas_kwh * 0.0407) + ifelse(syth20_summary$mean_gas_kwh > 0, (0.2612 * 7), 0)
syth20_summary$elec_estimate_cost2 = (syth20_summary$mean_elec_kwh * 0.2081) + (0.2489 * 7)

syth20_summary$gas_estimate_cost3 = (syth20_summary$total_gas_kwh * 0.0407) + (0.2612 * 7 * syth20_summary$meters_gas)
syth20_summary$elec_estimate_cost3 = (syth20_summary$total_elec_kwh * 0.2081) + (0.2489 * 7 * syth20_summary$meters_elec)

syth20_summary$gaselec_estimate_cost = syth20_summary$elec_estimate_cost + syth20_summary$gas_estimate_cost
syth20_summary$gaselec_estimate_cost2 = syth20_summary$elec_estimate_cost2 + syth20_summary$gas_estimate_cost2
syth20_summary$gaselec_estimate_cost3 = syth20_summary$elec_estimate_cost3 + syth20_summary$gas_estimate_cost3

summary(syth20_summary$gaselec_estimate_cost / syth20_summary$households)
summary(syth20_summary$gaselec * (365/7) / syth20_summary$households)

syth20_summary$gaselec_perhhousehold_lcfs = syth20_summary$gaselec * (365/7) / syth20_summary$households
syth20_summary$gaselec_perhhousehold_census = syth20_summary$gaselec_estimate_cost / syth20_summary$households
syth20_summary$gaselec_perhhousehold_census2 = syth20_summary$gaselec_estimate_cost2
syth20_summary$gaselec_perhhousehold_census3 = syth20_summary$gaselec_estimate_cost3 / syth20_summary$meters_elec

rmse_gaselec <- sqrt(mean((syth20_summary$gaselec_perhhousehold_census - syth20_summary$gaselec_perhhousehold_lcfs)^2))
r2_gaselec <- summary(lm(syth20_summary$gaselec_perhhousehold_census ~ syth20_summary$gaselec_perhhousehold_lcfs))$r.squared

rmse_gaselec2 <- sqrt(mean((syth20_summary$gaselec_perhhousehold_census2 - syth20_summary$gaselec_perhhousehold_lcfs)^2))
r2_gaselec2 <- summary(lm(syth20_summary$gaselec_perhhousehold_census2 ~ syth20_summary$gaselec_perhhousehold_lcfs))$r.squared

rmse_gaselec3 <- sqrt(mean((syth20_summary$gaselec_perhhousehold_census3 - syth20_summary$gaselec_perhhousehold_lcfs)^2))
r2_gaselec3 <- summary(lm(syth20_summary$gaselec_perhhousehold_census3 ~ syth20_summary$gaselec_perhhousehold_lcfs))$r.squared

ggplot(syth20_summary) +
  #geom_point(aes(x = gaselec_perhhousehold_census, y = gaselec_perhhousehold_lcfs), colour = "blue", shape = ".") +
  geom_point(aes(x = gaselec_perhhousehold_census3, y = gaselec_perhhousehold_lcfs), colour = "black", shape = ".") +
  #scale_colour_gradient2(low = "red", mid = "green",high = "purple",  midpoint = 1000) +
  #geom_smooth(method = 'lm') +
  annotate("text", x = 360, y = 2000,
           label = paste("R² =", round(r2_gaselec3, 2))) +
  annotate("text", x = 500, y = 1800,
           label = paste("RMSE =", round(rmse_gaselec3, 2))) +
  scale_x_continuous(limits=c(0, 4100), expand = c(0, 0),labels = scales::dollar_format(prefix="£")) +
  scale_y_continuous(limits=c(0, 2200), expand = c(0, 0),labels = scales::dollar_format(prefix="£")) +
  geom_abline(slope=1, intercept=0, colour = "red") +
  xlab("Average gas & electric bill: Meter readings") +
  ylab("Average gas & electric bill: Synthetic Population")

ggsave("plots/gaselec_synthpop.png", dpi = 600, width = 8, height = 4)
