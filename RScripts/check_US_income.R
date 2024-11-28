library(targets)
tar_load(us)
tar_load(sipher)
tar_load(income_lsoa)
tar_load(income_msoa)
tar_load(lookup_OA_LSOA_MSOA_classifications)
tar_load(parameters)

us_people = us$indresp
us_household = us$hhresp

path = file.path(parameters$path_data,"income")
income2018 = readxl::read_excel(file.path(path, "income2018.xls"), "Total annual income")

names(income2018) = c("MSOA11","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                      "total_annual_income","upper_limit","lower_limit","interval")
income2018 = income2018[5:nrow(income2018), ]
income2018 = income2018[!is.na(income2018$MSOAname),]
income2018$total_annual_income = as.numeric(income2018$total_annual_income)
income2018$upper_limit = as.numeric(income2018$upper_limit)
income2018$lower_limit = as.numeric(income2018$lower_limit)

incomeSIPHER = read.csv(file.path(path, "LSOAlevelestimatesofaveragehealthconditionsandhouseholdincome.csv"))
incomeSIPHER = incomeSIPHER[,c("LSOA_code","Household_net_income_monthly")]

# for(i in 1:length(us)){
#   if(any(grepl("fihhmnnet1",names(us[[i]]) ))){
#     message(names(us)[i])
#   }
# }

# w_fihhmnnet1_dv+ is the net household monthly income
us_household = us_household[,c("k_hidp","k_fihhmnnet1_dv","k_fihhmngrs_dv")]
us_household$k_fihhmnnet1_dv = as.numeric(as.character(us_household$k_fihhmnnet1_dv))
us_household$k_fihhmngrs_dv = as.numeric(as.character(us_household$k_fihhmngrs_dv))

quantile(us_household$k_fihhmngrs_dv * 12, seq(0,1,0.1), na.rm = TRUE)

us_people = us_people[,c("pidp","k_hidp","k_pno")]

us_people = dplyr::left_join(us_people, us_household, by = "k_hidp")
names(us_people) = c("personID","housholdID","personNo","household_income_net","household_income_gross")

sipher2 = dplyr::left_join(sipher, us_people, by = c("pidp" = "personID"))
sipher2 = sipher2[sipher2$personNo == 1,]

sipher_summary = dplyr::group_by(sipher2, synthetic_zone)
sipher_summary = dplyr::summarise(sipher_summary,
                                  household_income_net_mean = mean(household_income_net, na.rm = TRUE),
                                  household_income_net_median = median(household_income_net, na.rm = TRUE),
                                  household_income_gross_mean = mean(household_income_gross, na.rm = TRUE),
                                  household_income_gross_median = median(household_income_gross, na.rm = TRUE),
                                  )



check = dplyr::left_join(income_lsoa, sipher_summary, by = c("LSOA21CD" = "synthetic_zone"))
check$household_income_gross_mean = check$household_income_gross_mean * 12
check$household_income_gross_median = check$household_income_gross_median * 12

library(ggplot2)

ggplot(check, aes(x = income_lsoa, y = household_income_gross_mean)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, col = "red") +
  ylab("SIPHER/US") +
  xlab("ONS") +
  xlim(0,150000) +
  ylim(0,150000)

m = lm(income_lsoa ~  household_income_mean, data = check)
summary(m)



lookup = lookup_OA_LSOA_MSOA_classifications[,c("LSOA11CD","MSOA11CD","LAD17NM")]
lookup = lookup[!duplicated(lookup$LSOA11CD),]

la_cardiff = c("Cardiff","Blaenau Gwent","Bridgend","Caerphilly","Merthyr Tydfil","Monmouthshire","Newport","Rhondda Cynon Taf","Torfaen","The Vale of Glamorgan")
la_sheffield = c("Sheffield","Rotherham","Doncaster","Barnsley")
la_manchester = c("Manchester","Bolton","Bury","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")

lookup$la_group = "other"
lookup$la_group = ifelse(lookup$LAD17NM %in% la_cardiff,"cardiff",lookup$la_group)
lookup$la_group = ifelse(lookup$LAD17NM %in% la_sheffield,"sheffield",lookup$la_group)
lookup$la_group = ifelse(lookup$LAD17NM %in% la_manchester,"manchester",lookup$la_group)


sipher2 = dplyr::left_join(sipher2, lookup, by = c("synthetic_zone" = "LSOA11CD"))

sipher_summary_msoa = dplyr::group_by(sipher2, MSOA11CD, la_group)
sipher_summary_msoa = dplyr::summarise(sipher_summary_msoa,
                                       household_income_net_mean = mean(household_income_net, na.rm = TRUE),
                                       household_income_net_median = median(household_income_net, na.rm = TRUE),
                                       household_income_gross_mean = mean(household_income_gross, na.rm = TRUE),
                                       household_income_gross_median = median(household_income_gross, na.rm = TRUE),
)

check_msoa = dplyr::left_join(income2018, sipher_summary_msoa, by = c("MSOA11" = "MSOA11CD"))
check_msoa$household_income_net_mean = check_msoa$household_income_net_mean * 12
check_msoa$household_income_net_median = check_msoa$household_income_net_median * 12
check_msoa$household_income_gross_mean = check_msoa$household_income_gross_mean * 12
check_msoa$household_income_gross_median = check_msoa$household_income_gross_median * 12

ggplot(data = check_msoa[check_msoa$la_group == "other",], aes(y = total_annual_income, x = household_income_gross_mean)) +
  geom_point(size = 0.5, colour = "grey") +
  geom_point(data = check_msoa[check_msoa$la_group != "other",], size = 1, aes(colour = la_group)) +
  geom_abline(slope=1, intercept=0, col = "red") +
  xlab("SIPHER/Understanding Society Mean Gross Income") +
  ylab("ONS Income estimates") +
  xlim(20000,100000) +
  ylim(20000,100000) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)



ggplot(data = check_msoa[check_msoa$la_group == "sheffield",], aes(y = total_annual_income, x = household_income_gross_mean)) +
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit)) +
  geom_point(col = "blue") +
  geom_abline(slope=1, intercept=0, col = "red") +
  xlab("SIPHER/Understanding Society Mean Gross Income") +
  ylab("ONS Income estimates") +
  ggtitle("Sheffield")

check_msoa = check_msoa[order(check_msoa$total_annual_income),]

incomeSIPHER = dplyr::left_join(incomeSIPHER, lookup, by = c("LSOA_code" = "LSOA11CD"))
incomeSIPHER_summary = dplyr::group_by(incomeSIPHER, MSOA11CD, la_group)
incomeSIPHER_summary = dplyr::summarise(incomeSIPHER_summary,
                                        SIPHER_net_income_mean = mean(Household_net_income_monthly * 12, na.rm = TRUE),
                                        SIPHER_net_income_median = median(Household_net_income_monthly * 12, na.rm = TRUE)
)

# Ass SIPER estimates
check_msoa = dplyr::left_join(check_msoa, incomeSIPHER_summary, by = c("MSOA11" = "MSOA11CD", "la_group"))


check_msoa_three = check_msoa[check_msoa$la_group != "other",]
check_msoa_other = check_msoa[check_msoa$la_group == "other",]

check_msoa_three$index = 1:nrow(check_msoa_three)
check_msoa_other$index = 1:nrow(check_msoa_other)

ggplot(data = check_msoa_three, aes(x = index)) +
  geom_line(aes(y = upper_limit), colour = "red") +
  geom_line(aes(y = lower_limit), colour = "blue") +
  geom_line(aes(y = total_annual_income), colour = "green") +
  geom_line(aes(y = household_income_net_mean), colour = "black") +
  geom_line(aes(y = SIPHER_net_income_mean), colour = "orange") +
  ylab("Annual Income Estimate") +
  xlab("MSOA Number") +
  ggtitle("Cardiff, Manchester, Sheffield")


ggplot(data = check_msoa_other, aes(x = index)) +
  geom_line(aes(y = household_income_gross_mean), colour = "black") +
  geom_line(aes(y = upper_limit), colour = "red") +
  geom_line(aes(y = lower_limit), colour = "blue") +
  geom_line(aes(y = total_annual_income), colour = "green") +
  ylab("Annual Income Estimate") +
  xlab("MSOA Number") +
  ggtitle("Rest of England and Wales")




ggplot(data = check_msoa, aes(y = SIPHER_net_income_mean, x = household_income_net_mean)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, col = "red") +
  xlab("My Estimate using SIPHER") +
  ylab("Published on figshare") +
  xlim(20000,100000) +
  ylim(20000,100000) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Cardiff, Manchester, Sheffield MSOA: Net Mean Income 2018")

