read_scotland_households = function(path = "../inputdata/population_scotland/household-estimates.xlsx"){
  hh = list()
  for(i in 2014:2023){
    hh_sub = readxl::read_xlsx(path, sheet = as.character(i), skip = 3)
    hh_sub = hh_sub[,c(1,5:12)]
    names(hh_sub) = c("dz11cd","dwellings_total",
                      "occupied","vacant",
                      "unoccupied_taxexempt","longtermempty",
                      "second_homes","occupied_taxexempt",
                      "singleadult_discount")
    hh_sub$year = i
    hh[[i]] = hh_sub
  }
  hh = dplyr::bind_rows(hh)
  hh

}

# TODO: Review notes with this file

# "Clackmannanshire Council - In 2014 the council was unable to provide data
# zone level information on second homes although they were able to provide this
# information at council level. To ensure data zone level totals add to give the
# council level figures from the Scottish Government's council tax base return,
# the second homes figure from council tax base has been distributed evenly
# across all of Clackmannanshire's Data Zones.
#
#
#
# Aberdeen City - In 2014 issues with extracting data from this council's
# Council Tax billing system meant that the council were unable to provide a
# breakdown on long-term empty properties and second homes at small area level.
# A combination of the council tax base totals for these categories and an
# earlier year's small areas data has been used to estimate the number of
# long-term empty properties and the number of second homes in each data zone in
# Aberdeen City.
#
#
#
# Shetland Islands - a revision has been made to the total number of vacant
# dwellings in the Shetland Islands in 2014. This has had an impact on the data
# zone level estimates of occupied dwellings and vacant dwellings. The effect,
# at council area level, is to increase the number of vacant dwellings by 176,
# therefore reducing the number of occupied dwellings by 176. This difference
# will be spread across the data zones making up the Shetland Islands according
# to the proportion of vacant dwellings lying in each therefore the impact at
# data zone level will be small.
#
#
#
# Edinburgh - a correction has been made to the total number of vacant dwellings
# in Edinburgh City Council area in 2014. This has had an impact on the data
# zone level estimates of occupied and vacant dwellings. At council area level
# the number of vacant dwellings has increased by 2,370 and the number of
# occupied dwellings has reduced by a similar amount. The council were also able
# to provide more detailed information on how the vacant dwellings were spread
# across data zones. The impact at data zone level is relatively small.
#
#
#
# West Lothian - a correction has been made to the total number of vacant
# dwellings in West Lothian in 2014. This has had an impact on the data zone
# level estimates of occupied and vacant dwellings. At council area this has
# reduced the number of vacant dwellings by 126, and therefore increased the
# number of occupied dwellings by 126. This difference will be spread across the
# data zones that make up West Lothian according to the proportion of vacant
# dwellings lying in each therefore the impact on individual data zones will be
# small." Second homes figures were not available for Aberdeen City, City of
# Edinburgh and West Dunbartonshire for 2017. The 2016 totals were used instead
# Second homes figures for West Dunbartonshire were not available, the 2016
# totals were used. In a previous version of this file, three dwellings were
# erroneously assigned to Occupied dwellings exempt from paying Council Tax
# category in three data zone (S01007375, S01007381, S01007389) in Argyll and
# Bute.For the S01007389 data zone, this resulted in a negative value in the
# Unoccupied dwellings exempt from paying Council Tax category. This has been
# corrected and the dwellings have been assigned to the right category. This has
# increased the total number of Unoccupied dwellings exempt from paying Council
# Tax by three and reduced the total number of Occupied dwellings by one.
# Changes over time can occur not only as a result of  new building activity and
# demolition but also because of changes related to the postcodes of dwellings.
# Postcode changes can occur due to improvements  made to administrative systems
# or NRS data cleaning,  re-locating postcodes which were previously allocated
# to an incorrect Data Zones.  Differences can also occur due to postcodes being
# allocated to a different  Data Zone because the distribution of their
# population has changed.  These differences are likely to be minimal and have
# only a small effect on  change over time except when looking at small numbers
# of Data Zones.  In 2022, one data zone in Glasgow (S01010286) sees  an
# increase of more than 75% in the number of dwellings,  passing from 812 to
# 1,425.On the contrary, the Data Zone S01010283 sees a decrease of -44% in the
# number of dwellings.  This is a fictitious event due to changes in the
# postcodes allocation in 2021. In the past this information was recorded as
# 'long-term empty' properties, dwellings that have been empty for six months or
# more. At the beginning of 2024, we became aware of possible inconsistencies in
# the coverage of data submitted by councils on empty properties. In some
# instances properties empty for less than 6 months have been included in
# addition to those empty for 6 months or more. The number of long-term (6
# months or more) empty properties may therefore be overstated in such
# instances. Further analysis is planned to attempt to quantify the extent and
# scale of this.
