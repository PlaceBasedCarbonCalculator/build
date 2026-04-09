library(targets)
library(dplyr)
library(tidyr)
library(ggplot2)
tar_load(nts) #2002-2023

hh = nts$household
people = nts$individual
trips = nts$trip
vehicle = nts$vehicle
stage = nts$stage

# Filter to 2018/19 and 2022/23
# TODO: get the 2023/24 data
# Note 20,881 households but only 18,058 have trips???
years = c(2018, 2019, 2023, 2024)
hh = hh[hh$SurveyYear %in% years,]
people = people[people$SurveyYear %in% years,]
trips = trips[trips$SurveyYear %in% years,]
vehicle = vehicle[vehicle$SurveyYear %in% years,]
stage = stage[stage$SurveyYear %in% years,]

table(hh$`2011CensusOutputAreaClassificationSupergroup8bands`, useNA = "always")
# All DEAD
# Give each hh an OAC - In secure envrioment will use real data
hh$OAC = sample(c("Constrained city dwellers","Cosmopolitans",
                  "Ethnicity central","Hard-pressed living","Multicultural metropolitans",
                  "Rural residents","Suburbanites","Urbanites"), nrow(hh), replace = TRUE)

foo = trips[trips$HouseholdID == 2018000247,]

# Select Vars
# Household
# W0	int	Unweighted interview sample
# W1	int	Unweighted diary sample
# W2	decimal	Weighted diary sample
# W3	decimal	Weighted interview sample
hh = hh[,c("HouseholdID","PSUID","SurveyYear","OAC","W0","W1","W2","W3","NumCarVan")]
vehicle = vehicle[,c("VehicleID","HouseholdID","Privateorcompanycarownership","Typeoffuelusedsummaryfortimeseriespurposes3categories")] #"VehPropTypeN_B01ID"
people = people[,c("IndividualID","HouseholdID","PSUID","VehicleID","PersNo",
                   "AgeofpersonbandedageBandAAllages21categories","AgeofpersonbandedageBandDAllages9categories",
                   "Sexofperson","Istheindividualofstatepensionage","EthnicGroupfortimeseriespurposes2011bandings2categories",
                   "NationalStaisticsSocioEconomicClassificationNSSECofindividualhighlevelbreakdown5categories","Howoftendoyouworkathome","FrequencyofinternalairflightswithinGB","IntPlane")]

names(people)[names(people) == "NationalStaisticsSocioEconomicClassificationNSSECofindividualhighlevelbreakdown5categories"] = "NSSEC"

stage = stage[,c("StageID","TripID","DayID","IndividualID","HouseholdID","PSUID","VehicleID","IndTicketID","StageDistance","StageTime","Stagemodeoftraveldetailedbreakdown28categories","Stagemodeoftravelpublicationtablebreakdown13categories")]

# Extract number of trips, modal shares, distances by journey purpose for each Supergroup.
# Simply Trips Table
trips_tidy = trips[,c("SurveyYear","TripID","IndividualID","PSUID","PersNo","HouseholdID","NumStages",
                      "Mainmodeoftravelpublicationtablebreakdown13categories",
                      "TrippurposepublicationtablebreakdownbandC8categories",
                      "Tripdistanceincludingshortwalkmilesbandeddistance12categories",
                      "TripTravTime","TripDisIncSW","Shortwalktrip"
                      )]
trips_tidy$mode = as.character(trips_tidy$Mainmodeoftravelpublicationtablebreakdown13categories)
trips_tidy$mode = gsub("Bus in London","bus",trips_tidy$mode)
trips_tidy$mode = gsub("Car / van driver","carDriver",trips_tidy$mode)
trips_tidy$mode = gsub("Car / van passenger","carPassenger",trips_tidy$mode)
trips_tidy$mode = gsub("Taxi / minicab","taxi",trips_tidy$mode)
trips_tidy$mode = gsub("Walk","walk",trips_tidy$mode)
trips_tidy$mode = gsub("Pedal cycle","bicycle",trips_tidy$mode)
trips_tidy$mode = gsub("Motorcycle","motorcycle",trips_tidy$mode)
trips_tidy$mode = gsub("Other local bus","bus",trips_tidy$mode)
trips_tidy$mode = gsub("Non-local bus","bus",trips_tidy$mode)
trips_tidy$mode = gsub("Surface Rail","train",trips_tidy$mode)
trips_tidy$mode = gsub("London Underground","underground",trips_tidy$mode)
trips_tidy$mode = gsub("Other private transport","otherPrivate",trips_tidy$mode)
trips_tidy$mode = gsub("Other public transport","otherPublic",trips_tidy$mode)

trips_tidy$purpose = as.character(trips_tidy$TrippurposepublicationtablebreakdownbandC8categories)
trips_tidy$purpose = gsub("Business","business",trips_tidy$purpose)
trips_tidy$purpose = gsub("Commuting","commuting",trips_tidy$purpose)
trips_tidy$purpose = gsub("Education / escort education","education",trips_tidy$purpose)
trips_tidy$purpose = gsub("Leisure","leisure",trips_tidy$purpose)
trips_tidy$purpose = gsub("Other escort","otherEscort",trips_tidy$purpose)
trips_tidy$purpose = gsub("Other including just walk","Other",trips_tidy$purpose)
trips_tidy$purpose = gsub("Personal business","personalBusiness",trips_tidy$purpose)
trips_tidy$purpose = gsub("Shopping","shopping",trips_tidy$purpose)

trips_tidy = trips_tidy[,c("SurveyYear","TripID","IndividualID","PSUID","PersNo","HouseholdID","NumStages",
                      "mode","purpose",
                      "TripTravTime","TripDisIncSW","Shortwalktrip")]

#Join vehicle to stages
stage$VehicleID = as.numeric(stage$VehicleID)
stage = left_join(stage,
                   vehicle[c("VehicleID","Privateorcompanycarownership","Typeoffuelusedsummaryfortimeseriespurposes3categories")],
                   by = c("VehicleID"))

# Join on trip purpose
stage = left_join(stage,
                   trips_tidy[,c("TripID","purpose")],
                   by = "TripID")
names(stage)[names(stage) == "purpose"] = "tripPurpose"

# Join on person NSSEC
stage = left_join(stage,
                  people[,c("IndividualID",
                            "AgeofpersonbandedageBandAAllages21categories",
                            "AgeofpersonbandedageBandDAllages9categories",
                            "Sexofperson",
                            "Istheindividualofstatepensionage",
                            "EthnicGroupfortimeseriespurposes2011bandings2categories",
                            "NSSEC")],
                  by = "IndividualID")

#join on Household OAC (dumy data)
stage = left_join(stage,
                  hh[,c("HouseholdID","OAC")],
                  by = "HouseholdID")

write.csv(stage,"D:/OneDrive - University of Leeds/Data/National Travel Survey/Safeguarded/Stages_for_Angela.csv",
          na = "",row.names = FALSE)


trips_summary = trips_tidy |>
  group_by(SurveyYear, HouseholdID, mode, purpose) |>
  summarise(total_trips = n(),
            total_travelTime = sum(TripTravTime),
            mean_travelTime = mean(TripTravTime),
            total_travelDistance = sum(TripDisIncSW),
            mean_travelDistance = mean(TripDisIncSW))


trips_summary_wide = trips_summary |>
  pivot_wider(names_from = c("mode","purpose"),
              values_from = c("total_trips","total_travelTime","mean_travelTime",
                              "total_travelDistance","mean_travelDistance"))

# Add on OAC
trips_summary = left_join(trips_summary, hh[,c("HouseholdID","OAC")], by = "HouseholdID")

#Summarise Total trips by OAC

trips_summary_OAC = trips_summary |>
  group_by(SurveyYear, OAC) |>
  summarise(total_trips = sum(total_trips),
            total_travelDistance = sum(total_travelDistance),
            total_travelTime = sum(total_travelTime)
            )

trips_summary_OAC$mean_travelDistance = trips_summary_OAC$total_travelDistance / trips_summary_OAC$total_trips

ggplot(trips_summary_OAC,
       aes(x = SurveyYear, y = mean_travelDistance, colour = OAC)) +
  geom_line()




