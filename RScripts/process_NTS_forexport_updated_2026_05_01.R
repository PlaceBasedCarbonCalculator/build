library(dplyr)
library(tidyr)
library(ggplot2)
library(haven) # For SPSS .sav writing



load_NTS = function(path = file.path(parameters$path_secure_data,"National Travel Survey/Safeguarded/")){

  dir = file.path(tempdir(),"NTS")
  dir.create(dir)
  unzip(file.path(path,"NTS_Safeguarded_2002_2023_SPSS_V1.zip"), exdir = dir)

  fls = list.files(file.path(dir,"UKDA-5340-spss","spss","spss28"), full.names = FALSE)
  nms = gsub("_eul_2002-2023.sav","",fls)

  nts = list()

  for(i in 1:length(fls)){
    nts[[i]] = foreign::read.spss(file.path(file.path(dir,"UKDA-5340-spss","spss","spss28",fls[i])),
                                  to.data.frame=TRUE)
  }

  lookup_tables = readxl::read_excel(file.path(dir,"UKDA-5340-spss","mrdoc","excel","5340_nts_lookup_table_eul_2002_to_2023.xlsx"),
                                     sheet = "Multicoded Tables")

  lookup_cols = readxl::read_excel(file.path(dir,"UKDA-5340-spss","mrdoc","excel","5340_nts_lookup_table_response_levels_eul_2002_to_2023.xlsx"))
  lookup_cols = lookup_cols[,c("Table","Variable","Description","ID","Desc")]

  lookup_cols$Description = gsub(" - Created in SQL","",lookup_cols$Description)
  lookup_cols$Description = gsub(" - created in SQL","",lookup_cols$Description)
  lookup_cols$Description = gsub(",|\\(|\\)|<|=|>|?","",lookup_cols$Description)
  lookup_cols$Description = gsub("  regional / Metropolitan area breakdown","",lookup_cols$Description)
  lookup_cols$Description = gsub("/","",lookup_cols$Description)
  lookup_cols$Description = gsub("-","",lookup_cols$Description)
  lookup_cols$Description = gsub(" ","",lookup_cols$Description)

  lookup_tables$`Table Name` = tolower(lookup_tables$`Table Name`)

  list_names = data.frame(nms = nms)
  lookup_tables = dplyr::left_join(list_names,
                                   lookup_tables[,c("Table Name","Description")],
                                   by = c("nms" = "Table Name")
  )
  lookup_tables$Description = ifelse(is.na(lookup_tables$Description), lookup_tables$nms, lookup_tables$Description)



  lookup_tables$dup = duplicated(lookup_tables$Description)

  lookup_tables$occurrences <- ave(seq_along(lookup_tables$Description), lookup_tables$Description, FUN = seq_along)
  lookup_tables$Description = ifelse(lookup_tables$occurrences > 1,
                                     paste0(lookup_tables$Description," ",LETTERS[lookup_tables$occurrences]),
                                     lookup_tables$Description)

  names(nts) = lookup_tables$Description


  # Replace lookups with values
  for(i in 1:length(nts)){
    sub = nts[[i]]
    lookup_sub = lookup_cols[lookup_cols$Variable %in% names(sub),]
    for(j in names(sub)){
      col_sub = lookup_sub[lookup_sub$Variable == j,]
      col_sub$Desc = as.factor(col_sub$Desc)
      if(nrow(col_sub) > 0){
        names(col_sub)[5] = col_sub$Description[1]
        names(col_sub)[4] = col_sub$Variable[1]
        col_sub = col_sub[,c(4:5)]
        sub[[j]] = as.numeric(sub[[j]])
        sub = dplyr::left_join(sub, col_sub, by = j)
        sub[j] = NULL
      }
    }
    nts[[i]] = sub
  }

  # lookup_cols = readxl::read_excel(file.path(dir,"UKDA-5340-spss","mrdoc","excel","5340_nts_lookup_table_eul_2002_to_2023.xlsx"),
  #                                    sheet = "Main Table Variables")
  # lookup_cols = lookup_cols[,c("Variable","Description")]
  #
  # lookup_cols$clean = gsub(" - Created in SQL","",lookup_cols$Description)
  # lookup_cols$clean = gsub(" - created in SQL","",lookup_cols$clean)
  # lookup_cols$clean = gsub(",|\\(|\\)|<|=|>|?","",lookup_cols$clean)
  # lookup_cols$clean = gsub("  regional / Metropolitan area breakdown","",lookup_cols$clean)
  # lookup_cols$clean = gsub("/","",lookup_cols$clean)
  # lookup_cols$clean = gsub("-","",lookup_cols$clean)
  # lookup_cols$clean = gsub(" ","",lookup_cols$clean)
  #
  # for(i in 1:length(nts)){
  #   tab_nms = names(nts[[i]])
  #   tab_match = lookup_cols$clean[match(tab_nms, lookup_cols$Variable)]
  #   tab_match = ifelse(is.na(tab_match),tab_nms, tab_match)
  #
  #   names(nts[[i]]) = tab_match
  # }

  unlink(dir, recursive = TRUE)

  nts
}

nts = load_NTS("C:/Users/malco/OneDrive - University of Leeds/Data/National Travel Survey/Safeguarded/NTS_Safeguarded_2002_2023_SPSS_V1.zip")

saveRDS(nts,"data/NTS_all.Rds")


nts = readRDS("data/NTS_all.Rds")


hh = nts$household
people = nts$individual
trips = nts$trip
vehicle = nts$vehicle
stage = nts$stage
rm(nts)

# Filter to 2018/19 and 2022/23
# TODO: get the 2023/24 data
years = c(2018, 2019, 2023, 2024,2025)
hh = hh[hh$SurveyYear %in% years,]
people = people[people$SurveyYear %in% years,]
trips = trips[trips$SurveyYear %in% years,]
vehicle = vehicle[vehicle$SurveyYear %in% years,]
stage = stage[stage$SurveyYear %in% years,]

# Select Vars
# Household
# W0	int	Unweighted interview sample
# W1	int	Unweighted diary sample
# W2	decimal	Weighted diary sample
# W3	decimal	Weighted interview sample
nms_hh = c("HouseholdID","PSUID","SurveyYear","W0","W1","W2","W3","NumCarVan",
"NumVanLorry","HHIncome2002_B01ID","Ten1_B01ID","HHoldGOR_B01ID",
"HHoldGOR_B02ID","HHoldAreaType2_B01ID","HHoldStruct_B01ID",
"HRPSIC2007_B02ID")

nms_vehicle = c("VehicleID","HouseholdID","PSUID","IndividualID","CarOwn_B01ID",
                "VehPropTypeN_B01ID")

nms_people = c("IndividualID","HouseholdID","PSUID","VehicleID","PersNo",
               "Age","Age_B01ID","Age_B04ID",
               "Sex_B01ID","OfPenAge_B01ID","EthGroupTS_B01ID","EthGrp2_B01ID",
               "NSSec_B01ID","NSSec_B02ID","NSSec_B03ID",
               "OftHome_B01ID","PlaneFreq_B01ID","IntPlane")
nms_trips = c("SurveyYear","TripID","IndividualID","PSUID","PersNo","HouseholdID",
             "TravDay",
             "NumStages",
             "MainMode_B03ID","MainMode_B04ID",
             "TripPurpFrom_B01ID","TripPurpTo_B01ID","TripPurpose_B01ID",
             "TripPurpose_B02ID","TripPurpose_B04ID",
             "TripTotalTime",
             "TripDisIncSW","TripDisExSW","TripDisIncSW_B01ID","TripDisExSW_B01ID",
             "JJXSC","JOTXSC","JTTXSC","JD","W5","W5xHH",
             "TripTravTime","ShortWalkTrip_B01ID")

nms_stage = c("StageID","TripID","DayID","IndividualID","HouseholdID","PSUID",
              "VehicleID","IndTicketID","StageDistance","StageTime","StageMode_B03ID",
              "StageMode_B04ID")


hh = hh[,names(hh)[names(hh) %in% nms_hh]]
vehicle = vehicle[,names(vehicle)[names(vehicle) %in% nms_vehicle]]
people = people[,names(people)[names(people) %in% nms_people]]
trips = trips[,names(trips)[names(trips) %in% nms_trips]]
stage = stage[,names(stage)[names(stage) %in% nms_stage]]


# Clean Types
hh$NumCarVan = as.integer(hh$NumCarVan)
hh$NumVanLorry = as.integer(hh$NumVanLorry)
#hh$HHoldOACode2011 = as.factor(hh$HHoldOACode2011)
#hh$HHoldOACode2021 = as.factor(hh$HHoldOACode2021)
#hh$LSOA11CD = as.factor(hh$LSOA11CD)
#hh$LSOA21CD = as.factor(hh$LSOA21CD)
#hh$OAC2021 = as.factor(hh$OAC2021)
#hh$LSOAClass2021 = as.factor(hh$LSOAClass2021)


people$IntPlane = as.integer(people$IntPlane)

trips$PersNo = as.integer(trips$PersNo)

vehicle$VehicleID = as.integer(vehicle$VehicleID)
stage$VehicleID = as.integer(stage$VehicleID)


hh$SurveyYear = as.integer(hh$SurveyYear)
trips$SurveyYear = as.integer(trips$SurveyYear)

# Group Subgroups
#hh$OAC2021Supergroup = substr(hh$OAC2021,1,1)
#hh$LSOAClass2021Supergroup = substr(hh$LSOAClass2021,1,1)

# Save each as SPSS
write_sav(hh,"data/households.sav")
write_sav(vehicle,"data/vehicle.sav")
write_sav(people,"data/indervidual.sav")
write_sav(trips,"data/trips.sav")
write_sav(stage,"data/stages.sav")

# Big Wide Stages
stage_all = left_join(stage,
                  vehicle[c("VehicleID","CarOwn_B01ID","VehPropTypeN_B01ID")],
                  by = c("VehicleID"))

stage_all = left_join(stage_all,
                  trips[,c("SurveyYear","TripID","IndividualID",
                           "PersNo","HouseholdID","TravDay","NumStages",
                           "MainMode_B03ID","MainMode_B04ID","TripPurpFrom_B01ID","TripPurpTo_B01ID",
                           "TripPurpose_B01ID","TripPurpose_B02ID","TripPurpose_B04ID","TripTotalTime",
                           "TripDisIncSW","TripDisExSW","TripDisIncSW_B01ID","TripDisExSW_B01ID",
                           "JJXSC","JOTXSC","JTTXSC","JD",
                           "W5","W5xHH","TripTravTime","ShortWalkTrip_B01ID")],
                  by = c("TripID","IndividualID","HouseholdID"))

stage_all = left_join(stage_all,
                  people,
                  by = c("IndividualID","HouseholdID","PSUID","PersNo"))
names(stage_all)[names(stage_all) == "VehicleID.x"] = "VehicleID_stage"
names(stage_all)[names(stage_all) == "VehicleID.y"] = "VehicleID_person"

stage_all = left_join(stage_all,
                  hh,
                  by = c("HouseholdID","PSUID","SurveyYear"))

stage_all = stage_all |>
  arrange(HouseholdID, IndividualID, TripID, StageID)

nms = names(stage_all)
nms = nms[!nms %in% c("SurveyYear","HouseholdID","IndividualID","TripID","StageID","PSUID")]

stage_all = stage_all[,c("SurveyYear","HouseholdID","IndividualID","TripID",
                         "StageID","PSUID",nms)]

stage_all$HouseholdID = as.integer(stage_all$HouseholdID)
stage_all$IndividualID = as.integer(stage_all$IndividualID)
stage_all$TripID = as.integer(stage_all$TripID)
stage_all$StageID = as.integer(stage_all$StageID)

write_sav(stage_all,"data/stages_joined.sav")



