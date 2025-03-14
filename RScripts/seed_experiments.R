library(dplyr)
library(tidyr)

sizeCarTenureComp = read.csv("C:/Users/malco/Documents/GitHub/PlaceBasedCarbonCalculator/inputdata/population/census2021EW_Households_HouseholdComposition15_Tenure5_Size5_CarVar_5_EW.csv")
names(sizeCarTenureComp) = c("England.and.Wales.Code","England.and.Wales",
              "Household.size..5.categories..Code","hhSize5",
              "Car.or.van.availability..5.categories..Code", "CarVan5",
              "Tenure.of.household..5.categories..Code","Tenure5",
              "Household.composition..15.categories..Code","hhComp15",
              "households")

sizeCarTenureComp = sizeCarTenureComp[,c("hhSize5","CarVan5","Tenure5","hhComp15","households")]
sizeCarTenureComp = sizeCarTenureComp[sizeCarTenureComp$hhSize5 != "0 people in household",]
sizeCarTenureComp = sizeCarTenureComp[sizeCarTenureComp$Tenure5 != "Does not apply",]
sizeCarTenureComp = sizeCarTenureComp[sizeCarTenureComp$hhComp15 != "Does not apply",]
sizeCarTenureComp = sizeCarTenureComp[sizeCarTenureComp$CarVan5 != "Does not apply",]


AccsizeTenureComp = read.csv("C:/Users/malco/Documents/GitHub/PlaceBasedCarbonCalculator/inputdata/population/census2021EW_Households_HouseholdComposition15_Tenure5_Size5_Acc5_EW.csv")
names(AccsizeTenureComp) = c("England.and.Wales.Code","England.and.Wales",
                             "Household.size..5.categories..Code","hhSize5",
                             "Tenure.of.household..5.categories..Code","Tenure5",
                             "Household.composition..15.categories..Code","hhComp15",
                             "Accommodation.type..5.categories..Code","Acc5",
                             "households" )
AccsizeTenureComp = AccsizeTenureComp[,c("hhSize5","Acc5","Tenure5","hhComp15","households")]

AccsizeTenureComp = AccsizeTenureComp[AccsizeTenureComp$hhSize5 != "0 people in household",]
AccsizeTenureComp = AccsizeTenureComp[AccsizeTenureComp$Tenure5 != "Does not apply",]
AccsizeTenureComp = AccsizeTenureComp[AccsizeTenureComp$hhComp15 != "Does not apply",]
AccsizeTenureComp = AccsizeTenureComp[AccsizeTenureComp$Acc5 != "Does not apply",]


source("R/class_ethnic.R")
source("R/synthetic_pop_cenus2.R")

sizeCarTenureComp$hhComp15 = simplify_household15(sizeCarTenureComp$hhComp15)
sizeCarTenureComp$hhComp15 = gsub("Cohabit|Married","Couple",sizeCarTenureComp$hhComp15)
sizeCarTenureComp = dplyr::group_by(sizeCarTenureComp, hhSize5,CarVan5,Tenure5,hhComp15)
sizeCarTenureComp = dplyr::summarise(sizeCarTenureComp, households = sum(households))
sizeCarTenureComp = dplyr::ungroup(sizeCarTenureComp)

sizeCarTenureComp$hhSize5 = simplify_hhSize5(sizeCarTenureComp$hhSize5)
sizeCarTenureComp$CarVan5 = simplify_CarVan5(sizeCarTenureComp$CarVan5)
sizeCarTenureComp$Tenure5 = simplify_Tenure5(sizeCarTenureComp$Tenure5)

AccsizeTenureComp$hhComp15 = simplify_household15(AccsizeTenureComp$hhComp15)
AccsizeTenureComp$hhComp15 = gsub("Cohabit|Married","Couple",AccsizeTenureComp$hhComp15)
AccsizeTenureComp = dplyr::group_by(AccsizeTenureComp, hhSize5,Acc5,Tenure5,hhComp15)
AccsizeTenureComp = dplyr::summarise(AccsizeTenureComp, households = sum(households))
AccsizeTenureComp = dplyr::ungroup(AccsizeTenureComp)

AccsizeTenureComp$hhSize5 = simplify_hhSize5(AccsizeTenureComp$hhSize5)
AccsizeTenureComp$Acc5 = simplify_AccType5(AccsizeTenureComp$Acc5)
AccsizeTenureComp$Tenure5 = simplify_Tenure5(AccsizeTenureComp$Tenure5)

hist(AccsizeTenureComp$households)
quantile(AccsizeTenureComp$households, seq(0,1,0.1))

# Collapse to 2 dimensions

AccsizeTenureComp$shared = paste0(AccsizeTenureComp$hhSize5,"_",
                                  AccsizeTenureComp$Tenure5,"_",
                                  AccsizeTenureComp$hhComp15)
sizeCarTenureComp$shared = paste0(sizeCarTenureComp$hhSize5,"_",
                                  sizeCarTenureComp$Tenure5,"_",
                                  sizeCarTenureComp$hhComp15)

AccByShared = AccsizeTenureComp[,c("Acc5","shared","households")]
CarByShared = sizeCarTenureComp[,c("CarVan5","shared","households")]

AccByShared = pivot_wider(AccByShared, names_from = "Acc5", values_from = "households")
CarByShared = pivot_wider(CarByShared, names_from = "CarVan5", values_from = "households")

AccByShared = AccByShared[order(AccByShared$shared),]
CarByShared = CarByShared[order(CarByShared$shared),]

CarByShared = CarByShared[,c("shared","car0","car1","car2","car3+")]

summary(AccByShared$shared == CarByShared$shared)
summary(rowSums(AccByShared[,2:6]) == rowSums(CarByShared[,2:5]))
summary(rowSums(AccByShared[,2:6]) - rowSums(CarByShared[,2:5]))

CarByShared$shared[rowSums(AccByShared[,2:6]) != rowSums(CarByShared[,2:5])]

AccBySharedM = as.matrix(AccByShared[,2:6])
rownames(AccBySharedM) = AccByShared$shared

CarBySharedM = as.matrix(CarByShared[,2:5])
rownames(CarBySharedM) = CarByShared$shared

rownames(CarBySharedM) = gsub("_rented","rented",rownames(CarBySharedM))
rownames(AccBySharedM) = gsub("_rented","rented",rownames(AccBySharedM))

CarBySharedM = match_matrix_rsums(AccBySharedM, CarBySharedM)

summary(rowSums(AccBySharedM) == rowSums(CarBySharedM))

dim = c(nrow(AccBySharedM),ncol(AccBySharedM),ncol(CarBySharedM))
seed = array(rep(1,prod(dim)), dim = dim)

res_seed = humanleague::qisi(seed,
                             indices = list(c(1,2), c(1,3)),
                            marginals = list(AccBySharedM, CarBySharedM))

result_seed_df = expand.grid(rownames(AccBySharedM), colnames(AccBySharedM), colnames(CarBySharedM))
names(result_seed_df) = c("shared","Acc5","CarVan5")
result_seed_df$households = as.numeric(res_seed$result)

slt = strsplit(as.character(result_seed_df$shared),"_")

result_seed_df$hhSize5 = sapply(slt, `[[`, 1)
result_seed_df$Tenure5 = sapply(slt, `[[`, 2)
result_seed_df$hhComp15 = sapply(slt, `[[`, 3)

length(unique(result_seed_df$hhSize5))
length(unique(result_seed_df$Tenure5))
length(unique(result_seed_df$hhSize5))
length(unique(result_seed_df$hhComp15))
length(unique(result_seed_df$Acc5))
length(unique(result_seed_df$hhSizeCarVan))

result_seed_df$hhSizeCarVan = paste0(result_seed_df$hhSize5,"_",result_seed_df$CarVan5)

result_seed_df = result_seed_df[order(result_seed_df$Acc5,
                                      result_seed_df$Tenure5,
                                      result_seed_df$hhComp15,
                                      result_seed_df$hhSizeCarVan),]

result_seed_df$households_rel = result_seed_df$households / max(result_seed_df$households)
result_seed_df$households_bin = result_seed_df$households / 40000
result_seed_df$households_bin = ifelse(result_seed_df$households_bin > 1, 1, result_seed_df$households_bin)
result_seed_df$households_bin = ifelse(result_seed_df$households_bin < 1 & result_seed_df$households_bin > 0.1, 0.5, result_seed_df$households_bin)
result_seed_df$households_bin = ifelse(result_seed_df$households_bin < 0.1 & result_seed_df$households_bin > 0, 0.02, result_seed_df$households_bin)


result_seed_df$households_bin2 = result_seed_df$households / 90000
result_seed_df$households_bin2 = ifelse(result_seed_df$households_bin2 > 1, 1, result_seed_df$households_bin2)
result_seed_df$households_bin2 = ifelse(result_seed_df$households_bin2 < 1 & result_seed_df$households_bin2 > 0.03, 0.75, result_seed_df$households_bin2)
result_seed_df$households_bin2 = ifelse(result_seed_df$households_bin2 < 0.03 & result_seed_df$households_bin2 > 0.01, 0.5, result_seed_df$households_bin2)
result_seed_df$households_bin2 = ifelse(result_seed_df$households_bin2 < 0.01 & result_seed_df$households_bin2 > 0, 0.001, result_seed_df$households_bin2)


hist(result_seed_df$households_bin, seq(0,1,0.01))
hist(result_seed_df$households_bin2, seq(0,1,0.01))

seed_new = array(result_seed_df$households_rel, dim = c(5,4,11,16))
seed_new[seed_new == 0] = 1e-15

seed_new2 = array(result_seed_df$households_bin, dim = c(5,4,11,16))
seed_new2[seed_new2 == 0] = 1e-15

seed_new3 = array(result_seed_df$households_bin2, dim = c(5,4,11,16))
seed_new3[seed_new2 == 0] = 1e-15


#Case Examples

AccByTenure = matrix(c(98,136,6,48,0,57,124,7,50,0,3,6,0,6,0,9,22,1,74,0), nrow =5, ncol = 4)
hhCompByTenure = matrix(c(26,87,45,16,31,46,0,21,3,13,0,32,10,48,93,30,3,7,2,7,4,2,4,2,2,2,2,0,1,1,1,0,0,22,7,17,25,4,0,18,3,4,5,1), nrow =11, ncol = 4)
hhSizeCarVanByTenure = matrix(c(39,67,6,1,11,57,38,8,1,7,12,18,0,6,11,6,8,30,3,1,7,28,22,2,3,27,25,11,3,31,29,8,4,2,0,0,0,1,0,0,0,4,0,0,1,0,3,0,15,13,1,0,6,12,6,0,9,13,3,2,3,16,7,0), nrow =16, ncol = 4)

nms_Tenure = c("Outright","Mortgage","Social_rented","Private_rented")
nms_Acc = c("Detached","Semi","Terraced","Flat","caravan")
nms_hhComp = c("OnePersonOther","OnePersonOver66","CoupleNoChildren","CoupleChildren",
               "CoupleNonDepChildren","FamilyOver66","LoneParent","LoneParentNonDepChildren",
               "OtherChildren","OtherIncStudentOrOver66","OtherNoChildren")
nms_hhSizeCarVan = c("p1_car0","p1_car1","p1_car2","p1_car3+","p2_car0",
                     "p2_car1","p2_car2","p2_car3+","p3_car0","p3_car1","p3_car2",
                     "p3_car3+","p4+_car0","p4+_car1","p4+_car2","p4+_car3+")

colnames(AccByTenure) = nms_Tenure
colnames(hhCompByTenure) = nms_Tenure
colnames(hhSizeCarVanByTenure) = nms_Tenure

rownames(AccByTenure) = nms_Acc
rownames(hhCompByTenure) = nms_hhComp
rownames(hhSizeCarVanByTenure) = nms_hhSizeCarVan

# Make Seed
dim = c(nrow(AccByTenure),ncol(AccByTenure),nrow(hhCompByTenure),nrow(hhSizeCarVanByTenure))
seed_old = array(rep(1,prod(dim)), dim = dim)

# Constrain the Seed
for (i in 1:dim[1]) {
  for (j in 1:dim[2]) {
    for (k in 1:dim[3]) {
      for (l in 1:dim[4]) {

        if ((k %in% c(1:2)) & (!l %in% c(1:4))) {
          # If one person household and number of household is not 1 set to 0
          seed_old[i, j, k, l] = 1e-15
        }

        if ((k == 3) & (!l %in% c(5:8))) {
          # If two person household and number of household is not 2 set to 0
          seed_old[i, j, k, l] = 1e-15
        }

        if ((k %in% 4:11) & (l %in% c(1:4))) {
          # If at least two person household and number of household is 1 set to 0
          seed_old[i, j, k, l] = 1e-15
        }

      }
    }
  }
}


dim(seed_new)
dim(seed_old)

t1 = Sys.time()
r1 = humanleague::qisi(seed_old,
                       indices = list(c(1,2), c(3,2),c(4,2)),
                       marginals = list(AccByTenure, hhCompByTenure,hhSizeCarVanByTenure))
t2 = Sys.time()
r2 = humanleague::qisi(seed_new,
                       indices = list(c(1,2), c(3,2),c(4,2)),
                       marginals = list(AccByTenure, hhCompByTenure,hhSizeCarVanByTenure))
t3 = Sys.time()
r3 = humanleague::qisi(seed_new2,
                       indices = list(c(1,2), c(3,2),c(4,2)),
                       marginals = list(AccByTenure, hhCompByTenure,hhSizeCarVanByTenure))
t4 = Sys.time()
r4 = humanleague::qisi(seed_new2,
                       indices = list(c(1,2), c(3,2),c(4,2)),
                       marginals = list(AccByTenure, hhCompByTenure,hhSizeCarVanByTenure))
t5 = Sys.time()
difftime(t2,t1)
difftime(t3,t2)
difftime(t4,t3)
difftime(t5,t4)



hist(r1$result - r2$result, seq(-21,21,1))


seed_new = seed
seed_old = seed
seed_old[seed_old > 1e-15] = 1


t1 = Sys.time()
r1 = cenus_syth_combine_v3(Acc_tenure_com[[1]], hhComp_Tenure_com[[1]], Tenure_hhSize_CarVan_com[[1]], Tenure_NSSEC_com[[1]], seed = seed_old)
t2 = Sys.time()
r2 = cenus_syth_combine_v3(Acc_tenure_com[[1]], hhComp_Tenure_com[[1]], Tenure_hhSize_CarVan_com[[1]], Tenure_NSSEC_com[[1]], seed = seed_new)
t3 = Sys.time()

difftime(t2,t1)
difftime(t3,t2)



