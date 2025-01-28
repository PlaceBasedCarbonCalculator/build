# Make data

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
seed = array(rep(1,prod(dim)), dim = dim)

# Constrain the Seed
for (i in 1:dim[1]) {
  for (j in 1:dim[2]) {
    for (k in 1:dim[3]) {
      for (l in 1:dim[4]) {

        if ((k %in% c(1:2)) & (!l %in% c(1:4))) {
          # If one person household and number of household is not 1 set to 0
          seed[i, j, k, l] = 0
        }

        if ((k == 3) & (!l %in% c(5:8))) {
          # If two person household and number of household is not 2 set to 0
          seed[i, j, k, l] = 0
        }

        if ((k %in% 4:5) & (l %in% c(1:8))) {
          # If at least three person household and number of household is 1 or 2 set to 0
          seed[i, j, k, l] = 0
        }

        if ((k %in% 6:11) & (l %in% c(1:4))) {
          # If at least two person household and number of household is 1 set to 0
          seed[i, j, k, l] = 0
        }

      }
    }
  }
}

result = humanleague::qisi(seed,
                           indices = list(c(1,2), c(3,2),c(4,2)),
                           marginals = list(AccByTenure, hhCompByTenure,hhSizeCarVanByTenure))

result_df = expand.grid(rownames(AccByTenure), colnames(AccByTenure), rownames(hhCompByTenure), rownames(hhSizeCarVanByTenure))
names(result_df) = c("Acc","Tenure","hhComp","hhSizeCarVan")
result_df$households = as.numeric(result$result)
result_df = result_df[result_df$households > 0,]
result_df$conv = result$conv
result_df$pValue = result$pValue

result_df$households_round = round(result_df$households)
result_df$households_roundup = ifelse(round(result_df$households,1) > 0.5, ceiling(round(result_df$households,1)), 0)
