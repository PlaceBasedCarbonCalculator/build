# Builds a seed of the synthetic population of Scotland based on overall
# Scotland population As aggregated to whole of Scotland inputs can be
# multivariate without being disclosive. Nationally representative seed is then
# used as input to local synthetic populations which discourages the generation of
# usual household combinations
build_synth_pop_seed_scotland = function(path_data = file.path(parameters$path_data,"population_scotland")){

  # Read In Data
  householdComp10_CarVan5_Tenure5_hhSize5_scot = read_householdComp10_CarVan5_Tenure5_hhSize5_scot(file.path(path_data,"scotlandcenus2022_householdComp10_CarVan5_Tenure5_hhSize5_Scotland.xlsx"))
  AccTyp7_CarVan5_Tenure5_hhSize5_scot = read_AccTyp7_CarVan5_Tenure5_hhSize5_scot(file.path(path_data,"scotlandcenus2022_AccType7_CarVan5_Tenure5_hhSize5_Scotland.xlsx"))
  householdComp10_hhSize5_CarVan3_Tenure3_AccType3_scot = read_householdComp10_hhSize5_CarVan3_Tenure3_AccType3_scot(file.path(path_data,"scotlandcenus2022_householdComp10_hhSize5_CarVan3_Tenure3_AccType3.Scotland.xlsx"))
  AccType7_CarVan5_CarVan3_Tenure5_Tenure3_scot = read_AccType7_CarVan5_CarVan3_Tenure5_Tenure3_scot(file.path(path_data,"scotlandcenus2022_AccType7_CarVan5_CarVan3_Tenure5_Tenure3_Scotland.xlsx"))
  AccType7_AccType3_scot = read_AccType7_AccType3_scot(file.path(path_data,"scotlandcenus2022_AccType7_AccType3_Scotland.csv"))


  # Define Variable Names
  householdComp10 = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66")
  hhSize5 = c("p1","p2","p3","p4","p5+")
  Tenure3 = c("owned","socialrented","privaterented")
  Tenure5 = c("outright","mortgage","socialrented","privaterented","rentfree")
  CarVan5 = c("car0","car1","car2","car3","car4+")
  CarVan3 = c("car0","car1","car2+")
  AccType3 = c("house","flat","caravan")
  AccType7 = c("detached","semidetached","terraced","flatpurposebuilt","flatconverted","flatcommercial","caravan")

  # Build Arrays
  mat1 = expand.grid(householdComp10, CarVan5, Tenure5, hhSize5)
  names(mat1) = c("householdComp10", "CarVan5", "tenure5", "hhSize5")
  mat1 = dplyr::left_join(mat1, householdComp10_CarVan5_Tenure5_hhSize5_scot, by = c("householdComp10", "CarVan5", "tenure5", "hhSize5"))
  if(anyNA(mat1$value)){stop("NAs in values")}
  mat1 = array(mat1$value,
               dim = c(length(householdComp10), length(CarVan5), length(Tenure5), length(hhSize5)),
               dimnames = list(householdComp10, CarVan5, Tenure5, hhSize5)
               )

  mat2 = expand.grid(AccType7, CarVan5, hhSize5, Tenure5)
  names(mat2) = c("AccType7", "CarVan5", "hhSize5", "tenure5")
  mat2 = dplyr::left_join(mat2, AccTyp7_CarVan5_Tenure5_hhSize5_scot, by = c("AccType7", "CarVan5", "hhSize5", "tenure5"))
  if(anyNA(mat2$value)){stop("NAs in values")}
  mat2 = array(mat2$value,
               dim = c(length(AccType7), length(CarVan5), length(hhSize5), length(Tenure5)),
               dimnames = list(AccType7, CarVan5, hhSize5, Tenure5)
  )

  mat3 = expand.grid(AccType3, householdComp10, CarVan3, Tenure3, hhSize5)
  names(mat3) = c("AccType3", "householdComp10", "CarVan3", "Tenure3", "hhSize5")
  mat3 = dplyr::left_join(mat3, householdComp10_hhSize5_CarVan3_Tenure3_AccType3_scot, by = c("AccType3", "householdComp10", "CarVan3", "Tenure3", "hhSize5"))
  if(anyNA(mat3$value)){stop("NAs in values")}
  mat3 = array(mat3$value,
               dim = c(length(AccType3), length(householdComp10), length(CarVan3), length(Tenure3),length(hhSize5)),
               dimnames = list(AccType3, householdComp10, CarVan3, Tenure3, hhSize5)
  )

  mat4 = expand.grid(Tenure5,Tenure3,AccType7,CarVan5,CarVan3)
  names(mat4) = c("Tenure5","Tenure3","AccType7","CarVan5","CarVan3")
  mat4 = dplyr::left_join(mat4, AccType7_CarVan5_CarVan3_Tenure5_Tenure3_scot, by = c("Tenure5","Tenure3","AccType7","CarVan5","CarVan3"))
  if(anyNA(mat4$value)){stop("NAs in values")}
  mat4 = array(mat4$value,
               dim = c(length(Tenure5), length(Tenure3), length(AccType7), length(CarVan5),length(CarVan3)),
               dimnames = list(Tenure5, Tenure3, AccType7, CarVan5, CarVan3)
  )

  mat5 = expand.grid(AccType3,AccType7)
  names(mat5) = c("AccType3","AccType7")
  mat5 = dplyr::left_join(mat5, AccType7_AccType3_scot, by = c("AccType3","AccType7"))
  if(anyNA(mat5$value)){stop("NAs in values")}
  mat5 = array(mat5$value,
               dim = c(length(AccType3), length(AccType7)),
               dimnames = list(AccType3, AccType7)
  )

  # Seed Dim
  # householdComp10, CarVan5, CarVan3, Tenure5, Tenure3, hhSize5, AccType7, AccType3

  seed = array(1, dim = c(10,5,3,5,3,5,7,3))
  res <- mipfp::Ipfp(seed,
                     list(c(1,2,4,6),c(7,2,6,4),c(8,1,3,5,6),c(4,5,7,2,3),c(8,7)),
                     list(mat1, #1,2,4,6
                          mat2, #7,2,6,4
                          mat3, #8,1,3,5,6
                          mat4, #4,5,7,2,3
                          mat5)) #8,7

  result_df = expand.grid(
    householdComp10, CarVan5, CarVan3, Tenure5, Tenure3, hhSize5, AccType7, AccType3
  )
  names(result_df) = c("householdComp10", "CarVan5", "CarVan3", "Tenure5", "Tenure3", "hhSize5", "AccType7", "AccType3")
  result_df$households = int_trs(as.numeric(res$x.hat) * median(c(sum(mat1),sum(mat2),sum(mat3),sum(mat4),sum(mat5))))

  slt = strsplit(as.character(result_df$carSizeTenure),"_")

  result_df$CarVan5 = sapply(slt, `[[`, 1)
  result_df$hhSize5 = sapply(slt, `[[`, 2)
  result_df$Tenure5 = sapply(slt, `[[`, 3)

  result_df = result_df[,c("hhSize5","householdComp10","CarVan5","AccTyp7","Tenure5","households")]

  result_df$seed = result_df$households / 200
  result_df$seed = ifelse(result_df$seed > 1, 1,  result_df$seed)
  result_df$seed = ifelse(result_df$seed < 1 &    result_df$seed > 0.03, 0.75 , result_df$seed)
  result_df$seed = ifelse(result_df$seed < 0.03 & result_df$seed > 0.01, 0.5  , result_df$seed)
  result_df$seed = ifelse(result_df$seed < 0.01 & result_df$seed > 0   , 0.001, result_df$seed)

  result_df$seed[result_df$seed == 0] = 1e-15


  result_df

}


read_AccType7_AccType3_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_AccType7_AccType3_Scotland.csv"){

  raw_data <- readr::read_csv(path , skip = 11, col_names = FALSE, show_col_types = FALSE)
  raw_data = raw_data[1:3,1:8]
  names(raw_data) = c("AccType3","detached","semidetached","terraced","flatpurposebuilt","flatconverted","flatcommercial","caravan")

  raw_data$AccType3 = c("house","flat","caravan")
  raw_data$detached  = as.numeric(raw_data$detached)
  raw_data = tidyr::pivot_longer(raw_data, cols = names(raw_data)[2:8], names_to = "AccType7")

  raw_data

}



read_householdComp10_CarVan5_Tenure5_hhSize5_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_householdComp10_CarVan5_Tenure5_hhSize5_Scotland.xlsx") {

  p1 = readxl::read_xlsx(path, sheet = "0. One person", skip = 13, col_names = FALSE)
  p2 = readxl::read_xlsx(path, sheet = "1. Two people", skip = 13, col_names = FALSE)
  p3 = readxl::read_xlsx(path, sheet = "2. Three people", skip = 13, col_names = FALSE)
  p4 = readxl::read_xlsx(path, sheet = "3. Four people", skip = 13, col_names = FALSE)
  p5p = readxl::read_xlsx(path, sheet = "4. Five or more people", skip = 13, col_names = FALSE)

  nms = c("dud1","householdComp10",
          paste0(rep(c("outright","mortgage","socialrented","privaterented","rentfree","Total"), each = 5),
                 "_",
                 rep(c("car0","car1","car2","car3","car4+"), times = 6)))

  names(p1) = nms
  names(p2) = nms
  names(p3) = nms
  names(p4) = nms
  names(p5p) = nms

  nms2 = nms[nms != "dud1"]
  nms2 = nms2[!grepl("Total",nms2)]

  p1 = p1[1:10,nms2]
  p2 = p2[1:10,nms2]
  p3 = p3[1:10,nms2]
  p4 = p4[1:10,nms2]
  p5p = p5p[1:10,nms2]

  householdComp10 = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66")

  p1$householdComp10 = householdComp10
  p2$householdComp10 = householdComp10
  p3$householdComp10 = householdComp10
  p4$householdComp10 = householdComp10
  p5p$householdComp10 = householdComp10

  p1$hhSize5 = "p1"
  p2$hhSize5 = "p2"
  p3$hhSize5 = "p3"
  p4$hhSize5 = "p4"
  p5p$hhSize5 = "p5+"

  p1 = tidyr::pivot_longer(p1, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("tenure5","CarVan5"))
  p2 = tidyr::pivot_longer(p2, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("tenure5","CarVan5"))
  p3 = tidyr::pivot_longer(p3, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("tenure5","CarVan5"))
  p4 = tidyr::pivot_longer(p4, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("tenure5","CarVan5"))
  p5p = tidyr::pivot_longer(p5p, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("tenure5","CarVan5"))

  final = rbind(p1,p2,p3,p4,p5p)


}



read_AccTyp7_CarVan5_Tenure5_hhSize5_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_AccType7_CarVan5_Tenure5_hhSize5_Scotland.xlsx") {

  p1 = readxl::read_xlsx(path, sheet = "0. One person", skip = 13, col_names = FALSE)
  p2 = readxl::read_xlsx(path, sheet = "1. Two people", skip = 13, col_names = FALSE)
  p3 = readxl::read_xlsx(path, sheet = "2. Three people", skip = 13, col_names = FALSE)
  p4 = readxl::read_xlsx(path, sheet = "3. Four people", skip = 13, col_names = FALSE)
  p5p = readxl::read_xlsx(path, sheet = "4. Five or more people", skip = 13, col_names = FALSE)

  nms = c("dud1","CarVan5",
          paste0(rep(c("outright","mortgage","socialrented","privaterented","rentfree","Total"), each = 7),
                 "_",
                 rep(c("detached",
                       "semidetached",
                       "terraced",
                       "flatpurposebuilt",
                       "flatconverted",
                       "flatcommercial",
                       "caravan"), times = 6)))

  names(p1) = nms
  names(p2) = nms
  names(p3) = nms
  names(p4) = nms
  names(p5p) = nms

  nms2 = nms[nms != "dud1"]
  nms2 = nms2[!grepl("Total",nms2)]

  p1 = p1[1:5,nms2]
  p2 = p2[1:5,nms2]
  p3 = p3[1:5,nms2]
  p4 = p4[1:5,nms2]
  p5p = p5p[1:5,nms2]

  CarVan5 = c("car0","car1","car2","car3","car4+")

  p1$CarVan5 = CarVan5
  p2$CarVan5 = CarVan5
  p3$CarVan5 = CarVan5
  p4$CarVan5 = CarVan5
  p5p$CarVan5 = CarVan5

  p1$hhSize5 = "p1"
  p2$hhSize5 = "p2"
  p3$hhSize5 = "p3"
  p4$hhSize5 = "p4"
  p5p$hhSize5 = "p5+"

  p1 = tidyr::pivot_longer(p1, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("tenure5","AccType7"))
  p2 = tidyr::pivot_longer(p2, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("tenure5","AccType7"))
  p3 = tidyr::pivot_longer(p3, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("tenure5","AccType7"))
  p4 = tidyr::pivot_longer(p4, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("tenure5","AccType7"))
  p5p = tidyr::pivot_longer(p5p, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("tenure5","AccType7"))

  final = rbind(p1,p2,p3,p4,p5p)
  final

}


read_householdComp10_hhSize5_CarVan3_Tenure3_AccType3_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_householdComp10_hhSize5_CarVan3_Tenure3_AccType3.Scotland.xlsx") {

  p1 = readxl::read_xlsx(path, sheet = "0. One person household Aged 66", skip = 14, col_names = FALSE)
  p2 = readxl::read_xlsx(path, sheet = "1. One person household Aged un", skip = 14, col_names = FALSE)
  p3 = readxl::read_xlsx(path, sheet = "2. One family household All age", skip = 14, col_names = FALSE)
  p4 = readxl::read_xlsx(path, sheet = "3. One family household Couple ", skip = 14, col_names = FALSE)
  p5 = readxl::read_xlsx(path, sheet = "4. One family household Couple ", skip = 14, col_names = FALSE)
  p6 = readxl::read_xlsx(path, sheet = "5. One family household Couple ", skip = 14, col_names = FALSE)
  p7 = readxl::read_xlsx(path, sheet = "6. One family household Lone pa", skip = 14, col_names = FALSE)
  p8 = readxl::read_xlsx(path, sheet = "7. One family household Lone pa", skip = 14, col_names = FALSE)
  p9 = readxl::read_xlsx(path, sheet = "8. Other household types With d", skip = 14, col_names = FALSE)
  p10 = readxl::read_xlsx(path, sheet = "9. Other household types Other ", skip = 14, col_names = FALSE)

  nms = c("dud1","AccType3",
          paste0(rep(c("car0","car1","car2+","total"), each = 15),
                 "_",
                 rep(rep(c("owned","socialrented","privaterented"), each = 5), times = 4),
                 "_",
                 rep(c("p1","p2","p3","p4","p5+"), times = 12)))

  names(p1) = nms
  names(p2) = nms
  names(p3) = nms
  names(p4) = nms
  names(p5) = nms
  names(p6) = nms
  names(p7) = nms
  names(p8) = nms
  names(p9) = nms
  names(p10) = nms

  nms2 = nms[nms != "dud1"]
  nms2 = nms2[!grepl("total",nms2)]

  p1 = p1[1:3,nms2]
  p2 = p2[1:3,nms2]
  p3 = p3[1:3,nms2]
  p4 = p4[1:3,nms2]
  p5 = p5[1:3,nms2]
  p6 = p6[1:3,nms2]
  p7 = p7[1:3,nms2]
  p8 = p8[1:3,nms2]
  p9 = p9[1:3,nms2]
  p10 = p10[1:3,nms2]

  AccType3 = c("house","flat","caravan")

  p1$AccType3 = AccType3
  p2$AccType3 = AccType3
  p3$AccType3 = AccType3
  p4$AccType3 = AccType3
  p5$AccType3 = AccType3
  p6$AccType3 = AccType3
  p7$AccType3 = AccType3
  p8$AccType3 = AccType3
  p9$AccType3 = AccType3
  p10$AccType3 = AccType3

  p1$householdComp10 = "OnePersonOver66"
  p2$householdComp10 = "OnePersonOther"
  p3$householdComp10 = "FamilyOver66"
  p4$householdComp10 = "CoupleNoChildren"
  p5$householdComp10 = "CoupleChildren"
  p6$householdComp10 = "CoupleNonDepChildren"
  p7$householdComp10 = "LoneParent"
  p8$householdComp10 = "LoneParentNonDepChildren"
  p9$householdComp10 = "OtherChildren"
  p10$householdComp10 = "OtherIncStudentOrOver66"

  p1 = tidyr::pivot_longer(p1, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("CarVan3","Tenure3","hhSize5"))
  p2 = tidyr::pivot_longer(p2, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("CarVan3","Tenure3","hhSize5"))
  p3 = tidyr::pivot_longer(p3, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("CarVan3","Tenure3","hhSize5"))
  p4 = tidyr::pivot_longer(p4, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("CarVan3","Tenure3","hhSize5"))
  p5 = tidyr::pivot_longer(p5, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("CarVan3","Tenure3","hhSize5"))
  p6 = tidyr::pivot_longer(p6, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("CarVan3","Tenure3","hhSize5"))
  p7 = tidyr::pivot_longer(p7, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("CarVan3","Tenure3","hhSize5"))
  p8 = tidyr::pivot_longer(p8, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("CarVan3","Tenure3","hhSize5"))
  p9 = tidyr::pivot_longer(p9, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("CarVan3","Tenure3","hhSize5"))
  p10 = tidyr::pivot_longer(p10, cols = nms2[2:length(nms2)], names_sep = "_", names_to = c("CarVan3","Tenure3","hhSize5"))

  final = rbind(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
  final

}

read_AccType7_CarVan5_CarVan3_Tenure5_Tenure3_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_AccType7_CarVan5_CarVan3_Tenure5_Tenure3_Scotland.xlsx") {

  p1 = readxl::read_xlsx(path, sheet = "0. Whole house or bungalow Deta", skip = 13, col_names = FALSE)
  p2 = readxl::read_xlsx(path, sheet = "1. Whole house or bungalow Semi", skip = 13, col_names = FALSE)
  p3 = readxl::read_xlsx(path, sheet = "2. Whole house or bungalow Terr", skip = 13, col_names = FALSE)
  p4 = readxl::read_xlsx(path, sheet = "3. Flat, maisonette or apartmen", skip = 13, col_names = FALSE)
  p5 = readxl::read_xlsx(path, sheet = "4. Flat, maisonette or apartmen", skip = 13, col_names = FALSE)
  p6 = readxl::read_xlsx(path, sheet = "5. Flat, maisonette or apartmen", skip = 13, col_names = FALSE)
  p7 = readxl::read_xlsx(path, sheet = "6. Caravan or other mobile or t", skip = 13, col_names = FALSE)

  nms = c("dud1","Tenure5","Tenure3",
          paste0(rep(c("car0","car1","car2","car3","car4+","total"), each = 3),
                 "_",
                 rep(c("car0","car1","car2+"), times = 6)
                 ))

  names(p1) = nms
  names(p2) = nms
  names(p3) = nms
  names(p4) = nms
  names(p5) = nms
  names(p6) = nms
  names(p7) = nms

  nms2 = nms[nms != "dud1"]
  nms2 = nms2[!grepl("total",nms2)]

  p1 = p1[1:15,nms2]
  p2 = p2[1:15,nms2]
  p3 = p3[1:15,nms2]
  p4 = p4[1:15,nms2]
  p5 = p5[1:15,nms2]
  p6 = p6[1:15,nms2]
  p7 = p7[1:15,nms2]

  Tenure3 = rep(c("owned","socialrented","privaterented"), times = 5)
  Tenure5 = rep(c("outright","mortgage","socialrented","privaterented","rentfree"), each = 3)

  p1$Tenure3 = Tenure3
  p2$Tenure3 = Tenure3
  p3$Tenure3 = Tenure3
  p4$Tenure3 = Tenure3
  p5$Tenure3 = Tenure3
  p6$Tenure3 = Tenure3
  p7$Tenure3 = Tenure3

  p1$Tenure5 = Tenure5
  p2$Tenure5 = Tenure5
  p3$Tenure5 = Tenure5
  p4$Tenure5 = Tenure5
  p5$Tenure5 = Tenure5
  p6$Tenure5 = Tenure5
  p7$Tenure5 = Tenure5


  p1$AccType7 = "detached"
  p2$AccType7 = "semidetached"
  p3$AccType7 = "terraced"
  p4$AccType7 = "flatpurposebuilt"
  p5$AccType7 = "flatconverted"
  p6$AccType7 = "flatcommercial"
  p7$AccType7 = "caravan"


  p1 = tidyr::pivot_longer(p1, cols = nms2[3:length(nms2)], names_sep = "_", names_to = c("CarVan5","CarVan3"))
  p2 = tidyr::pivot_longer(p2, cols = nms2[3:length(nms2)], names_sep = "_", names_to = c("CarVan5","CarVan3"))
  p3 = tidyr::pivot_longer(p3, cols = nms2[3:length(nms2)], names_sep = "_", names_to = c("CarVan5","CarVan3"))
  p4 = tidyr::pivot_longer(p4, cols = nms2[3:length(nms2)], names_sep = "_", names_to = c("CarVan5","CarVan3"))
  p5 = tidyr::pivot_longer(p5, cols = nms2[3:length(nms2)], names_sep = "_", names_to = c("CarVan5","CarVan3"))
  p6 = tidyr::pivot_longer(p6, cols = nms2[3:length(nms2)], names_sep = "_", names_to = c("CarVan5","CarVan3"))
  p7 = tidyr::pivot_longer(p7, cols = nms2[3:length(nms2)], names_sep = "_", names_to = c("CarVan5","CarVan3"))


  final = rbind(p1,p2,p3,p4,p5,p6,p7)
  final

}




read_AccType7_AccTyp3_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_AccType7_AccType3_Scotland.csv") {

  raw_data <- readr::read_csv(path , skip = 12, col_names = FALSE, show_col_types = FALSE)

  simplified_names <- c("IZName",
                        "car0_p1", "car0_p2", "car0_p3", "car0_p4", "car0_p5+",
                        "car1_p1", "car1_p2", "car1_p3", "car1_p4", "car1_p5+",
                        "car2_p1", "car2_p2", "car2_p3", "car2_p4", "car2_p5+",
                        "car3_p1", "car3_p2", "car3_p3", "car3_p4", "car3_p5+",
                        "car4+_p1", "car4+_p2", "car4+_p3", "car4+_p4", "car4+_p5+",
                        "Total_p1", "Total_p2", "Total_p3", "Total_p4", "Total_p5+")


  colnames(raw_data) <- simplified_names

  raw_data = raw_data[,c("IZName",
                         "car0_p1", "car0_p2", "car0_p3", "car0_p4", "car0_p5+",
                         "car1_p1", "car1_p2", "car1_p3", "car1_p4", "car1_p5+",
                         "car2_p1", "car2_p2", "car2_p3", "car2_p4", "car2_p5+",
                         "car3_p1", "car3_p2", "car3_p3", "car3_p4", "car3_p5+",
                         "car4+_p1", "car4+_p2", "car4+_p3", "car4+_p4", "car4+_p5+")]

  raw_data = raw_data[!raw_data$IZName %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  if(all(raw_data$IZName == bounds_iz22$IZName)){
    raw_data$IZCode = bounds_iz22$IZCode
  } else {
    stop("Unable to add IZCode")
  }

  raw_data = tidyr::pivot_longer(raw_data,
                                 cols = names(raw_data)[2:(ncol(raw_data)-1)],
                                 names_sep = "_",
                                 names_to = c("CarVan5","hhSize5"))

  names(raw_data)[names(raw_data) == "value"] = "households"

  return(raw_data)
}
