build_synth_pop_seed_scotland = function(path_data = file.path(parameters$path_data,"population_scotland")){

  householdComp10_CarVan5_Tenure5_hhSize5_scot = read_householdComp10_CarVan5_Tenure5_hhSize5_scot(file.path(path_data,"scotlandcenus2022_householdComp10_CarVan5_Tenure5_hhSize5_Scotland.xlsx"))
  AccTyp7_CarVan5_Tenure5_hhSize5_scot = read_AccTyp7_CarVan5_Tenure5_hhSize5_scot(file.path(path_data,"scotlandcenus2022_AccType7_CarVan5_Tenure5_hhSize5_Scotland.xlsx"))

  householdComp10_CarVan5_Tenure5_hhSize5_scot$carSizeTenure = paste0(householdComp10_CarVan5_Tenure5_hhSize5_scot$CarVan5,"_",
                                                                      householdComp10_CarVan5_Tenure5_hhSize5_scot$hhSize5,"_",
                                                                      householdComp10_CarVan5_Tenure5_hhSize5_scot$tenure5)

  AccTyp7_CarVan5_Tenure5_hhSize5_scot$carSizeTenure = paste0(AccTyp7_CarVan5_Tenure5_hhSize5_scot$CarVan5,"_",
                                                              AccTyp7_CarVan5_Tenure5_hhSize5_scot$hhSize5,"_",
                                                              AccTyp7_CarVan5_Tenure5_hhSize5_scot$tenure5)


  householdComp10_CarVan5_Tenure5_hhSize5_scot = householdComp10_CarVan5_Tenure5_hhSize5_scot[,c("carSizeTenure","householdComp10","value")]
  AccTyp7_CarVan5_Tenure5_hhSize5_scot = AccTyp7_CarVan5_Tenure5_hhSize5_scot[,c("carSizeTenure","AccType7","value")]

  householdComp10_CarVan5_Tenure5_hhSize5_scot = tidyr::pivot_wider(householdComp10_CarVan5_Tenure5_hhSize5_scot, names_from = "householdComp10", values_from = "value")
  AccTyp7_CarVan5_Tenure5_hhSize5_scot = tidyr::pivot_wider(AccTyp7_CarVan5_Tenure5_hhSize5_scot, names_from = "AccType7", values_from = "value")

  householdComp10_CarVan5_Tenure5_hhSize5_scot = householdComp10_CarVan5_Tenure5_hhSize5_scot[order(householdComp10_CarVan5_Tenure5_hhSize5_scot$carSizeTenure),]
  AccTyp7_CarVan5_Tenure5_hhSize5_scot = AccTyp7_CarVan5_Tenure5_hhSize5_scot[order(AccTyp7_CarVan5_Tenure5_hhSize5_scot$carSizeTenure),]

  #summary(householdComp10_CarVan5_Tenure5_hhSize5_scot$carSizeTenure == AccTyp7_CarVan5_Tenure5_hhSize5_scot$carSizeTenure)

  hh10 = as.matrix(householdComp10_CarVan5_Tenure5_hhSize5_scot[,2:ncol(householdComp10_CarVan5_Tenure5_hhSize5_scot)])
  rownames(hh10) = householdComp10_CarVan5_Tenure5_hhSize5_scot$carSizeTenure

  Acc7 = as.matrix(AccTyp7_CarVan5_Tenure5_hhSize5_scot[,2:ncol(AccTyp7_CarVan5_Tenure5_hhSize5_scot)])
  rownames(Acc7) = AccTyp7_CarVan5_Tenure5_hhSize5_scot$carSizeTenure

  Acc7 = match_matrix_rsums(hh10,Acc7)


  seed = array(1, dim = c(125,10,7))

  result = humanleague::qisi(seed,
             indices = list(c(1,2),c(1,3)),
             marginals = list(hh10,
                              Acc7)
             )

  result_df = expand.grid(
    rownames(hh10),
    colnames(hh10),
    colnames(Acc7)
  )
  names(result_df) = c("carSizeTenure","householdComp10","AccTyp7")
  result_df$households = as.numeric(result$result)

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
