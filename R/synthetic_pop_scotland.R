#' Read Hhcomp Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_hhComp_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_householdComp10_householdComp4_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 10, col_names = FALSE)

  names(raw) = c("counting","LSOA21CD","householdComp4","householdComp10","households")
  raw$counting = NULL
  raw = raw[!is.na(raw$LSOA21CD),]
  raw = raw[raw$LSOA21CD != "Total",]
  raw = raw[substr(raw$LSOA21CD,1,1) == "S",]
  raw = raw[raw$householdComp4 != "Total",]

  raw$householdComp4[raw$householdComp4 == "One person household"] = "oneperson"
  raw$householdComp4[raw$householdComp4 == "One family household: Couple family"] = "couplefamily"
  raw$householdComp4[raw$householdComp4 == "One family household: Lone parent"] = "loneparentfamily"
  raw$householdComp4[raw$householdComp4 == "Other household types"] = "other"

  raw$householdComp10[raw$householdComp10 == "One person household: Aged 66 and over"] = "OnePersonOver66"
  raw$householdComp10[raw$householdComp10 == "One person household: Aged under 66" ] = "OnePersonOther"
  raw$householdComp10[raw$householdComp10 == "One family household: All aged 66 and over"] = "FamilyOver66"
  raw$householdComp10[raw$householdComp10 == "One family household: Couple family: No children" ] = "CoupleNoChildren"
  raw$householdComp10[raw$householdComp10 == "One family household: Couple family: With dependent children"] = "CoupleChildren"
  raw$householdComp10[raw$householdComp10 == "One family household: Couple family: All children non-dependent"] = "CoupleNonDepChildren"
  raw$householdComp10[raw$householdComp10 == "One family household: Lone parent: With dependent children" ] = "LoneParent"
  raw$householdComp10[raw$householdComp10 == "One family household: Lone parent: All children non-dependent" ] = "LoneParentNonDepChildren"
  raw$householdComp10[raw$householdComp10 == "Other household types: With dependent children" ] = "OtherChildren"
  raw$householdComp10[raw$householdComp10 == "Other household types: Other (including all full-time students and all aged 66 and over)"] = "OtherIncStudentOrOver66"

  raw

}

#' Read Hhsize Householdcomp Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @param bounds_iz22 IZ boundaries providing IZCode/IZName; used to match
#'   the extract's zone names to codes.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_hhSize_HouseholdComp_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_householdComp10_hhSize5_IntermediateZone.csv", bounds_iz22) {

  raw_data <- readr::read_csv(path , skip = 12, col_names = FALSE, show_col_types = FALSE)

  simplified_names <- c("IZName",
                        "OnePersonOver66_p1", "OnePersonOver66_p2", "OnePersonOver66_p3", "OnePersonOver66_p4", "OnePersonOver66_p5+",
                        "OnePersonOther_p1", "OnePersonOther_p2", "OnePersonOther_p3", "OnePersonOther_p4", "OnePersonOther_p5+",
                        "FamilyOver66_p1", "FamilyOver66_p2", "FamilyOver66_p3", "FamilyOver66_p4", "FamilyOver66_p5+",
                        "CoupleNoChildren_p1", "CoupleNoChildren_p2", "CoupleNoChildren_p3", "CoupleNoChildren_p4", "CoupleNoChildren_p5+",
                        "CoupleChildren_p1", "CoupleChildren_p2", "CoupleChildren_p3", "CoupleChildren_p4", "CoupleChildren_p5+",
                        "CoupleNonDepChildren_p1", "CoupleNonDepChildren_p2", "CoupleNonDepChildren_p3", "CoupleNonDepChildren_p4", "CoupleNonDepChildren_p5+",
                        "LoneParent_p1", "LoneParent_p2", "LoneParent_p3", "LoneParent_p4", "LoneParent_p5+",
                        "LoneParentNonDepChildren_p1", "LoneParentNonDepChildren_p2", "LoneParentNonDepChildren_p3", "LoneParentNonDepChildren_p4", "LoneParentNonDepChildren_p5+",
                        "OtherChildren_p1", "OtherChildren_p2", "OtherChildren_p3", "OtherChildren_p4", "OtherChildren_p5+",
                        "OtherIncStudentOrOver66_p1", "OtherIncStudentOrOver66_p2", "OtherIncStudentOrOver66_p3", "OtherIncStudentOrOver66_p4", "OtherIncStudentOrOver66_p5+",
                        "Total_p1", "Total_p2", "Total_p3", "Total_p4", "Total_p5+")


  colnames(raw_data) <- simplified_names

  raw_data = raw_data[,c("IZName",
                         "OnePersonOver66_p1", "OnePersonOver66_p2", "OnePersonOver66_p3", "OnePersonOver66_p4", "OnePersonOver66_p5+",
                         "OnePersonOther_p1", "OnePersonOther_p2", "OnePersonOther_p3", "OnePersonOther_p4", "OnePersonOther_p5+",
                         "FamilyOver66_p1", "FamilyOver66_p2", "FamilyOver66_p3", "FamilyOver66_p4", "FamilyOver66_p5+",
                         "CoupleNoChildren_p1", "CoupleNoChildren_p2", "CoupleNoChildren_p3", "CoupleNoChildren_p4", "CoupleNoChildren_p5+",
                         "CoupleChildren_p1", "CoupleChildren_p2", "CoupleChildren_p3", "CoupleChildren_p4", "CoupleChildren_p5+",
                         "CoupleNonDepChildren_p1", "CoupleNonDepChildren_p2", "CoupleNonDepChildren_p3", "CoupleNonDepChildren_p4", "CoupleNonDepChildren_p5+",
                         "LoneParent_p1", "LoneParent_p2", "LoneParent_p3", "LoneParent_p4", "LoneParent_p5+",
                         "LoneParentNonDepChildren_p1", "LoneParentNonDepChildren_p2", "LoneParentNonDepChildren_p3", "LoneParentNonDepChildren_p4", "LoneParentNonDepChildren_p5+",
                         "OtherChildren_p1", "OtherChildren_p2", "OtherChildren_p3", "OtherChildren_p4", "OtherChildren_p5+",
                         "OtherIncStudentOrOver66_p1", "OtherIncStudentOrOver66_p2", "OtherIncStudentOrOver66_p3", "OtherIncStudentOrOver66_p4", "OtherIncStudentOrOver66_p5+")]

  raw_data = raw_data[!raw_data$IZName %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  if(all(raw_data$IZName == bounds_iz22$IZName)){
    raw_data$IZCode = bounds_iz22$IZCode
  } else {
    stop("Unable to add IZCode")
  }

  raw_data = tidyr::pivot_longer(raw_data,
                                  cols = names(raw_data)[2:(ncol(raw_data)-1)],
                                  names_sep = "_",
                                  names_to = c("hhComp10","hhSize5"))

  names(raw_data)[names(raw_data) == "value"] = "households"


  return(raw_data)
}

#' Read Hhsize Tenure Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @param bounds_iz22 IZ boundaries providing IZCode/IZName; used to match
#'   the extract's zone names to codes.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_hhSize_Tenure_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_Tenure5_hhSize5_IntermediateZone.csv",bounds_iz22) {

  raw_data <- readr::read_csv(path , skip = 12, col_names = FALSE, show_col_types = FALSE)

  simplified_names <- c("IZName",
                        "p1_outright", "p1_mortgage", "p1_socialrented", "p1_privaterented", "p1_rentfree",
                        "p2_outright", "p2_mortgage", "p2_socialrented", "p2_privaterented", "p2_rentfree",
                        "p3_outright", "p3_mortgage", "p3_socialrented", "p3_privaterented", "p3_rentfree",
                        "p4_outright", "p4_mortgage", "p4_socialrented", "p4_privaterented", "p4_rentfree",
                        "p5+_outright", "p5+_mortgage", "p5+_socialrented", "p5+_privaterented", "p5+_rentfree",
                        "Total_outright", "Total_mortgage", "Total_socialrented", "Total_privaterented", "Total_rentfree")


  colnames(raw_data) <- simplified_names

  raw_data = raw_data[,c("IZName",
                         "p1_outright", "p1_mortgage", "p1_socialrented", "p1_privaterented", "p1_rentfree",
                         "p2_outright", "p2_mortgage", "p2_socialrented", "p2_privaterented", "p2_rentfree",
                         "p3_outright", "p3_mortgage", "p3_socialrented", "p3_privaterented", "p3_rentfree",
                         "p4_outright", "p4_mortgage", "p4_socialrented", "p4_privaterented", "p4_rentfree",
                         "p5+_outright", "p5+_mortgage", "p5+_socialrented", "p5+_privaterented", "p5+_rentfree")]

  raw_data = raw_data[!raw_data$IZName %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  if(all(raw_data$IZName == bounds_iz22$IZName)){
    raw_data$IZCode = bounds_iz22$IZCode
  } else {
    stop("Unable to add IZCode")
  }

  raw_data = tidyr::pivot_longer(raw_data,
                                 cols = names(raw_data)[2:(ncol(raw_data)-1)],
                                 names_sep = "_",
                                 names_to = c("hhSize5","tenure5"))

  names(raw_data)[names(raw_data) == "value"] = "households"

  return(raw_data)
}

#' Read Hhsize Acctype Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @param bounds_iz22 IZ boundaries providing IZCode/IZName; used to match
#'   the extract's zone names to codes.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_hhSize_AccType_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_AccType7_hhSize5_IntermediateZone.csv",bounds_iz22) {

  raw_data <- readr::read_csv(path , skip = 12, col_names = FALSE, show_col_types = FALSE)

  simplified_names <- c("IZName",
                        "p1_detached", "p1_semidetached", "p1_terraced", "p1_flatpurposebuilt", "p1_flatconverted",
                        "p1_flatcommercial", "p1_caravan", "p2_detached", "p2_semidetached", "p2_terraced",
                        "p2_flatpurposebuilt", "p2_flatconverted", "p2_flatcommercial", "p2_caravan", "p3_detached",
                        "p3_semidetached", "p3_terraced", "p3_flatpurposebuilt", "p3_flatconverted",
                        "p3_flatcommercial", "p3_caravan", "p4_detached", "p4_semidetached", "p4_terraced",
                        "p4_flatpurposebuilt", "p4_flatconverted", "p4_flatcommercial", "p4_caravan", "p5+_detached",
                        "p5+_semidetached", "p5+_terraced", "p5+_flatpurposebuilt", "p5+_flatconverted",
                        "p5+_flatcommercial", "p5+_caravan", "Total_detached", "Total_semidetached", "Total_terraced",
                        "Total_flatpurposebuilt", "Total_flatconverted", "Total_flatcommercial", "Total_caravan")


  colnames(raw_data) <- simplified_names

  raw_data = raw_data[,c("IZName",
                         "p1_detached", "p1_semidetached", "p1_terraced", "p1_flatpurposebuilt", "p1_flatconverted",
                         "p1_flatcommercial", "p1_caravan", "p2_detached", "p2_semidetached", "p2_terraced",
                         "p2_flatpurposebuilt", "p2_flatconverted", "p2_flatcommercial", "p2_caravan", "p3_detached",
                         "p3_semidetached", "p3_terraced", "p3_flatpurposebuilt", "p3_flatconverted",
                         "p3_flatcommercial", "p3_caravan", "p4_detached", "p4_semidetached", "p4_terraced",
                         "p4_flatpurposebuilt", "p4_flatconverted", "p4_flatcommercial", "p4_caravan", "p5+_detached",
                         "p5+_semidetached", "p5+_terraced", "p5+_flatpurposebuilt", "p5+_flatconverted",
                         "p5+_flatcommercial", "p5+_caravan")]

  raw_data = raw_data[!raw_data$IZName %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  if(all(raw_data$IZName == bounds_iz22$IZName)){
    raw_data$IZCode = bounds_iz22$IZCode
  } else {
    stop("Unable to add IZCode")
  }


  raw_data = tidyr::pivot_longer(raw_data,
                                 cols = names(raw_data)[2:(ncol(raw_data)-1)],
                                 names_sep = "_",
                                 names_to = c("hhSize5","AccType7"))

  names(raw_data)[names(raw_data) == "value"] = "households"

  return(raw_data)
}

#' Read Tenure Householdcomp Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @param bounds_iz22 IZ boundaries providing IZCode/IZName; used to match
#'   the extract's zone names to codes.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_Tenure_HouseholdComp_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_Tenure5_householdComp10_IntermediateZone.csv",bounds_iz22) {

  raw_data <- readr::read_csv(path , skip = 12, col_names = FALSE, show_col_types = FALSE)

  simplified_names <- c("IZName",
                        "outright_OnePersonOver66", "outright_OnePersonOther", "outright_FamilyOver66", "outright_CoupleNoChildren", "outright_CoupleChildren",
                        "outright_CoupleNonDepChildren", "outright_LoneParent", "outright_LoneParentNonDepChildren",
                        "outright_OtherChildren", "outright_OtherIncStudentOrOver66",
                        "mortgage_OnePersonOver66", "mortgage_OnePersonOther", "mortgage_FamilyOver66", "mortgage_CoupleNoChildren", "mortgage_CoupleChildren",
                        "mortgage_CoupleNonDepChildren", "mortgage_LoneParent", "mortgage_LoneParentNonDepChildren",
                        "mortgage_OtherChildren", "mortgage_OtherIncStudentOrOver66",
                        "socialrented_OnePersonOver66", "socialrented_OnePersonOther", "socialrented_FamilyOver66", "socialrented_CoupleNoChildren", "socialrented_CoupleChildren",
                        "socialrented_CoupleNonDepChildren", "socialrented_LoneParent", "socialrented_LoneParentNonDepChildren",
                        "socialrented_OtherChildren", "socialrented_OtherIncStudentOrOver66",
                        "privaterented_OnePersonOver66", "privaterented_OnePersonOther", "privaterented_FamilyOver66", "privaterented_CoupleNoChildren", "privaterented_CoupleChildren",
                        "privaterented_CoupleNonDepChildren", "privaterented_LoneParent", "privaterented_LoneParentNonDepChildren",
                        "privaterented_OtherChildren", "privaterented_OtherIncStudentOrOver66",
                        "rentfree_OnePersonOver66", "rentfree_OnePersonOther", "rentfree_FamilyOver66", "rentfree_CoupleNoChildren", "rentfree_CoupleChildren",
                        "rentfree_CoupleNonDepChildren", "rentfree_LoneParent", "rentfree_LoneParentNonDepChildren",
                        "rentfree_OtherChildren", "rentfree_OtherIncStudentOrOver66",
                        "Total_OnePersonOver66", "Total_OnePersonOther", "Total_FamilyOver66", "Total_CoupleNoChildren", "Total_CoupleChildren",
                        "Total_CoupleNonDepChildren", "Total_LoneParent", "Total_LoneParentNonDepChildren",
                        "Total_OtherChildren", "Total_OtherIncStudentOrOver66")


  colnames(raw_data) <- simplified_names

  raw_data = raw_data[,c("IZName",
                         "outright_OnePersonOver66", "outright_OnePersonOther", "outright_FamilyOver66", "outright_CoupleNoChildren", "outright_CoupleChildren",
                         "outright_CoupleNonDepChildren", "outright_LoneParent", "outright_LoneParentNonDepChildren",
                         "outright_OtherChildren", "outright_OtherIncStudentOrOver66",
                         "mortgage_OnePersonOver66", "mortgage_OnePersonOther", "mortgage_FamilyOver66", "mortgage_CoupleNoChildren", "mortgage_CoupleChildren",
                         "mortgage_CoupleNonDepChildren", "mortgage_LoneParent", "mortgage_LoneParentNonDepChildren",
                         "mortgage_OtherChildren", "mortgage_OtherIncStudentOrOver66",
                         "socialrented_OnePersonOver66", "socialrented_OnePersonOther", "socialrented_FamilyOver66", "socialrented_CoupleNoChildren", "socialrented_CoupleChildren",
                         "socialrented_CoupleNonDepChildren", "socialrented_LoneParent", "socialrented_LoneParentNonDepChildren",
                         "socialrented_OtherChildren", "socialrented_OtherIncStudentOrOver66",
                         "privaterented_OnePersonOver66", "privaterented_OnePersonOther", "privaterented_FamilyOver66", "privaterented_CoupleNoChildren", "privaterented_CoupleChildren",
                         "privaterented_CoupleNonDepChildren", "privaterented_LoneParent", "privaterented_LoneParentNonDepChildren",
                         "privaterented_OtherChildren", "privaterented_OtherIncStudentOrOver66",
                         "rentfree_OnePersonOver66", "rentfree_OnePersonOther", "rentfree_FamilyOver66", "rentfree_CoupleNoChildren", "rentfree_CoupleChildren",
                         "rentfree_CoupleNonDepChildren", "rentfree_LoneParent", "rentfree_LoneParentNonDepChildren",
                         "rentfree_OtherChildren", "rentfree_OtherIncStudentOrOver66")]

  raw_data = raw_data[!raw_data$IZName %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  if(all(raw_data$IZName == bounds_iz22$IZName)){
    raw_data$IZCode = bounds_iz22$IZCode
  } else {
    stop("Unable to add IZCode")
  }

  raw_data = tidyr::pivot_longer(raw_data,
                                 cols = names(raw_data)[2:(ncol(raw_data)-1)],
                                 names_sep = "_",
                                 names_to = c("tenure5","hhComp10"))

  names(raw_data)[names(raw_data) == "value"] = "households"

  return(raw_data)
}

#' Read Acc Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_Acc_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_AccType7_AccType3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  AccType3 = c("house","flat","caravan","total")
  AccType7 = c("detached","semidetached","terraced","flatpurposebuilt","flatconverted","flatcommercial","caravan")

  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(AccType3, each = 7),
                   "_",
                   rep(AccType7, times = 4)),
                 "dud")
  raw$dud = NULL
  raw = raw[,!grepl("total",names(raw))]
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("AccType3","AccType7"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Carvan Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_CarVan_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_CarVan5_CarVan3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  CarVan3 = c("car0","car1","car2+","total")
  CarVan5 = c("car0","car1","car2","car3","car4+")

  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(CarVan3, each = 5),
                   "_",
                   rep(CarVan5, times = 4)),
                 "dud")
  raw$dud = NULL
  raw = raw[,!grepl("total",names(raw))]
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("CarVan3","CarVan5"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Hhsize Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_hhSize_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_hhSize5_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 11, col_names = FALSE)

  names(raw) = c("LSOA21CD","p1","p2","p3","p4","p5+","Total","dummy")
  raw = raw[,c("LSOA21CD","p1","p2","p3","p4","p5+")]

  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw

}

#' Read Tenure Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_Tenure_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_Tenure5_Tenure3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  Tenure3 = c("owned","socialrented","privaterented")
  Tenure5 = c("outright","mortgage","socialrented","privaterented","rentfree")

  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(Tenure3, each = 5),
                   "_",
                   rep(Tenure5, times = 3)),
                 "dud")
  raw$dud = NULL
  raw = raw[,!grepl("total",names(raw))]
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("Tenure3","Tenure5"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Acctype Carvan Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @param bounds_iz22 IZ boundaries providing IZCode/IZName; used to match
#'   the extract's zone names to codes.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_AccType_CarVan_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_AccType7_CarVan3_IntermediateZone.csv", bounds_iz22){

  raw_data = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  names(raw_data) = c("IZName","car0_detached","car1_semidetached",
                 "car2+_terraced","car0_flatpurposebuilt","car1_flatconverted",
                 "car2+_flatcommercial","car0_caravan","car1_detached",
                 "car2+_semidetached","car0_terraced","car1_flatpurposebuilt",
                 "car2+_flatconverted","car0_flatcommercial","car1_caravan",
                 "car2+_detached","car0_semidetached","car1_terraced",
                 "car2+_flatpurposebuilt","car0_flatconverted","car1_flatcommercial",
                 "car2+_caravan","car0_detached","car1_semidetached",
                 "car2+_terraced","IZName_flatpurposebuilt","car0_flatconverted",
                 "car1_flatcommercial","car2+_caravan","car0_detached",
                 "car1_semidetached","car2+_terraced","car0_flatpurposebuilt",
                 "car1_flatconverted","car2+_flatcommercial","car0_caravan",
                 "total_1","total_2","total_3","total_4","total_5","total_6",
                 "dummy")

  raw_data = raw_data[,c("IZName","car0_detached","car1_semidetached",
               "car2+_terraced","car0_flatpurposebuilt","car1_flatconverted",
               "car2+_flatcommercial","car0_caravan","car1_detached",
               "car2+_semidetached","car0_terraced","car1_flatpurposebuilt",
               "car2+_flatconverted","car0_flatcommercial","car1_caravan",
               "car2+_detached","car0_semidetached","car1_terraced",
               "car2+_flatpurposebuilt","car0_flatconverted","car1_flatcommercial",
               "car2+_caravan","car0_detached","car1_semidetached",
               "car2+_terraced","IZName_flatpurposebuilt","car0_flatconverted",
               "car1_flatcommercial","car2+_caravan","car0_detached",
               "car1_semidetached","car2+_terraced","car0_flatpurposebuilt",
               "car1_flatconverted","car2+_flatcommercial","car0_caravan")]

  raw_data = raw_data[!raw_data$IZName %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  if(all(raw_data$IZName == bounds_iz22$IZName)){
    raw_data$IZCode = bounds_iz22$IZCode
  } else {
    stop("Unable to add IZCode")
  }

  raw_data = tidyr::pivot_longer(raw_data,
                                 cols = names(raw_data)[2:(ncol(raw_data)-1)],
                                 names_sep = "_",
                                 names_to = c("AccType7","CarVan3"))

  names(raw_data)[names(raw_data) == "value"] = "households"

  raw

}

#' Read Acctype Householdcomp Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @param bounds_iz22 IZ boundaries providing IZCode/IZName; used to match
#'   the extract's zone names to codes.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_AccType_householdComp_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_AccType7_householdComp4_IntermediateZone.csv",bounds_iz22) {
  # Read the CSV file, skipping the first few lines to remove headers
  raw_data <- readr::read_csv(path , skip = 12, col_names = FALSE, show_col_types = FALSE)

  # Define simplified column names
  simplified_names <- c("IZName", "p1_detached", "p1_semidetached",
                        "p1_terraced", "p1_flatpurposebuilt",
                        "p1_flatconverted", "p1_flatcommercial",
                        "p1_caravan", "Couple_detached", "Couple_semidetached",
                        "Couple_terraced", "Couple_flatpurposebuilt", "Couple_flatconverted",
                        "Couple_flatcommercial", "Couple_caravan", "LoneParent_detached",
                        "LoneParent_semidetached", "LoneParent_terraced", "LoneParent_flatpurposebuilt",
                        "LoneParent_flatconverted", "LoneParent_flatcommercial",
                        "LoneParent_caravan", "Other_detached", "Other_semidetached",
                        "Other_terraced", "Other_flatpurposebuilt", "Other_flatconverted",
                        "Other_flatcommercial", "Other_caravan", "Total_detached",
                        "Total_semidetached", "Total_terraced", "Total_flatpurposebuilt",
                        "Total_flatconverted", "Total_flatcommercial", "Total_caravan")

  # Rename columns
  colnames(raw_data) <- simplified_names

  raw_data = raw_data[,c("IZName", "p1_detached", "p1_semidetached",
                         "p1_terraced", "p1_flatpurposebuilt",
                         "p1_flatconverted", "p1_flatcommercial",
                         "p1_caravan", "Couple_detached", "Couple_semidetached",
                         "Couple_terraced", "Couple_flatpurposebuilt", "Couple_flatconverted",
                         "Couple_flatcommercial", "Couple_caravan", "LoneParent_detached",
                         "LoneParent_semidetached", "LoneParent_terraced", "LoneParent_flatpurposebuilt",
                         "LoneParent_flatconverted", "LoneParent_flatcommercial",
                         "LoneParent_caravan", "Other_detached", "Other_semidetached",
                         "Other_terraced", "Other_flatpurposebuilt", "Other_flatconverted",
                         "Other_flatcommercial", "Other_caravan")]

  raw_data = raw_data[!raw_data$IZName %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  if(all(raw_data$IZName == bounds_iz22$IZName)){
    raw_data$IZCode = bounds_iz22$IZCode
  } else {
    stop("Unable to add IZCode")
  }

  raw_data = tidyr::pivot_longer(raw_data,
                                 cols = names(raw_data)[2:(ncol(raw_data)-1)],
                                 names_sep = "_",
                                 names_to = c("hhComp10","AccType7"))

  names(raw_data)[names(raw_data) == "value"] = "households"

  return(raw_data)
}

#' Read Acctype Tenure Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @param bounds_iz22 IZ boundaries providing IZCode/IZName; used to match
#'   the extract's zone names to codes.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_AccType_Tenure_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_AccType7_Tenure3_IntermediateZone.csv",bounds_iz22) {

  raw_data <- readr::read_csv(path , skip = 12, col_names = FALSE, show_col_types = FALSE)

  simplified_names <- c("IZName", "detached_Owned", "detached_socialrented", "detached_privaterented",
                        "semidetached_Owned", "semidetached_socialrented", "semidetached_privaterented",
                        "terraced_Owned", "terraced_socialrented", "terraced_privaterented",
                        "flatpurposebuilt_Owned", "flatpurposebuilt_socialrented", "flatpurposebuilt_privaterented",
                        "flatconverted_Owned", "flatconverted_socialrented", "flatconverted_privaterented",
                        "flatcommercial_Owned", "flatcommercial_socialrented", "flatcommercial_privaterented",
                        "caravan_Owned", "caravan_socialrented", "caravan_privaterented",
                        "Total_Owned", "Total_socialrented", "Total_privaterented")


  colnames(raw_data) <- simplified_names

  raw_data = raw_data[,c("IZName", "detached_Owned", "detached_socialrented", "detached_privaterented",
                         "semidetached_Owned", "semidetached_socialrented", "semidetached_privaterented",
                         "terraced_Owned", "terraced_socialrented", "terraced_privaterented",
                         "flatpurposebuilt_Owned", "flatpurposebuilt_socialrented", "flatpurposebuilt_privaterented",
                         "flatconverted_Owned", "flatconverted_socialrented", "flatconverted_privaterented",
                         "flatcommercial_Owned", "flatcommercial_socialrented", "flatcommercial_privaterented",
                         "caravan_Owned", "caravan_socialrented", "caravan_privaterented")]

  raw_data = raw_data[!raw_data$IZName %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  if(all(raw_data$IZName == bounds_iz22$IZName)){
    raw_data$IZCode = bounds_iz22$IZCode
  } else {
    stop("Unable to add IZCode")
  }

  raw_data = tidyr::pivot_longer(raw_data,
                                 cols = names(raw_data)[2:(ncol(raw_data)-1)],
                                 names_sep = "_",
                                 names_to = c("AccType7","tenure5"))

  names(raw_data)[names(raw_data) == "value"] = "households"

  return(raw_data)
}

#' Read Hhsize Carvan Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @param bounds_iz22 IZ boundaries providing IZCode/IZName; used to match
#'   the extract's zone names to codes.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_hhSize_CarVan_scot <- function(path = "../inputdata/population_scotland/scotlandcenus2022_hhSize5_CarVan5_IntermediateZone.csv",bounds_iz22) {

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

#' Read Hhsize5 Tenure3 Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_hhSize5_Tenure3_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_hhSize5_Tenure3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  hhSize5 = c("p1","p2","p3","p4","p5+")
  Tenure3 = c("owned","socialrented","privaterented")

  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(hhSize5, each = 3),
                   "_",
                   rep(Tenure3, times = 5)),
                   "dud")
  raw$dud = NULL
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("hhSize5","Tenure3"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Hhsize5 Householdcomp4 Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_hhSize5_householdComp4_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_hhSize5_householdComp4_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  hhSize5 = c("p1","p2","p3","p4","p5+")
  householdComp4 = c("oneperson","couplefamily","loneparentfamily","other")


  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(hhSize5, each = 4),
                   "_",
                   rep(householdComp4, times = 5)),
                 "dud")
  raw$dud = NULL
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("hhSize5","householdComp4"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Hhsize5 Acctype3 Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_hhSize5_AccType3_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_hhSize5_AccType3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  hhSize5 = c("p1","p2","p3","p4","p5+")
  AccType3 = c("house","flat","caravan")

  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(AccType3, each = 5),
                   "_",
                   rep(hhSize5, times = 3)),
                 "dud")
  raw$dud = NULL
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("AccType3","hhSize5"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Carvan5 Acctype3 Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_CarVan5_AccType3_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_CarVan5_AccType3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  CarVan5 = c("car0","car1","car2","car3","car4+")
  AccType3 = c("house","flat","caravan")

  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(AccType3, times = 5),
                   "_",
                   rep(CarVan5, each = 3)),
                 "dud")
  raw$dud = NULL
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("AccType3","CarVan5"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Carvan5 Tenure3 Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_CarVan5_Tenure3_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_CarVan5_Tenure3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  Tenure3 = c("owned","socialrented","privaterented")
  CarVan5 = c("car0","car1","car2","car3","car4+")

  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(Tenure3, each = 5),
                   "_",
                   rep(CarVan5, times = 3)),
                 "dud")


  raw$dud = NULL
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("Tenure3","CarVan5"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Householdcomp4 Carvan3 Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_householdComp4_CarVan3_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_householdComp4_CarVan3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  CarVan3 = c("car0","car1","car2+")
  householdComp4 = c("oneperson","couplefamily","loneparentfamily","other","total")


  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(CarVan3, times = 5),
                   "_",
                   rep(householdComp4, each = 3)),
                 "dud")
  raw$dud = NULL
  raw = raw[,!grepl("total",names(raw))]
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("CarVan3","householdComp4"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Householdcomp4 Acctype3 Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_householdComp4_AccType3_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_householdComp4_AccType3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  AccType3 = c("house","flat","caravan")
  householdComp4 = c("oneperson","couplefamily","loneparentfamily","other","total")


  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(AccType3, times = 5),
                   "_",
                   rep(householdComp4, each = 3)),
                 "dud")
  raw$dud = NULL
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]
  raw = raw[,!grepl("total",names(raw))]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("AccType3","householdComp4"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Householdcomp4 Tenure3 Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_householdComp4_Tenure3_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_householdComp4_Tenure3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  Tenure3 = c("owned","socialrented","privaterented")
  householdComp4 = c("oneperson","couplefamily","loneparentfamily","other","total")


  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(Tenure3, times = 5),
                   "_",
                   rep(householdComp4, each = 3)),
                 "dud")
  raw$dud = NULL
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]
  raw = raw[,!grepl("total",names(raw))]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("Tenure3","householdComp4"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Hhsize5 Carvan3 Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_hhSize5_CarVan3_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_hhSize5_CarVan3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  CarVan3 = c("car0","car1","car2+")
  hhSize5 = c("p1","p2","p3","p4","p5+","total")

  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(CarVan3, times = 6),
                   "_",
                   rep(hhSize5, each = 3)),
                 "dud")
  raw$dud = NULL
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]
  raw = raw[,!grepl("total",names(raw))]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("CarVan3","hhSize5"),
                            values_to = "households",
                            names_sep = "_")

  raw

}

#' Read Tenure5 Acctype3 Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_Tenure5_AccType3_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_Tenure5_AccType3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 10, col_names = FALSE)

  names(raw) = c("counted","LSOA21CD","Tenure5","AccType3","households")
  raw$counted = NULL
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]
  raw = raw[!is.na(raw$households),]

  raw$AccType3[raw$AccType3 == "Whole house or bungalow" ] = "house"
  raw$AccType3[raw$AccType3 == "Flat, maisonette or apartment"] = "flat"
  raw$AccType3[raw$AccType3 == "Caravan or other mobile or temporary structure"] = "caravan"

  raw$Tenure5[raw$Tenure5  == "Owned: Owned outright"] = "outright"
  raw$Tenure5[raw$Tenure5  == "Owned: Owns with a mortgage or loan or shared ownership" ] = "mortgage"
  raw$Tenure5[raw$Tenure5  == "Social Rented: Council (LA) or Housing Association/ Registered Social Landlord"] = "socialrented"
  raw$Tenure5[raw$Tenure5  == "Private rented"] = "privaterented"
  raw$Tenure5[raw$Tenure5  == "Living rent free" ] = "rentfree"


  raw

}

#' Read Tenure5 Carvan3 Scot
#'
#' @description Reads the census cross-tabulation extract at `path` - household
#'   counts by the categories named in the function name - drops 'Does not
#'   apply' rows and shortens the category labels. One of the marginal/seed
#'   tables for the synthetic population IPF (see sythetic_census functions).
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_Tenure5_CarVan3_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_Tenure5_CarVan3_DataZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  CarVan3 = c("car0","car1","car2+")
  Tenure5 = c("outright","mortgage","socialrented","privaterented","rentfree")

  names(raw) = c("LSOA21CD",
                 paste0(
                   rep(CarVan3, times = 5),
                   "_",
                   rep(Tenure5, each = 3)),
                 "dud")
  raw$dud = NULL
  raw = raw[!raw$LSOA21CD %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw = tidyr::pivot_longer(raw,
                            cols = names(raw)[2:ncol(raw)],
                            names_to = c("CarVan3","Tenure5"),
                            values_to = "households",
                            names_sep = "_")

  raw


}

#' Build the Scottish synthetic household population from Census 2022
#'
#' @description Estimates, for every 2022 Data Zone, the five-way cross-tab
#'   of households by accommodation type x tenure x household composition x
#'   size x car ownership. Loads fifteen Data Zone-level cross-tab extracts,
#'   splits them per zone and runs `scot_syth_combine()` in parallel with
#'   the national seed. This is the long-running `scot_synth_households`
#'   target, the Scottish counterpart of `sythetic_census()`.
#' @param path_data Folder of Scotland Census 2022 extracts.
#' @param synth_pop_seed_scotland National seed arrays
#'   (`synth_pop_seed_scotland` target).
#' @return A data frame of household counts per Data Zone and attribute
#'   combination, with convergence diagnostics.
#' @keywords internal
sythetic_census_scot = function(path_data = file.path(parameters$path_data,"population_scotland"), synth_pop_seed_scotland){

  # Load Data
  dz_CarVan = read_CarVan_scot(file.path(path_data,"scotlandcenus2022_CarVan5_CarVan3_DataZone.csv"))
  dz_HouseholdComp = read_hhComp_scot(file.path(path_data,"scotlandcenus2022_householdComp10_householdComp4_DataZone.csv"))
  dz_Tenure = read_Tenure_scot(file.path(path_data,"scotlandcenus2022_Tenure5_Tenure3_DataZone.csv"))
  dz_AccType = read_Acc_scot(file.path(path_data,"scotlandcenus2022_AccType7_AccType3_DataZone.csv"))
  dz_hhSize5_Tenure3 = read_hhSize5_Tenure3_scot(file.path(path_data,"scotlandcenus2022_hhSize5_Tenure3_DataZone.csv"))
  dz_hhSize5_householdComp4 = read_hhSize5_householdComp4_scot(file.path(path_data,"scotlandcenus2022_hhSize5_householdComp4_DataZone.csv"))
  dz_hhSize5_AccType3 = read_hhSize5_AccType3_scot(file.path(path_data,"scotlandcenus2022_hhSize5_AccType3_DataZone.csv"))
  dz_CarVan5_AccType3 = read_CarVan5_AccType3_scot(file.path(path_data,"scotlandcenus2022_CarVan5_AccType3_DataZone.csv"))
  dz_CarVan5_Tenure3 = read_CarVan5_Tenure3_scot(file.path(path_data,"scotlandcenus2022_CarVan5_Tenure3_DataZone.csv"))
  dz_householdComp4_CarVan3 = read_householdComp4_CarVan3_scot(file.path(path_data,"scotlandcenus2022_householdComp4_CarVan3_DataZone.csv"))
  dz_householdComp4_AccType3 = read_householdComp4_AccType3_scot(file.path(path_data,"scotlandcenus2022_householdComp4_AccType3_DataZone.csv"))
  dz_householdComp4_Tenure3 = read_householdComp4_Tenure3_scot(file.path(path_data,"scotlandcenus2022_householdComp4_Tenure3_DataZone.csv"))
  dz_hhSize5_CarVan3 = read_hhSize5_CarVan3_scot(file.path(path_data,"scotlandcenus2022_hhSize5_CarVan3_DataZone.csv"))
  dz_Tenure5_AccType3 = read_Tenure5_AccType3_scot(file.path(path_data,"scotlandcenus2022_Tenure5_AccType3_DataZone.csv"))
  dz_Tenure5_CarVan3 = read_Tenure5_CarVan3_scot(file.path(path_data,"scotlandcenus2022_Tenure5_CarVan3_DataZone.csv"))

  #Order
  dz_CarVan = dz_CarVan[order(dz_CarVan$LSOA21CD),]
  dz_HouseholdComp = dz_HouseholdComp[order(dz_HouseholdComp$LSOA21CD),]
  dz_Tenure = dz_Tenure[order(dz_Tenure$LSOA21CD),]
  dz_AccType = dz_AccType[order(dz_AccType$LSOA21CD),]
  dz_hhSize5_Tenure3 = dz_hhSize5_Tenure3[order(dz_hhSize5_Tenure3$LSOA21CD),]
  dz_hhSize5_householdComp4 = dz_hhSize5_householdComp4[order(dz_hhSize5_householdComp4$LSOA21CD),]
  dz_hhSize5_AccType3 = dz_hhSize5_AccType3[order(dz_hhSize5_AccType3$LSOA21CD),]
  dz_CarVan5_AccType3 = dz_CarVan5_AccType3[order(dz_CarVan5_AccType3$LSOA21CD),]
  dz_CarVan5_Tenure3 = dz_CarVan5_Tenure3[order(dz_CarVan5_Tenure3$LSOA21CD),]
  dz_householdComp4_CarVan3 = dz_householdComp4_CarVan3[order(dz_householdComp4_CarVan3$LSOA21CD),]
  dz_householdComp4_AccType3 = dz_householdComp4_AccType3[order(dz_householdComp4_AccType3$LSOA21CD),]
  dz_householdComp4_Tenure3 = dz_householdComp4_Tenure3[order(dz_householdComp4_Tenure3$LSOA21CD),]
  dz_hhSize5_CarVan3 = dz_hhSize5_CarVan3[order(dz_hhSize5_CarVan3$LSOA21CD),]
  dz_Tenure5_AccType3 = dz_Tenure5_AccType3[order(dz_Tenure5_AccType3$LSOA21CD),]
  dz_Tenure5_CarVan3 = dz_Tenure5_CarVan3[order(dz_Tenure5_CarVan3$LSOA21CD),]

  #Split
  dz_CarVan = dplyr::group_split(dplyr::ungroup(dz_CarVan), LSOA21CD)
  dz_HouseholdComp = dplyr::group_split(dplyr::ungroup(dz_HouseholdComp), LSOA21CD)
  dz_Tenure = dplyr::group_split(dplyr::ungroup(dz_Tenure), LSOA21CD)
  dz_AccType = dplyr::group_split(dplyr::ungroup(dz_AccType), LSOA21CD)
  dz_hhSize5_Tenure3 = dplyr::group_split(dplyr::ungroup(dz_hhSize5_Tenure3), LSOA21CD)
  dz_hhSize5_householdComp4 = dplyr::group_split(dplyr::ungroup(dz_hhSize5_householdComp4), LSOA21CD)
  dz_hhSize5_AccType3 = dplyr::group_split(dplyr::ungroup(dz_hhSize5_AccType3), LSOA21CD)
  dz_CarVan5_AccType3 = dplyr::group_split(dplyr::ungroup(dz_CarVan5_AccType3), LSOA21CD)
  dz_CarVan5_Tenure3 = dplyr::group_split(dplyr::ungroup(dz_CarVan5_Tenure3), LSOA21CD)
  dz_householdComp4_CarVan3 = dplyr::group_split(dplyr::ungroup(dz_householdComp4_CarVan3), LSOA21CD)
  dz_householdComp4_AccType3 = dplyr::group_split(dplyr::ungroup(dz_householdComp4_AccType3), LSOA21CD)
  dz_householdComp4_Tenure3 = dplyr::group_split(dplyr::ungroup(dz_householdComp4_Tenure3), LSOA21CD)
  dz_hhSize5_CarVan3 = dplyr::group_split(dplyr::ungroup(dz_hhSize5_CarVan3), LSOA21CD)
  dz_Tenure5_AccType3 = dplyr::group_split(dplyr::ungroup(dz_Tenure5_AccType3), LSOA21CD)
  dz_Tenure5_CarVan3 = dplyr::group_split(dplyr::ungroup(dz_Tenure5_CarVan3), LSOA21CD)

  # dz_CarVan_sub = dz_CarVan[[1]]
  # dz_HouseholdComp_sub = dz_HouseholdComp[[1]]
  # dz_Tenure_sub = dz_Tenure[[1]]
  # dz_AccType_sub = dz_AccType[[1]]
  # dz_hhSize5_Tenure3_sub  = dz_hhSize5_Tenure3[[1]]
  # dz_hhSize5_householdComp4_sub  = dz_hhSize5_householdComp4[[1]]
  # dz_hhSize5_AccType3_sub  = dz_hhSize5_AccType3[[1]]
  # dz_CarVan5_AccType3_sub  = dz_CarVan5_AccType3[[1]]
  # dz_CarVan5_Tenure3_sub  = dz_CarVan5_Tenure3[[1]]
  # dz_householdComp4_CarVan3_sub  = dz_householdComp4_CarVan3[[1]]
  # dz_householdComp4_AccType3_sub  = dz_householdComp4_AccType3[[1]]
  # dz_householdComp4_Tenure3_sub  = dz_householdComp4_Tenure3[[1]]
  # dz_hhSize5_CarVan3_sub  = dz_hhSize5_CarVan3[[1]]
  # dz_Tenure5_AccType3_sub  = dz_Tenure5_AccType3[[1]]
  # dz_Tenure5_CarVan3_sub  = dz_Tenure5_CarVan3[[1]]

  # Combine
  future::plan("multisession")
  res_all = furrr::future_pmap(.l = list(dz_CarVan,
                                         dz_HouseholdComp,
                                         dz_Tenure,
                                         dz_AccType,
                                         dz_hhSize5_Tenure3,
                                         dz_hhSize5_householdComp4,
                                         dz_hhSize5_AccType3,
                                         dz_CarVan5_AccType3,
                                         dz_CarVan5_Tenure3,
                                         dz_householdComp4_CarVan3,
                                         dz_householdComp4_AccType3,
                                         dz_householdComp4_Tenure3,
                                         dz_hhSize5_CarVan3,
                                         dz_Tenure5_AccType3,
                                         dz_Tenure5_CarVan3),
                               .f = scot_syth_combine,
                               seed = synth_pop_seed_scotland,
                               .progress = TRUE,  .options = furrr::furrr_options(seed = 1234))
  future::plan("sequential")
  res_all = dplyr::bind_rows(res_all)


  res_all

}

#' Convert a long cross-tab into a named IPF constraint array
#'
#' @description Expands the full grid of the supplied category vectors,
#'   joins on the household counts and reshapes into an n-dimensional array
#'   whose dimnames are the argument names. Errors if any combination is
#'   missing from `x`.
#' @param x Long data frame with `households` and one column per category
#'   dimension.
#' @param ... Named category-level vectors, one per dimension, in the order
#'   the array should have.
#' @return A numeric array of household counts.
#' @keywords internal
array_maker = function(x, ...){
  args <- substitute(list(...))[-1]  # Capture the unevaluated arguments
  arg_names <- sapply(args, function(x) deparse(x))
  mat1 = expand.grid(...)
  names(mat1) = arg_names
  mat1 = dplyr::left_join(mat1, x, by = arg_names)
  if(anyNA(mat1$households)){stop("NAs in values")}
  mat1 = array(mat1$households,
               dim = c(lengths(list(...))),
               dimnames = list(...)
  )
  mat1
}

#' IPF one Data Zone's household cross-tab from its census marginals
#'
#' @description Worker for `sythetic_census_scot()`. Builds constraint
#'   arrays from the zone's fifteen one/two-way census tables (via
#'   `array_maker()`), reconciles their inconsistent totals, runs
#'   `mipfp::Ipfp` on the national seed, integerises with `int_trs()` and
#'   validates against the marginals with `vaidate_syth_pop2()`.
#' @param dz_CarVan_sub,dz_HouseholdComp_sub,dz_Tenure_sub,dz_AccType_sub,dz_hhSize5_Tenure3_sub,dz_hhSize5_householdComp4_sub,dz_hhSize5_AccType3_sub,dz_CarVan5_AccType3_sub,dz_CarVan5_Tenure3_sub,dz_householdComp4_CarVan3_sub,dz_householdComp4_AccType3_sub,dz_householdComp4_Tenure3_sub,dz_hhSize5_CarVan3_sub,dz_Tenure5_AccType3_sub,dz_Tenure5_CarVan3_sub
#'   One zone's rows of each census cross-tab.
#' @param seed National seed arrays (`synth_pop_seed_scotland` target).
#' @return A one-zone data frame of household counts per attribute
#'   combination with convergence diagnostics.
#' @keywords internal
scot_syth_combine = function(dz_CarVan_sub,
                             dz_HouseholdComp_sub,
                             dz_Tenure_sub,
                             dz_AccType_sub,
                             dz_hhSize5_Tenure3_sub,
                             dz_hhSize5_householdComp4_sub,
                             dz_hhSize5_AccType3_sub,
                             dz_CarVan5_AccType3_sub,
                             dz_CarVan5_Tenure3_sub,
                             dz_householdComp4_CarVan3_sub,
                             dz_householdComp4_AccType3_sub,
                             dz_householdComp4_Tenure3_sub,
                             dz_hhSize5_CarVan3_sub,
                             dz_Tenure5_AccType3_sub,
                             dz_Tenure5_CarVan3_sub,
                             seed) {

  # Check Zone match
  if(length(unique(c(dz_CarVan_sub$LSOA21CD,
                     dz_HouseholdComp_sub$LSOA21CD,
                     dz_Tenure_sub$LSOA21CD,
                     dz_AccType_sub$LSOA21CD,
                     dz_hhSize5_Tenure3_sub$LSOA21CD,
                     dz_hhSize5_householdComp4_sub$LSOA21CD,
                     dz_hhSize5_AccType3_sub$LSOA21CD,
                     dz_CarVan5_AccType3_sub$LSOA21CD,
                     dz_CarVan5_Tenure3_sub$LSOA21CD,
                     dz_householdComp4_CarVan3_sub$LSOA21CD,
                     dz_householdComp4_AccType3_sub$LSOA21CD,
                     dz_householdComp4_Tenure3_sub$LSOA21CD,
                     dz_hhSize5_CarVan3_sub$LSOA21CD,
                     dz_Tenure5_AccType3_sub$LSOA21CD,
                     dz_Tenure5_CarVan3_sub$LSOA21CD
  ))) != 1){
    stop("More than one LSOA21CD")
  }

  # Define Variable Names
  householdComp10 = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66")
  householdComp4 = c("oneperson","couplefamily","loneparentfamily","other")
  hhSize5 = c("p1","p2","p3","p4","p5+")
  Tenure3 = c("owned","socialrented","privaterented")
  Tenure5 = c("outright","mortgage","socialrented","privaterented","rentfree")
  CarVan5 = c("car0","car1","car2","car3","car4+")
  CarVan3 = c("car0","car1","car2+")
  AccType3 = c("house","flat","caravan")
  AccType7 = c("detached","semidetached","terraced","flatpurposebuilt","flatconverted","flatcommercial","caravan")



  MCarVan = array_maker(dz_CarVan_sub, CarVan5, CarVan3)
  MHouseholdComp = array_maker(dz_HouseholdComp_sub,householdComp4, householdComp10)
  MTenure = array_maker(dz_Tenure_sub,Tenure3,Tenure5)
  MAccType = array_maker(dz_AccType_sub,AccType3, AccType7)
  MhhSize5_Tenure3 = array_maker(dz_hhSize5_Tenure3_sub,hhSize5, Tenure3)
  MhhSize5_householdComp4 = array_maker(dz_hhSize5_householdComp4_sub,hhSize5, householdComp4)
  MhhSize5_AccType3 = array_maker(dz_hhSize5_AccType3_sub,AccType3, hhSize5)
  MCarVan5_AccType3 = array_maker(dz_CarVan5_AccType3_sub,AccType3,CarVan5)
  MCarVan5_Tenure3 = array_maker(dz_CarVan5_Tenure3_sub,Tenure3,CarVan5)
  MhouseholdComp4_CarVan3 = array_maker(dz_householdComp4_CarVan3_sub,CarVan3, householdComp4)
  MhouseholdComp4_AccType3 = array_maker(dz_householdComp4_AccType3_sub,AccType3, householdComp4)
  MhouseholdComp4_Tenure3 = array_maker(dz_householdComp4_Tenure3_sub,Tenure3, householdComp4)
  MhhSize5_CarVan3 = array_maker(dz_hhSize5_CarVan3_sub, CarVan3, hhSize5)
  MTenure5_AccType3 = array_maker(dz_Tenure5_AccType3_sub, AccType3, Tenure5)
  MTenure5_CarVan3 = array_maker(dz_Tenure5_CarVan3_sub, CarVan3, Tenure5)


  # Seed Dim
  # householdComp10, CarVan5, CarVan3, Tenure5, Tenure3, hhSize5, AccType7, AccType3, householdComp4
  seed = array(seed$seed, dim = c(10,5,3,5,3,5,7,3,4))
  med_pop = median(c(sum(MCarVan),
                     sum(MHouseholdComp),
                     sum(MTenure),
                     sum(MAccType),
                     sum(MhhSize5_Tenure3),
                     sum(MhhSize5_householdComp4),
                     sum(MhhSize5_AccType3),
                     sum(MCarVan5_AccType3),
                     sum(MCarVan5_Tenure3),
                     sum(MhouseholdComp4_CarVan3),
                     sum(MhouseholdComp4_AccType3),
                     sum(MhouseholdComp4_Tenure3),
                     sum(MhhSize5_CarVan3),
                     sum(MTenure5_AccType3),
                     sum(MTenure5_CarVan3)
  ))
  dimnames(seed) = list(householdComp10, CarVan5, CarVan3, Tenure5, Tenure3, hhSize5, AccType7, AccType3, householdComp4)

  seed_weighted = (seed /sum(seed)) * med_pop

  res <- mipfp::Ipfp(seed_weighted,
                     list(c(2,3),c(9,1),c(5,4),c(8,7),c(6,5),c(6,9),c(8,6),
                          c(8,2),c(5,2),c(3,9),c(8,9),c(5,9),c(3,6),c(8,4),c(3,4)
                          ),
                     list(
                       MCarVan, #2,3
                       MHouseholdComp, #9,1
                       MTenure, #5,4
                       MAccType, #8,7
                       MhhSize5_Tenure3, #6,5
                       MhhSize5_householdComp4, #6,9
                       MhhSize5_AccType3, #8,6
                       MCarVan5_AccType3, #8,2
                       MCarVan5_Tenure3, #5,2
                       MhouseholdComp4_CarVan3, #3,9
                       MhouseholdComp4_AccType3, #8,9
                       MhouseholdComp4_Tenure3,#5,9
                       MhhSize5_CarVan3,#3,6
                       MTenure5_AccType3,#8,4
                       MTenure5_CarVan3#3,4
                       ))

  dimnames(res$x.hat) = dimnames(seed_weighted)

  res2 = int_trs(res$x.hat * med_pop)

  # result_df = expand.grid(
  #   householdComp10, CarVan5, CarVan3, Tenure5, Tenure3, hhSize5, AccType7, AccType3, householdComp4, stringsAsFactors = FALSE
  # )
  # names(result_df) = c("householdComp10", "CarVan5", "CarVan3", "Tenure5", "Tenure3", "hhSize5", "AccType7", "AccType3","householdComp4")
  # result_df$households = as.numeric(res2)
  # result_df$error_margins = max(res$error.margins)
  # result_df$conv = res$conv

  result_df <- as.data.frame.table(res2)
  names(result_df) = c("householdComp10", "CarVan5", "CarVan3", "Tenure5", "Tenure3", "hhSize5", "AccType7", "AccType3","householdComp4","households")

  # Integrity checks
  chk1 = vaidate_syth_pop(result_df,MhouseholdComp4_CarVan3,"CarVan3","householdComp4")
  chk2 = vaidate_syth_pop(result_df,MhhSize5_householdComp4,"hhSize5","householdComp4")
  chk3 = vaidate_syth_pop(result_df,MCarVan5_Tenure3,"Tenure3","CarVan5")
  chk4 = vaidate_syth_pop(result_df,MTenure5_AccType3,"AccType3","Tenure5")
  chk5 = vaidate_syth_pop(result_df,MCarVan,"CarVan5","CarVan3")
  chk6 = vaidate_syth_pop(result_df,MHouseholdComp,"householdComp4","householdComp10")
  chk7 = vaidate_syth_pop(result_df,MTenure,"Tenure3","Tenure5") # 6.6
  chk8 = vaidate_syth_pop(result_df,MAccType,"AccType3","AccType7")
  chk9 = vaidate_syth_pop(result_df,MhhSize5_Tenure3,"hhSize5","Tenure3")
  chk10 = vaidate_syth_pop(result_df,MhhSize5_AccType3,"AccType3","hhSize5")
  chk11 = vaidate_syth_pop(result_df,MhouseholdComp4_AccType3,"AccType3","householdComp4")
  chk12 = vaidate_syth_pop(result_df,MhouseholdComp4_Tenure3,"Tenure3","householdComp4")
  chk13 = vaidate_syth_pop(result_df,MhhSize5_CarVan3,"CarVan3","hhSize5")
  chk14 = vaidate_syth_pop(result_df,MCarVan5_AccType3,"AccType3","CarVan5")

  #Collapse to Main Variables
  result_df2 = dplyr::group_by(result_df, householdComp10,CarVan5,Tenure5,hhSize5,AccType7) |>
    dplyr::summarise(households = sum(households),
                     error_margins = max(res$error.margins),
                     conv = res$conv
    )

  result_df2 = result_df2[result_df2$households > 0,]

  result_df2$MAE = max(chk1,chk2,chk3,chk4,chk5,chk6,chk7,chk8,chk9,chk10,chk11,chk12,chk13,chk14)

  result_df2$LSOA21CD = dz_CarVan_sub$LSOA21CD[1]

  result_df2

}

#' Mean absolute error of a synthetic population against one marginal
#'
#' @description Collapses the synthetic household table to a var1 x var2
#'   matrix and compares it with the corresponding census constraint,
#'   returning the mean absolute error over non-zero constraint cells.
#' @param x Synthetic household data frame with `households`.
#' @param y Census constraint matrix (var1 rows x var2 columns).
#' @param var1,var2 Column names of the two dimensions to check.
#' @return The mean absolute error (single numeric).
#' @keywords internal
vaidate_syth_pop = function(x = result_df,y = MhouseholdComp4_CarVan3, var1 = "CarVan3", var2 =  "householdComp4"){
  x = x[,c(var1,var2,"households")]
  x2 = dplyr::group_by_at(x, dplyr::all_of(c(var1,var2))) |>
    dplyr::summarise(households = sum(households)) |>
    tidyr::pivot_wider(names_from = var2, values_from = "households")
  mat = as.matrix(x2[,2:ncol(x2)])

  rownames(mat) = x2[[var1]]
  diff = mat - y
  #print(diff)
  return(sum(abs(diff))/sum(y!= 0)) # Return Mean Absolute Error
}

#' Mean absolute error against a marginal, accepting >2D constraint arrays
#'
#' @description As `vaidate_syth_pop()` but `y` may be an n-dimensional
#'   array: it is summed over the other dimensions (located by dimname) and
#'   aligned to the synthetic table's categories before comparison.
#' @param x Synthetic household data frame with `households`.
#' @param y Census constraint matrix or array with named dimensions.
#' @param var1,var2 Dimension names to compare on.
#' @return The mean absolute error (single numeric).
#' @keywords internal
vaidate_syth_pop2 = function(x = result_df,y = MhouseholdComp4_CarVan3, var1 = "CarVan3", var2 =  "householdComp4"){
  x = x[,c(var1,var2,"households")]

  x2 <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(var1, var2)))) |>
    dplyr::summarise(households = sum(households), .groups = "drop") |>
    tidyr::pivot_wider(names_from = var2, values_from = households)
  mat = as.matrix(x2[,2:ncol(x2)])

  rownames(mat) = x2[[var1]]
  if(length(dim(y)) != 2){
    # Convert y to matrix of var1 x var2
    # If y is an array with >2 dimensions, try to sum over the other dimensions
    if(!is.null(dim(y)) && length(dim(y)) >= 3){
      dn <- dimnames(y)
      dn_names <- names(dn)

      if(!is.null(dn_names) && var1 %in% dn_names && var2 %in% dn_names){
        idx1 <- which(dn_names == var1)
        idx2 <- which(dn_names == var2)
        # Sum over all dimensions except idx1 and idx2
        y_mat <- apply(y, MARGIN = c(idx1, idx2), FUN = sum)
        # ensure matrix
        y_mat <- as.matrix(y_mat)
        # keep dimnames if present
        if(!is.null(dn[[idx1]])) rownames(y_mat) <- dn[[idx1]]
        if(!is.null(dn[[idx2]])) colnames(y_mat) <- dn[[idx2]]
      } else {
        # Fallback: collapse to first two dimensions (sum over others)
        stop(paste0("vaidate_syth_pop2: dimension names '", var1, "' and/or '", var2, "' not found in 'y' dimnames"))
      }

      # Align y_mat to mat by row/col names. Create a zero-filled matrix with mat's dimnames
      y_aligned <- matrix(0, nrow = nrow(mat), ncol = ncol(mat), dimnames = list(rownames(mat), colnames(mat)))

      if(!is.null(rownames(y_mat)) && !is.null(colnames(y_mat))){
        common_r <- intersect(rownames(mat), rownames(y_mat))
        common_c <- intersect(colnames(mat), colnames(y_mat))
        if(length(common_r) > 0 && length(common_c) > 0){
          y_aligned[common_r, common_c] <- y_mat[common_r, common_c, drop = FALSE]
        }
      } else if(nrow(y_mat) == nrow(mat) && ncol(y_mat) == ncol(mat)){
        # same shape and no dimnames: assume same ordering
        y_aligned[,] <- y_mat
      } else {
        stop("vaidate_syth_pop2: unable to align 'y' to expected var1 x var2 matrix; dimnames are missing or mismatched")
      }

      y <- y_aligned
    } else {
      # if y has no dim or is a vector, coerce to matrix
      y <- as.matrix(y)
    }
  }


  diff = mat - y
  #print(diff)
  return(sum(abs(diff))/sum(y!= 0)) # Return Mean Absolute Error
}


# Intergerisation method
# From https://spatial-microsim-book.robinlovelace.net/smsimr#sintegerisation
#' Truncate-replicate-sample integerisation
#'
#' @description Converts fractional weights/counts to integers while
#'   preserving the total: takes the integer part of each value, then
#'   allocates the remaining units by sampling proportional to the
#'   fractional remainders (Lovelace & Ballas TRS method). Known issue: can
#'   misbehave when all inputs are already integers (see TODO where called).
#' @param x Numeric vector or array of non-negative values.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
int_trs <- function(x){
  # For generalisation purpose, x becomes a vector
  xv <- as.vector(x) # allows trs to work on matrices
  xint <- floor(xv) # integer part of the weight
  r <- xv - xint # decimal part of the weight
  def <- round(sum(r)) # the deficit population
  # the weights be 'topped up' (+ 1 applied)
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
}
