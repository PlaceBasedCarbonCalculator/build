# source("R/class_ethnic.R")
# source("R/furness_balancing.R")
#
# library(dplyr)
#
# households_nssec = read_household_nssec()
# residents_ethnic = read_NSSEC_ethinic()


#' Build Household Types
#'
#' @description Build household types and return the generated output.
#' @param households_nssec Input object or parameter named `households_nssec`.
#' @param residents_ethnic){ Input object or parameter named `residents_ethnic){`.
#' @return A generated data object, usually a data frame or spatial feature collection.
#' @keywords internal
build_household_types = function(households_nssec, residents_ethnic){
  # Simplify Ethnicity to White, Black, Other
  residents_ethnic$ethnic6[residents_ethnic$ethnic6 %in% c("Asian","Mixed","Other")] = "Other"

  residents_ethnic = dplyr::group_by(residents_ethnic, LSOA21CD, NSSEC10, ethnic6)
  residents_ethnic = dplyr::summarise(residents_ethnic, residents = sum(residents))

  residents_ethnic$NSSEC5 = "DNA"
  residents_ethnic$NSSEC5[residents_ethnic$NSSEC10 == "L14"] = "unemployed"
  residents_ethnic$NSSEC5[residents_ethnic$NSSEC10 == "L15"] = "students"
  residents_ethnic$NSSEC5[residents_ethnic$NSSEC10 %in% c("L1L2L3","L4L5L6")] = "higher"
  residents_ethnic$NSSEC5[residents_ethnic$NSSEC10 %in% c("L7","L8L9")] = "intermediate"
  residents_ethnic$NSSEC5[residents_ethnic$NSSEC10 %in% c("L10L11","L12","L13")] = "routine"

  residents_ethnic = dplyr::group_by(residents_ethnic, LSOA21CD, NSSEC5, ethnic6)
  residents_ethnic = dplyr::summarise(residents_ethnic, residents = sum(residents))

  # Fill in Missing Data
  # Assume all white as missing area are at least 97% white
  residents_missing = unique(households_nssec$LSOA21CD[!households_nssec$LSOA21CD %in% unique(residents_ethnic$LSOA21CD)])

  residents_missing = expand.grid(residents_missing, unique(residents_ethnic$NSSEC5), unique(residents_ethnic$ethnic6))
  names(residents_missing) = c("LSOA21CD","NSSEC5","ethnic6")
  residents_missing$residents = 0
  residents_missing$residents[residents_missing$ethnic6 == "White"] = 1

  residents_ethnic = rbind(residents_ethnic, residents_missing)

  residents_ethnic =  dplyr::group_by(residents_ethnic, LSOA21CD, NSSEC5)
  residents_ethnic =  dplyr::mutate(residents_ethnic, percent = residents/sum(residents))
  residents_ethnic =  dplyr::ungroup(residents_ethnic)

  # Group NSSEC Into fewer bands
  # 3 Class + Student and Unemployed
  # 1,2,3,4,5,6 - Higher managerial, administrative and professional occupations
  # 7,8,9 - Intermediate occupations
  # 10,11,12,13 - Routine and manual occupations
  # 14 - Long Term Unemployed
  # 15 - Students


  #TODO: earlier we pivot wide only to pivot back, that wastes about and hour!
  households_nssec = tidyr::pivot_longer(households_nssec, cols = names(households_nssec)[2:ncol(households_nssec)],
                                         names_sep = "_", names_to = c("householdComposition","NSSEC5"), values_to = "households")

  residents_ethnic = residents_ethnic[order(residents_ethnic$LSOA21CD),]
  households_nssec = households_nssec[order(households_nssec$LSOA21CD),]

  residents_ethnic = dplyr::group_split(residents_ethnic, LSOA21CD)
  households_nssec = dplyr::group_split(households_nssec, LSOA21CD)

  # Add Thenicity to NNSEC Houshodls
  hh = households_nssec[[1]]
  eth = residents_ethnic[[1]]

  future::plan("multisession")

  combined = furrr::future_map2(households_nssec, residents_ethnic,
                                combine_nssec_enthinic, .progress = TRUE)

  future::plan("sequential")

  combined = dplyr::bind_rows(combined)
  # summary(combined$err_row_total)
  # summary(combined$err_col_total)
  # names(combined)
  # sum(colSums(combined[,3:26], na.rm = TRUE))


  combined_long = combined[,!grepl("err_",names(combined))]
  combined_long = tidyr::pivot_longer(combined_long,
                                      cols = names(combined_long)[!names(combined_long) %in% c("NSSEC5","LSOA21CD")],
                                      names_sep = "_",
                                      values_to = "households",
                                      names_to = c("householdComposition","ethnic")
  )

  combined_long = combined_long[!is.na(combined_long$households),]
  combined_long = combined_long[combined_long$households > 0,]
  combined_long
}


#' Select Household Pics
#'
#' @description Perform processing for select household pics.
#' @param combined_long){ Input object or parameter named `combined_long){`.
#' @return A data frame produced by the function.
#' @keywords internal
select_household_pics = function(combined_long){

  long_lst = dplyr::group_split(combined_long, combined_long$LSOA21CD, .keep = FALSE)
  cats = purrr::map(long_lst, top_architypes, n = 48, .progress = TRUE)
  cats = dplyr::bind_rows(cats)

  # Doing 48 images per LSOA for 90% of housholds in 90% of LSOAs
  cats_top = cats[cats$cumpic <= 48,]
  cats_top$id = paste0(cats_top$NSSEC5,"_",cats_top$householdComposition,"_",cats_top$ethnic)
  cats_top = cats_top[,c("LSOA21CD","id","pic")]
  cats_top

}


#' Combine NS-SEC Ethnic
#'
#' @description Combine NS-SEC Ethnic inputs into a single consolidated result.
#' @details This function is used to prepare intermediate analysis tables for later pipeline targets.
#' @param hh Input object or parameter named `hh`.
#' @param eth){ Input object or parameter named `eth){`.
#' @return A combined data frame or table merging the provided inputs.
#' @keywords internal
combine_nssec_enthinic = function(hh, eth){
  if(hh$LSOA21CD[1] != eth$LSOA21CD[1]){
    stop("LSOAs don't match")
  } else {
    lsoa = hh$LSOA21CD[1]
    hh$LSOA21CD = NULL
    eth$LSOA21CD = NULL
  }



  #cats = c("DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15")
  cats = c("DNA","routine","unemployed","students","higher","intermediate")
  res = list()
  for(i in seq_along(cats)){
    h = hh[hh$NSSEC5 == cats[i],]
    e = eth[eth$NSSEC5 == cats[i],]
    #h$NSSEC5 = NULL

    mat = matrix(1, nrow = nrow(e), ncol = nrow(h))
    csum = h$households
    rownames(mat) = e$ethnic6
    colnames(mat) = h$householdComposition

    if(sum(csum) == 0){
      res[[i]] = data.frame(err_row_total = 0)
    } else {
      #names(csum) = names(h)
      rsum = round(e$percent * sum(csum), 3)

      mat2 = furness_balance(mat, rsum, csum, check = FALSE, int_only = TRUE)
      err_row = rowSums(mat2, na.rm = TRUE) - rsum
      err_col = colSums(mat2, na.rm = TRUE) - csum
      mat2 = as.data.frame(mat2)
      mat2$ethnic = rownames(mat2)
      mat2 = tidyr::pivot_wider(mat2,
                                values_from = names(mat2)[names(mat2) != "ethnic"],
                                names_from = "ethnic")
      mat2$err_row_total = round(sum(abs(err_row)), 2)
      mat2$err_row_max = round(max(abs(err_row)), 2)
      mat2$err_col_total = round(sum(abs(err_col)), 2)
      mat2$err_col_max = round(max(abs(err_col)), 2)
      res[[i]] = mat2

    }
  }
  names(res) = cats
  res = dplyr::bind_rows(res, .id = "NSSEC5")
  res$LSOA21CD = lsoa
  res
}


