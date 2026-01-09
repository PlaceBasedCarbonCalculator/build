# source("R/class_ethnic.R")
# source("R/furness_balancing.R")
#
# library(dplyr)
#
# households_nssec = read_household_nssec()
# residents_ethnic = read_NSSEC_ethinic()


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

  # households_nssec$NSSEC5 = "DNA"
  # households_nssec$NSSEC5[households_nssec$NSSEC10 == "L14"] = "unemployed"
  # households_nssec$NSSEC5[households_nssec$NSSEC10 == "L15"] = "students"
  # households_nssec$NSSEC5[households_nssec$NSSEC10 %in% c("L1L2L3","L4L5L6")] = "higher"
  # households_nssec$NSSEC5[households_nssec$NSSEC10 %in% c("L7","L8L9")] = "intermediate"
  # households_nssec$NSSEC5[households_nssec$NSSEC10 %in% c("L10L11","L12","L13")] = "routine"
  #
  #
  # households_nssec =  dplyr::group_by(households_nssec, LSOA21CD, NSSEC5)
  # households_nssec = dplyr::summarise(households_nssec,
  #                                     LoneParent = sum(LoneParent),
  #                                     DNA = sum(DNA),
  #                                     OnePersonOver66 = sum(OnePersonOver66),
  #                                     OnePersonOther = sum(OnePersonOther),
  #                                     CoupleNoChildren = sum(CoupleNoChildren),
  #                                     CoupleChildren = sum(CoupleChildren),
  #                                     CoupleNoDepChild = sum(CoupleNoDepChild),
  #                                     Other8 = sum(Other8))

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


select_household_pics = function(combined_long){



  long_lst = dplyr::group_split(combined_long, combined_long$LSOA21CD, .keep = FALSE)
  cats = purrr::map(long_lst, top_architypes, n = 48, .progress = TRUE)
  cats = dplyr::bind_rows(cats)

  # #Check 20 is enough no close
  #
  #
  # chk = cats[cats$cumpic >= 48,]
  # chk = dplyr::group_split(chk, chk$LSOA21CD, .keep = FALSE)
  # chk = purrr::map(chk, function(x){x[1,]}, .progress = TRUE)
  # chk = dplyr::bind_rows(chk)
  # summary(chk$cum)
  # summary(duplicated(chk$LSOA21CD))
  # quantile(chk$cum, seq(0,1,0.05))
  # # Need between 14 and 50 types to cover 80% of the populations, mean of 22
  # x = cats[cats$LSOA21CD == "E01000501",]
  #
  # # Look for unusual categorie to merge
  #
  # cat_sum = cats %>%
  #   group_by(NSSEC10,  NSSEC,ethnic) %>%
  #   summarise(households = sum(households, na.rm = T))

  # Doing 48 images per LSOA for 90% of hosuheols in 90% of LSOAs

  cats_top = cats[cats$cumpic <= 48,]
  #cats_top = cats_top[,c("LSOA21CD","NSSEC5","householdComposition","ethnic","pic")]
  cats_top$id = paste0(cats_top$NSSEC5,"_",cats_top$householdComposition,"_",cats_top$ethnic)
  cats_top = cats_top[,c("LSOA21CD","id","pic")]
  # cats_summary = cats_top %>%
  #   group_by(NSSEC5, householdComposition, ethnic) %>%
  #   summarise(households = sum(households),
  #             pic = sum(pic))
  cats_top

}


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


