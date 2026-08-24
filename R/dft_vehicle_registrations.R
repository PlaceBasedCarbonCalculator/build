#' Download DfT vehicle registration tables VEH0125/0135/0145
#'
#' @description Downloads the DfT licensed-vehicle (VEH0125), ULEV (VEH0135)
#'   and plug-in vehicle (VEH0145) LSOA-level CSVs. Skipped if `path` already
#'   holds more than 2 CSVs. The corresponding target is currently commented
#'   out in `_targets.R` (files are kept in the inputdata repo instead).
#' @param path Folder to store the downloads; created if missing.
#' @return `path`.
#' @keywords internal
download_dft_vehicle_registrations <- function(path = file.path(data_path(),"vehicle_registrations")){
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    fls = list.files(path, pattern = "csv")
    if(length(fls) > 2){
      return(path)
    }
  }

  url_VEH0125 = "https://assets.publishing.service.gov.uk/media/65734e2058fa300013b1419d/df_VEH0125.csv"
  url_VEH0135 = "https://assets.publishing.service.gov.uk/media/65734c3533b7f20012b72176/df_VEH0135.csv"
  url_VEH0145 = "https://assets.publishing.service.gov.uk/media/65734e7a58fa300013b1419e/df_VEH0145.csv"

  download.file(url_VEH0125, file.path(path,"df_VEH0125.csv"), mode = "wb")
  download.file(url_VEH0135, file.path(path,"df_VEH0135.csv"), mode = "wb")
  download.file(url_VEH0145, file.path(path,"df_VEH0145.csv"), mode = "wb")

  path

}


#' Load and clean DfT vehicle registrations (VEH0125) per LSOA
#'
#' @description Reads the VEH0125 table (vehicles by body type, keepership
#'   and licence status per 2021 LSOA), keeps Q1 of each year, and repairs
#'   the statistical disclosure control in parallel: values suppressed as
#'   "[c]" (1-4 vehicles) or missing are reconstructed from the row/column
#'   totals via Furness balancing in `fill_gaps()`. Used by the
#'   `vehicle_registrations` target.
#' @param path Folder containing `df_VEH0125.csv`.
#' @return A wide data frame with `LSOA21CD`, `quarter` and one column per
#'   `<BodyType>_<Keepership>_<LicenceStatus>`.
#' @keywords internal
load_dft_vehicle_registrations <- function(path = file.path(data_path(),"vehicle_registrations")){

  d125 <- readr::read_csv(file.path(path,"df_VEH0125.csv"))

  d125_long <- tidyr::pivot_longer(d125,
                            cols = names(d125)[6:ncol(d125)],
                            names_to = "quarter",
                            values_to = "count")

  #table(d125_long$count[is.na(as.numeric(d125_long$count))])
  # [c] (1-4)    [x] (Not Available)  [z](Not applicable)
  # 5908204      388550
  d125_long <- d125_long[grepl("Q1",d125_long$quarter),]
  d125_long$LSOA21NM <- NULL
  d125_long <- dplyr::group_by(d125_long, LSOA21CD, quarter, LicenceStatus)
  d125_long <- dplyr::group_split(d125_long)

  # Tests c(10624, 74860, 74862, 27538)
  future::plan("multisession")
  d125_clean = furrr::future_map(d125_long, .f = fill_gaps,
                                 .options = furrr::furrr_options(seed = 1234L),
                                 .progress = TRUE)
  future::plan("sequential")

  d125_clean = data.table::rbindlist(d125_clean, fill=TRUE)
  rm(d125_long)

  d125_wide = tidyr::pivot_wider(d125_clean,
                          id_cols = c("LSOA21CD","quarter"),
                          names_from = c("BodyType","Keepership","LicenceStatus"),
                          values_from = "count"
  )
  d125_wide

}


#' Load and clean DfT ULEV registrations (VEH0135) per LSOA
#'
#' @description As `load_dft_vehicle_registrations()` but for the
#'   ultra-low-emission vehicle table (by fuel and keepership). Suppressed
#'   values are reconstructed with `fill_gaps_135()`. This is the ~2 hour
#'   `ulev_registrations` target.
#' @param path Folder containing `df_VEH0135.csv`.
#' @return A wide data frame with `LSOA21CD`, `quarter` and one column per
#'   `<Fuel>_<Keepership>`.
#' @keywords internal
load_dft_ulev_registrations <- function(path = "../inputdata/vehicle_registrations"){

  d135 <- readr::read_csv(file.path(path,"df_VEH0135.csv"))
  d135 <- d135[d135$LSOA21CD != "Miscellaneous",]


  d135_long <- tidyr::pivot_longer(d135,
                                   cols = names(d135)[5:ncol(d135)],
                                   names_to = "quarter",
                                   values_to = "count")
  rm(d135)

  d135_long <- d135_long[grepl("Q1",d135_long$quarter),]
  d135_long$LSOA21NM <- NULL
  d135_long <- dplyr::group_by(d135_long, LSOA21CD, quarter)
  d135_long <- dplyr::group_split(d135_long)

  # Tests c(10624, 74860, 74862, 27538)
  #d135_clean = pbapply::pblapply(d135_list, fill_gaps_135)
  future::plan("multisession")
  d135_clean = furrr::future_map(d135_long, .f = fill_gaps_135,
                                   .options = furrr::furrr_options(seed = 1234L),
                                   .progress = TRUE)
  future::plan("sequential")




  d135_clean = data.table::rbindlist(d135_clean)
  rm(d135_long)

  d135_wide = tidyr::pivot_wider(d135_clean,
                                 id_cols = c("LSOA21CD","quarter"),
                                 names_from = c("Fuel","Keepership",),
                                 values_from = "count"
  )
  d135_wide

}


#' Load and clean DfT plug-in vehicle registrations (VEH0145) per LSOA
#'
#' @description As `load_dft_vehicle_registrations()` but for the plug-in
#'   vehicle table (battery electric, plug-in hybrid, range-extended).
#'   Suppressed values are reconstructed with `fill_gaps_145()`. Used by the
#'   `ev_registrations` target.
#' @param path Folder containing `df_VEH0145.csv`.
#' @return A wide data frame with `LSOA21CD`, `quarter` and one column per
#'   `<Fuel>_<Keepership>`.
#' @keywords internal
load_dft_ev_registrations <- function(path = "../inputdata/vehicle_registrations"){

  d145 <- readr::read_csv(file.path(path,"df_VEH0145.csv"))
  d145 <- d145[d145$LSOA21CD != "Miscellaneous",]
  d145 <- d145[substr(d145$LSOA21CD,1,1) %in% c("E","S","W"),]

  d145_long <- tidyr::pivot_longer(d145,
                                   cols = names(d145)[5:ncol(d145)],
                                   names_to = "quarter",
                                   values_to = "count")
  rm(d145)

  d145_long <- d145_long[grepl("Q1",d145_long$quarter),]
  d145_long$LSOA21NM <- NULL
  d145_long <- dplyr::group_by(d145_long, LSOA21CD, quarter)
  d145_long <- dplyr::group_split(d145_long)

  future::plan("multisession")
  d145_clean = furrr::future_map(d145_long, .f = fill_gaps_145,
                                 .options = furrr::furrr_options(seed = 1234L),
                                 .progress = TRUE)
  future::plan("sequential")

  d145_clean = data.table::rbindlist(d145_clean)
  rm(d145_long)

  d145_wide = tidyr::pivot_wider(d145_clean,
                                 id_cols = c("LSOA21CD","quarter"),
                                 names_from = c("Fuel","Keepership",),
                                 values_from = "count"
  )
  d145_wide

}


#' Reconstruct suppressed VEH0125 counts for one LSOA-quarter
#'
#' @description Worker for `load_dft_vehicle_registrations()`. DfT suppress
#'   counts of 1-4 as "[c]" and omit some rows entirely. This rebuilds the
#'   full body-type x keepership matrix for one LSOA-quarter-licence group:
#'   missing rows are added (0 or NA depending on whether the marginal totals
#'   imply hidden vehicles), and the remaining gaps are filled by Furness
#'   balancing against the published row/column totals
#'   (`furness_partial()`/`furness_incomplete()`). When even the overall
#'   total is suppressed, small values are imputed heuristically.
#' @param x Long data frame for one LSOA/quarter/licence-status group.
#' @return The completed long data frame (totals rows removed), or NULL if
#'   the group has no data at all.
#' @keywords internal
fill_gaps = function(x){
  incomplete = FALSE
  suppressWarnings(x$count2 <- as.numeric(x$count))


  # Sometimes data is missing
  if(nrow(x) != 12){
    x_missing = data.frame(LSOA21CD = x$LSOA21CD[1],
                           BodyType = rep(c("Cars","Motorcycles","Other vehicles","Total"), 3),
                           Keepership = rep(c("COMPANY","PRIVATE","Total"), each = 4),
                           LicenceStatus = x$LicenceStatus[1],
                           quarter = x$quarter[1],
                           count2 = 0
    )
    # Better to assume missing rows are 0???
    x$id = paste0(x$BodyType,"-",x$Keepership)
    x_missing$id = paste0(x_missing$BodyType,"-",x_missing$Keepership)
    x_missing = x_missing[!x_missing$id %in% x$id,]
    # Special case we only have totals e.g. E01000005 2019 Q1 there are [c] Private vehicles in total but no rows about them

    # Check for missing totals
    if(any(c(x_missing$Keepership == "Total", x_missing$BodyType == "Total"))){
      x_missing$count2 = NA
    } else {
      x_total_Keepership = unique(x$Keepership[x$BodyType == "Total"])
      x_total_Keepership = x_total_Keepership[x_total_Keepership != "Total"]
      x_total_body = unique(x$BodyType[x$Keepership == "Total"])
      x_total_body = x_total_body[x_total_body != "Total"]
      x_missing$count2 = ifelse(x_missing$BodyType %in% x_total_body & x_missing$Keepership %in% x_total_Keepership, NA, 0)
    }
    x = dplyr::bind_rows(x, x_missing)
  }

  if(all(is.na(x$count2))){
    # No Data
    return(NULL)
  }

  if(all(!is.na(x$count2))){
    # All Data
    x$count = x$count2
    x$count2 = NULL
    x$id = NULL
    x = x[x$BodyType != "Total",]
    x = x[x$Keepership != "Total",]
    return(x)
  }

  #Missing Data
  x_totals = x[x$BodyType == "Total" | x$Keepership == "Total",]
  x_others = x[!(x$BodyType == "Total" | x$Keepership == "Total"),]

  if(any(is.na(x_totals$count2))){
    tt = x_totals$count2[x_totals$BodyType == "Total" & x_totals$Keepership == "Total"]

    if(is.na(tt)){
      # Overall Total is between 1 and 4
      # Missing Totals
      if(sum(is.na(x_others$count2)) > 4){
        # Too many options, pefer option where known [c] over generated option
        x_others$count2[is.na(x_others$count)] = 0
      }
      if(sum(is.na(x_others$count2)) > 4){
        # If there are still too many options, sick 1 in just the first 4 options
        x_others$count2[is.na(x_others$count2)] = c(rep(1,4), rep(0, sum(is.na(x_others$count2)) - 4))
      } else {
        x_others$count2 = ifelse(is.na(x_others$count2),1,x_others$count2)
      }

      x_others$count = x_others$count2

      #check
      if(sum(x_others$count) > 4){
        print(x)
        stop("Assumed values greater than total ")
      }


      x_others = x_others[,c("LSOA21CD","BodyType","Keepership","LicenceStatus", "quarter","count")]

      return(x_others)

    } else {
      incomplete = TRUE
    }


  }


  # Fill Gaps
  # Make Matrix
  y = x_others[,c("BodyType","Keepership","count2")]
  # y$id = paste0(y$BodyType, y$Keepership)
  # y_missing = data.frame(BodyType = rep(c("Cars","Motorcycles","Other vehicles"), 2),
  #                        Keepership = rep(c("COMPANY","PRIVATE"), each = 3),
  #                        count2 = 0
  # )
  # y_missing$id = paste0(y_missing$BodyType, y_missing$Keepership)
  # y_missing = y_missing[!y_missing$id %in% y$id,]
  # y = rbind(y, y_missing)
  # y$id <- NULL
  y2 <- tidyr::pivot_wider(y, names_from = "Keepership", values_from = "count2")
  y2_mat = as.matrix(y2[,2:3])
  rownames(y2_mat) = y2$BodyType
  rsum = x_totals[x_totals$Keepership == "Total" & x_totals$BodyType != "Total",]
  rsum = rsum$count2[match(rownames(y2_mat), rsum$BodyType)]

  csum = x_totals[x_totals$Keepership != "Total" & x_totals$BodyType == "Total",]
  csum = csum$count2[match(colnames(y2_mat), csum$Keepership)]

  # Use Furness balancing to fill gaps
  if(incomplete){
    newmat = furness_incomplete(mat = y2_mat, rsum, csum, tt)
  } else {
    newmat = furness_partial(mat = y2_mat, rsum, csum, check = TRUE)
  }
  newdf = as.data.frame(newmat)
  newdf$BodyType <- rownames(newdf)
  newdf = tidyr::pivot_longer(newdf, cols = c("COMPANY","PRIVATE"),names_to = "Keepership", values_to = "count")
  newdf$LSOA21CD = x_others$LSOA21CD[1]
  newdf$LicenceStatus = x_others$LicenceStatus[1]
  newdf$quarter = x_others$quarter[1]
  newdf = newdf[,c("LSOA21CD","BodyType","Keepership","LicenceStatus", "quarter","count")]
  return(newdf)
}


#' Reconstruct suppressed VEH0135 (ULEV) counts for one LSOA-quarter
#'
#' @description As `fill_gaps()` but for the 10-fuel x 3-keepership ULEV
#'   table. Distinguishes whether the suppressed cells sum exactly to the
#'   published totals (plain Furness fill) or the totals imply additional
#'   hidden vehicles (`furness_partial_integer_total()`).
#' @param x Long data frame for one LSOA/quarter group.
#' @return The completed long data frame, or NULL if the group has no data.
#' @keywords internal
fill_gaps_135 = function(x){

  incomplete = FALSE
  suppressWarnings(x$count2 <- as.numeric(x$count))

  # Sometimes data is missing
  if(nrow(x) != 30){
    x_missing = data.frame(LSOA21CD = x$LSOA21CD[1],
                           Fuel = rep(c("PETROL","DIESEL","BATTERY ELECTRIC","HYBRID ELECTRIC (DIESEL)",
                                            "HYBRID ELECTRIC (PETROL)","PLUG-IN HYBRID ELECTRIC (DIESEL)",
                                            "PLUG-IN HYBRID ELECTRIC (PETROL)","RANGE EXTENDED ELECTRIC",
                                            "FUEL CELLS","Total"), 3),
                           Keepership = rep(c("COMPANY","PRIVATE","Total"), each = 10),
                           quarter = x$quarter[1],
                           count2 = 0
    )
    # Better to assume missing rows are 0???
    x$id = paste0(x$Fuel,"-",x$Keepership)
    x_missing$id = paste0(x_missing$Fuel,"-",x_missing$Keepership)
    x_missing = x_missing[!x_missing$id %in% x$id,]
    # Special case we only have totals e.g. E01000005 2019 Q1 there are [c] Private vehicles in total but no rows about them

    if(any(c(x_missing$Fuel == "Total", x_missing$Keepership == "Total"))){
      x_missing$count2 = NA
    } else {
      x_total_fuel = unique(x$Fuel[x$Keepership == "Total"])
      x_total_Keepership = unique(x$Keepership[x$Fuel == "Total"])
      x_total_fuel = x_total_fuel[x_total_fuel != "Total"]
      x_total_Keepership = x_total_Keepership[x_total_Keepership != "Total"]
      x_missing$count2 = ifelse(x_missing$Fuel %in% x_total_fuel & x_missing$Keepership %in% x_total_Keepership, NA, 0)
    }

    x = dplyr::bind_rows(x, x_missing)
  }

  if(all(is.na(x$count2))){
    # No Data
    return(NULL)
  }

  if(all(!is.na(x$count2))){
    # All Data
    x$count = x$count2
    x$count2 = NULL
    x$id = NULL
    x = x[x$Keepership != "Total",]
    x = x[x$Fuel != "Total",]
    return(x)
  }

  #Missing Data
  x_totals = x[x$Keepership == "Total" | x$Fuel == "Total",]
  x_others = x[!(x$Keepership == "Total" | x$Fuel == "Total"),]

  # Make Matrix
  y = x_others[,c("Fuel","Keepership","count2")]
  keeperships = unique(y$Keepership)

  y2 <- tidyr::pivot_wider(y, names_from = "Keepership", values_from = "count2")
  y2_mat = as.matrix(y2[,seq(2, ncol(y2))])
  rownames(y2_mat) = y2$Fuel

  rsum = x_totals[x_totals$Keepership == "Total" & x_totals$Fuel != "Total",]
  rsum = rsum$count2[match(rownames(y2_mat), rsum$Fuel)]

  csum = x_totals[x_totals$Keepership != "Total" & x_totals$Fuel == "Total",]
  csum = csum$count2[match(colnames(y2_mat), csum$Keepership)]

  tt = x_totals$count2[x_totals$Fuel == "Total" & x_totals$Keepership == "Total"]
  tt_alt = min(sum(rsum, na.rm = TRUE), sum(csum, na.rm = TRUE), na.rm = TRUE)

  # Option 1: tt is NA - We have very limited info
  # Option 2: tt > tt_alt - We know there are missing variaibles
  # Option 3: tt = tt_alt - We only have to fill in the matrix
  # Option 4: tt_alt > tt - Something has gone wrong.

  # Check for incomplete or partial totals
  if(any(is.na(x_totals$count2))){

    if(is.na(tt)){
      # Overall Total is between 1 and 4
      # Missing Totals
      if(sum(is.na(x_others$count2)) > 4){
        # Too many options, pefer option where known [c] over generated option
        x_others$count2[is.na(x_others$count)] = 0
      }
      if(sum(is.na(x_others$count2)) > 4){
        # If there are still too many options, sick 1 in just the first 4 options
        x_others$count2[is.na(x_others$count2)] = c(rep(1,4), rep(0, sum(is.na(x_others$count2)) - 4))
      } else {
        x_others$count2 = ifelse(is.na(x_others$count2),1,x_others$count2)
      }

      x_others$count = x_others$count2
      #x_others$count2 = NULL

      #check
      if(sum(x_others$count) > 4){
        print(x)
        stop("Assumed values greater than total ")
      }

      x_others = x_others[,c("LSOA21CD","Fuel","Keepership", "quarter","count")]

      return(x_others)

    } else {
      incomplete = TRUE
    }


  }

  if(tt > tt_alt) {
    # Can't assume missing data is zero
    x$count2 = ifelse(is.na(x$count),NA,x$count2)

    x_totals = x[x$Keepership == "Total" | x$Fuel == "Total",]
    x_others = x[!(x$Keepership == "Total" | x$Fuel == "Total"),]

    y = x_others[,c("Fuel","Keepership","count2")]
    keeperships = unique(y$Keepership)

    y2 <- tidyr::pivot_wider(y, names_from = "Keepership", values_from = "count2")
    y2_mat = as.matrix(y2[,seq(2, ncol(y2))])
    rownames(y2_mat) = y2$Fuel

    rsum = x_totals[x_totals$Keepership == "Total" & x_totals$Fuel != "Total",]
    rsum = rsum$count2[match(rownames(y2_mat), rsum$Fuel)]

    csum = x_totals[x_totals$Keepership != "Total" & x_totals$Fuel == "Total",]
    csum = csum$count2[match(colnames(y2_mat), csum$Keepership)]

    tt = x_totals$count2[x_totals$Fuel == "Total" & x_totals$Keepership == "Total"]

    incomplete = TRUE
  }

  # Fill Gaps


  # Use Furness balancing to fill gaps
  if(incomplete){
    #newmat = furness_incomplete(mat = y2_mat, rsum, csum, tt)
    newmat = furness_partial_integer_total(mat = y2_mat, rsum, csum, tt)
  } else {
    newmat = furness_partial(mat = y2_mat, rsum, csum, check = TRUE)
  }

  newdf = as.data.frame(newmat)
  newdf$Fuel <- rownames(newdf)
  newdf = tidyr::pivot_longer(newdf, cols = dplyr::all_of(keeperships), names_to = "Keepership", values_to = "count")
  newdf$LSOA21CD = x_others$LSOA21CD[1]
  #newdf$LicenceStatus = x_others$LicenceStatus[1]
  newdf$quarter = x_others$quarter[1]
  newdf = newdf[,c("LSOA21CD","Fuel","Keepership", "quarter","count")]
  return(newdf)




}

#' Reconstruct suppressed VEH0145 (plug-in vehicle) counts for one LSOA-quarter
#'
#' @description As `fill_gaps_135()` but for the 4-fuel plug-in vehicle
#'   table.
#' @param x Long data frame for one LSOA/quarter group.
#' @return The completed long data frame, or NULL if the group has no data.
#' @keywords internal
fill_gaps_145 = function(x){

  incomplete = FALSE
  suppressWarnings(x$count2 <- as.numeric(x$count))

  # Sometimes data is missing
  if(nrow(x) != 15){
    x_missing = data.frame(LSOA21CD = x$LSOA21CD[1],
                           Fuel = rep(c("BATTERY ELECTRIC","PLUG-IN HYBRID ELECTRIC (DIESEL)",
                                        "PLUG-IN HYBRID ELECTRIC (PETROL)","RANGE EXTENDED ELECTRIC",
                                        "Total" ), 3),
                           Keepership = rep(c("COMPANY","PRIVATE","Total"), each = 5),
                           quarter = x$quarter[1],
                           count2 = 0
    )
    # Better to assume missing rows are 0???
    x$id = paste0(x$Fuel,"-",x$Keepership)
    x_missing$id = paste0(x_missing$Fuel,"-",x_missing$Keepership)
    x_missing = x_missing[!x_missing$id %in% x$id,]
    # Special case we only have totals e.g. E01000005 2019 Q1 there are [c] Private vehicles in total but no rows about them

    if(any(c(x_missing$Fuel == "Total", x_missing$Keepership == "Total"))){
      x_missing$count2 = NA
    } else {
      x_total_fuel = unique(x$Fuel[x$Keepership == "Total"])
      x_total_Keepership = unique(x$Keepership[x$Fuel == "Total"])
      x_total_fuel = x_total_fuel[x_total_fuel != "Total"]
      x_total_Keepership = x_total_Keepership[x_total_Keepership != "Total"]
      x_missing$count2 = ifelse(x_missing$Fuel %in% x_total_fuel & x_missing$Keepership %in% x_total_Keepership, NA, 0)
    }

    x = dplyr::bind_rows(x, x_missing)
  }

  if(all(is.na(x$count2))){
    # No Data
    return(NULL)
  }

  if(all(!is.na(x$count2))){
    # All Data
    x$count = x$count2
    x$count2 = NULL
    x$id = NULL
    x = x[x$Keepership != "Total",]
    x = x[x$Fuel != "Total",]
    return(x)
  }

  #Missing Data
  x_totals = x[x$Keepership == "Total" | x$Fuel == "Total",]
  x_others = x[!(x$Keepership == "Total" | x$Fuel == "Total"),]

  # Make Matrix
  y = x_others[,c("Fuel","Keepership","count2")]
  keeperships = unique(y$Keepership)

  y2 <- tidyr::pivot_wider(y, names_from = "Keepership", values_from = "count2")
  y2_mat = as.matrix(y2[,seq(2, ncol(y2))])
  rownames(y2_mat) = y2$Fuel

  rsum = x_totals[x_totals$Keepership == "Total" & x_totals$Fuel != "Total",]
  rsum = rsum$count2[match(rownames(y2_mat), rsum$Fuel)]

  csum = x_totals[x_totals$Keepership != "Total" & x_totals$Fuel == "Total",]
  csum = csum$count2[match(colnames(y2_mat), csum$Keepership)]

  tt = x_totals$count2[x_totals$Fuel == "Total" & x_totals$Keepership == "Total"]
  tt_alt = min(sum(rsum, na.rm = TRUE), sum(csum, na.rm = TRUE), na.rm = TRUE)

  # Option 1: tt is NA - We have very limited info
  # Option 2: tt > tt_alt - We know there are missing variaibles
  # Option 3: tt = tt_alt - We only have to fill in the matrix
  # Option 4: tt_alt > tt - Something has gone wrong.

  # Check for incomplete or partial totals
  if(any(is.na(x_totals$count2))){
    if(is.na(tt)){
      # Overall Total is between 1 and 4
      # Missing Totals
      if(sum(is.na(x_others$count2)) > 4){
        # Too many options, pefer option where known [c] over generated option
        x_others$count2[is.na(x_others$count)] = 0
      }
      if(sum(is.na(x_others$count2)) > 4){
        # If there are still too many options, sick 1 in just the first 4 options
        x_others$count2[is.na(x_others$count2)] = c(rep(1,4), rep(0, sum(is.na(x_others$count2)) - 4))
      } else {
        x_others$count2 = ifelse(is.na(x_others$count2),1,x_others$count2)
      }

      x_others$count = x_others$count2
      #x_others$count2 = NULL

      #check
      if(sum(x_others$count) > 4){
        print(x)
        stop("Assumed values greater than total ")
      }

      x_others = x_others[,c("LSOA21CD","Fuel","Keepership", "quarter","count")]

      return(x_others)

    } else {
      incomplete = TRUE
    }
  }

  if (tt > tt_alt) {
    # Can't assume missing data is zero
    x$count2 = ifelse(is.na(x$count),NA,x$count2)

    x_totals = x[x$Keepership == "Total" | x$Fuel == "Total",]
    x_others = x[!(x$Keepership == "Total" | x$Fuel == "Total"),]

    y = x_others[,c("Fuel","Keepership","count2")]
    keeperships = unique(y$Keepership)

    y2 <- tidyr::pivot_wider(y, names_from = "Keepership", values_from = "count2")
    y2_mat = as.matrix(y2[,seq(2, ncol(y2))])
    rownames(y2_mat) = y2$Fuel

    rsum = x_totals[x_totals$Keepership == "Total" & x_totals$Fuel != "Total",]
    rsum = rsum$count2[match(rownames(y2_mat), rsum$Fuel)]

    csum = x_totals[x_totals$Keepership != "Total" & x_totals$Fuel == "Total",]
    csum = csum$count2[match(colnames(y2_mat), csum$Keepership)]

    tt = x_totals$count2[x_totals$Fuel == "Total" & x_totals$Keepership == "Total"]

    incomplete = TRUE
  }

  # Fill Gaps


  # Use Furness balancing to fill gaps
  if(incomplete){
    #newmat = furness_incomplete(mat = y2_mat, rsum, csum, tt)
    newmat = furness_partial_integer_total(mat = y2_mat, rsum, csum, tt)
  } else {
    newmat = furness_partial(mat = y2_mat, rsum, csum, check = TRUE)
  }

  newdf = as.data.frame(newmat)
  newdf$Fuel <- rownames(newdf)
  newdf = tidyr::pivot_longer(newdf, cols = dplyr::all_of(keeperships), names_to = "Keepership", values_to = "count")
  newdf$LSOA21CD = x_others$LSOA21CD[1]
  #newdf$LicenceStatus = x_others$LicenceStatus[1]
  newdf$quarter = x_others$quarter[1]
  newdf = newdf[,c("LSOA21CD","Fuel","Keepership", "quarter","count")]
  return(newdf)




}
