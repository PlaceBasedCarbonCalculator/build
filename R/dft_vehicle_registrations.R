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
                                 .options = furrr::furrr_options(seed = TRUE),
                                 .progress = TRUE)
  future::plan("sequential")

  d125_clean = data.table::rbindlist(d125_clean)
  rm(d125_long)

  d125_wide = tidyr::pivot_wider(d125_clean,
                          id_cols = c("LSOA21CD","quarter"),
                          names_from = c("BodyType","Keepership","LicenceStatus"),
                          values_from = "count"
  )
  d125_wide

}


load_dft_ulev_registrations <- function(path = file.path(data_path(),"vehicle_registrations")){

  d135 <- readr::read_csv(file.path(path,"df_VEH0135.csv"))

  d135_long <- tidyr::pivot_longer(d135,
                                   cols = names(d135)[5:ncol(d135)],
                                   names_to = "quarter",
                                   values_to = "count")

  d135_long <- d135_long[grepl("Q1",d135_long$quarter),]
  d135_long$LSOA11NM <- NULL
  d135_long <- dplyr::group_by(d135_long, LSOA21CD, quarter)
  d135_long <- dplyr::group_split(d135_long)

  # Tests c(10624, 74860, 74862, 27538)
  #d135_clean = pbapply::pblapply(d135_list, fill_gaps_135)
  future::plan("multisession")
  d135_clean = furrr::future_map(d135_long, .f = fill_gaps_135,
                                 .options = furrr::furrr_options(seed = TRUE),
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


load_dft_ev_registrations <- function(path = file.path(data_path(),"vehicle_registrations")){

  d145 <- readr::read_csv(file.path(path,"df_VEH0145.csv"))

  d145_long <- tidyr::pivot_longer(d145,
                                   cols = names(d145)[5:ncol(d145)],
                                   names_to = "quarter",
                                   values_to = "count")

  d145_long <- d145_long[grepl("Q1",d145_long$quarter),]
  d145_long$LSOA11NM <- NULL
  d145_long <- dplyr::group_by(d145_long, LSOA21CD, quarter)
  d145_long <- dplyr::group_split(d145_long)

  # Tests c(10624, 74860, 74862, 27538)
  #d145_clean = pbapply::pblapply(d145_list, fill_gaps_135)
  future::plan("multisession")
  d145_clean = furrr::future_map(d145_long, .f = fill_gaps_135,
                                 .options = furrr::furrr_options(seed = TRUE),
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


fill_gaps = function(x){
  incomplete = FALSE
  suppressWarnings(x$count2 <- as.numeric(x$count))
  if(all(is.na(x$count2))){
    # No Data
    return(NULL)
  }

  if(all(!is.na(x$count2))){
    # All Data
    x$count = x$count2
    x$count2 = NULL
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
      x_others$count2 = ifelse(x_others$count == "[c]",1,x_others$count2)
      x_others$count = x_others$count2
      x_others$count2 = NULL

      #check
      if(sum(x_others$count) > 4){
        print(x)
        stop("Assumed values greater than total ")
      }

      return(x_others)

    } else {
      incomplete = TRUE
    }


  }


  # Fill Gaps
  # Make Matrix
  y = x_others[,c("BodyType","Keepership","count2")]
  y$id = paste0(y$BodyType, y$Keepership)
  y_missing = data.frame(BodyType = rep(c("Cars","Motorcycles","Other vehicles"), 2),
                         Keepership = rep(c("Company","Private"), each = 3),
                         count2 = 0
  )
  y_missing$id = paste0(y_missing$BodyType, y_missing$Keepership)
  y_missing = y_missing[!y_missing$id %in% y$id,]
  y = rbind(y, y_missing)
  y$id <- NULL
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


fill_gaps_135 = function(x){

  incomplete = FALSE
  suppressWarnings(x$count2 <- as.numeric(x$count))
  if(all(is.na(x$count2))){
    # No Data
    return(NULL)
  }

  if(all(!is.na(x$count2))){
    # All Data
    x$count = x$count2
    x$count2 = NULL
    x = x[x$Keepership != "Total",]
    x = x[x$Fuel != "Total",]
    return(x)
  }

  #Missing Data
  x_totals = x[x$Keepership == "Total" | x$Fuel == "Total",]
  x_others = x[!(x$Keepership == "Total" | x$Fuel == "Total"),]

  if(any(is.na(x_totals$count2))){
    tt = x_totals$count2[x_totals$Fuel == "Total" & x_totals$Keepership == "Total"]

    if(is.na(tt)){
      # Overall Total is between 1 and 4
      # Missing Totals
      x_others$count2 = ifelse(x_others$count == "[c]",1,x_others$count2)
      x_others$count = x_others$count2
      x_others$count2 = NULL

      #check
      if(sum(x_others$count) > 4){
        print(x)
        stop("Assumed values greater than total ")
      }

      return(x_others)

    } else {
      incomplete = TRUE

      # # Overall Total Know but partial total missing
      # x_totals_fuel = x_totals[x_totals$Fuel != "Total", ]
      # x_totals_keep = x_totals[x_totals$Keepership != "Total", ]
      #
      # if(anyNA(x_totals_fuel$count2)){
      #   x_totals_fuel$count2[is.na(x_totals_fuel$count2)] <- distribute(tt - sum(x_totals_fuel$count2, na.rm = TRUE),
      #                                                                  sum(is.na(x_totals_fuel$count2)))
      # }
      # if(anyNA(x_totals_keep$count2)){
      #   x_totals_keep$count2[is.na(x_totals_keep$count2)] <- distribute(tt - sum(x_totals_keep$count2, na.rm = TRUE),
      #                                                                   sum(is.na(x_totals_keep$count2)))
      # }
      #
      # x_totals = rbind(x_totals_fuel, x_totals_keep,
      #                  x_totals[x_totals$Fuel == "Total" & x_totals$Keepership == "Total", ])
      #
      # check = FALSE # Disable the check on Furness balancing, as above guesses can be inconsitent
      # # TODO: come up with a fix for this
      # # Example
      # #     ?   5          1   5
      # # ?   0   ?      2   0   2
      # # ?   ?   0  --> 2   2!  2
      # # ?   0   ?      2   0   2
     }


  }

  # Fill Gaps
  # Make Matrix
  y = x_others[,c("Fuel","Keepership","count2")]
  y$id = paste0(y$Fuel, y$Keepership)
  fuels = unique(y$Fuel)
  keeperships = unique(y$Keepership)
  y_missing = data.frame(Fuel = rep(fuels, length(keeperships)),
                         Keepership = rep(keeperships, each = length(fuels)),
                         count2 = 0
  )
  y_missing$id = paste0(y_missing$Fuel, y_missing$Keepership)
  y_missing = y_missing[!y_missing$id %in% y$id,]
  y = rbind(y, y_missing)
  y$id <- NULL
  y2 <- tidyr::pivot_wider(y, names_from = "Keepership", values_from = "count2")
  y2_mat = as.matrix(y2[,seq(2, ncol(y2))])
  rownames(y2_mat) = y2$Fuel
  rsum = x_totals[x_totals$Keepership == "Total" & x_totals$Fuel != "Total",]
  rsum = rsum$count2[match(rownames(y2_mat), rsum$Fuel)]

  csum = x_totals[x_totals$Keepership != "Total" & x_totals$Fuel == "Total",]
  csum = csum$count2[match(colnames(y2_mat), csum$Keepership)]

  # Use Furness balancing to fill gaps
  if(incomplete){
    newmat = furness_incomplete(mat = y2_mat, rsum, csum, tt)
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
