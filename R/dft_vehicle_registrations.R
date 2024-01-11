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
                            cols = names(d125)[6:59],
                            names_to = "quarter",
                            values_to = "count")

  #table(d125_long$count[is.na(as.numeric(d125_long$count))])
  # [c] (1-4)    [x] (Not Available)  [z](Not applicable)
  # 5908204      388550
  d125_long <- d125_long[grepl("Q1",d125_long$quarter),]
  d125_long$LSOA11NM <- NULL
  d125_list <- dplyr::group_by(d125_long, LSOA11CD, quarter, LicenceStatus)
  d125_list <- dplyr::group_split(d125_list)

  # Tests c(10624, 74860, 74862, 27538)
  d125_clean = pbapply::pblapply(d125_list, fill_gaps)
  #saveRDS(d125_clean, "data/DfT LSOA/df_VEH0125_list.Rds")

  #d125_clean = readRDS("data/DfT LSOA/df_VEH0125_list.Rds")
  d125_clean = data.table::rbindlist(d125_clean)


  d125_wide = tidyr::pivot_wider(d125_clean,
                          id_cols = c("LSOA11CD","quarter"),
                          names_from = c("BodyType","Keepership","LicenceStatus"),
                          values_from = "count"
  )
  d125_wide

}


fill_gaps = function(x){
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
    # Missing Totals
    x_others$count2 = ifelse(x_others$count == "[c]",1,x_others$count2)
    x_others$count = x_others$count2
    x_others$count2 = NULL
    return(x_others)
  }

  # Fill Gaps
  # Make Matrix
  y = x_others[,c("BodyType","Keepership","count2")]
  y$id = paste0(y$BodyType, y$Keepership)
  y_missing = data.frame(BodyType = rep(c("Cars","Motorcycles","Other body types"), 2),
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
  newmat = furness_partial(mat = y2_mat, rsum, csum)
  newdf = as.data.frame(newmat)
  newdf$BodyType <- rownames(newdf)
  newdf = tidyr::pivot_longer(newdf, cols = c("Company","Private"),names_to = "Keepership", values_to = "count")
  newdf$LSOA11CD = x_others$LSOA11CD[1]
  newdf$LicenceStatus = x_others$LicenceStatus[1]
  newdf$quarter = x_others$quarter[1]
  newdf = newdf[,c("LSOA11CD","BodyType","Keepership","LicenceStatus", "quarter","count")]
  return(newdf)


  stop(x$LSOA11CD[1]," ",x$quarter[1]," ",x$LicenceStatus[1])

}
