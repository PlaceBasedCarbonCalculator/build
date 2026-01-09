load_ubdc_house_prices = function(path = "../inputdata/house prices/ppdid_uprn_usrn.zip"){
  dir.create(file.path(tempdir(),"ubdc"))
  unzip(path, exdir = file.path(tempdir(),"ubdc"))

  dat = readr::read_csv(file.path(tempdir(),"ubdc","ppdid_uprn_usrn.csv"))

  dat
}


load_lr_price_paid = function(path = "../inputdata/house prices/land registry/"){
  fls = list.files(path)

  pp = list()

  for(i in 1:length(fls)){
     sub = readr::read_csv(file.path(path,fls[i]),
                              col_names = c("transactionid","price","date","postcode",
                                            "property_type","new_build","freehold",
                                            "address1","address2","address3","address4","town","la","county",
                                            "record_status","transaction_category"))
     sub = sub[!duplicated(sub$transactionid),]

      pp[[i]] = sub
  }

  pp = dplyr::bind_rows(pp)

  pp$property_type = as.factor(pp$property_type)
  pp$new_build = as.factor(pp$new_build)
  pp$freehold = as.factor(pp$freehold)
  pp$record_status = as.factor(pp$record_status)
  pp$transaction_category = as.factor(pp$transaction_category)


  pp
}
