house_price_add_uprn = function(house_price_lr, house_prices_ubdc, uprn){

  house_price_lr = dplyr::left_join(house_price_lr, house_prices_ubdc, by = "transactionid")

  house_price_lr_withuprn = house_price_lr[!is.na(house_price_lr$uprn),]
  house_price_lr_nouprn = house_price_lr[is.na(house_price_lr$uprn),]

  # Check for matching addresses
  unique_address = house_price_lr_withuprn[,c("postcode","property_type",
                                              "address1","address2","address3","address4","town","la","county",
                                              "uprn","parentuprn","usrn")]
  unique_address = unique_address[!duplicated(unique_address[,c("postcode","property_type",
                                                                "address1","address2","address3",
                                                                "address4","town","la","county")]),]

  house_price_lr_nouprn$uprn = NULL
  house_price_lr_nouprn$usrn = NULL
  house_price_lr_nouprn$parentuprn = NULL

  house_price_lr_nouprn = dplyr::left_join(house_price_lr_nouprn,
                                           unique_address,
                                           by = c("postcode","property_type",
                                                  "address1","address2","address3",
                                                  "address4","town","la","county"))

  house_price_lr_nouprn = house_price_lr_nouprn[!duplicated(house_price_lr_nouprn$transactionid),]

  house_price_lr2 = rbind(house_price_lr_withuprn, house_price_lr_nouprn)

  house_price_lr2 = dplyr::left_join(house_price_lr2, uprn, by = c("uprn" = "UPRN"))
  house_price_lr2 = st_as_sf(house_price_lr2)

  house_price_lr2
}
