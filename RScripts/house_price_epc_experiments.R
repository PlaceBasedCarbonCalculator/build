library(data.table)
library(sf)
library(targets)

tar_load(house_price_lr)
tar_load(house_prices_ubdc)
tar_load(parameters)
tar_load(uprn_historical)
tar_load(bounds_lsoa_GB_full)
tar_load(lookup_postcode_OA_LSOA_MSOA_2021)
tar_load(postcode_points)

uprn = st_as_sf(uprn_historical, coords = c("LONGITUDE","LATITUDE"), crs = 4326)

epc = readRDS(file.path(parameters$path_data,"epc/GB_domestic_epc.Rds"))
epc = epc[,c("UPRN","addr","ADDRESS2","ADDRESS3","POSTCODE","year")]
epc_nondom = readRDS(file.path(parameters$path_data,"epc/GB_nondomestic_epc.Rds"))

land = read_sf("../../mem48/LandOwnership/data/tilegeojson/uk_owners.geojson")
land = land[land$geocode_type == "Address",]
land = land[!duplicated(land$geocoded_address),]

points_membership <- function(x, y, prec = 6L) {
  stopifnot(inherits(x, "sf"), inherits(y, "sf"))
  if (st_crs(x) != st_crs(y)) {
    stop("CRS Don't match")
  }

  # Remove duplicated location UPRNs and ambiguose
  dups = unique(y$geometry[duplicated(y$geometry)])
  is_dup = y$geometry %in% dups
  y = y[!is_dup,]

  x_coord <- st_coordinates(x)
  y_coord <- st_coordinates(y)




  if (!is.null(prec)) {
    scale <- 10^prec
    ix_x <- as.integer(round(x_coord[,1] * scale))
    iy_x <- as.integer(round(x_coord[,2] * scale))
    ix_y <- as.integer(round(y_coord[,1] * scale))
    iy_y <- as.integer(round(y_coord[,2] * scale))
  } else {
    ix_x <- x_coord[,1]; iy_x <- x_coord[,2]
    ix_y <- y_coord[,1]; iy_y <- y_coord[,2]
  }

  xdt <- data.table(ix = ix_x, iy = iy_x, title = x$Title, address = x$geocoded_address, type = x$geocode_type)
  ydt <- data.table(ix = ix_y, iy = iy_y, uprn = y$UPRN)

  # remove any duplicated uprn locations as match will be ambiguous
  dups = ydt[,.(ix,iy)]
  dups = unique(dups[duplicated(dups),])
  dups$duplicate = TRUE

  ydt = dups[ydt, on = .(ix, iy)]
  ydt$duplicate[is.na(ydt$duplicate)] = FALSE
  ydt = ydt[!ydt$duplicate,]

  # setkey(xdt, ix, iy)
  # setkey(ydt, ix, iy)

  # xdt[, in_y := FALSE]
  # xdt[ydt, on = .(ix, iy), in_y := TRUE]
  # xdt[, dup_in_x := .N > 1L, by = .(ix, iy)]
  # ydt[, dup_in_y := .N > 1L, by = .(ix, iy)]

  # list(
  #   in_y = xdt[["in_y"]],
  #   dup_in_x = xdt[["dup_in_x"]],
  #   dup_in_y = ydt[["dup_in_y"]],
  #   x_duplicates_summary = xdt[, .N, by = .(ix, iy)][N > 1L][order(-N)],
  #   y_duplicates_summary = ydt[, .N, by = .(ix, iy)][N > 1L][order(-N)]
  # )
  land_uprn = ydt[xdt, on = .(ix, iy)]

  land_uprn = land_uprn[!is.na(land_uprn$uprn),]
  dups = unique(land_uprn$uprn[duplicated(land_uprn$uprn)])
  land_uprn = land_uprn[land_uprn$uprn %in% dups, ]

  land_uprn
}


land_uprn = points_membership(x = land, y = uprn)

# Lots of duplciated UPRNS
summary(duplicated(land_uprn$uprn)) # Seem to be varients of the same address
# Mode   FALSE    TRUE
# logical   16973   18368

postcode_rx = c("\\b(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})\\b")
land_uprn$postcode <- unlist(stringr::str_extract_all(land_uprn$address, postcode_rx))
land_uprn$join_address <- trimws(toupper(gsub(",","",gsub(postcode_rx,"",land_uprn$address))), which = "both")

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
summary(duplicated(unique_address$uprn))

house_price_lr_nouprn$uprn = NULL
house_price_lr_nouprn$usrn = NULL
house_price_lr_nouprn$parentuprn = NULL

nrow(house_price_lr_nouprn) + nrow(house_price_lr_withuprn) == nrow(house_price_lr)

house_price_lr_nouprn = dplyr::left_join(house_price_lr_nouprn,
                                         unique_address,
                                         by = c("postcode","property_type",
                                         "address1","address2","address3",
                                         "address4","town","la","county"))

nrow(house_price_lr_nouprn) + nrow(house_price_lr_withuprn) == nrow(house_price_lr)

house_price_lr_nouprn = house_price_lr_nouprn[!duplicated(house_price_lr_nouprn$transactionid),]

nrow(house_price_lr_nouprn) + nrow(house_price_lr_withuprn) == nrow(house_price_lr)

house_price_lr2 = rbind(house_price_lr_withuprn, house_price_lr_nouprn)

summary(is.na(house_price_lr2$uprn))

# breaks = seq(lubridate::ymd("1993-01-01"),lubridate::ymd("2025-12-31"), 365)
# hist(as.Date(house_price_lr2$date[is.na(house_price_lr2$uprn)]), breaks = breaks)
# house_price_lr2 = dplyr::left_join(house_price_lr2, uprn, by = c("uprn" = "UPRN"))
# house_price_lr2 = st_as_sf(house_price_lr2)
# summary(st_is_empty(house_price_lr2))


house_price_lr2_nopoint = house_price_lr2[is.na(house_price_lr2$uprn),]

#TODO: some have uprns but no location
# Old UPRN?
#summary(is.na(house_price_lr2_nopoint$uprn))
# Mode   FALSE    TRUE
# logical    3683 2890301

house_price_lr2_nopoint$address1[is.na(house_price_lr2_nopoint$address1)] = ""
house_price_lr2_nopoint$address2[is.na(house_price_lr2_nopoint$address2)] = ""
house_price_lr2_nopoint$address3[is.na(house_price_lr2_nopoint$address3)] = ""
house_price_lr2_nopoint$address4[is.na(house_price_lr2_nopoint$address4)] = ""

# Check addresses against EPCs
house_price_lr2_nopoint$join_address <- trimws(gsub("\\s+", " ",
                                                    paste(house_price_lr2_nopoint$address1,
                                                          house_price_lr2_nopoint$address2,
                                                          house_price_lr2_nopoint$address3)))

# Two slighyl differnt appraoches to addresses are being harmonised
house_price_lr2_nopoint$join_address2 <- trimws(gsub("\\s+", " ",
                                                    paste(house_price_lr2_nopoint$address1,
                                                          house_price_lr2_nopoint$address2,
                                                          house_price_lr2_nopoint$address3,
                                                          house_price_lr2_nopoint$address4)))


epc$addr[is.na(epc$addr)] = ""
epc$ADDRESS2[is.na(epc$ADDRESS2)] = ""
epc$ADDRESS3[is.na(epc$ADDRESS3)] = ""

epc$join_address <- toupper(trimws(gsub("\\s+", " ",
                          paste(epc$addr,
                                epc$ADDRESS2,
                                epc$ADDRESS3))))
epc$join_address <- gsub(",","",epc$join_address) # Some addresses have commas

# Small number of duplicated addresses with different UPRNs
# All very close to each other. Possible multiple UPRN for same address?
# Take the newest one as definitive version
epc = epc[,c("UPRN","join_address","POSTCODE","year")]
epc = epc[order(epc$year, decreasing = TRUE),]
epc = epc[!duplicated(st_drop_geometry(epc[,c("join_address","POSTCODE")])),]

# TODO: This removes 49,147 transactions without postcodes, most have full addresses
house_price_lr2_nopoint = house_price_lr2_nopoint[!is.na(house_price_lr2_nopoint$postcode),]
house_price_lr2_nopoint = dplyr::left_join(house_price_lr2_nopoint, epc, by = c("join_address" = "join_address", "postcode" = "POSTCODE"))
house_price_lr2_nopoint = sf::st_as_sf(house_price_lr2_nopoint)

summary(st_is_empty(house_price_lr2_nopoint))

house_price_lr2_nopoint_good = house_price_lr2_nopoint[!is.na(house_price_lr2_nopoint$UPRN),] #1,000,962
house_price_lr2_nopoint_bad = house_price_lr2_nopoint[is.na(house_price_lr2_nopoint$UPRN),] # 1,893,022

house_price_lr2_nopoint_bad = sf::st_drop_geometry(house_price_lr2_nopoint_bad)
house_price_lr2_nopoint_bad$UPRN = NULL
house_price_lr2_nopoint_bad$year = NULL

house_price_lr2_nopoint_bad = dplyr::left_join(house_price_lr2_nopoint_bad, epc, by = c("join_address2" = "join_address", "postcode" = "POSTCODE"))

house_price_lr2_nopoint_bad_good = house_price_lr2_nopoint_bad[!is.na(house_price_lr2_nopoint_bad$UPRN),]
house_price_lr2_nopoint_bad_bad = house_price_lr2_nopoint_bad[is.na(house_price_lr2_nopoint_bad$UPRN),]

house_price_lr2_nopoint_bad_good = st_as_sf(house_price_lr2_nopoint_bad_good)

house_price_lr2_nopoint_bad_bad = dplyr::left_join(house_price_lr2_nopoint_bad_bad, postcode_points,
                                           by = c("postcode"))
house_price_lr2_nopoint_bad_bad = st_as_sf(house_price_lr2_nopoint_bad_bad)
summary(st_is_empty(house_price_lr2_nopoint_bad_bad))


house_price_lr2_nopoint = house_price_lr2_nopoint[!st_is_empty(house_price_lr2_nopoint),]
#house_price_lr2_point = house_price_lr2[!st_is_empty(house_price_lr2),]


house_price_lr2_nopoint_good = st_transform(house_price_lr2_nopoint_good, 27700)
house_price_lr2_nopoint_bad_good = st_transform(house_price_lr2_nopoint_bad_good, 27700)
house_price_lr2_nopoint_bad_bad = st_transform(house_price_lr2_nopoint_bad_bad, 27700)
#house_price_lr2_point = st_transform(house_price_lr2_point, 27700)

# Nof worht it for only 17 more addresses
# foo = dplyr::left_join(house_price_lr2_nopoint_bad_bad,
#                        land_uprn[,c("uprn","postcode","join_address")],
#                        by = c("join_address2" = "join_address","postcode")
#                        )
#
# summary(is.na(foo$uprn.y))

house_price_lr3 = rbind(house_price_lr2_point,
                        house_price_lr2_nopoint_good,
                        house_price_lr2_nopoint_bad_good,
                        house_price_lr2_nopoint_bad_bad)

house_price_lr3 = st_join(house_price_lr3, bounds_lsoa_GB_full)

summary(is.na(house_price_lr3$LSOA21CD)) # Few costal and on water propertities like warfs

# foo = house_price_lr3[is.na(house_price_lr3$LSOA21CD),]

house_price_lr3$year = lubridate::year(house_price_lr3$date)

house_price_lsoa = house_price_lr3 |>
  sf::st_drop_geometry() |>
  dplyr::group_by(LSOA21CD, year, property_type, freehold) |>
  dplyr::summarise(transactions = dplyr::n(),
                   price_min = min(price),
                   price_median = median(price),
                   price_max = max(price)
                   )
