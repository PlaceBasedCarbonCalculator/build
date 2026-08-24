# Old / unused code ---------------------------------------------------------
# Functions moved here during the July 2026 audit because they are not called
# by _targets.R or any active function. Kept for reference; several are
# superseded versions or unfinished stubs. This file is still sourced by
# tar_source(), so commented-out targets that reference these functions can be
# re-enabled without changes.

# --- from R/make_json.R ------------------------------
#' Write a single zone's data to a JSON file
#'
#' @description Takes a data frame for a single zone, drops the ID column and
#'   writes the remaining columns to `<path>/<id>.json`. Currently unused:
#'   `export_zone_json()` uses `convert2json()` + `write2file()` instead.
#' @param sub Data frame containing rows for exactly one zone.
#' @param idcol Name of the column holding the zone ID; its first value names
#'   the output file.
#' @param path Directory in which to write the JSON file.
#' @param dataframe Passed to `yyjsonr::write_json_file()`; either "rows" or
#'   "columns", controlling the JSON orientation.
#' @param na Unused.
#' @return The path of the file written.
#' @keywords internal
write_one <- function(sub, idcol, path = "", dataframe, na){
  #sub <- x[x[[idcol]] == idv, , drop = FALSE]
  # ensure a data.frame (avoid tibble overhead)
  sub <- as.data.frame(sub)
  nmsub <- sub[[idcol]][1]
  sub[[idcol]] <- NULL
  outfile <- file.path(path, paste0(nmsub, ".json"))
  yyjsonr::write_json_file(sub, outfile, dataframe = dataframe)
  outfile
}

# --- from R/lsoa_population_estimates.R ------------------------------
#' Read the mid-2022 population estimates on 2021 LSOAs
#'
#' @description Reads the mid-2022 estimates from `pop2022.xlsx`, sums the
#'   male/female single-year-of-age columns and aggregates to 5-year bands.
#'   Superseded by `build_lsoa_population_2022_24()`; not currently called by
#'   any target.
#' @param path Folder containing `pop2022.xlsx`.
#' @return A data frame with `LSOA21CD`, `all_ages` and age-band columns.
#' @keywords internal
build_lsoa_population_2022 = function(path) {
  #2022
  pop22 <- readxl::read_excel(file.path(path,"pop2022.xlsx"),
                              sheet = "Mid-2022 LSOA 2021")
  pop22 <- as.data.frame(pop22)
  names(pop22) <- pop22[3,]
  pop22 <- pop22[4:nrow(pop22),]
  pop22[5:ncol(pop22)] <- lapply(pop22[5:ncol(pop22)], as.numeric)

  for(i in 0:90){
    pop22[paste0("A",i)] = pop22[paste0("M",i)] + pop22[paste0("F",i)]
  }

  pop22 = pop22[,c("LSOA 2021 Code","Total",paste0("A",0:90))]

  bands = c("0-4","5-9","10-14","15-19","20-24","25-29",
    "30-34","35-39","40-44","45-49",
    "50-54","55-59","60-64","65-69",
    "70-74","75-79","80-84","85-89")

  for(i in 1:length(bands)){
    bnd = bands[i]
    b1 = unlist(strsplit(bnd,"-"))
    b2 = as.numeric(b1[2])
    b1 = as.numeric(b1[1])
    pop22[bnd] = rowSums(pop22[paste0("A",b1:b2)], na.rm = TRUE)
  }
  pop22["90+"] = pop22$A90

  pop22 = pop22[,c("LSOA 2021 Code","Total",bands,"90+")]
  names(pop22)[1:2] = c("LSOA21CD","all_ages")

  pop22

}

# --- from R/population_scotland.R ------------------------------
#' Combine GB population estimates onto 2021 zones (superseded)
#'
#' @description Older approach to building the single GB population series,
#'   superseded by `combine_populations2()` and no longer called by any
#'   target. Converts the E&W 2002-2020 estimates from 2011 to 2021 LSOAs
#'   using the ONS change indicator: unchanged zones (U) pass through, merged
#'   zones (M) are summed, split zones (S) are apportioned by the 2021
#'   population ratio, and the handful of fragmented zones (X) are matched
#'   one-to-one after removing known ambiguous pairs. Appends the 2021 census,
#'   2022 estimates and Scottish series (still on 2011 Data Zones).
#' @param population_2002_2020 E&W estimates on 2011 LSOAs.
#' @param population_2021 2021 census population (`load_population_2021()`).
#' @param population_2022 Mid-2022 estimates on 2021 LSOAs.
#' @param population_scot Scottish estimates on 2011 Data Zones.
#' @param lookup_lsoa_2011_21 ONS 2011-to-2021 LSOA lookup with `CHGIND`.
#' @return A data frame with `year`, `LSOA21CD`, `all_ages` and age bands
#'   "0-4" ... "85+".
#' @keywords internal
combine_populations = function(population_2002_2020, population_2021, population_2022, population_scot, lookup_lsoa_2011_21) {

  #TODO: Get Scotland 2022 population
  population_2002_2020$`85+` = population_2002_2020$`85-89` + population_2002_2020$`90+`
  population_2002_2020$`85-89` = NULL
  population_2002_2020$`90+` = NULL

  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA11CD","LSOA21CD","CHGIND")]

  lookup_lsoa_2011_21_U = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "U",]
  lookup_lsoa_2011_21_M = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "M",]
  lookup_lsoa_2011_21_S = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "S",]
  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "X",]

  population_2002_2020_U = population_2002_2020[population_2002_2020$LSOA11CD %in% lookup_lsoa_2011_21_U$LSOA11CD,]
  population_2002_2020_M = population_2002_2020[population_2002_2020$LSOA11CD %in% lookup_lsoa_2011_21_M$LSOA11CD,]
  population_2002_2020_S = population_2002_2020[population_2002_2020$LSOA11CD %in% lookup_lsoa_2011_21_S$LSOA11CD,]
  population_2002_2020_X = population_2002_2020[population_2002_2020$LSOA11CD %in% lookup_lsoa_2011_21_X$LSOA11CD,]

  # Merge
  population_2002_2020_M = dplyr::left_join(population_2002_2020_M, lookup_lsoa_2011_21_M, by = "LSOA11CD")
  population_2002_2020_M = dplyr::group_by(population_2002_2020_M, year, LSOA21CD)
  population_2002_2020_M = dplyr::summarise(population_2002_2020_M,
                                            all_ages = sum(all_ages),
                                            `0-4` = sum(`0-4`),
                                            `5-9` = sum(`5-9`),
                                            `10-14` = sum(`10-14`),
                                            `15-19` = sum(`15-19`),
                                            `20-24` = sum(`20-24`),
                                            `25-29` = sum(`25-29`),
                                            `30-34` = sum(`30-34`),
                                            `35-39` = sum(`35-39`),
                                            `40-44` = sum(`40-44`),
                                            `45-49` = sum(`45-49`),
                                            `50-54` = sum(`50-54`),
                                            `55-59` = sum(`55-59`),
                                            `60-64` = sum(`60-64`),
                                            `65-69` = sum(`65-69`),
                                            `70-74` = sum(`70-74`),
                                            `75-79` = sum(`75-79`),
                                            `80-84` = sum(`80-84`),
                                            `85+` = sum(`85+`))
  population_2002_2020_M = dplyr::ungroup(population_2002_2020_M)

  # Split
  split_pop = population_2021[,c("LSOA21","all_ages")]
  names(split_pop) = c("LSOA21","pop2021")
  split_pop = dplyr::left_join(lookup_lsoa_2011_21_S, split_pop, by = c("LSOA21CD" = "LSOA21"))
  split_pop = dplyr::group_by(split_pop, LSOA11CD)
  split_pop = dplyr::mutate(split_pop, pop_ratio = pop2021 / sum(pop2021))
  split_pop = dplyr::ungroup(split_pop, LSOA21CD)

  population_2002_2020_S = dplyr::left_join(split_pop, population_2002_2020_S,
                                            by = "LSOA11CD", relationship = "many-to-many")
  population_2002_2020_S = as.data.frame(population_2002_2020_S)

  for(i in 7:25){
    population_2002_2020_S[i] = round(population_2002_2020_S[,i ,drop = TRUE] * population_2002_2020_S$pop_ratio)
  }

  # Other
  # b11 = bounds_lsoa11_full[bounds_lsoa11_full$LSOA11CD %in% lookup_lsoa_2011_21_X$LSOA11CD,]
  # b21 = bounds_lsoa21_full[bounds_lsoa21_full$LSOA21CD %in% lookup_lsoa_2011_21_X$LSOA21CD,]
  #
  # tm_shape(b11) +
  #   tm_fill("blue", alpha = 0.1) +
  #   tm_borders() +
  # tm_shape(b21) +
  #   tm_fill("red", alpha = 0.1) +
  #   tm_borders()
  # Other changes are very subtle so go for a 1 to 1 match

  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01027506" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035624"),]
  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01008187" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035637"),]

  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01023964" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035581"),]
  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01023679" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035608"),]

  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01023508" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035582"),]
  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01023768" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035609"),]

  population_2002_2020_X = dplyr::left_join(lookup_lsoa_2011_21_X, population_2002_2020_X, by = "LSOA11CD")

  population_2002_2020_U$LSOA21CD = population_2002_2020_U$LSOA11CD

  nms = c("year","LSOA21CD","all_ages","0-4","5-9","10-14","15-19","20-24",
          "25-29","30-34","35-39","40-44","45-49",
          "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")

  population_2002_2020_U = population_2002_2020_U[,nms]
  population_2002_2020_M = population_2002_2020_M[,nms]
  population_2002_2020_S = population_2002_2020_S[,nms]
  population_2002_2020_X = population_2002_2020_X[,nms]

  population_2022$year = 2022
  population_2022$`85+` = population_2022$`85-89` + population_2022$`90+`
  population_2022 = population_2022[,nms]

  names(population_2021)[names(population_2021) == "LSOA21"] = "LSOA21CD"

  population_2020_2021 = rbind( population_2002_2020_U,
                                population_2002_2020_M,
                                population_2002_2020_S,
                                population_2002_2020_X,
                                population_2021, population_2022)

  names(population_scot)[names(population_scot) == "LSOA11CD"] = "LSOA21CD"
  population_scot = population_scot[population_scot$year > 2001,]
  population_scot$`85+` = population_scot$`85-89` + population_scot$`90+`
  population_scot  = population_scot[,nms]
  population_2020_2021 = rbind(population_2020_2021, population_scot)
  population_2020_2021

}

# --- from R/class_ethnic.R ------------------------------
#' Read 2021 census approximated social grade by ethnicity for MSOAs
#'
#' @description Parses the formatted census extract of approximated social
#'   grade (AB/C1/C2/DE) by 5-group ethnicity, where each MSOA appears as a
#'   block headed by "Area Name :", into one wide row per MSOA. Not currently
#'   called by any target.
#' @param path Path to the `census2021EW_HouseholdComposition.zip` archive
#'   containing `census2021EW_class_ethnic.csv`.
#' @return A wide data frame with `msoa21cd` and columns `AB_Asian`,
#'   `C1_White`, etc.
#' @keywords internal
read_class_ethinic = function(path = "../inputdata/population/census2021EW_HouseholdComposition.zip"){

  dir = file.path(tempdir(),"class_ethnic")
  dir.create(dir)
  unzip(path, exdir = dir)
  raw = read.csv(file.path(dir,"census2021EW_class_ethnic.csv"))
  unlink(dir, recursive = TRUE)

  names(raw) = c("x","Total","AB","C1","C2","DE")
  raw_numb = raw[raw$x %in% c("Asian, Asian British or Asian Welsh",
                              "Black, Black British, Black Welsh, Caribbean or African",
                              "Mixed or Multiple ethnic groups",
                              "White",
                              "Other ethnic group"),]
  raw_msoa = raw$Total[raw$x == "Area Name  :"]
  raw_numb$x[raw_numb$x == "Asian, Asian British or Asian Welsh"] = "Asian"
  raw_numb$x[raw_numb$x == "Black, Black British, Black Welsh, Caribbean or African"] = "Black"
  raw_numb$x[raw_numb$x == "Mixed or Multiple ethnic groups"] = "Mixed"
  raw_numb$x[raw_numb$x == "Other ethnic group"] = "Other"
  raw_numb$Total = NULL

  raw_msoa = strsplit(raw_msoa," : ")
  raw_msoa = sapply(raw_msoa, `[[`, 1)
  raw_numb$msoa21cd = rep(raw_msoa, each = 5)
  raw_numb[2:5] = lapply(raw_numb[2:5], as.numeric)


  wide = tidyr::pivot_wider(raw_numb, names_from = "x", values_from = c("AB","C1","C2","DE"))

  wide

}

# --- from R/class_ethnic.R ------------------------------
#' Read households by NS-SEC and composition (old 8-category version)
#'
#' @description Older version of `read_household_nssec()` based on the
#'   8-category household composition table. Where the full NS-SEC x
#'   composition cross-tab is unavailable it estimates the missing LSOAs by
#'   Furness-balancing the two marginal tables. Superseded and not called by
#'   any target.
#' @param path Path to the `census2021EW_HouseholdComposition.zip` archive.
#' @return A wide data frame per LSOA of household counts by NS-SEC and
#'   composition.
#' @keywords internal
read_household_nssec_old = function(path = "../inputdata/population/census2021EW_HouseholdComposition.zip"){

  dir = file.path(tempdir(),"class_ethnic")
  dir.create(dir)
  unzip(path, exdir = dir)
  raw6 = read.csv(file.path(dir,"census2021EW_HouseholdComposition_NSSEC6.csv"))
  raw8 = read.csv(file.path(dir,"census2021EW_HouseholdComposition_NSSEC8_partial.csv"))
  raw_hc = read.csv(file.path(dir,"census2021EW_HouseholdComposition.csv"))
  raw_nssec = read.csv(file.path(dir,"census2021EW_NSSEC.csv"))
  unlink(dir, recursive = TRUE)

  names(raw6) = c("LSOA21CD","LSOA21NM","NSSEC10CD","NSSEC10","household6CD","household6","count")
  names(raw8) = c("LSOA21CD","LSOA21NM","NSSEC10CD","NSSEC10","household8CD","household8","count")
  names(raw_hc) = c("LSOA21CD","LSOA21NM","household8CD","household8","count")
  names(raw_nssec) = c("LSOA21CD","LSOA21NM","NSSEC10CD","NSSEC10","count")

  raw6 = raw6[,c("LSOA21CD","NSSEC10","household6","count")]
  raw8 = raw8[,c("LSOA21CD","NSSEC10","household8","count")]
  raw_hc = raw_hc[,c("LSOA21CD","household8","count")]
  raw_nssec = raw_nssec[,c("LSOA21CD","NSSEC10","count")]

  raw_hc = raw_hc[!raw_hc$LSOA21CD %in% raw8$LSOA21CD,]
  raw_nssec = raw_nssec[!raw_nssec$LSOA21CD %in% raw8$LSOA21CD,]

  raw6$NSSEC10 = simplify_nssec(raw6$NSSEC10)
  raw8$NSSEC10 = simplify_nssec(raw8$NSSEC10)
  raw_nssec$NSSEC10 = simplify_nssec(raw_nssec$NSSEC10)

  raw6$household6 = simplify_household6(raw6$household6)
  raw8$household8 = simplify_household8(raw8$household8)
  raw_hc$household8 = simplify_household8(raw_hc$household8)

  raw_hc = raw_hc[order(raw_hc$LSOA21CD),]
  raw_nssec = raw_nssec[order(raw_nssec$LSOA21CD),]

  # Drop DNA and Lone Parent as same in 6 and 8
  raw_hc = raw_hc[!raw_hc$household8 %in% c("LoneParent","DNA"),]

  lst_hc = dplyr::group_split(raw_hc, raw_hc$LSOA21CD, .keep = FALSE)
  lst_nssec = dplyr::group_split(raw_nssec, raw_nssec$LSOA21CD, .keep = FALSE)

  lst_bal = purrr::map2(lst_hc, lst_nssec, balance_nssec, .progress = TRUE)
  lst_bal = dplyr::bind_rows(lst_bal)

  wide6 = tidyr::pivot_wider(raw6, names_from = "household6", values_from = "count")
  wide8 = tidyr::pivot_wider(raw8, names_from = "household8", values_from = "count")
  wide8$DNA = NULL
  wide8$LoneParent = NULL
  lst_bal = lst_bal[names(wide8)]
  wide8 = rbind(wide8, lst_bal)


  wide = dplyr::left_join(wide6[,c("LSOA21CD", "NSSEC10","LoneParent","DNA")],
                          wide8, by = c("LSOA21CD", "NSSEC10"))


  wide

}

# --- from R/class_ethnic.R ------------------------------
#' Furness-balance NS-SEC x household composition for one LSOA (old version)
#'
#' @description Estimates the 10 x 6 NS-SEC by household-composition cross-tab
#'   for one LSOA from its two marginal totals using
#'   `furness_partial()`. Used only by the superseded
#'   `read_household_nssec_old()`.
#' @param x Household-composition marginal counts for one LSOA.
#' @param y NS-SEC marginal counts for the same LSOA.
#' @return A data frame of the balanced matrix with `NSSEC10` and `LSOA21CD`.
#' @keywords internal
balance_nssec = function(x, y){
  # Check
  if(x$LSOA21CD[1] != y$LSOA21CD[1]){
    stop("LSOAs of X and Y don't match")
  }
  mat = matrix(NA, nrow = 10, ncol = 6)
  rownames(mat) = y$NSSEC10
  colnames(mat) = x$household8


  mat = furness_partial(mat, rsum = y$count, csum = x$count, n = 200, check = FALSE, int_only = FALSE)
  mat = as.data.frame(mat)
  mat$NSSEC10 = rownames(mat)
  rownames(mat) = NULL
  mat$LSOA21CD = x$LSOA21CD[1]
  mat

}

# --- from R/house_prices_uprn.R ------------------------------
#' Attach UPRNs and coordinates to price paid transactions (old version)
#'
#' @description Joins the UBDC transaction-to-UPRN lookup onto the Land
#'   Registry data, fills unmatched transactions by matching their full
#'   address against transactions that did get a UPRN, then joins UPRN
#'   coordinates. Superseded by `land_registry_add_uprn()` in
#'   house_prices_address.R; not called by any target.
#' @param house_price_lr Land Registry transactions (`house_price_lr`).
#' @param house_prices_ubdc UBDC linkage table (`house_prices_ubdc`).
#' @param uprn sf UPRN points.
#' @return An sf data frame of transactions with UPRN geometry.
#' @keywords internal
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

# --- from R/anoymised_mot_data.R ------------------------------
#' Convert car-km estimates from 2011 to 2021 LSOAs (superseded)
#'
#' @description Old boundary-conversion helper for the wide-format car/van km
#'   table; the corresponding target is commented out in `_targets.R` and the
#'   function appears broken as written (it renames `van_km_*`/`car_km_*`
#'   columns to `vankm_*`/`carkm_*` for the split case, then selects the old
#'   names, which would error). Kept for reference only.
#' @param car_km_lsoa_11 Wide car/van km per 2011 LSOA.
#' @param lsoa_11_21_tools Conversion lookups (`lsoa_11_21_tools` target).
#' @return A data frame per 2021 LSOA (if it ran).
#' @keywords internal
car_km_11_to_21 = function(car_km_lsoa_11, lsoa_11_21_tools){

  names(car_km_lsoa_11)[1] = "LSOA11CD"

  car_km_S = car_km_lsoa_11[car_km_lsoa_11$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  car_km_M = car_km_lsoa_11[car_km_lsoa_11$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  car_km_U = car_km_lsoa_11[car_km_lsoa_11$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  car_km_U = dplyr::left_join(car_km_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # Merge
  car_km_M = dplyr::left_join(car_km_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  car_km_M = dplyr::select(car_km_M, -LSOA11CD)
  car_km_M = dplyr::group_by(car_km_M, LSOA21CD)
  car_km_M = dplyr::summarise_all(car_km_M, sum, na.rm = TRUE)
  car_km_M = dplyr::ungroup(car_km_M)

  #Split
  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA11CD","LSOA21CD","year","household_ratio")]
  lookup_split = lookup_split[lookup_split$year %in% 2009:2023,]

  names(car_km_S) = gsub("vans_total","vanstotal",names(car_km_S))
  names(car_km_S) = gsub("van_km","vankm",names(car_km_S))
  names(car_km_S) = gsub("car_km","carkm",names(car_km_S))

  car_km_S = dplyr::left_join(lsoa_11_21_tools$lookup_split, car_km_S,
                                    by = "LSOA11CD", relationship = "many-to-many")
  car_km_S = as.data.frame(car_km_S)
  for(i in 5:6){
    car_km_S[i] = car_km_S[,i ,drop = TRUE] * car_km_S$pop_ratio
  }

  nms = c("LSOA21CD",paste0("van_km_",10:23),paste0("car_km_",10:23))

  car_km_S = car_km_S[,nms]
  car_km_M = car_km_M[,nms]
  car_km_U = car_km_U[,nms]

  final = rbind(car_km_S, car_km_M, car_km_U)
  final

}

# --- from R/vehicle_reg_to_21.R ------------------------------
#' Convert vehicle registration tables to 2021/2022 boundaries (superseded)
#'
#' @description Boundary conversion for the DfT registration tables from the
#'   era when they were published on 2011 zones: unchanged/merged/split E&W
#'   LSOAs handled via `lsoa_11_21_tools`, Scottish Data Zones re-apportioned
#'   by UPRN split share. `mode` selects the expected column set
#'   ("vehicle_registrations", "ulev_registrations" or "ev_registrations").
#'   The corresponding targets are commented out in `_targets.R` because DfT
#'   now publish directly on 2021 LSOAs.
#' @param vehicle_registrations Wide registrations table on 2011 zones.
#' @param lsoa_11_21_tools Conversion lookups (`lsoa_11_21_tools` target).
#' @param lookup_dz_2011_22 Data Zone split shares (`lookup_dz_2011_22`).
#' @param mode Which table layout to expect.
#' @return A wide data frame on 2021/2022 zones with `LSOA21CD` and `year`.
#' @keywords internal
vehicle_reg_to_21 = function(vehicle_registrations,lsoa_11_21_tools, lookup_dz_2011_22, mode = "vehicle_registrations"){

  if(mode == "vehicle_registrations"){
    nms = c("Cars_Company_Licensed","Cars_Private_Licensed",
            "Other body types_Company_Licensed","Other body types_Private_Licensed",
            "Motorcycles_Company_Licensed","Motorcycles_Private_Licensed",
            "Cars_Company_SORN","Cars_Private_SORN","Other body types_Company_SORN",
            "Other body types_Private_SORN","Motorcycles_Company_SORN",
            "Motorcycles_Private_SORN","Cars_Disposal_Licensed",
            "Motorcycles_Disposal_Licensed","Other body types_Disposal_Licensed",
            "Cars_Disposal_SORN","Motorcycles_Disposal_SORN",
            "Other body types_Disposal_SORN")
  } else if (mode == "ulev_registrations") {
    nms = c("Plug-in hybrid electric (petrol)_Company",
            "Battery electric_Private","Plug-in hybrid electric (petrol)_Private","Battery electric_Company",
            "Range extended electric_Private","Range extended electric_Company","Hybrid electric (petrol)_Private",
            "Hybrid electric (petrol)_Company","Plug-in hybrid electric (diesel)_Company","Plug-in hybrid electric (diesel)_Private",
            "Diesel_Private",
            "Diesel_Company","Hybrid electric (diesel)_Company","Hybrid electric (diesel)_Private",
            "Fuel cell electric_Company","Fuel cell electric_Private",
            "Petrol_Company","Petrol_Private",
            "Battery electric_Disposal","Fuel cell electric_Disposal","Hybrid electric (petrol)_Disposal",
            "Plug-in hybrid electric (diesel)_Disposal","Plug-in hybrid electric (petrol)_Disposal","Range extended electric_Disposal",
            "Diesel_Disposal","Hybrid electric (diesel)_Disposal")
  } else if (mode == "ev_registrations") {
    nms = c("Battery electric_Company",
            "Plug-in hybrid electric (petrol)_Company","Battery electric_Private","Plug-in hybrid electric (petrol)_Private",
            "Range extended electric_Private","Range extended electric_Company","Plug-in hybrid electric (diesel)_Company",
            "Plug-in hybrid electric (diesel)_Private","Battery electric_Disposal","Plug-in hybrid electric (diesel)_Disposal",
            "Plug-in hybrid electric (petrol)_Disposal","Range extended electric_Disposal")
  } else {
    stop("Unknown mode")
  }

  # Can only do up to year have population data
  vehicle_registrations$year = as.integer(gsub(" Q1","",vehicle_registrations$quarter))
  vehicle_registrations$quarter = NULL

  vehicle_registrations = vehicle_registrations[vehicle_registrations$year %in% unique(lsoa_11_21_tools$lookup_split$year), ]


  # Scotlamd
  vehicle_registrations_Scot = vehicle_registrations[vehicle_registrations$LSOA11CD %in% lookup_dz_2011_22$LSOA11CD,]

  vehicle_registrations_Scot = dplyr::left_join(vehicle_registrations_Scot,
                                       lookup_dz_2011_22,
                                       by = c("LSOA11CD"),
                                       relationship = "many-to-many")




  vehicle_registrations_Scot <- vehicle_registrations_Scot |>
    dplyr::group_by(year, LSOA21CD) |>
    dplyr::summarise(
      dplyr::across(
        .cols = where(is.numeric) & !matches("splitshare"),
        .fns = ~ round(sum(.x * splitshare, na.rm = TRUE)),
        .names = "{.col}"
      )
    ) |>
    dplyr::ungroup()

  # England and Wales

  # vehicle_registrations 2010 - 2023 (2011 bounds)
  # car_emissions_perkm 2001 - 2018 (converted to 2021 bounds)
  # population 2002 - 2022 (converted to 2021 bounds)

  # Step 1: Convert vehicle_registrations  to 2021

  vehicle_registrations_S = vehicle_registrations[vehicle_registrations$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  vehicle_registrations_M = vehicle_registrations[vehicle_registrations$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  vehicle_registrations_U = vehicle_registrations[vehicle_registrations$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  vehicle_registrations_U = dplyr::left_join(vehicle_registrations_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # Merge
  vehicle_registrations_M = dplyr::left_join(vehicle_registrations_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  vehicle_registrations_M = dplyr::select(vehicle_registrations_M, -LSOA11CD)
  vehicle_registrations_M = dplyr::group_by(vehicle_registrations_M, year, LSOA21CD)
  vehicle_registrations_M = dplyr::summarise_all(vehicle_registrations_M, sum, na.rm = TRUE)
  vehicle_registrations_M = dplyr::ungroup(vehicle_registrations_M)


  #Split
  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA11CD","LSOA21CD","year","household_ratio")]
  lookup_split = lookup_split[lookup_split$year %in% unique(vehicle_registrations_S$year),]
  vehicle_registrations_S = dplyr::left_join(lookup_split, vehicle_registrations_S,
                                    by = c("LSOA11CD", "year"),
                                    relationship = "many-to-many")
  vehicle_registrations_S = as.data.frame(vehicle_registrations_S)



  for(i in nms){
    vehicle_registrations_S[i] = vehicle_registrations_S[i] * vehicle_registrations_S$household_ratio
  }

  nms = c("LSOA21CD","year",nms)

  vehicle_registrations_S = vehicle_registrations_S[,nms]
  vehicle_registrations_M = vehicle_registrations_M[,nms]
  vehicle_registrations_U = vehicle_registrations_U[,nms]
  vehicle_registrations_Scot = vehicle_registrations_Scot[,nms]

  vehicle_registrations = rbind(vehicle_registrations_S, vehicle_registrations_M, vehicle_registrations_U, vehicle_registrations_Scot)



  vehicle_registrations

}

# --- from R/vehicle_reg_to_21.R ------------------------------
#' Convert the RAC/MOT 2009-2011 car-km table to 2021 LSOAs (unused)
#'
#' @description Boundary conversion for the wide `read_motoring_along()`
#'   output: unchanged/merged handled via lookups, split zones pivoted long,
#'   apportioned by household ratio and pivoted back. Scottish rows are
#'   passed through on their 2011 codes (see TODO). Currently commented out
#'   of the `car_km_2009_2011` target, which passes the 2011-boundary data
#'   straight through.
#' @param car_km_2009_2011 Output of `read_motoring_along()`.
#' @param lsoa_11_21_tools Conversion lookups (`lsoa_11_21_tools` target).
#' @return A wide data frame per 2021 LSOA of car/van counts and km.
#' @keywords internal
car_km_2009_2011_to_2021 = function(car_km_2009_2011, lsoa_11_21_tools){

  ckm_S = car_km_2009_2011[car_km_2009_2011$LSOA11 %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  ckm_M = car_km_2009_2011[car_km_2009_2011$LSOA11 %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  ckm_U = car_km_2009_2011[car_km_2009_2011$LSOA11 %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #TODO: Fix for old Scotland 2011 Bounds
  ckm_scot = car_km_2009_2011[substr(car_km_2009_2011$LSOA11,1,1) == "S",]
  ckm_scot$LSOA21CD = ckm_scot$LSOA11

  #Unchanged
  ckm_U = dplyr::left_join(ckm_U, lsoa_11_21_tools$lookup_unchanged, by = c("LSOA11" = "LSOA11CD"))
  ckm_U = rbind(ckm_U, ckm_scot)

  # Merge
  ckm_M = dplyr::left_join(ckm_M, lsoa_11_21_tools$lookup_merge, by = c("LSOA11" = "LSOA11CD"))
  ckm_M = dplyr::select(ckm_M, -LSOA11)
  ckm_M = dplyr::group_by(ckm_M, LSOA21CD)
  ckm_M = dplyr::summarise_all(ckm_M, sum, na.rm = TRUE)
  ckm_M = dplyr::ungroup(ckm_M)

  #Split
  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA11CD","LSOA21CD","year","household_ratio")]
  lookup_split = lookup_split[lookup_split$year %in% 2009:2011,]

  names(ckm_S) = gsub("vans_total","vanstotal",names(ckm_S))
  names(ckm_S) = gsub("cars_total","carstotal",names(ckm_S))
  names(ckm_S) = gsub("cars_total","carstotal",names(ckm_S))
  names(ckm_S) = gsub("car_km","carkm",names(ckm_S))
  names(ckm_S) = gsub("van_km","vankm",names(ckm_S))

  ckm_S = tidyr::pivot_longer(ckm_S,
                            cols = c("carstotal_09","carstotal_10","carstotal_11",
                                     "vanstotal_09","vanstotal_10","vanstotal_11",
                                     "carkm_09","carkm_10","carkm_11","vankm_09",
                                     "vankm_10","vankm_11"),
                            names_to = c(".value","year"),
                            names_sep = "_"
                            )
  ckm_S$year = as.integer(paste0("20",ckm_S$year))

  ckm_S = dplyr::left_join(lookup_split,
                           ckm_S,
                             by = c("LSOA11CD" = "LSOA11", "year"),
                             relationship = "many-to-many")
  ckm_S = as.data.frame(ckm_S)

  nms = c("carstotal","vanstotal","carkm","vankm")

  for(i in nms){
    ckm_S[i] = ckm_S[i] * ckm_S$household_ratio
  }

  ckm_S$year = substr(as.character(ckm_S$year),3,4)
  ckm_S = ckm_S[,c("LSOA21CD","year","carstotal","vanstotal","carkm","vankm")]

  ckm_S = tidyr::pivot_wider(ckm_S, id_cols = "LSOA21CD", names_sep = "_",
                             values_from = c("carstotal","vanstotal","carkm","vankm"),
                             names_from = "year")

  names(ckm_S) = gsub("vanstotal","vans_total",names(ckm_S))
  names(ckm_S) = gsub("carstotal","cars_total",names(ckm_S))
  names(ckm_S) = gsub("carstotal","cars_total",names(ckm_S))
  names(ckm_S) = gsub("carkm","car_km",names(ckm_S))
  names(ckm_S) = gsub("vankm","van_km",names(ckm_S))

  ckm_M = ckm_M[,names(ckm_S)]
  ckm_U = ckm_U[,names(ckm_S)]

  ckm = rbind(ckm_S, ckm_M, ckm_U)


  ckm


}

# --- from R/synth_pop_LCFS.R ------------------------------
#' Find the LCFS households most similar to one census household type
#'
#' @description Scores every LCFS household against a census household type
#'   by summing similarity-matrix scores across tenure, composition, size,
#'   car ownership and OAC (zones with several OACs take the best), keeps
#'   the top scorers, then filters/adjusts by the zone's income limits.
#'   This income-aware variant is retained for reference; the current
#'   pipeline path uses `match_hh_census2()`/`match_hh_census3()`.
#' @param Tenure5,hhComp15,hhSize5,CarVan5 Census household attributes.
#' @param OACs Space-separated OAC codes of the zone.
#' @param upper_limit,lower_limit Zone income confidence limits.
#' @param hh LCFS household table.
#' @param similarity_matrices List of similarity matrices per attribute.
#' @return A one-row data frame with the match score, number of matches and
#'   a list-column of candidate `household_id`s.
#' @keywords internal
match_hh_census <- function(Tenure5,hhComp15,hhSize5,CarVan5,OACs, upper_limit, lower_limit, hh, similarity_matrices) {


  # Create named vectors for the input variables to match the dimension names in the similarity matrices
  input_vars <- list(
    Tenure5 = as.character(Tenure5),
    hhComp15 = as.character(hhComp15),
    hhSize5 = hhSize5,
    CarVan5 = CarVan5,
    OAC = unlist(strsplit(OACs," "))
  )

  # Initialize similarity scores as a numeric vector
  similarity_scores <- numeric(nrow(hh))

  # Calculate similarity scores using vectorized operations
  for (var in names(input_vars)) {
    sim_matrix <- similarity_matrices[[var]]
    input_value <- input_vars[[var]]
    hh_values <- hh[[var]]

    if(var == "OAC"){
      #Special case LSOAs can have multiple OACs,
      #input_value$subgroup = as.character(input_value$subgroup)
      #sim_matrix[,!colnames(sim_matrix) %in% input_value$subgroup] = 0
      #sim_matrix[,!colnames(sim_matrix) %in% input_value] = 0

      # Map the input value and household values to their corresponding indices
      input_index <- which(rownames(sim_matrix) %in% input_value)
      hh_indices <- match(hh_values, colnames(sim_matrix))

      # Extract the similarity scores for all households at once
      scores <- sim_matrix[input_index, hh_indices]
      if(inherits(scores,"matrix")){
        scores <- apply(scores, 2, max, na.rm = TRUE)
      }


    } else {
      # Map the input value and household values to their corresponding indices
      input_index <- which(rownames(sim_matrix) == input_value)
      hh_indices <- match(hh_values, colnames(sim_matrix))

      # Extract the similarity scores for all households at once
      scores <- sim_matrix[input_index, hh_indices]
    }

    similarity_scores <- similarity_scores + scores


  }

  # Find the maximum similarity score
  max_score <- max(similarity_scores, na.rm = TRUE)

  # Get all households with the maximum similarity score
  hh_sub <- hh[similarity_scores == max_score, ]

  # Multiple Options so check income
  if(any(hh_sub$annual_income  >= lower_limit)){
    hh_sub <- hh_sub[hh_sub$annual_income  >= lower_limit,]
    max_score = max_score + 1
  } else {
    hh_sub <- hh_sub[hh_sub$annual_income  == max(hh_sub$annual_income),]
    max_score = max_score + min(max(hh_sub$annual_income)/lower_limit,1)
  }

  if(any(hh_sub$annual_income  <= upper_limit)){
    hh_sub <- hh_sub[hh_sub$annual_income  <= upper_limit,]
    max_score = max_score + 1
  } else {
    hh_sub <- hh_sub[hh_sub$annual_income  == min(hh_sub$annual_income),]
    max_score = max_score + min(min(hh_sub$annual_income)/lower_limit,1)
  }


  if (nrow(hh_sub) > 0) {
    return(data.frame(
      Tenure5 = Tenure5,
      hhComp15 = hhComp15,
      hhSize5 = hhSize5,
      CarVan5 = CarVan5,
      OACs = OACs,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      n_match = nrow(hh_sub),
      match_score = max_score / 7,
      household_id = I(list(hh_sub$household_id))
    ))
  } else {
    message(unlist(input_vars))
    stop()
  }
}

# --- from R/syth_pop_historical.R ------------------------------
#' Adjust the synthetic population to historical years (empty stub)
#'
#' @description Placeholder for adjusting the census-2021 synthetic
#'   households to match historical population data. Currently empty and
#'   not called by any target (historical adjustment is instead done inside
#'   `match_LCFS_synth_pop()` via `select_synth_pop_year()`).
#' @param census21_synth_households Synthetic households (unused).
#' @param population GB population table (unused).
#' @return NULL.
#' @keywords internal
syth_pop_hisotrical = function(census21_synth_households, population){

}

# --- from R/OAC_summary.R ------------------------------
#' Summarise emissions by OAC21 group (unfinished stub)
#'
#' @description Incomplete placeholder for an emissions-by-OAC21 summary;
#'   currently just subsets the OAC table and returns it invisibly. Not
#'   called by any target (see `make_oac_summary()` in la_summaries.R for
#'   the version in use).
#' @param lsoa_emissions_all Per-LSOA emissions (unused).
#' @param oac21 OAC21 table.
#' @return The subset OAC table, invisibly.
#' @keywords internal
summarise_oac = function(lsoa_emissions_all, oac21){
  #TODO: Scotland

  oac21 = oac21[,c("oa21cd","la23cd","supergroup","group","subgroup")]

}

# --- from R/bulk_outputs.R ------------------------------
#' Bulk export cleaned INSPIRE polygons (not yet implemented)
#'
#' @description Empty stub for a planned bulk export of the cleaned INSPIRE
#'   land-registry polygons. Not called by any target.
#' @return NULL.
#' @keywords internal
bulk_export_inspire = function(){

}

# --- from R/land_uses.R ------------------------------
#' Difference against a pre-unioned set of polygons
#'
#' @description Helper that unions overlapping members of `y` separately from
#'   non-overlapping ones before a single `st_difference()`, which is faster
#'   than differencing feature by feature. Not currently called.
#' @param x sf object to subtract from.
#' @param y sf polygons to subtract.
#' @return `x` with the union of `y` removed.
#' @keywords internal
fast_st_difference = function(x, y){
  inter = lengths(sf::st_intersects(y))
  y_inter = sf::st_union(y[inter > 1,])
  y_solo = sf::st_combine(y[inter == 1,])
  message(Sys.time()," Start difference")
  sf::st_difference(x,sf::st_union(y_inter,y_solo))
}

# --- from R/os_zoomstack.R ------------------------------
#' Build LSOA-level zoomstack building tile layers.
#'
#' @param buildings_heights An `sf` object with building heights and geometry.
#' @param dl_os_zoomstack Path to the downloaded OS Zoomstack zip archive.
#' @param bounds_lsoa_GB_full Full-resolution LSOA boundaries.
#' @param bounds_lsoa_GB_generalised Generalised LSOA boundaries.
#' @param bounds_lsoa_GB_super_generalised Super-generalised LSOA boundaries.
#' @return A named list of `sf` objects for high, medium, low, and verylow zoomstack layers.
#' @keywords internal
zoomstack_buildings_lsoa = function(buildings_heights, dl_os_zoomstack, bounds_lsoa_GB_full, bounds_lsoa_GB_generalised, bounds_lsoa_GB_super_generalised) {
  # TODO: Finish this function

  sf::sf_use_s2(FALSE)

  b_high = buildings_heights[,c("height_max","geometry")]
  rm(buildings_heights)

  dir.create(file.path(tempdir(),"zoomstack"))
  unzip(dl_os_zoomstack, exdir = file.path(tempdir(),"zoomstack"))

  #b_high = sf::st_read(file.path(tempdir(),"zoomstack","OS_Open_Zoomstack.gpkg"), layer = "local_buildings")
  b_med = sf::st_read(file.path(tempdir(),"zoomstack","OS_Open_Zoomstack.gpkg"), layer = "district_buildings")
  b_low = sf::st_read(file.path(tempdir(),"zoomstack","OS_Open_Zoomstack.gpkg"), layer = "urban_areas")
  b_verylow = b_low[b_low$type == "National",]
  b_low = b_low[b_low$type == "Regional",]

  # b_high = gsub("OS_Open_Zoomstack.zip","",dl_os_zoomstack)
  # b_high = sf::st_read(file.path(b_high,"building_heights_gb.gpkg"))
  # b_high = b_high[,c("height_max")]
  names(b_high)[names(b_high) == "height_max"] = "height"

  unlink(file.path(tempdir(),"zoomstack"), recursive = TRUE)

  #b_high$uuid = NULL
  b_low$type = NULL
  b_verylow$type = NULL

  #b_high = change_geom_name(b_high)
  b_med = change_geom_name(b_med)
  b_low = change_geom_name(b_low)
  b_verylow = change_geom_name(b_verylow)

  b_high$id <- 1:nrow(b_high)
  b_med$id <- 1:nrow(b_med)
  b_low$id <- 1:nrow(b_low)
  b_verylow$id <- 1:nrow(b_verylow)

  b_high <- sf::st_join(b_high, bounds_lsoa_GB_full)
  b_med <- sf::st_join(b_med, bounds_lsoa_GB_full)
  b_low <- sf::st_join(b_low, bounds_lsoa_GB_generalised)
  b_verylow <- sf::st_join(b_verylow, bounds_lsoa_GB_super_generalised)

  # Split Duplicates
  b_high = split_merge(b_high, bounds_lsoa_GB_full)
  b_med = split_merge(b_med, bounds_lsoa_GB_full)
  b_low = split_merge(b_low, bounds_lsoa_GB_generalised)
  b_verylow = split_merge(b_verylow, bounds_lsoa_GB_super_generalised)


  b_high = sf::st_transform(b_high, 4326)
  b_med = sf::st_transform(b_med, 4326)
  b_low = sf::st_transform(b_low, 4326)
  b_verylow = sf::st_transform(b_verylow, 4326)

  b_high = sf::st_make_valid(b_high)
  b_med = sf::st_make_valid(b_med)
  b_low = sf::st_make_valid(b_low)
  b_verylow = sf::st_make_valid(b_verylow)


  res = list(high = b_high,
             medium = b_med,
             low = b_low,
             verylow = b_verylow)


  res


}

# --- from R/os_zoomstack.R ------------------------------
#' Merge adjacent woodland polygons into a single outline (experimental)
#'
#' @description Attempts to dissolve clusters of adjacent woodland polygons
#'   into one boundary by buffering out/in, tracing the retained edge points
#'   and re-stitching them into a polygon. Experimental and fragile (some
#'   edge segments can be lost); not called by any target.
#' @param poly sf polygons of woodland patches (EPSG:27700).
#' @return A single sfc POLYGON of the merged outline.
#' @keywords internal
merge_woods = function(poly){

  # Buffer out and then in
  bout = sf::st_union(sf::st_buffer(poly, 10))
  bin = sf::st_buffer(bout, -30)

  poly$id = seq(1, nrow(poly))

  # Make points
  pts = suppressWarnings(sf::st_segmentize(sf::st_cast(poly, "LINESTRING"), dfMaxLength = 10))
  pts = suppressWarnings(sf::st_cast(pts, "POINT"))
  pts$pid = 1:nrow(pts)

  # Points on the edge
  pids = pts[bin, ]
  pts = pts[!pts$pid %in% pids$pid,]

  #qtm(poly, fill = "green") + qtm(pts)

  pts = dplyr::group_split(pts, id)
  pts = lapply(pts, function(x){
    x = sf::st_coordinates(x)[,1:2]
    x = sf::st_as_sf(sf::st_sfc(sf::st_linestring(x), crs = 27700))
    x
  })
  pts = dplyr::bind_rows(pts)
  pts = sf::st_difference(pts, bin)

  #Clip off the start and end of each line
  pts = sf::st_collection_extract(pts, "LINESTRING")
  pts = sf::st_cast(pts, "LINESTRING")
  pts = lapply(pts$x, function(x){
    x = sf::st_coordinates(x)[,1:2]
    if(nrow(x) < 4){
      return(NULL)
    }
    x = x[seq(2,nrow(x) - 1),]
    x = sf::st_as_sf(sf::st_sfc(sf::st_linestring(x), crs = 27700))
    x
  })
  pts = dplyr::bind_rows(pts)
  pts$id = seq(1,nrow(pts))

  pts_start = sf::st_as_sf(lwgeom::st_startpoint(pts))
  pts_end = sf::st_as_sf(lwgeom::st_endpoint(pts))
  pts_start$id = pts$id
  pts_end$id = pts$id

  # Pair up start and end
  nn = suppressMessages(nngeo::st_nn(pts_end, pts_start, k = 1, maxdist = 100, progress = FALSE))
  nn = unlist(nn)
  ordering = list()
  nnn = seq_along(nn)
  for(i in nnn){
    if(i == 1){
      ordering[[i]] = nnn[i]
    } else {
      ordering[[i]] = nn[nnn == ordering[[i-1]]]
    }
  }
  ordering = unlist(ordering)
  summary(nnn %in% ordering) # some lines lost

  pts$order = seq_along(ordering)[match(pts$id, ordering)]

  pts_na = pts[is.na(pts$order),]
  pts = pts[!is.na(pts$order),]
  pts = pts[order(pts$order),]

  pts = suppressWarnings(sf::st_cast(pts, "POINT"))
  pts = sf::st_coordinates(pts[,1:2])
  pts = rbind(pts, pts[1,])
  pts = sf::st_polygon(list(pts))
  pts = sf::st_sfc(pts, crs = 27700)

  pts

}

# --- from R/furness_balancing.R ------------------------------
#' Fast composition enumeration via stars-and-bars
#'
#' @description Faster alternative to `generate_combinations()` using
#'   `utils::combn()` bar placements to enumerate the compositions of `t`
#'   into `n` parts. Not currently called.
#' @param t Total to compose.
#' @param n Number of parts.
#' @return A list of integer vectors.
#' @keywords internal
compositions_fast <- function(t, n) {
  # number of bars to place
  k <- n - 1L
  m <- t + n - 1L

  # list of bar placements (in C, extremely fast)
  bars <- utils::combn(m, k)

  # convert bar positions -> composition via gap lengths
  gaps <- rbind(
    bars[1, ] - 1L,
    apply(bars, 2, diff) - 1L,
    m - bars[k, ]
  )

  # return as list of integer vectors
  lapply(seq_len(ncol(gaps)), function(i) gaps[, i])
}

# --- from R/furness_balancing.R ------------------------------
#' Split an integer total evenly across bins
#'
#' @description Divides `total` into `bins` integer parts, giving the first
#'   bins one extra unit each until the remainder is used up.
#' @param total Integer total to distribute.
#' @param bins Number of bins.
#' @return An integer vector of length `bins` summing to `total`.
#' @keywords internal
distribute <- function(total, bins) {
  # Calculate the base value for each bin
  base_value <- total %/% bins

  # Calculate the remainder
  remainder <- total %% bins

  # Create a vector with the base value repeated 'bins' times
  result <- rep(base_value, bins)

  # Distribute the remainder over the first 'remainder' bins
  if(remainder > 0){
    result[seq(1, remainder)] <- result[seq(1, remainder)] + 1
  }

  return(result)
}

# --- from R/consumption_footprint.R ------------------------------
#' Convert incomes to ventile bands (0-19)
#'
#' @description Assigns each income its 5%-band (ventile) index using the
#'   same duplicate-collapsing approach as `percentile()`.
#' @param dat Numeric income vector.
#' @return An integer vector of ventile indices from 0 to 19.
#' @keywords internal
income_bands <- function(dat){

  pt1 <- quantile(dat, probs = seq(0, 1, by = 0.05), type = 7, na.rm = TRUE)
  pt2 <- unique(as.data.frame(pt1), fromLast = TRUE)
  pt3 <- rownames(pt2)
  pt4 <- as.integer(strsplit(pt3, "%"))

  if(0 %in% pt2$pt1){
    cts <- c(-0.000001, pt2$pt1)
  } else {
    cts <- c(0, pt2$pt1)
  }
  datp <- pt4[as.integer(cut(dat, cts, labels = 1:length(pt3)))]
  datp <- datp/5 - 1
  datp[datp < 0]= 0

  datp

}

# --- from R/NTS.R ------------------------------
#' Derive an area-type zone label for NTS households (incomplete)
#'
#' @description Work-in-progress helper that selects household attributes and
#'   builds a region x rural-urban `zone` label. The function currently ends
#'   after the assignment and never returns the data frame, so it is not
#'   usable as-is; it is not called by any target.
#' @param nts List of NTS tables from `load_NTS()`.
#' @return Nothing useful yet (the final assignment's value, invisibly).
#' @keywords internal
build_nts_zones = function(nts){
  nts_household = nts$household
  #nts_ldj = nts$ldj
  nts_psu = nts$psu

  nts_household = nts_household[,c("HouseholdID", "PSUID","TWSYear",
                                   "Typeofpropertyfoundattheaddress",
                                   "HouseholdRegion","Countryofresidence",
                                   "IsLUlightrailmetrotramstopcloserthanrailwaystation",
                                   "ONSRuralUrbanClassificationofresidence2011Censussummary5categories",
                                   "2011CensusOutputAreaClassificationSupergroup8bands")]

  nts_household$zone = paste0(nts_household$HouseholdRegion," ",nts_household$ONSRuralUrbanClassificationofresidence2011Censussummary5categories)




}

# --- from R/admin_bounds.R ------------------------------
#' Load the Scotland 2022 OA/DataZone/IntermediateZone lookup
#'
#' @description Unzips and reads the lookup between 2022 Scottish Output
#'   Areas, Data Zones and Intermediate Zones. Not currently wired to a target
#'   (see `read_datazone_lookup_2022()` in datazone_lookup.R for the version
#'   used by `lookup_DataZone_2022`).
#' @param path Boundaries folder containing `oa22_dz22_iz22.zip`.
#' @return A data frame with the OA22/DZ22/IZ22 lookup.
#' @keywords internal
load_OA_DZ_IZ_2022_lookup <- function(path = "../inputdata/boundaries/"){
  dir.create(file.path(tempdir(),"lookup"))
  unzip(file.path(path,"oa22_dz22_iz22.zip"), exdir = file.path(tempdir(),"lookup"))
  lookup = readr::read_csv(file.path(tempdir(),"lookup","OA22_DZ22_IZ22.csv"))
  lookup
}

# --- from R/synthetic_pop_cenus3.R ------------------------------
#' Split a census table into a per-LSOA list (common zones only)
#'
#' @description Filters a census cross-tab to the zones in `lsoa_common`,
#'   sorts by zone and splits into a list of one data frame per LSOA, ready
#'   for parallel IPF. Superseded by `split_for_arrays3()`.
#' @param x Census cross-tab with `LSOA21CD`.
#' @param lsoa_common Zone codes to keep.
#' @return A list of data frames, one per zone.
#' @keywords internal
split_for_arrays = function(x, lsoa_common){
  x = x[x$LSOA21CD %in% lsoa_common,]
  x = x[order(x$LSOA21CD),]
  x = dplyr::group_split(x, LSOA21CD)
  x
}

# --- from R/synthetic_pop_cenus3.R ------------------------------
#' Split a census table into a per-LSOA list with NULL gaps (slow version)
#'
#' @description Builds a list aligned to `lsoa_all` with NULL for zones
#'   missing from `x`. Simple but O(n^2); superseded by
#'   `split_for_arrays3()`.
#' @param x Census cross-tab with `LSOA21CD`.
#' @param lsoa_all Full ordered vector of zone codes.
#' @return A list aligned to `lsoa_all` (NULL where absent).
#' @keywords internal
split_for_arrays2 = function(x, lsoa_all){
  y = lapply(lsoa_all, function(lsoa){
    sub = x[x$LSOA21CD == lsoa,]
    if(nrow(sub) == 0){
      return(NULL)
    }
    sub
    })
  y
}

# --- from R/synthetic_pop_scotland.R (superseded two-stage approach) -----
#' Older two-stage Scottish synthetic population (superseded)
#'
#' @description Earlier approach that fitted the cross-tab at Intermediate
#'   Zone level then downscaled to Data Zones
#'   (`downscale_to_datazone()`). Superseded by `sythetic_census_scot()`,
#'   which works directly at Data Zone level; not called by any target.
#' @param path_data Folder of Scotland Census 2022 extracts.
#' @param bounds_iz22 IZ boundaries providing IZCode/IZName.
#' @param synth_pop_seed_scotland National seed arrays.
#' @param lookup_DataZone_2022 DZ22-to-IZ22 lookup.
#' @return A data frame of household counts per Data Zone.
#' @keywords internal
sythetic_census_scot_old = function(path_data = file.path(parameters$path_data,"population_scotland"), bounds_iz22, synth_pop_seed_scotland, lookup_DataZone_2022){

  # Intermediate Zone Data

  int_hhSize_CarVan = read_hhSize_CarVan_scot(file.path(path_data,"scotlandcenus2022_hhSize5_CarVan5_IntermediateZone.csv"),bounds_iz22)
  int_hhSize_HouseholdComp = read_hhSize_HouseholdComp_scot(file.path(path_data,"scotlandcenus2022_householdComp10_hhSize5_IntermediateZone.csv"),bounds_iz22)
  int_hhSize_Tenure = read_hhSize_Tenure_scot(file.path(path_data,"scotlandcenus2022_Tenure5_hhSize5_IntermediateZone.csv"),bounds_iz22)
  int_hhSize_AccType = read_hhSize_AccType_scot(file.path(path_data,"scotlandcenus2022_AccType7_hhSize5_IntermediateZone.csv"),bounds_iz22)
  int_Tenure_HouseholdComp = read_Tenure_HouseholdComp_scot(file.path(path_data,"scotlandcenus2022_Tenure5_householdComp10_IntermediateZone.csv"),bounds_iz22)

  int_hhSize_CarVan$IZName = NULL
  int_hhSize_HouseholdComp$IZName = NULL
  int_hhSize_Tenure$IZName = NULL
  int_hhSize_AccType$IZName = NULL
  int_Tenure_HouseholdComp$IZName = NULL

  # Check all Zones
  # length(unique(int_hhSize_CarVan$IZName))
  # length(unique(int_hhSize_HouseholdComp$IZName))
  # length(unique(int_hhSize_Tenure$IZName))
  # length(unique(int_hhSize_AccType$IZName))
  # length(unique(int_Tenure_HouseholdComp$IZName))

  # Pivot
  int_hhSize_CarVan = tidyr::pivot_wider(int_hhSize_CarVan, names_from = "hhSize5", values_from = "households", values_fill = 0)
  int_hhSize_HouseholdComp = tidyr::pivot_wider(int_hhSize_HouseholdComp, names_from = "hhSize5", values_from = "households", values_fill = 0)
  int_hhSize_Tenure = tidyr::pivot_wider(int_hhSize_Tenure, names_from = "hhSize5", values_from = "households", values_fill = 0)
  int_hhSize_AccType = tidyr::pivot_wider(int_hhSize_AccType, names_from = "hhSize5", values_from = "households", values_fill = 0)
  int_Tenure_HouseholdComp = tidyr::pivot_wider(int_Tenure_HouseholdComp, names_from = "tenure5", values_from = "households", values_fill = 0)

  int_hhSize_CarVan    = int_hhSize_CarVan[order(int_hhSize_CarVan$IZCode),]
  int_hhSize_HouseholdComp    = int_hhSize_HouseholdComp[order(int_hhSize_HouseholdComp$IZCode),]
  int_hhSize_Tenure    = int_hhSize_Tenure[order(int_hhSize_Tenure$IZCode),]
  int_hhSize_AccType    = int_hhSize_AccType[order(int_hhSize_AccType$IZCode),]
  int_Tenure_HouseholdComp    = int_Tenure_HouseholdComp[order(int_Tenure_HouseholdComp$IZCode),]

  int_hhSize_CarVan = dplyr::group_split(dplyr::ungroup(int_hhSize_CarVan), IZCode)
  int_hhSize_HouseholdComp = dplyr::group_split(dplyr::ungroup(int_hhSize_HouseholdComp), IZCode)
  int_hhSize_Tenure = dplyr::group_split(dplyr::ungroup(int_hhSize_Tenure), IZCode)
  int_hhSize_AccType = dplyr::group_split(dplyr::ungroup(int_hhSize_AccType), IZCode)
  int_Tenure_HouseholdComp = dplyr::group_split(dplyr::ungroup(int_Tenure_HouseholdComp), IZCode)

  # Make the seed
  seed_df = expand.grid(c("OnePersonOver66","OnePersonOther","FamilyOver66",
                          "CoupleNoChildren",
                          "CoupleChildren","CoupleNonDepChildren","LoneParent",
                          "LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66"),
                        c("p1","p2","p3","p4","p5+"),
                        c("car0","car1","car2","car3","car4+"),
                        c("outright","mortgage","socialrented","privaterented","rentfree"),
                        c("detached","semidetached","terraced","flatpurposebuilt","flatconverted","flatcommercial","caravan"), stringsAsFactors = FALSE)
  names(seed_df) = c("householdComp10","hhSize5","CarVan5","Tenure5","AccTyp7")

  # Match Seed to Inputs
  synth_pop_seed_scotland = synth_pop_seed_scotland[,c("householdComp10","hhSize5","CarVan5","Tenure5","AccTyp7","seed")]

  # Create a key column by concatenating columns' values
  seed_df$key <- apply(seed_df, 1, paste, collapse = "_")
  synth_pop_seed_scotland$key <- apply(synth_pop_seed_scotland[,c("householdComp10","hhSize5","CarVan5","Tenure5","AccTyp7")], 1, paste, collapse = "_")

  # Match rows of A to B using the key column
  synth_pop_seed_scotland <- synth_pop_seed_scotland[match(seed_df$key, synth_pop_seed_scotland$key), ]

  # Remove the key column
  synth_pop_seed_scotland$key <- NULL
  seed_df$key <- NULL

  seed = array(synth_pop_seed_scotland$seed, dim = c(10,5,5,5,7))

  future::plan("multisession")
  res_com = furrr::future_pmap(.l = list(int_hhSize_CarVan,
                                         int_hhSize_HouseholdComp,
                                         int_hhSize_Tenure,
                                         int_hhSize_AccType),
                               .f = scot_syth_combine, seed = seed,
                               .progress = TRUE, .options = furrr::furrr_options(seed = 1234))
  future::plan("sequential")
  res_com = dplyr::bind_rows(res_com)

  if(FALSE){
    int_hhSize_HouseholdComp_sub = int_hhSize_HouseholdComp[[1]]
    int_hhSize_CarVan_sub = int_hhSize_CarVan[[1]]
    int_hhSize_Tenure_sub = int_hhSize_Tenure[[1]]
    int_hhSize_AccType_sub = int_hhSize_AccType[[1]]
    int_Tenure_HouseholdComp_sub = int_Tenure_HouseholdComp[[1 ]]
    scot_syth_combine(int_hhSize_CarVan_sub,
                      int_hhSize_HouseholdComp_sub, int_hhSize_Tenure_sub, int_hhSize_AccType_sub,
                      seed)
  }

  # Down Scale to Data Zone
  dz_CarVan = read_CarVan_scot(file.path(path_data,"scotlandcenus2022_CarVan5_DataZone.csv"))
  dz_HouseholdComp = read_hhComp_scot(file.path(path_data,"scotlandcenus2022_householdComp10_DataZone.csv"))
  dz_Tenure = read_Tenure_scot(file.path(path_data,"scotlandcenus2022_Tenure5_DataZone.csv"))
  dz_AccType = read_Acc_scot(file.path(path_data,"scotlandcenus2022_AccType7_DataZone.csv"))
  dz_hhsize = read_hhSize_scot(file.path(path_data,"scotlandcenus2022_hhSize5_DataZone.csv"))


  lookup_DataZone_2022 = lookup_DataZone_2022[,c("DZ22_Code","IZ22_Code")]
  names(lookup_DataZone_2022) = c("DZ22_Code","IZCode")

  dz_CarVan = dplyr::left_join(dz_CarVan, lookup_DataZone_2022, by = c("LSOA21CD" = "DZ22_Code"))
  dz_HouseholdComp = dplyr::left_join(dz_HouseholdComp, lookup_DataZone_2022, by = c("LSOA21CD" = "DZ22_Code"))
  dz_Tenure = dplyr::left_join(dz_Tenure, lookup_DataZone_2022, by = c("LSOA21CD" = "DZ22_Code"))
  dz_AccType = dplyr::left_join(dz_AccType, lookup_DataZone_2022, by = c("LSOA21CD" = "DZ22_Code"))
  dz_hhsize = dplyr::left_join(dz_hhsize, lookup_DataZone_2022, by = c("LSOA21CD" = "DZ22_Code"))

  dz_CarVan    = dz_CarVan[order(dz_CarVan$IZCode),]
  dz_HouseholdComp    = dz_HouseholdComp[order(dz_HouseholdComp$IZCode),]
  dz_Tenure    = dz_Tenure[order(dz_Tenure$IZCode),]
  dz_AccType    = dz_AccType[order(dz_AccType$IZCode),]
  dz_hhsize    = dz_hhsize[order(dz_hhsize$IZCode),]
  res_com = res_com[order(res_com$IZCode),]

  dz_CarVan = dplyr::group_split(dplyr::ungroup(dz_CarVan), IZCode)
  dz_HouseholdComp = dplyr::group_split(dplyr::ungroup(dz_HouseholdComp), IZCode)
  dz_Tenure = dplyr::group_split(dplyr::ungroup(dz_Tenure), IZCode)
  dz_AccType = dplyr::group_split(dplyr::ungroup(dz_AccType), IZCode)
  dz_hhsize = dplyr::group_split(dplyr::ungroup(dz_hhsize), IZCode)
  res_com = dplyr::group_split(dplyr::ungroup(res_com), IZCode)

  dz_CarVan_sub = dz_CarVan[[1]]
  dz_HouseholdComp_sub = dz_HouseholdComp[[1]]
  dz_Tenure_sub = dz_Tenure[[1]]
  dz_AccType_sub = dz_AccType[[1]]
  dz_hhsize_sub = dz_hhsize[[1]]
  res_com_sub = res_com[[1]]
}

#' Downscale an IZ household cross-tab to its Data Zones (superseded)
#'
#' @description Part of the old two-stage Scottish approach: IPF-fits the
#'   Data Zone dimension onto the Intermediate Zone result using the DZ
#'   marginal tables. Only called from `sythetic_census_scot_old()`.
#' @param dz_CarVan_sub,dz_HouseholdComp_sub,dz_Tenure_sub,dz_AccType_sub,dz_hhsize_sub
#'   One IZ's Data Zone marginal tables.
#' @param res_com_sub The IZ-level fitted cross-tab.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
downscale_to_datazone = function(dz_CarVan_sub,
                                 dz_HouseholdComp_sub,
                                 dz_Tenure_sub,
                                 dz_AccType_sub ,
                                 dz_hhsize_sub ,
                                 res_com_sub){

  # Check Zone match
  if(length(unique(c(dz_CarVan_sub$IZCode,
                     dz_HouseholdComp_sub$IZCode,
                     dz_Tenure_sub$IZCode,
                     dz_AccType_sub$IZCode,
                     dz_hhsize_sub$IZCode,
                     res_com_sub$IZCode

  ))) != 1){
    stop("More than one IZCode")
  }

  # Make arrays
  array_df = expand.grid(c("OnePersonOver66","OnePersonOther","FamilyOver66",
                          "CoupleNoChildren",
                          "CoupleChildren","CoupleNonDepChildren","LoneParent",
                          "LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66"),
                        c("p1","p2","p3","p4","p5+"),
                        c("car0","car1","car2","car3","car4+"),
                        c("outright","mortgage","socialrented","privaterented","rentfree"),
                        c("detached","semidetached","terraced","flatpurposebuilt","flatconverted","flatcommercial","caravan"), stringsAsFactors = FALSE)
  names(array_df) = c("hhComp10","hhSize","CarVan5","Tenure5","AccType7")

  array_df = dplyr::left_join(array_df, res_com_sub[,c("hhComp10","hhSize","CarVan5","Tenure5","AccType7","households")],
                              by = c("hhComp10","hhSize","CarVan5","Tenure5","AccType7"))
  array_df$households[is.na(array_df$households)] = 0

  iz_array = array(array_df$households, dim = c(10,5,5,5,7))

  dz_CarVan_sub = dz_CarVan_sub[order(dz_CarVan_sub$LSOA21CD),]
  dz_HouseholdComp_sub = dz_HouseholdComp_sub[order(dz_HouseholdComp_sub$LSOA21CD),]
  dz_Tenure_sub = dz_Tenure_sub[order(dz_Tenure_sub$LSOA21CD),]
  dz_AccType_sub = dz_AccType_sub[order(dz_AccType_sub$LSOA21CD),]
  dz_hhsize_sub = dz_hhsize_sub[order(dz_hhsize_sub$LSOA21CD),]

  dzbycar = as.matrix(dz_CarVan_sub[,2:(ncol(dz_CarVan_sub) - 1)])
  dzbyhhcomp = as.matrix(dz_HouseholdComp_sub[,2:(ncol(dz_HouseholdComp_sub) - 1)])
  dzbytenure = as.matrix(dz_Tenure_sub[,2:(ncol(dz_Tenure_sub) - 1)])
  dzbyacc = as.matrix(dz_AccType_sub[,2:(ncol(dz_AccType_sub) - 1)])
  dzbyhhsize = as.matrix(dz_hhsize_sub[,2:(ncol(dz_hhsize_sub) - 1)])

  rownames(dzbyhhcomp) = dz_CarVan_sub$LSOA21CD
  rownames(dzbyhhsize) = dz_CarVan_sub$LSOA21CD
  rownames(dzbycar) = dz_CarVan_sub$LSOA21CD
  rownames(dzbytenure) = dz_CarVan_sub$LSOA21CD
  rownames(dzbyacc) = dz_CarVan_sub$LSOA21CD

  # Summaries for each variable
  t_hhComp10 = array_df |> dplyr::group_by(hhComp10) |> dplyr::summarise(households = sum(households))
  t_hhSize = array_df |> dplyr::group_by(hhSize) |> dplyr::summarise(households = sum(households))
  t_CarVan5 = array_df |> dplyr::group_by(CarVan5) |> dplyr::summarise(households = sum(households))
  t_Tenure5 = array_df |> dplyr::group_by(Tenure5) |> dplyr::summarise(households = sum(households))
  t_AccType7 = array_df |> dplyr::group_by(AccType7) |> dplyr::summarise(households = sum(households))

  t_hhComp10 = prep_for_cleaning(t_hhComp10,dzbyhhcomp)
  t_hhSize = prep_for_cleaning(t_hhSize,dzbyhhsize)
  t_CarVan5 = prep_for_cleaning(t_CarVan5,dzbycar)
  t_Tenure5 = prep_for_cleaning(t_Tenure5,dzbytenure)
  t_AccType7 = prep_for_cleaning(t_AccType7,dzbyacc)

  dzbyhhcomp = match_matrix_csums(t_hhComp10, dzbyhhcomp)
  dzbyhhsize = match_matrix_csums(t_hhSize, dzbyhhsize)
  dzbycar = match_matrix_csums(t_CarVan5, dzbycar)
  dzbytenure = match_matrix_csums(t_Tenure5, dzbytenure)
  dzbyacc = match_matrix_csums(t_AccType7, dzbyacc)

  seed = array(1, dim = c(10,5,5,5,7, nrow(dzbyhhcomp)))

  result = try(humanleague::qisi(seed,
                                 indices = list(c(1,2,3,4,5),c(6,1),c(6,2),c(6,3),c(6,4),c(6,5)),
                                 marginals = list(iz_array,
                                                  dzbyhhcomp,
                                                  dzbyhhsize,
                                                  dzbycar,
                                                  dzbytenure,
                                                  dzbyacc
                                 )),
               silent = TRUE)


  humanleague::qisi(seed,
                    indices = list(c(1,2,3,4,5),c(6,1)),
                    marginals = list(iz_array,
                                     dzbyhhcomp
                    ))



}

#' Align a household-composition table for cleaning (superseded)
#'
#' @description Helper for the old two-stage Scottish approach; reconciles
#'   a composition table with the DZ-by-composition counts. Only called
#'   from the superseded code path.
#' @param t_hhComp10 Household composition totals.
#' @param dzbyhhcomp Data Zone by composition counts.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
prep_for_cleaning = function(t_hhComp10,dzbyhhcomp ){
  t_hhComp10_m = as.matrix(t_hhComp10[,2])
  rownames(t_hhComp10_m) = t_hhComp10[[1]]
  t_hhComp10_m = t_hhComp10_m[match(colnames(dzbyhhcomp), rownames(t_hhComp10_m)),,drop = FALSE]
  t_hhComp10_m = t(t_hhComp10_m)
  t_hhComp10_m
}


#' IPF one Intermediate Zone's cross-tab (superseded)
#'
#' @description Worker for the old two-stage Scottish approach: fits the
#'   five-way cross-tab at Intermediate Zone level from the IZ marginal
#'   tables. Only called from `sythetic_census_scot_old()`.
#' @param int_hhSize_CarVan_sub,int_hhSize_HouseholdComp_sub,int_hhSize_Tenure_sub,int_hhSize_AccType_sub,int_Tenure_HouseholdComp_sub
#'   One IZ's marginal tables.
#' @param seed National seed arrays.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
scot_syth_combine_old = function(int_hhSize_CarVan_sub,
                                 int_hhSize_HouseholdComp_sub,
                                 int_hhSize_Tenure_sub,
                                 int_hhSize_AccType_sub,
                                 int_Tenure_HouseholdComp_sub,
                                 seed){

  # Check Zone match
  if(length(unique(c(int_hhSize_CarVan_sub$IZCode,
                     int_hhSize_HouseholdComp_sub$IZCode,
                     int_hhSize_Tenure_sub$IZCode,
                     int_hhSize_AccType_sub$IZCode,
                     int_Tenure_HouseholdComp_sub$IZCode
  ))) != 1){
    stop("More than one IZCode")
  }

  CarVanByhhSize = as.matrix(int_hhSize_CarVan_sub[,3:ncol(int_hhSize_CarVan_sub)])
  HouseholdCompByhhSize = as.matrix(int_hhSize_HouseholdComp_sub[,3:ncol(int_hhSize_HouseholdComp_sub)])
  TenureByhhSize = as.matrix(int_hhSize_Tenure_sub[,3:ncol(int_hhSize_Tenure_sub)])
  AccTypeByhhSize = as.matrix(int_hhSize_AccType_sub[,3:ncol(int_hhSize_AccType_sub)])
  HouseholdCompByTenure = as.matrix(int_Tenure_HouseholdComp_sub[,3:ncol(int_Tenure_HouseholdComp_sub)])

  rownames(CarVanByhhSize) = int_hhSize_CarVan_sub$CarVan5
  rownames(HouseholdCompByhhSize) = int_hhSize_HouseholdComp_sub$hhComp10 # Use a population reference
  rownames(TenureByhhSize) = int_hhSize_Tenure_sub$tenure5
  rownames(AccTypeByhhSize) = int_hhSize_AccType_sub$AccType7
  rownames(HouseholdCompByTenure) = int_Tenure_HouseholdComp_sub$hhComp10


  # Alt Method uing mipfp
  seed_weighted = seed * sum(CarVanByhhSize)

  res <- mipfp::Ipfp(seed_weighted,
                     list(c(1,2),c(3,2),c(4,2),c(5,2),c(1,3)),
                     list(HouseholdCompByhhSize,
                         TenureByhhSize,
                         CarVanByhhSize,
                         AccTypeByhhSize,
                         HouseholdCompByTenure))

  res2 = res$x.hat * sum(CarVanByhhSize)

  result_df = expand.grid(
    rownames(HouseholdCompByhhSize),
    colnames(HouseholdCompByhhSize),
    rownames(TenureByhhSize),
    rownames(CarVanByhhSize),
    rownames(AccTypeByhhSize)
  )
  names(result_df) = c("hhComp10","hhSize","Tenure5","CarVan5","AccType7")

  result_df$households = int_trs(as.numeric(res2))
  result_df$error_margins = res$error.margins
  result_df$conv = res$conv
  result_df = result_df[result_df$households > 0,]

  # # Harmonise by household size
  # # The number of 1 person and more than one person households should match
  # HouseholdCompByhhSize = HouseholdCompByhhSize[c("OnePersonOther","OnePersonOver66", # Only 1
  #                                   "CoupleNoChildren", # Only 2
  #                                   "CoupleChildren","CoupleNonDepChildren","FamilyOver66","LoneParent", # At least 2
  #                                   "LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66"),]
  #
  # TenureByhhSize = TenureByhhSize[c("outright","mortgage","socialrented","privaterented","rentfree"),]
  # AccTypeByhhSize = AccTypeByhhSize[c("detached","semidetached","terraced","flatpurposebuilt","flatconverted","flatcommercial","caravan"),]
  # #HouseholdCompByTenure = HouseholdCompByTenure[rownames(HouseholdCompByhhSize),rownames(TenureByhhSize)]
  #
  # CarVanByhhSize = match_matrix_csums(HouseholdCompByhhSize, CarVanByhhSize)
  # TenureByhhSize = match_matrix_csums(HouseholdCompByhhSize, TenureByhhSize)
  # AccTypeByhhSize = match_matrix_csums(HouseholdCompByhhSize, AccTypeByhhSize)
  # #HouseholdCompByTenure2 = match_matrix_rsums_csums(rsum2 = rowSums(HouseholdCompByhhSize), csum2 = rowSums(TenureByhhSize), matO = HouseholdCompByTenure)
  #
  # #HouseholdCompByTenure2 = furness_balance(HouseholdCompByTenure, rowSums(HouseholdCompByhhSize), rowSums(TenureByhhSize), int_only = TRUE)
  #
  #
  # # Pop Synth
  # result = try(humanleague::qisi(seed,
  #                                indices = list(c(1,2),c(3,2),c(4,2),c(5,2)),
  #                                marginals = list(HouseholdCompByhhSize,
  #                                                 TenureByhhSize,
  #                                                 CarVanByhhSize,
  #                                                 AccTypeByhhSize
  #                                )),
  #              silent = TRUE)

  # seed2 = seed
  # seed2[seed2 <= 1e-15] = 1e-3
  #
  # result = try(humanleague::qisi(seed2,
  #                                indices = list(c(1,2),c(3,2),c(4,2),c(5,2),c(1,3)),
  #                                marginals = list(HouseholdCompByhhSize,
  #                                                 TenureByhhSize,
  #                                                 CarVanByhhSize,
  #                                                 AccTypeByhhSize,HouseholdCompByTenure2
  #                                )),
  #              silent = TRUE)

  # if(inherits(result,"try-error")){
  #   message("QISI failed for ",int_hhSize_CarVan_sub$IZCode[1]," ",result[1])
  #   return(NULL)
  # }

  # result_df = expand.grid(
  #   rownames(HouseholdCompByhhSize),
  #   colnames(HouseholdCompByhhSize),
  #   rownames(TenureByhhSize),
  #   rownames(CarVanByhhSize),
  #   rownames(AccTypeByhhSize)
  # )
  # names(result_df) = c("hhComp10","hhSize","Tenure5","CarVan5","AccType7")
  #
  #
  # result_df$households = round(as.numeric(result$result))
  # result_df = result_df[result_df$households > 0,]
  # result_df$conv = result$conv
  # result_df$pValue = result$pValue

  # Integrity checks
  # Should only be small differences in total populations
  if(abs(sum(result_df$households[result_df$hhSize == "p1" & result_df$hhComp10 == "OnePersonOver66"]) -
     HouseholdCompByhhSize["OnePersonOver66","p1"]) > 10){
    warning("check 1 failed for:",int_hhSize_CarVan_sub$IZCode[1])
  }
  if(abs(sum(result_df$households[result_df$hhSize == "p2" & result_df$Tenure5 == "outright"]) -
     TenureByhhSize["outright","p2"]) > 10){
    warning("check 2 failed for:",int_hhSize_CarVan_sub$IZCode[1])
  }
  if(abs(sum(result_df$households[result_df$hhSize == "p3" & result_df$AccType7 == "detached"]) -
     AccTypeByhhSize["detached","p3"]) > 10){
    warning("check 3 failed for:",int_hhSize_CarVan_sub$IZCode[1])
  }


  result_df$IZCode = int_hhSize_CarVan_sub$IZCode[1]

  # Validation Check
  if(FALSE){
    chk = result_df |>
      dplyr::group_by(hhComp10,Tenure5) |>
      dplyr::summarise(households = sum(households)) |>
      tidyr::pivot_wider(names_from = "Tenure5", values_from = "households", values_fill = 0)
    chkmat = as.matrix(chk[2:ncol(chk)])
    round(abs(chkmat - HouseholdCompByTenure) / HouseholdCompByTenure * 100) # % error
  }

  result_df

}


#' Rebalance a matrix to given row and column sums (integer Furness)
#'
#' @description Repeatedly applies `match_matrix_rsums()`/`match_matrix_csums()`
#'   style +/-1 adjustments until the matrix satisfies both target
#'   marginals, keeping values integer. Used to reconcile inconsistent
#'   census tables before IPF.
#' @param rsum2 Target row sums.
#' @param csum2 Target column sums.
#' @param matO Matrix to adjust.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
match_matrix_rsums_csums = function(rsum2, csum2, matO){

  mat_rsum = rowSums(matO)
  mat_csum = colSums(matO)

  if(all(rsum2 == mat_rsum) & all(csum2 == mat_csum)){
    return(matO)
  }

  # Calc Differences
  row_diff = rsum2 - mat_rsum
  col_diff = csum2 - mat_csum

  mat = matrix(1, nrow = length(rsum2), ncol = length(csum2))

  # furness_balance works poorly with zeros
  # offset the problem by adding 1000



  mat3 = furness_balance(mat, rsum = row_diff + 1000, csum = col_diff + 1000, int_only = TRUE, quiet = FALSE)

  mat_new = matO + mat3

  mat_new
}

# --- from R/synth_pop_LCFS.R ------------------------------
#' Find the LCFS households most similar to one census household type
#'
#' @description As `match_hh_census()` but without the income filtering -
#'   candidates are returned for downstream income-weighted sampling via
#'   `select_id_income()`. Superseded by the faster `match_hh_census3()`,
#'   which the pipeline now uses for both England & Wales and Scotland.
#' @param Tenure5,hhComp15,hhSize5,CarVan5 Census household attributes.
#' @param OACs Space-separated OAC codes of the zone.
#' @param hh LCFS household table.
#' @param similarity_matrices List of similarity matrices per attribute.
#' @return A one-row data frame with the match score, number of matches and
#'   a list-column of candidate `household_id`s.
#' @keywords internal
match_hh_census2 <- function(Tenure5,hhComp15,hhSize5,CarVan5,OACs, hh, similarity_matrices) {


  # Create named vectors for the input variables to match the dimension names in the similarity matrices
  input_vars <- list(
    Tenure5 = as.character(Tenure5),
    hhComp15 = as.character(hhComp15),
    hhSize5 = hhSize5,
    CarVan5 = CarVan5,
    OAC = unlist(strsplit(OACs," "))
  )

  # Initialize similarity scores as a numeric vector
  similarity_scores <- numeric(nrow(hh))

  # Calculate similarity scores using vectorized operations
  for (var in names(input_vars)) {
    sim_matrix <- similarity_matrices[[var]]
    input_value <- input_vars[[var]]
    hh_values <- hh[[var]]

    if(var == "OAC"){
      #Special case LSOAs can have multiple OACs,
      #input_value$subgroup = as.character(input_value$subgroup)
      #sim_matrix[,!colnames(sim_matrix) %in% input_value$subgroup] = 0
      #sim_matrix[,!colnames(sim_matrix) %in% input_value] = 0

      # Map the input value and household values to their corresponding indices
      input_index <- which(rownames(sim_matrix) %in% input_value)
      hh_indices <- match(hh_values, colnames(sim_matrix))

      # Extract the similarity scores for all households at once
      scores <- sim_matrix[input_index, hh_indices]
      if(inherits(scores,"matrix")){
        scores <- apply(scores, 2, max, na.rm = TRUE)
      }


    } else {
      # Map the input value and household values to their corresponding indices
      input_index <- which(rownames(sim_matrix) == input_value)
      hh_indices <- match(hh_values, colnames(sim_matrix))

      # Extract the similarity scores for all households at once
      scores <- sim_matrix[input_index, hh_indices]
    }

    similarity_scores <- similarity_scores + scores


  }

  # Find the maximum similarity score
  max_score <- max(similarity_scores, na.rm = TRUE)

  # Get all households with the maximum similarity score
  hh_sub <- hh[similarity_scores == max_score, ]

  if (nrow(hh_sub) > 0) {
    return(data.frame(
      Tenure5 = Tenure5,
      hhComp15 = hhComp15,
      hhSize5 = hhSize5,
      CarVan5 = CarVan5,
      OACs = OACs,
      n_match = nrow(hh_sub),
      match_score = max_score / 5,
      household_id = I(list(hh_sub$household_id))
    ))
  } else {
    message(unlist(input_vars))
    stop()
  }
}

