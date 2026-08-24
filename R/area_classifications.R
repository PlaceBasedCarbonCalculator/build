#' Download the ONS 2011 residential area classification data
#'
#' @description Downloads the ONS 2011 area classification census data zip
#'   into `path` (skipped if already present). This is the
#'   `dl_area_classifications` target.
#' @param path Folder to store the download; created if missing.
#' @return `path`.
#' @keywords internal
download_area_classifications = function(path){
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    if(file.exists(file.path(path, "2011censusdata.zip"))){
      return(path)
    }
  }

  url = "https://www.ons.gov.uk/file?uri=/methodology/geography/geographicalproducts/areaclassifications/2011areaclassifications/datasets/2011censusdata.zip"
  download.file(url, file.path(path, "2011censusdata.zip"), mode = "wb")
  path

}


#' Load the 2011 LSOA residential area classification (SOAC)
#'
#' @description Unzips and reads the ONS 2011 area classification workbook,
#'   keeping the supergroup and group codes/names per 2011 LSOA/Data Zone.
#'   Used by the `area_classifications` target.
#' @param path Folder containing `2011censusdata.zip`
#'   (`dl_area_classifications` target).
#' @return A data frame with `LSOACD11`, `supergroup_code`,
#'   `supergroup_name`, `lsoa_class_code`, `lsoa_class_name`.
#' @keywords internal
load_area_classifications = function(path){

  dir.create(file.path(tempdir(),"area_classifications"))
  unzip(file.path(path,"2011censusdata.zip"),
        exdir = file.path(tempdir(),"area_classifications"))

  classif <- readxl::read_xls(file.path(tempdir(),"area_classifications","2011_Census_Data.xls"))
  classif <- as.data.frame(classif)
  names(classif) <- as.character(classif[5,])
  classif = classif[,c("SOA Code","Supergroup Code","Supergroup Name","Group Code","Group Name")]
  classif = classif[6:nrow(classif),]
  classif = classif[!is.na(classif$`SOA Code`),]
  names(classif) = c("LSOACD11","supergroup_code","supergroup_name","lsoa_class_code","lsoa_class_name")
  classif
}

#' Carry the 2011 area classification onto 2021/2022 zones
#'
#' @description Assigns each 2021 LSOA / 2022 Data Zone the classification
#'   of its best-matching 2011 zone: Scottish zones use the largest UPRN
#'   split share, merged/split E&W zones take the first matching parent.
#'   Used by the `area_classifications_11_21` target, which feeds the map
#'   outputs, OAC emissions summaries and overview JSONs.
#' @param area_classifications 2011 classifications
#'   (`area_classifications` target).
#' @param lookup_dz_2011_22 Data Zone split shares (`lookup_dz_2011_22`).
#' @param lsoa_11_21_tools E&W conversion lookups (`lsoa_11_21_tools`).
#' @return A data frame with `LSOA21CD` and the classification columns.
#' @keywords internal
match_2011_classifications_2021 = function(area_classifications, lookup_dz_2011_22, lsoa_11_21_tools){

  lookup_dz_2011_22 = lookup_dz_2011_22[order(lookup_dz_2011_22$splitshare, decreasing = TRUE),]
  lookup_dz_2011_22 = lookup_dz_2011_22[!duplicated(lookup_dz_2011_22$LSOA21CD),]
  lookup_dz_2011_22$splitshare = NULL

  lsoa_11_21_tools_U = lsoa_11_21_tools$lookup_unchanged
  lsoa_11_21_tools_M = lsoa_11_21_tools$lookup_merge
  lsoa_11_21_tools_S = lsoa_11_21_tools$lookup_split

  lsoa_11_21_tools_M = lsoa_11_21_tools_M[!duplicated(lsoa_11_21_tools_M$LSOA21CD),]
  lsoa_11_21_tools_S = lsoa_11_21_tools_S[!duplicated(lsoa_11_21_tools_S$LSOA21CD),]
  lsoa_11_21_tools_S = lsoa_11_21_tools_S[,c("LSOA11CD","LSOA21CD")]

  lsoa = rbind(lookup_dz_2011_22, lsoa_11_21_tools_U, lsoa_11_21_tools_M, lsoa_11_21_tools_S)

  lsoa = dplyr::left_join(lsoa, area_classifications, by = c("LSOA11CD" = "LSOACD11"))
  lsoa$LSOA11CD = NULL

  lsoa

}


