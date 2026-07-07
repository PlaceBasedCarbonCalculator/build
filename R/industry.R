#' Read 2021 census workday jobs by industry per MSOA
#'
#' @description Unzips and reads the WD015 workday-population-by-industry
#'   table. Not currently wired to a target.
#' @param path Path to `wd015.zip`.
#' @return A data frame with `MSOA21CD`, industry code/name and job counts.
#' @keywords internal
read_jobs_industry = function(path = "../inputdata/industry/wd015.zip"){
  dir.create(file.path(tempdir(),"industry"))
  unzip(path, exdir = file.path(tempdir(),"industry"))
  dat <- readr::read_csv(file.path(tempdir(),"industry","WD015_msoa.csv"))
  unlink(file.path(tempdir(),"industry"), recursive = TRUE)
  names(dat) = c("MSOA21CD","MSOA21NM","industry_code","industry_name","count")
  dat
}


#' Read the manufacturing flexibility classification of industries
#'
#' @description Reads the workbook categorising manufacturing activities by
#'   perceived flexibility/workforce. Not currently wired to a target.
#' @param path Path to the xlsx workbook.
#' @return A data frame with `industry_name` and `category`.
#' @keywords internal
read_industry_classifications = function(path = "../inputdata/industry/Manufacturing activities by perceived flexibility and workforce.xlsx"){
  classif <- readxl::read_xlsx(path)
  names(classif) <- c("industry_name","count","category")
  classif = classif[,c("industry_name","category")]
  classif
}
