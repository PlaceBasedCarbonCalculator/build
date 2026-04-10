#' Read Jobs Industry
#'
#' @description Read jobs industry from disk into an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_jobs_industry = function(path = "../inputdata/industry/wd015.zip"){
  dir.create(file.path(tempdir(),"industry"))
  unzip(path, exdir = file.path(tempdir(),"industry"))
  dat <- readr::read_csv(file.path(tempdir(),"industry","WD015_msoa.csv"))
  unlink(file.path(tempdir(),"industry"), recursive = TRUE)
  names(dat) = c("MSOA21CD","MSOA21NM","industry_code","industry_name","count")
  dat
}


#' Read Industry Classifications
#'
#' @description Read industry classifications from disk into an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_industry_classifications = function(path = "../inputdata/industry/Manufacturing activities by perceived flexibility and workforce.xlsx"){
  classif <- readxl::read_xlsx(path)
  names(classif) <- c("industry_name","count","category")
  classif = classif[,c("industry_name","category")]
  classif
}
