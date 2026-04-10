#' Download Pct
#'
#' @description Download the pct resource and return the local file path.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path){ Input object or parameter named `path){`.
#' @return The local path or file name of the downloaded resource.
#' @keywords internal
download_pct <- function(path){
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    fls = list.files(path, pattern = "Rds")
    if(length(fls) > 3){
      return(path)
    }
  }

  download.file("https://github.com/npct/pct-outputs-national/raw/master/school/lsoa/z_all.Rds",
                destfile = file.path(path,"school_zones.Rds"), mode = "wb")
  download.file("https://github.com/npct/pct-outputs-national/raw/master/commute/lsoa/z_all.Rds",
                destfile = file.path(path,"commute_zones.Rds"), mode = "wb")

  download.file("https://github.com/npct/pct-outputs-national/raw/master/commute/lsoa/rnet_all.Rds",
                destfile = file.path(path,"commute_rnet.Rds"), mode = "wb")
  download.file("https://github.com/npct/pct-outputs-national/raw/master/school/lsoa/rnet_all.Rds",
                destfile = file.path(path,"school_rnet.Rds"), mode = "wb")

  return(path)

}
