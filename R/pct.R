#' Download Propensity to Cycle Tool national outputs
#'
#' @description Downloads the PCT commute and school LSOA zone and route
#'   network Rds files from the npct/pct-outputs-national GitHub repo.
#'   Skipped if `path` already holds more than 3 Rds files. This is the
#'   `dl_pct` target; the files are not yet consumed by other targets.
#' @param path Folder to store the downloads; created if missing.
#' @return `path`.
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
