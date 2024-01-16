download_consumption_footprint <- function(path){
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    fls = list.files(path, pattern = "ods")
    if(length(fls) > 0){
      return(path)
    }
  }

  url = "https://assets.publishing.service.gov.uk/media/64ca46236ae44e001311b3df/2020_Defra_results_UK_rev.ods"
  dowload.file(url, file.path(path, "2020_Defra_results_UK_rev.ods"))
  return(path)

}

load_consumption_footprint <- function(path){

}
