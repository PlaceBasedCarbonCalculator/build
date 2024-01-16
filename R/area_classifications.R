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
