data_path <- function(){
  if(dir.exists("../inputdata")){
    return("../inputdata")
  } else {
    stop("Can't find the input data folder, please place 'inputdata' folder next to the 'build' folder")
  }
}
