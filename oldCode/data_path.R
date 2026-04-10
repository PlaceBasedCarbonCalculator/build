#' Data Path
#'
#' @description Perform processing for data path.
#' @param path File or directory path.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
data_path <- function(path = parameters$path_data){
  if(dir.exists(path)){
    return(path)
  } else {
    stop("Can't find the input data folder, please place 'inputdata' folder next to the 'build' folder")
  }
}


#' Data Path Secure
#'
#' @description Perform processing for data path secure.
#' @param path File or directory path.
#' @return The function result, typically a data frame or list used in the pipeline.
#' @keywords internal
data_path_secure <- function(path = parameters$path_secure_data){
  if(dir.exists(path)){
    return(path)
  } else {
    stop("Can't find the secure data folder, please define the location for secure data")
  }
}
