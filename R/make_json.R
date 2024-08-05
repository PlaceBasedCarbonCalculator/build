#' Function to convert a data.frame into a folder of JSON files
#' @param x data frame with column called geo_code
#' @parm idcol name of column with unique id
#' @param path folder to save JSON
#' @param zip logical, if true zips the json into a single zip folder
#' @param rounddp numeric, how many decimpla places to round to
#' @dataframe passed to toJSON
#' @na passed to toJSON
#'
#' Will drop any sf geometry and name files based on geo_code

export_zone_json <- function(x,  idcol = "LSOA21CD", path = "outputdata/json",
                             zip = TRUE, rounddp = 2, dataframe = "rows", reduce = TRUE,
                             na = "null"){

  if(!dir.exists(path)){
    if(dir.exists("outputdata")) {
      dir.create(path)
    } else {
      stop("path is not a valid folder")
    }
  }

  if(!inherits(x, "data.frame")){
    stop("x is not a data.frame")
  }

  if(inherits(x, "sf")){
    x <- sf::st_drop_geometry(x)
  }

  if(inherits(x, "tibble") | inherits(x, "tbl")){
    x <- as.data.frame(x)
  }

  # Reduce size with short file names
  if(reduce){
    new_nms = reduce_name_length(names(x))
    names(x) = new_nms$y

    idcol = new_nms$y[new_nms$x == idcol]
  } else {
    new_nms = "Unchanged"
  }


  # Round to 2DP
  for(i in seq_len(ncol(x))){
    if(inherits(x[[i]],"numeric")){
      x[[i]] = round(x[[i]], rounddp)
    }
  }

  x <- dplyr::group_split(x, x[[idcol]], .keep = FALSE)

  if(zip){
    dir.create(file.path(tempdir(),paste0("jsonzip",idcol)))



    message("Writing JSON")
    for(i in seq(1,length(x))){
      sub <- as.data.frame(x[[i]])
      nmsub <- sub[,idcol][1]
      sub[idcol] <- NULL
      jsonlite::write_json(sub,
                           file.path(tempdir(),paste0("jsonzip",idcol),paste0(nmsub,".json")),
                           dataframe = dataframe, na = na)
    }
    files <- list.files(file.path(tempdir(),paste0("jsonzip",idcol)))
    message("Zipping JSON")
    my_wd <- getwd()
    setwd(file.path(tempdir(),paste0("jsonzip",idcol)))

    if(file.exists(file.path(my_wd,path,paste0(idcol,"_json.zip")))){
      unlink(file.path(my_wd,path,paste0(idcol,"_json.zip")))
    }

    utils::zip(file.path(my_wd,path,paste0(idcol,"_json.zip")),files, flags="-q")
    setwd(my_wd)

    if(file.exists(file.path(my_wd,path,paste0(idcol,"_json.zip")))){
      unlink(file.path(tempdir(),paste0("jsonzip",idcol)), recursive = TRUE)
      write.csv(new_nms, file.path(path,"names_lookup.csv"), row.names = FALSE)
      return(file.path(my_wd,path,paste0(idcol,"_json.zip")))
    } else {
      stop("Zipping failed")
    }

  } else {
    message("Writing JSON")
    for(i in seq(1,length(x))){
      sub <- as.data.frame(x[[i]])
      nmsub <- sub[,idcol][1]
      sub[idcol] <- NULL
      jsonlite::write_json(sub,
                           file.path(path,paste0(nmsub,".json")),
                           dataframe = dataframe, na = na)
    }
  }

  write.csv(new_nms, file.path(path,"names_lookup.csv"), row.names = FALSE)

  return(path)

}

# Long names make the JSON files larger so reduce names and save a lookup

reduce_name_length = function(x){

  # Round1: split at _ and take first letter, or numbers
  y = strsplit(x,"_")
  y1 = sapply(y, function(ys){
    ys = sapply(ys, function(ys2){
      if(is.na(suppressWarnings(as.integer(ys2)))){
        ys2 = substr(ys2,1,1)
      }
      ys2
    }, USE.NAMES = FALSE)
    ys = paste(ys, collapse = "")
    ys
  }, USE.NAMES = FALSE)

  dat = data.frame(x = x, y = y1)
  dat$dup = duplicated(dat$y)

  if(any(dat$dup)){
    dat$occurrences <- ave(seq_along(dat$y), dat$y, FUN = seq_along)
    if(max(dat$occurrences) > 26){
      stop("More than 26 identical column names")
    }
    dat$y = ifelse(dat$occurrences > 1, paste0(LETTERS[dat$occurrences],dat$y), dat$y)

  }
  dat
}

#' Function to convert a data.frame into a folder of JSON files
#' @param x data frame with column called geo_code
#' @parm idcol name of column with unique id
#' @param path folder to save JSON
#' @param zip logical, if true zips the json into a single zip folder
#' @param rounddp numeric, how many decimpla places to round to
#'
#' Will drop any sf geometry and name files based on geo_code

