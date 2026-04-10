#' Write One
#'
#' @description Export one to disk or a specified output location.
#' @param sub Subset object used within the function.
#' @param idcol Input object or parameter named `idcol`.
#' @param path File or directory path.
#' @param dataframe Input object or parameter named `dataframe`.
#' @param na){ Input object or parameter named `na){`.
#' @return Usually returns the input invisibly after saving output to disk.
#' @keywords internal
write_one <- function(sub, idcol, path = "", dataframe, na){
  #sub <- x[x[[idcol]] == idv, , drop = FALSE]
  # ensure a data.frame (avoid tibble overhead)
  sub <- as.data.frame(sub)
  nmsub <- sub[[idcol]][1]
  sub[[idcol]] <- NULL
  outfile <- file.path(path, paste0(nmsub, ".json"))
  yyjsonr::write_json_file(sub, outfile, dataframe = dataframe)
  outfile
}

#' Convert Json
#'
#' @description Perform processing for convert2json.
#' @param sub Subset object used within the function.
#' @param idcol Input object or parameter named `idcol`.
#' @param dataframe){ Input object or parameter named `dataframe){`.
#' @return A data frame produced by the function.
#' @keywords internal
convert2json <- function(sub, idcol, dataframe){
  sub <- as.data.frame(sub)
  nmsub <- sub[[idcol]][1]
  sub[[idcol]] <- NULL
  json = yyjsonr::write_json_str(sub, dataframe = dataframe)
  names(json) = nmsub
  json
}

#' Write File
#'
#' @description Perform processing for write2file.
#' @param content Input object or parameter named `content`.
#' @param filename) Input object or parameter named `filename)`.
#' @return Usually returns the input invisibly after saving output to disk.
#' @keywords internal
write2file <- function(content, filename) {
  writeLines(content, filename)
}


#' Function to convert a data.frame into a folder of JSON files
#' @param x data frame with column called geo_code
#' @param idcol name of column with unique id
#' @param path folder to save JSON
#' @param zip logical, if true zips the json into a single zip folder
#' @param rounddp numeric, how many decimpla places to round to
#' @dataframe passed to toJSON
#' @na passed to toJSON
#'
#' Will drop any sf geometry and name files based on geo_code

#' Export Zone Json
#'
#' @description Export zone json to disk or a specified output location.
#' @param x Input data object.
#' @param idcol Input object or parameter named `idcol`.
#' @param path File or directory path.
#' @param zip Logical zip ouputs
#' @param rounddp Number of decimal points to round data to
#' @param dataframe Input object or parameter named `dataframe`.
#' @param reduce Reduce column names length
#' @param na What should NAs be in JSON
#' @param parallel Input object or parameter named `parallel`.
#' @param workers Number of workers to use if `parallel` is TRUE
#' @return Usually returns the input invisibly after saving output to disk.
#' @keywords internal
export_zone_json <- function(x,
                             idcol = "LSOA21CD",
                             path = "outputdata/json",
                             zip = TRUE,
                             rounddp = 2,
                             dataframe = "rows",
                             reduce = FALSE,
                             na = "null",
                             parallel = TRUE,
                             workers = NULL){

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
      if(rounddp == 0){
        x[[i]] = as.integer(x[[i]])
      } else {
        x[[i]] = round(x[[i]], rounddp)
      }

    }
  }

  x <- dplyr::group_split(x, .data[[idcol]], .keep = TRUE)

  # Avoid building a large list in memory; iterate unique ids and write files per id.
  # Prepare output directory (for zip we write to a temp dir first)
  if(zip){
    temp_json_dir <- file.path(tempdir(), paste0("jsonzip", idcol))
    if(!dir.exists(temp_json_dir)) dir.create(temp_json_dir, recursive = TRUE)
  } else {
    if(!dir.exists(path)) dir.create(path, recursive = TRUE)
    temp_json_dir <- path
  }

  message("Converting JSON ",Sys.time())

  json <- purrr::map(x, convert2json, idcol = idcol,
                     dataframe = dataframe,
                     .progress = TRUE)
  json <- unlist(json)
  paths = file.path(path,paste0(names(json),".json"))

  message("Writing JSON ",Sys.time())

  # Try to use parallel execution if requested and available (furrr + future).
  if(parallel && requireNamespace("future", quietly = TRUE) && requireNamespace("furrr", quietly = TRUE)){
    # choose workers
    if(is.null(workers)) workers <- max(1, future::availableCores() - 1)
    future::plan("multisession")
    foo = furrr::future_map2(.x = json, .y = paths, .f = write2file, .progress = TRUE)
    future::plan("sequential")
  } else {
    foo = purrr::map2(.x = json, .y = paths, .f = write2file, .progress = TRUE)
  }

  if(zip){
    files <- list.files(temp_json_dir)
    message("Zipping JSON")
    my_wd <- getwd()
    setwd(temp_json_dir)

    if(file.exists(file.path(my_wd, path, paste0(idcol, "_json.zip")))){
      unlink(file.path(my_wd, path, paste0(idcol, "_json.zip")))
    }

    zip::zip(file.path(my_wd, path, paste0(idcol, "_json.zip")), files)
    setwd(my_wd)

    message("Cleaning up")
    if(file.exists(file.path(my_wd, path, paste0(idcol, "_json.zip")))){
      unlink(temp_json_dir, recursive = TRUE)
      write.csv(new_nms, file.path(path, "names_lookup.csv"), row.names = FALSE)
      return(file.path(my_wd, path, paste0(idcol, "_json.zip")))
    } else {
      stop("Zipping failed")
    }
  }

  write.csv(new_nms, file.path(path,"names_lookup.csv"), row.names = FALSE)

  return(path)

}

# Long names make the JSON files larger so reduce names and save a lookup

#' Reduce Name Length
#'
#' @description Perform processing for reduce name length.
#' @param x){ Input object or parameter named `x){`.
#' @return A data frame produced by the function.
#' @keywords internal
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

