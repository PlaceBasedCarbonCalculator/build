
#' Convert a single zone's data to a JSON string
#'
#' @description Takes a data frame for a single zone, drops the ID column and
#'   serialises the remaining columns to a JSON string. Used by
#'   `export_zone_json()` via `purrr::map()` over the group-split data.
#' @param sub Data frame containing rows for exactly one zone.
#' @param idcol Name of the column holding the zone ID; its first value is used
#'   to name the returned string.
#' @param dataframe Passed to `yyjsonr::write_json_str()`; either "rows" or
#'   "columns", controlling the JSON orientation.
#' @return A named length-1 character vector: the JSON string, named by zone ID.
#' @keywords internal
convert2json <- function(sub, idcol, dataframe){
  sub <- as.data.frame(sub)
  nmsub <- sub[[idcol]][1]
  sub[[idcol]] <- NULL
  json = yyjsonr::write_json_str(sub, dataframe = dataframe)
  names(json) = nmsub
  json
}

#' Write a character string to a file
#'
#' @description Thin wrapper around `writeLines()` used by `export_zone_json()`
#'   so it can be mapped (optionally in parallel via `furrr`) over pairs of
#'   JSON strings and file paths.
#' @param content Character string (JSON) to write.
#' @param filename Path of the file to write.
#' @return The result of `writeLines()`, invisibly (NULL).
#' @keywords internal
write2file <- function(content, filename) {
  writeLines(content, filename)
}


#' Export a data frame as one JSON file per zone
#'
#' @description Splits a data frame by zone ID and writes one JSON file per
#'   zone (named `<id>.json`) into `path`. Superseded: the `_targets.R` export
#'   targets now use `export_zone_bin()` (see R/json_to_bin.R), which packs the
#'   per-zone JSON into a single range-requestable binary instead. Any sf
#'   geometry is dropped, numeric columns are rounded (or converted to integer
#'   when `rounddp = 0`), and a `names_lookup.csv` recording any column-name
#'   shortening is written alongside the output.
#' @param x Data frame (may be sf or tibble) with one or more rows per zone.
#' @param idcol Name of the column holding the unique zone ID (e.g. "LSOA21CD").
#' @param path Output folder for the JSON files; created if missing, provided
#'   the parent "outputdata" folder exists.
#' @param zip Logical. If TRUE, the individual JSON files are bundled into
#'   `<idcol>_json.zip` inside `path` and the loose files removed.
#' @param rounddp Number of decimal places to round numeric columns to; 0
#'   converts to integer.
#' @param dataframe JSON orientation, "rows" or "columns"; passed to `yyjsonr`.
#' @param reduce Logical. If TRUE, column names are shortened via
#'   `reduce_name_length()` to reduce file size, with the mapping saved to
#'   `names_lookup.csv`.
#' @param na Unused (retained for backwards compatibility; NA handling is
#'   whatever `yyjsonr` does by default, i.e. JSON null).
#' @param parallel Logical. If TRUE and `future`/`furrr` are installed, files
#'   are written in parallel using a multisession plan.
#' @param workers Number of parallel workers; defaults to available cores - 1.
#' @return The output `path` (or the zip file path when `zip = TRUE`), after
#'   writing all files as a side effect.
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
        x[[i]] = as.integer(round(x[[i]]))
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
  # temp_json_dir equals path when zip = FALSE, so the zip is built from the
  # files actually written
  paths = file.path(temp_json_dir,paste0(names(json),".json"))

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

#' Shorten column names to reduce JSON file size
#'
#' @description Abbreviates column names by splitting on "_" and keeping the
#'   first letter of each non-numeric part (numbers are kept whole), so e.g.
#'   "total_emissions_2019" becomes "te2019". Duplicate abbreviations are
#'   disambiguated with a letter prefix (B, C, ...). Long names make the
#'   per-zone JSON files larger, so `export_zone_json(reduce = TRUE)` uses this
#'   and saves the mapping as `names_lookup.csv`.
#' @param x Character vector of original column names.
#' @return A data frame with columns `x` (original names) and `y` (shortened
#'   names), plus `dup`/`occurrences` bookkeeping columns when duplicates arise.
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

