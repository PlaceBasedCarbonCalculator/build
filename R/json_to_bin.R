# Alternative to creating a folder full of JSON: writes a single .bin file of
# brotli-compressed JSON records plus a lookup index, so the website can fetch
# one zone's data with an HTTP range request instead of one file per zone.

#' Pack named JSON strings into a compressed binary plus a lookup index
#'
#' @description Shared workhorse behind `export_zone_bin()` and
#'   `export_boundary_bin()`. Brotli-compresses each JSON string and
#'   concatenates them into a single date-stamped binary file, with a JSON
#'   index (plus gzipped copy) recording each record's byte offset and length
#'   so a client can extract one record with an HTTP range request.
#'
#'   Output files are `data_<name>_<YYYY-MM-DD>.bin`,
#'   `index_<name>_<YYYY-MM-DD>.json` and `index_<name>_<YYYY-MM-DD>.json.gz`.
#'   Older date-stamped files for the same `name` are deleted after a
#'   successful write, so `path` only ever holds the latest version of each
#'   dataset.
#'
#'   The index has two top-level keys: `meta` (dataset `name`, `created` date,
#'   `bin_file` name, `compression`, any caller-supplied fields from `meta`,
#'   and record count `n_zones`) and `zones`, a mapping of record ID to
#'   `offset`, `compressed_length` and `original_length` in bytes.
#' @param json Named character vector of JSON strings; names are the record
#'   IDs (must be unique and non-NA).
#' @param path Output folder for the bin and index files; created if missing,
#'   provided the parent "outputdata" folder exists.
#' @param name Dataset name used in the output file names (e.g. "epc_dom").
#'   Letters, numbers, underscores and hyphens only.
#' @param quality Brotli compression level 0-11. The default 11 gives the
#'   smallest files; use a lower value for very large datasets where
#'   compression time matters more than a few percent of size.
#' @param meta Named list of extra fields to record in the index's `meta`
#'   block (e.g. `list(dataframe = "columns")`).
#' @return Character vector of the three file paths written (bin, index,
#'   gzipped index), invisibly.
#' @keywords internal
write_json_bin <- function(json, path, name, quality = 11, meta = list()){

  if(missing(name) || length(name) != 1 || !grepl("^[A-Za-z0-9_-]+$", name)){
    stop("name must be a single string of letters, numbers, '_' or '-'")
  }

  if(is.null(names(json)) || anyNA(names(json)) || any(names(json) == "")){
    stop("json must be a named character vector with no missing names")
  }
  if(anyDuplicated(names(json))){
    stop("json names contain duplicates: ",
         paste(unique(names(json)[duplicated(names(json))]), collapse = ", "))
  }

  if(!dir.exists(path)){
    if(dir.exists("outputdata")) {
      dir.create(path, recursive = TRUE)
    } else {
      stop("path is not a valid folder")
    }
  }

  created <- format(Sys.Date(), "%Y-%m-%d")
  bin_name <- paste0("data_", name, "_", created, ".bin")
  index_name <- paste0("index_", name, "_", created, ".json")
  bin_path <- file.path(path, bin_name)
  index_path <- file.path(path, index_name)
  index_gz_path <- paste0(index_path, ".gz")

  message("Writing ", bin_name, " ", Sys.time())

  con <- file(bin_path, open = "wb")
  # Don't leave the connection open if compression fails part way through
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  index <- vector("list", length(json))
  offset <- 0

  for(i in seq_along(json)) {

    # Convert to raw bytes
    json_raw <- charToRaw(json[i])

    # Brotli compress
    compressed <- brotli::brotli_compress(json_raw, quality = quality)

    # Record location
    index[[i]] <- list(
      offset = offset,
      compressed_length = length(compressed),
      original_length = length(json_raw)
    )

    # Append to file
    writeBin(compressed, con)

    # Update offset
    offset <- offset + length(compressed)
  }

  close(con)
  names(index) <- names(json)

  jsonlite::write_json(
    list(
      meta = c(
        list(
          name = name,
          created = created,
          bin_file = bin_name,
          compression = "brotli"
        ),
        meta,
        list(n_zones = length(index))
      ),
      zones = index
    ),
    index_path,
    auto_unbox = TRUE
  )

  # Create gzipped version of the index
  R.utils::gzip(
    index_path,
    destname = index_gz_path,
    overwrite = TRUE, remove = FALSE
  )

  # Remove older date-stamped versions of this dataset
  stale <- list.files(path,
    pattern = paste0("^(data|index)_", name, "_\\d{4}-\\d{2}-\\d{2}\\.(bin|json|json\\.gz)$"),
    full.names = TRUE)
  stale <- stale[!stale %in% c(bin_path, index_path, index_gz_path)]
  if(length(stale) > 0){
    unlink(stale)
  }

  invisible(c(bin_path, index_path, index_gz_path))

}


#' Export a data frame as a single binary of compressed JSON plus an index
#'
#' @description Splits a data frame by zone ID, converts each zone's rows to a
#'   JSON string and packs them into a single date-stamped binary plus lookup
#'   index via `write_json_bin()` (see there for the file and index format).
#'   Any sf geometry is dropped and numeric columns are rounded (or converted
#'   to integer when `rounddp = 0`), as in `export_zone_json()`.
#' @param x Data frame (may be sf or tibble) with one or more rows per zone.
#' @param idcol Name of the column holding the unique zone ID (e.g. "LSOA21CD");
#'   must not contain NAs.
#' @param path Output folder for the bin and index files; created if missing,
#'   provided the parent "outputdata" folder exists.
#' @param name Dataset name used in the output file names (e.g. "epc_dom").
#'   Letters, numbers, underscores and hyphens only.
#' @param rounddp Number of decimal places to round numeric columns to; 0
#'   converts to integer.
#' @param dataframe JSON orientation, "rows" or "columns"; passed to `yyjsonr`
#'   and recorded in the index's `meta` block.
#' @param quality Brotli compression level 0-11; see `write_json_bin()`.
#' @return Character vector of the three file paths written (bin, index,
#'   gzipped index), invisibly.
#' @keywords internal
export_zone_bin <- function(x,
                            idcol = "LSOA21CD",
                            path = "outputdata/jsonbin",
                            name,
                            rounddp = 2,
                            dataframe = "rows",
                            quality = 11){

  # Fail fast, before the (slow) JSON conversion; write_json_bin re-checks
  if(missing(name) || length(name) != 1 || !grepl("^[A-Za-z0-9_-]+$", name)){
    stop("name must be a single string of letters, numbers, '_' or '-'")
  }
  dataframe <- match.arg(dataframe, c("rows","columns"))

  if(!inherits(x, "data.frame")){
    stop("x is not a data.frame")
  }

  if(inherits(x, "sf")){
    x <- sf::st_drop_geometry(x)
  }

  if(inherits(x, "tibble") | inherits(x, "tbl")){
    x <- as.data.frame(x)
  }

  if(anyNA(x[[idcol]])){
    stop("idcol '", idcol, "' contains NA values")
  }

  # Round numeric columns
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

  message("Converting JSON ",Sys.time())

  json <- purrr::map(x, convert2json, idcol = idcol,
                     dataframe = dataframe,
                     .progress = TRUE)
  json <- unlist(json)

  write_json_bin(json, path = path, name = name, quality = quality,
                 meta = list(dataframe = dataframe))

}
