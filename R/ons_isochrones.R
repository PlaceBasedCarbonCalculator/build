#' Load the ONS walking isochrones for Output Areas
#'
#' @description Unzips and reads each ONS isochrone shapefile (15/30/45/60
#'   minute walking areas around OA centroids; `iso_cutoff` in seconds) and
#'   binds them into one layer. Files keyed on 2011 OAs are relabelled to the
#'   common `OA21CD` column so binding is order-independent; their codes
#'   simply won't match the 2021 lookups downstream. Used by the
#'   `ons_isochrones` target, input to the `access_poi_iso_*` targets.
#' @param path Folder of ONS isochrone zip files (secure data).
#' @return An sf data frame with `OA21CD`, `iso_cutoff` and isochrone
#'   polygons.
#' @keywords internal
load_ons_isochrones = function(path){
  fls = list.files(path, full.names = TRUE, pattern = "zip")
  isos = list()
  for(i in seq_along(fls)){
    dir.create(file.path(tempdir(),"isochrones"))
    unzip(fls[i], exdir = file.path(tempdir(),"isochrones"))
    fl = list.files(file.path(tempdir(),"isochrones"), full.names = TRUE, pattern = "shp")
    iso = sf::st_read(fl)
    if("OA21CD" %in% names(iso)){
      iso = iso[,c("OA21CD","iso_cutoff")]
    } else {
      # Older releases key on 2011 OAs; use one column name so files bind
      # consistently regardless of order (2011 codes won't match the
      # 2021-based lookups downstream and are ignored there)
      iso = iso[,c("OA11CD","iso_cutoff")]
      names(iso)[names(iso) == "OA11CD"] = "OA21CD"
    }
    isos[[i]] = iso
    unlink(file.path(tempdir(),"isochrones"), recursive = TRUE)
  }
  isos = bind_sf(isos)
  isos
}
