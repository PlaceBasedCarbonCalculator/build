#' Load Scottish INSPIRE cadastral parcels
#'
#' @description Reads every zipped INSPIRE cadastral parcel shapefile (one per
#'   local authority) in `path` and binds them into one layer tagged with the
#'   source zip name as `local_authority`. Unlike the England & Wales loader,
#'   no grid-artefact cleaning is applied. Used by the `inspire_scotland`
#'   target, which feeds `combine_os_osm_buildings()`.
#' @param path Folder of INSPIRE Scotland zip files.
#' @return An sf data frame with `local_authority`, `inspireid` and geometry.
#' @keywords internal
load_inspire_scotland = function(path = file.path(parameters$path_data,"INSPIRE_scotland")) {
  zips = list.files(path, pattern = ".zip", full.names = TRUE)
  polys <- list()
  for(i in 1:length(zips)){
    dir.create(file.path(tempdir(),"inspire"))
    unzip(zips[i], exdir = file.path(tempdir(),"inspire"))
    fl = list.files(file.path(tempdir(),"inspire"), pattern = "_bng.shp$")
    poly <- sf::read_sf(file.path(tempdir(),"inspire",fl))
    poly <- poly[,c("inspireid")]
    message(Sys.time()," ",zips[i]," ",nrow(poly)," polygons")
    unlink(file.path(tempdir(),"inspire"), recursive = TRUE)
    polys[[i]] = poly
  }
  names(polys) = gsub(".zip","",zips)

  polys = dplyr::bind_rows(polys, .id = "local_authority")
  polys


}

# load_inspire() (England & Wales) has moved to the LandOwnership repo, which
# owns all UPRN / address / land-parcel work as of July 2026 - see
# LandOwnership/pipeline/R/inspire_uprn_lookup.R::load_inspire_clean(), which
# runs the same 500m grid-artefact repair in parallel over the 2026 release.
# The `inspire` target here reads its output (R/landownership_resources.R).
# Scotland is not covered there, so load_inspire_scotland() above stays.
