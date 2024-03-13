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
      iso = iso[,c("OA11CD","iso_cutoff")]
      names(iso) = c("OA11CD","iso_cutoff","geometry")
    }
    isos[[i]] = iso
    unlink(file.path(tempdir(),"isochrones"), recursive = TRUE)
  }
  isos = bind_sf(isos)
  isos
}
