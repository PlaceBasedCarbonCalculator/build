library(targets)
library(sf)
library(tmap)
tmap_mode("view")

tar_load(buildings)
tar_load(parameters)
epc_dom = readRDS(file.path(parameters$path_data,"epc/GB_domestic_epc.Rds"))
epc_nondom = readRDS(file.path(parameters$path_data,"epc/GB_nondomestic_epc.Rds"))

epc_dom = st_transform(epc_dom, 27700)
epc_nondom = st_transform(epc_nondom, 27700)

tar_load(bounds_lsoa21_super_generalised)

zone = bounds_lsoa21_super_generalised[bounds_lsoa21_super_generalised$LSOA21CD == "E01011364",]
zone = st_buffer(zone, 2000)

bsub = buildings[zone,]
edsub = epc_dom[zone,]
endsub = epc_nondom[zone,]

charNA = function(x, y){
  ifelse(is.na(x),TRUE,x!=y)
}

bsub = bsub[charNA(bsub$building,"no"),]
bsub$building[bsub$building == "yes"] = NA
bsub$building[bsub$building == "garages"] = "garage"

edsub = edsub[,c("UPRN")]
endsub = endsub[,c("UPRN","type")]
edsub$type_domestic = "domestic"
names(endsub)[2] = "type_nondomestic"

bsub$id = 1:nrow(bsub)

dominter = st_intersects(bsub, edsub)
bsub$domestic = lengths(dominter) > 0
nondominter = st_intersects(bsub, endsub)
bsub$nondomestic = purrr::map_chr(nondominter, function(x){
  if(length(x) == 0){
    return(NA_character_)
  }
  if(length(x) == 1){
    return(endsub$type_nondomestic[x])
  } else{
    return("mixed non-domestic")
  }
})

bsub$use = purrr::map2_chr(bsub$domestic, bsub$nondomestic, function(x,y){
  if(x & is.na(y)){
    return("domestic")
  }
  if(x & !is.na(y)){
    return("mixed domestic and non-domestic")
  }
  if(!x & !is.na(y)){
    return(y)
  }
  if(!x & is.na(y)){
    return("unknown")
  }
})

bsub$use2 = purrr::map2_chr(bsub$use, bsub$building, function(x,y){
  if(x == "unknown" & !is.na(y)){
    if(y %in% c("house","terrace","semidetached_house","residential","apartments")){
      return("domestic")
    }
    return(y)
  }
  return(x)
})

bsub$use3 = purrr::map_chr(bsub$use2, function(x){
  if(x == "domestic"){
    return("domestic")
  }
  if(x == "unknown"){
    return("unknown")
  }
  if(x == "mixed domestic and non-domestic"){
    return("mixed domestic and non-domestic")
  }
  return("non-domestic")
})


bsub = st_join(bsub, bounds_lsoa21_super_generalised[,c("LSOA21CD")])

bsublist = dplyr::group_split(bsub,  LSOA21CD)

domlsoa = function(x){
  x =  x[x$use3 %in% c("domestic","mixed domestic and non-domestic"),]
  if(nrow(x) == 0){
    return(NULL)
  }
  concaveman::concaveman(sf::st_cast(x,"POINT"),
                             concavity = 1,
                             length_threshold = 100)
}

bsublist2 = purrr::map(bsublist, domlsoa, .progress = TRUE)
bsublist2 = dplyr::bind_rows(bsublist2)

qtm(bsublist2) + qtm(bsub, fill = "use3")


qtm(bsub, fill = "use3")
