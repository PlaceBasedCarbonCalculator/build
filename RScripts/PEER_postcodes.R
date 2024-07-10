path = "D:/OneDrive - University of Leeds/Documents/EDRC/PEER"

postcodes = readRDS("D:/OneDrive - University of Leeds/Data/Postcodes/code_point_open_2023.Rds")

library(targets)
library(sf)
library(dplyr)
tar_load(bounds_lsoa_GB_full)

postcodes$postcodes_clean = gsub(" ","",postcodes$postcode)

dat = readLines(file.path(path,"postcodes2.csv"))
dat = dat[2:length(dat)]
dat = gsub('"',"",dat)
dat = as.data.frame(dat)
names(dat) = "postcode"



dat = left_join(dat, postcodes, by = c("postcode" = "postcodes_clean"))
dat = st_as_sf(dat)

dat2 = st_join(dat, bounds_lsoa_GB_full)
dat2 = cbind(st_drop_geometry(dat2), st_coordinates(dat2))
names(dat2)[2] = "postcode_clean"
write.csv(dat2, file.path(path,"postcodes_LSOA2021_DZ2011.csv"), row.names = FALSE)
