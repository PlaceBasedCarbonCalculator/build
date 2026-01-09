library(sf)
library(dplyr)
path = "D:/OneDrive - University of Leeds/Data/OS/Open UPRN/Download_OS+Open+UPRN+2020-2025_2860883.zip"
dir.create(file.path(tempdir(),"uprn"))
unzip(path, exdir = file.path(tempdir(),"uprn"))
files = list.files(file.path(tempdir(),"uprn"), recursive = TRUE)
files = files[grepl(".gpkg",files)]

uprn_all = list()
for(i in 1:length(files)){
  sub = read_sf(file.path(tempdir(),"uprn",files[i]))
  sub = sub[,c("UPRN")]
  sub$year_month = substr(files[i],nchar(files[i]) - 10,nchar(files[i])-5)
  uprn_all[[i]] = sub
}
uprn_all = bind_rows(uprn_all)

uprn_all$year = as.integer(substr(uprn_all$year_month,1,4))

uprn_summary = uprn_all |>
  group_by(UPRN) |>
  summarise(year_min = min(year),
            year_max = max(year),
            geom = last(geom)
            )
