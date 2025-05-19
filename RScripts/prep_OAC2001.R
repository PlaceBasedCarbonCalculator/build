path = "D:/OneDrive/Data/Neighbourhood Statistics/Indicies of Deprivation and Classification/NS 2001 Area Classification of OA"

library(readr)

zips = list.files(path, pattern = ".zip")

res = list()

for(i in 1:length(zips)){
  dir.create(file.path(tempdir(),"OAC"))
  unzip(file.path(path,zips[i]), exdir = file.path(tempdir(),"OAC"))
  fl = list.files(file.path(tempdir(),"OAC"), pattern = ".CSV")
  fl = fl[!grepl("FNOTE_",fl)]
  header = readr::read_csv(file.path(tempdir(),"OAC",fl),
                           n_max = 5,
                           show_col_types = FALSE)
  sub = readr::read_csv(file.path(tempdir(),"OAC",fl),
                        skip = 5,
                        show_col_types = FALSE,
                        col_types = cols(LA_CODE = "c",
                                         GOR_CODE = "c"))
  nms = as.character(header[2,grepl("DATA_VALUE",names(sub))])
  names(sub)[grepl("DATA_VALUE",names(sub))] = nms
  res[[i]] = sub
  unlink(file.path(tempdir(),"OAC"), recursive = TRUE)
}

res_all = dplyr::bind_rows(res)

res_all = res_all[,c("OA_CODE","LSOA_CODE","LA_NAME","Supergroup Name","Group Name","Subgroup Code")]
res_all = res_all[!duplicated(res_all$OA_CODE),]


saveRDS(res_all, "../inputdata/area_classifications/2001/OAC_2001.Rds")
