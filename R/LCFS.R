# Living Costs and Food Survey

load_LCFS = function(path = "C:/Users/malco/OneDrive - University of Leeds/Data/Living Costs and Food Survey/Safeguarded"){

  dir = file.path(tempdir(),"lcfs")
  dir.create(dir)
  unzip(file.path(path,"LCFS_20192020_V1.zip"), exdir = dir)

  fls = list.files(file.path(dir,"UKDA-8803-spss","spss","spss25"), full.names = FALSE)
  nms = gsub("lcfs_2019_","",fls)
  nms = gsub("_ukanon.sav","",nms)
  nms = gsub("_ukanon201920.sav","",nms)
  nms = gsub("_ukanon_final.sav","",nms)

  lcfs = list()

  for(i in 1:length(fls)){
    lcfs[[i]] = foreign::read.spss(file.path(file.path(dir,"UKDA-8803-spss","spss","spss25",fls[i])),
                                  to.data.frame=TRUE)
  }
  names(lcfs) = nms


  households = lcfs$dvhh
  households$A062

  people = lcfs$dvper


}
