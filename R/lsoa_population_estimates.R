# Build LSOA Population for ONS Data
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates

dowload_lsoa_population <- function(path = file.path(data_path(),"population")){
  if(!dir.exists(path)){
    dir.create(path)
  }

  base_url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/"

  url_2020 = "mid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"
  url_2019 = "mid2019sape22dt2/sape22dt2mid2019lsoasyoaestimatesunformatted.zip"
  url_2018 = "mid2018sape21dt1a/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip"
  url_2017 = "mid2017/sape20dt1mid2017lsoasyoaestimatesformatted.zip"
  url_2016 = "mid2016/sape20dt1mid2016lsoasyoaestimatesformatted.zip"
  url_2015 = "mid2015/sape20dt1mid2015lsoasyoaestimatesformatted.zip"
  url_2014 = "mid2014/sape20dt1mid2014lsoasyoaestimatesformatted.zip"
  url_2013 = "mid2013/sape20dt1mid2013lsoasyoaestimatesformatted.zip"
  url_2012 = "mid2012/sape20dt1mid2012lsoasyoaestimatesformatted.zip"
  url_2011 = "mid2011/rftmid2011lsoatable.zip"
  url_200211 = "mid2002tomid2011persons/rftlsoaunformattedtablepersons.zip"

  download.file(paste0(base_url,url_2020), destfile = file.path(path,"pop2020.xlsx"))
  download.file(paste0(base_url,url_2019), destfile = file.path(path,"pop2019.zip"))
  download.file(paste0(base_url,url_2018), destfile = file.path(path,"pop2018.zip"))
  download.file(paste0(base_url,url_2017), destfile = file.path(path,"pop2017.zip"))
  download.file(paste0(base_url,url_2016), destfile = file.path(path,"pop2016.zip"))
  download.file(paste0(base_url,url_2015), destfile = file.path(path,"pop2015.zip"))
  download.file(paste0(base_url,url_2014), destfile = file.path(path,"pop2014.zip"))
  download.file(paste0(base_url,url_2013), destfile = file.path(path,"pop2013.zip"))
  download.file(paste0(base_url,url_2012), destfile = file.path(path,"pop2012.zip"))
  download.file(paste0(base_url,url_2011), destfile = file.path(path,"pop2011.zip"))
  download.file(paste0(base_url,url_200211), destfile = file.path(path,"pop200211.zip"))
}



build_lsoa_population <- function(path = file.path(data_path(),"population")){

  # 2020
  dir.create(file.path(tempdir(),"pop"))
  unzip(file.path(path,"sape22dt2mid2020lsoasyoaestimatesunformatted.zip"),
        exdir = "tmp")
  pop20 <- readxl::read_excel("tmp/SAPE22DT2-mid-2020-lsoa-syoa-estimates-unformatted.xlsx",
                              sheet = "Mid-2020 Persons")
  unlink(file.path(tempdir(),"pop"), recursive = TRUE)
  pop20 <- as.data.frame(pop20)
  names(pop20) <- pop20[4,]
  pop20 <- pop20[5:nrow(pop20),]

  # 2019
  dir.create(file.path(tempdir(),"pop"))
  unzip(file.path(path,"sape22dt2mid2019lsoasyoaestimatesunformatted.zip"),
        exdir = "tmp")
  pop19 <- readxl::read_excel("tmp/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx",
                              sheet = "Mid-2019 Persons")
  unlink(file.path(tempdir(),"pop"), recursive = TRUE)
  pop19 <- as.data.frame(pop19)
  names(pop19) <- pop19[4,]
  pop19 <- pop19[5:nrow(pop19),]

  # 2018
  dir.create(file.path(tempdir(),"pop"))
  unzip(file.path(path,"pop2018.zip"),
        exdir = "tmp")
  pop18 <- readxl::read_excel("tmp/SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx",
                              sheet = "Mid-2018 Persons")
  unlink(file.path(tempdir(),"pop"), recursive = TRUE)
  pop18 <- as.data.frame(pop18)
  names(pop18) <- pop18[4,]
  pop18 <- pop18[5:nrow(pop18),]

  # 2017
  dir.create(file.path(tempdir(),"pop"))
  unzip(file.path(path,"pop2017.zip"),
        exdir = "tmp")
  pop17 <- readxl::read_excel("tmp/SAPE20DT1-mid-2017-lsoa-syoa-estimates-formatted.XLS",
                              sheet = "Mid-2017 Persons")
  unlink(file.path(tempdir(),"pop"), recursive = TRUE)
  pop17 <- as.data.frame(pop17)
  names(pop17) <- pop17[4,]
  pop17 <- pop17[5:nrow(pop17),]

  # 2016
  dir.create(file.path(tempdir(),"pop"))
  unzip(file.path(path,"pop2016.zip"),
        exdir = "tmp")
  pop16 <- readxl::read_excel("tmp/SAPE20DT1-mid-2016-lsoa-syoa-estimates-formatted.XLS",
                              sheet = "Mid-2016 Persons")
  unlink(file.path(tempdir(),"pop"), recursive = TRUE)
  pop16 <- as.data.frame(pop16)
  names(pop16) <- pop16[4,]
  pop16 <- pop16[5:nrow(pop16),]

  # 2015
  dir.create(file.path(tempdir(),"pop"))
  unzip(file.path(path,"pop2015.zip"),
        exdir = "tmp")
  pop15 <- readxl::read_excel("tmp/SAPE20DT1-mid-2015-lsoa-syoa-estimates-formatted.XLS",
                              sheet = "Mid-2015 Persons")
  unlink(file.path(tempdir(),"pop"), recursive = TRUE)
  pop15 <- as.data.frame(pop15)
  names(pop15) <- pop15[4,]
  pop15 <- pop15[5:nrow(pop15),]

  # 2014
  dir.create(file.path(tempdir(),"pop"))
  unzip(file.path(path,"pop2014.zip"),
        exdir = "tmp")
  pop14 <- readxl::read_excel("tmp/SAPE20DT1-mid-2014-lsoa-syoa-estimates-formatted.XLS",
                              sheet = "Mid-2014 Persons")
  unlink(file.path(tempdir(),"pop"), recursive = TRUE)
  pop14 <- as.data.frame(pop14)
  names(pop14) <- pop14[4,]
  pop14 <- pop14[5:nrow(pop14),]

  # 2013
  dir.create(file.path(tempdir(),"pop"))
  unzip(file.path(path,"pop2013.zip"),
        exdir = "tmp")
  pop13 <- readxl::read_excel("tmp/SAPE20DT1-mid-2013-lsoa-syoa-estimates-formatted.XLS",
                              sheet = "Mid-2013 Persons")
  unlink(file.path(tempdir(),"pop"), recursive = TRUE)
  pop13 <- as.data.frame(pop13)
  names(pop13) <- pop13[4,]
  pop13 <- pop13[5:nrow(pop13),]

  # 2012
  dir.create(file.path(tempdir(),"pop"))
  unzip(file.path(path,"pop2012.zip"),
        exdir = "tmp")
  pop12 <- readxl::read_excel("tmp/SAPE20DT1-mid-2012-lsoa-syoa-estimates-formatted.XLS",
                              sheet = "Mid-2012 Persons")
  unlink(file.path(tempdir(),"pop"), recursive = TRUE)
  pop12 <- as.data.frame(pop12)
  names(pop12) <- pop12[4,]
  pop12 <- pop12[5:nrow(pop12),]

  # 2011
  dir.create(file.path(tempdir(),"pop"))
  unzip(file.path(path,"pop2011.zip"),
        exdir = "tmp")
  pop11 <- readxl::read_excel("tmp/mid-2011-lsoa-quinary-estimates.xls",
                              sheet = "Mid-2011 Persons")
  unlink(file.path(tempdir(),"pop"), recursive = TRUE)
  pop11 <- as.data.frame(pop11)
  names(pop11) <- pop11[3,]
  pop11 <- pop11[4:nrow(pop11),]

  # 2002 - 2011
  dir.create(file.path(tempdir(),"pop"))
  unzip(file.path(path,"pop2002-2011.zip"),
        exdir = "tmp")
  pop02 <- readxl::read_excel("tmp/SAPE8DT1a-LSOA-syoa-unformatted-persons-mid2002-to-mid2006.xls",
                              sheet = "Mid-2002")
  pop03 <- readxl::read_excel("tmp/SAPE8DT1a-LSOA-syoa-unformatted-persons-mid2002-to-mid2006.xls",
                              sheet = "Mid-2003")
  pop04 <- readxl::read_excel("tmp/SAPE8DT1a-LSOA-syoa-unformatted-persons-mid2002-to-mid2006.xls",
                              sheet = "Mid-2004")
  pop05 <- readxl::read_excel("tmp/SAPE8DT1a-LSOA-syoa-unformatted-persons-mid2002-to-mid2006.xls",
                              sheet = "Mid-2005")
  pop06 <- readxl::read_excel("tmp/SAPE8DT1a-LSOA-syoa-unformatted-persons-mid2002-to-mid2006.xls",
                              sheet = "Mid-2006")
  pop07 <- readxl::read_excel("tmp/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls",
                              sheet = "Mid-2007")
  pop08 <- readxl::read_excel("tmp/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls",
                              sheet = "Mid-2008")
  pop09 <- readxl::read_excel("tmp/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls",
                              sheet = "Mid-2009")
  pop10 <- readxl::read_excel("tmp/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls",
                              sheet = "Mid-2010")
  unlink(file.path(tempdir(),"pop"), recursive = TRUE)
  pop02 <- as.data.frame(pop02)
  pop03 <- as.data.frame(pop03)
  pop04 <- as.data.frame(pop04)
  pop05 <- as.data.frame(pop05)
  pop06 <- as.data.frame(pop06)
  pop07 <- as.data.frame(pop07)
  pop08 <- as.data.frame(pop08)
  pop09 <- as.data.frame(pop09)
  pop10 <- as.data.frame(pop10)

  pop_02_10 <- list(pop02, pop03, pop04, pop05, pop06, pop07, pop08, pop09, pop10)
  names(pop_02_10) <- c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)
  pop_02_10 <- bind_rows(pop_02_10, .id = "year")
  names(pop_02_10)[96] = c("90+")
  names(pop_02_10)[6:95] <- gsub("p","",names(pop_02_10)[6:95] )

  pop_2012_2020 <- list(pop12, pop13, pop14, pop15, pop16, pop17, pop18, pop19, pop20)
  names(pop_2012_2020) <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)
  pop_2012_2020 <- bind_rows(pop_2012_2020, .id = "year")

  pop_02_10 <- pop_02_10[,c("year","LSOA11CD","all_ages",as.character(0:89),"90+")]
  pop_2012_2020 <- pop_2012_2020[,c("year","Area Codes" ,"All Ages",as.character(0:89),"90+")]
  names(pop_2012_2020)[1:3] <- c("year","LSOA11CD","all_ages")

  pop_non_2011 <- rbind(pop_02_10, pop_2012_2020)

  pop_non_2011 <- pop_non_2011[substr(pop_non_2011$LSOA11CD,1,3) %in% c("E01","W01"),]
  pop_non_2011$`90+` <- as.numeric(pop_non_2011$`90+`)

  # Add age bands
  pop_non_2011$`0-4` <- rowSums(pop_non_2011[,as.character(0:4)])
  pop_non_2011$`5-9`<- rowSums(pop_non_2011[,as.character(5:9)])
  pop_non_2011$`10-14`<- rowSums(pop_non_2011[,as.character(10:14)])
  pop_non_2011$`15-19`<- rowSums(pop_non_2011[,as.character(15:19)])
  pop_non_2011$`20-24`<- rowSums(pop_non_2011[,as.character(20:24)])
  pop_non_2011$`25-29`<- rowSums(pop_non_2011[,as.character(25:29)])
  pop_non_2011$`30-34`<- rowSums(pop_non_2011[,as.character(30:34)])
  pop_non_2011$`35-39`<- rowSums(pop_non_2011[,as.character(35:39)])
  pop_non_2011$`40-44`<- rowSums(pop_non_2011[,as.character(40:44)])
  pop_non_2011$`45-49`<- rowSums(pop_non_2011[,as.character(45:49)])
  pop_non_2011$`50-54`<- rowSums(pop_non_2011[,as.character(50:54)])
  pop_non_2011$`55-59`<- rowSums(pop_non_2011[,as.character(55:59)])
  pop_non_2011$`60-64`<- rowSums(pop_non_2011[,as.character(60:64)])
  pop_non_2011$`65-69`<- rowSums(pop_non_2011[,as.character(65:69)])
  pop_non_2011$`70-74`<- rowSums(pop_non_2011[,as.character(70:74)])
  pop_non_2011$`75-79`<- rowSums(pop_non_2011[,as.character(75:79)])
  pop_non_2011$`80-84`<- rowSums(pop_non_2011[,as.character(80:84)])
  pop_non_2011$`85-89`<- rowSums(pop_non_2011[,as.character(85:89)])


  pop_all <- pop_non_2011[,c("year","LSOA11CD","all_ages",
                             "0-4","5-9",
                             "10-14","15-19","20-24","25-29",
                             "30-34","35-39","40-44","45-49",
                             "50-54","55-59","60-64","65-69",
                             "70-74","75-79","80-84","85-89","90+")]
  pop_all$all_ages <- as.numeric(pop_all$all_ages)

  pop11 <- pop11[,c(1,4:23)]
  pop11$year <- "2011"
  pop11[2:21] <- lapply(pop11[2:21], as.numeric)#
  names(pop11)[1:2] <- c("LSOA11CD","all_ages")
  pop11 <- pop11[substr(pop11$LSOA11CD,1,3) %in% c("E01","W01"),]


  pop_final <- bind_rows(list(pop_all, pop11))
  pop_final$year <- as.numeric(pop_final$year)

  pop_final <- pop_final[order(pop_final$LSOA11CD,pop_final$year),]

  return(pop_final)

}


