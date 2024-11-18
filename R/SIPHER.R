load_SIPHER = function(path = file.path(parameters$path_secure_data,"SIPHER Syntheic Population")){

  dir = file.path(tempdir(),"SIPHER")
  dir.create(dir)
  unzip(file.path(path,"SIPHER_V1.zip"), exdir = dir)

  readLines(file.path(dir,"UKDA-9277-csv/csv/sp_ind_wavek_census2011_est2020_8cons.csv"), n = 10)

  sipher = readr::read_csv(file.path(dir,"UKDA-9277-csv/csv/sp_ind_wavek_census2011_est2020_8cons.csv"))

  unlink(dir, recursive = TRUE)

  sipher

}


load_US = function(path = file.path(parameters$path_secure_data,"Understanding Society/Safeguarded")){

  dir = file.path(tempdir(),"US")
  dir.create(dir)
  unzip(file.path(path,"US Wave 1-13 2009-2022 and BHPS Wave 1-180 1991-2009_SPSS_V1.zip"), exdir = dir)

  fls = list.files(file.path(dir,"UKDA-6614-spss","spss","spss25","ukhls"), full.names = FALSE,
                   pattern = "k_") # Only Wave K for join to SIPHER
  nms = gsub("k_","",gsub(".sav","",fls))

  us = list()

  for(i in 1:length(fls)){
    us[[i]] = foreign::read.spss(file.path(dir,"UKDA-6614-spss","spss","spss25","ukhls",fls[i]),
                                 to.data.frame=TRUE)
  }

  names(us) = nms

  # Cross Wave
  hhrel = foreign::read.spss(file.path(dir,"UKDA-6614-spss","spss","spss25","ukhls","xhhrel.sav")) #Family matrix file
  wavedat = foreign::read.spss(file.path(dir,"UKDA-6614-spss","spss","spss25","ukhls","xwavedat.sav")) #Stable characteristics of individuals
  waveid = foreign::read.spss(file.path(dir,"UKDA-6614-spss","spss","spss25","ukhls","xwaveid.sav")) #Some basic sampling information from each wave

  us = c(us, list(hhrel = hhrel, wavedat = wavedat, waveid = waveid))


  # callrec Includes information about each interview call made to each household, such  as outcome of the cal
  # child Childcare, consents and school information of all children (0-15 years) in the  household
  # chmain Information about child maintenance arrangements
  # egoalt Kin and other relationships between pairs of individuals in the household.
  #         This is a derived data file based on information collected in the household
  #         grid about relationships between household members.
  # hhresp Substantive data collected from responding households
  # hhsamp information about each household that the interviewer
  #         collects about the condition of the property, neighbourhood, interview
  #          outcome and so on
  # income  This file contains reports of unearned income and state benefits for each
  #         individual.
  # indall  Household grid data for all persons in household, including children and non-respondents.
  # indresp Substantive data collected from responding adults (16+) including proxies
  # indsamp Includes current interview outcome for anyone enumerated in the last
  #         interview wave, for example, whether they have responded, only
  #         enumerated, couldn’t be contacted or refused, or were ineligible
  # newborn basic information about new born children such as brithweight
  # parstyle , information about parenting styles was collected
  # youth   Substantive data from youth questionnaire.

  us
}


join_sipher_us = function(sipher, us){

  us_households = us$hhresp
  us_income = us$income
  us_adults = us$indresp
  us_children = us$youth

  us_adults = us_adults[,c(
    "pidp","k_hidp","k_pno","k_sex","k_dvage","k_birthy","k_nchunder16",
    "k_jbstat","k_caruse","k_drive","k_netpuse","k_onlinebuy",
    "k_ukborn","k_plbornc","k_qfhigh","k_j1none","k_racel_dv",
    "k_currmstat","k_qualhigh","K_jbhas","k_jbterm1","k_scsf1",
    "k_marstat","k_employ","k_prearnw_","k_fimngrs_dv","k_fimnlabgrs_dv",
    "k_ff_jbsemp",
    "k_jbft_dv","k_jbes2000","k_jbseg_dv","k_jbrgsc_dv","k_jbnssec_dv","k_jbnssec8_dv",
    "k_hhtype_dv","k_hrpid","k_isced11_dv"
  )]

  addr = attributes(us_adults)

  foo = list()
  for(i in 1:ncol(us_adults)){
    tab = as.data.frame(table(us_adults[[i]]))
    cnt = tab$Freq[tab$Var1 %in% c("inapplicable","Not mentioned","missing","Inapplicable")]
    cnt = sum(cnt)
    foo[[i]] = cnt
  }
  foo2 = unlist(foo)
  foo2 = foo2/32008
  round(quantile(foo2, seq(0,0.2,0.01)),3)

  per = 0.5
  labs = addr$variable.labels[foo2 < per]
  us_adults = us_adults[,foo2 < per]
  attributes(us_adults) = list(names = names(us_adults),
                               class = class(us_adults),
                               row.names = rownames(us_adults),
                               variable.labels = labs)

  summary(sipher$pidp %in% us_adults$pidp)
  summary(sipher$pidp %in% us_households$pidp)


}


