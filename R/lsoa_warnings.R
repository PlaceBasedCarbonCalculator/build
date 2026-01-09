make_lsoa_warnings = function(lookup_lsoa_2011_21,lookup_dz_2011_22, population){

  lookup_dz_2011_22 = lookup_dz_2011_22[,"LSOA21CD"]
  lookup_dz_2011_22 = lookup_dz_2011_22[!duplicated(lookup_dz_2011_22$LSOA21CD),]

  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND != "U",]
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA21CD","CHGIND")]
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[!duplicated(lookup_lsoa_2011_21$LSOA21CD),]

  lookup_dz_2011_22$warningcode = 1L

  lookup_lsoa_2011_21$warningcode = ifelse(lookup_lsoa_2011_21$CHGIND == "X", 2L, NA_integer_)
  lookup_lsoa_2011_21$warningcode = ifelse(lookup_lsoa_2011_21$CHGIND == "M", 3L, lookup_lsoa_2011_21$warningcode)
  lookup_lsoa_2011_21$warningcode = ifelse(lookup_lsoa_2011_21$CHGIND == "S", 4L, lookup_lsoa_2011_21$warningcode)

  lookup_lsoa_2011_21$CHGIND = NULL

  population = population[,c("LSOA21CD","all_ages")]
  population = population[population$all_ages == 0,]
  population = population[!duplicated(population$LSOA21CD),]
  population$warningcode = 5L
  population$all_ages = NULL

  warningcode = rbind(lookup_dz_2011_22, lookup_lsoa_2011_21,population)
  warningcode = warningcode[order(warningcode$LSOA21CD),]

  # Bespoke warnings

  extra = read.csv("data/lsoa_warnings.csv")
  warningcode = rbind(warningcode, extra)


  warningcode
}
