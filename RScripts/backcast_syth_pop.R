library(targets)

tar_load(dwellings_type_backcast)
tar_load(census21_synth_households)
tar_load(population)

year = 2019

population = population[,c("LSOA21CD","year","households_est","all_properties")]
population = population[population$year == year,]

dwellings_type_backcast = dwellings_type_backcast[dwellings_type_backcast$year == year,]

dwellings_type_backcast$Detached = dwellings_type_backcast$house_detached + dwellings_type_backcast$bungalow + dwellings_type_backcast$unknown
dwellings_type_backcast$Semi = dwellings_type_backcast$house_semi + dwellings_type_backcast$annexe
dwellings_type_backcast$Terraced = dwellings_type_backcast$house_terraced
dwellings_type_backcast$Flat = dwellings_type_backcast$flat_mais
dwellings_type_backcast$caravan = dwellings_type_backcast$caravan_houseboat_mobilehome

dwellings_type_backcast = dwellings_type_backcast[,c("year","lsoa21cd","Detached","Semi","Terraced","Flat","caravan")]

census21_synth_households = dplyr::group_split(census21_synth_households, LSOA)
dwellings_type_backcast = dplyr::group_split(dwellings_type_backcast, lsoa21cd)
population = dplyr::group_split(population, LSOA21CD)

cen = census21_synth_households[[1]]
pop = population[[1]]
bk = dwellings_type_backcast[[1]]



foo = population[population$LSOA21CD == "E01000001",]
plot(foo$year, foo$all_ages, type = "l", ylim = c(0,1800))
lines(foo$year, foo$households_est, col = "red")
lines(foo$year, foo$all_properties, col = "green")


# lsoa = "E01034220"
#
# bk = dwellings_type_backcast[dwellings_type_backcast$lsoa21cd == lsoa,]
# pop = census21_synth_households[census21_synth_households$LSOA == lsoa,]
#
# bk$Detached = bk$house_detached + bk$bungalow + bk$unknown
# bk$Semi = bk$house_semi + bk$annexe
# bk$Terraced = bk$house_terraced
# bk$Flat = bk$flat_mais
# bk$caravan = bk$caravan_houseboat_mobilehome
#
# bk = bk[,c("year","lsoa21cd","Detached","Semi","Terraced","Flat","caravan")]
#
# pop_long = pop[rep(1:nrow(pop), times = pop$households),]
# pop_long$households = NULL
# pop_long = dplyr::group_split(pop_long, Acc5)
#
# pops = list()
#
# yrs = seq(2019,2010)
#
# for(i in 1:length(yrs)){
#   # Step back each year and ajust population
#   bk_sub = bk[bk$year == yrs[i],]
#   pop_long2 = list()
#   for(j in seq(1, length(pop_long))){
#     pop_sub = pop_long[[j]]
#     cnt = bk_sub[[as.character(pop_sub$Acc5[1])]]
#     if(cnt > 0){
#       if(cnt <= nrow(pop_sub)){
#         pop_long2[[j]] = pop_sub[sample(seq(1, nrow(pop_sub)), cnt),]
#       } else {
#         pop_long2[[j]] = rbind(pop_sub, pop_sub[sample(seq(1, nrow(pop_sub)), cnt - nrow(pop_sub)),])
#       }
#
#     } else {
#       pop_long2[[j]] = NULL
#     }
#   }
#   pops[[i]] = dplyr::bind_rows(pop_long2)
# }
#
# # Make

