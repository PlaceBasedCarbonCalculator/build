central_heating_2021_list = dplyr::group_split(central_heating_2021, LSOA21CD)
central_heating_2011_list = dplyr::group_split(central_heating_2011, LSOA21CD)

ch21 = central_heating_2021_list[[1]]
ch11 = central_heating_2011_list[[1]]

distribute_other_heating = function(ch21, ch11){

  if(length(unique(c(ch21$LSOA21CD,ch11$LSOA21CD))) != 1){
    stop("LSOAs don't match")
  }

  if(nrow(ch21) != 1 & nrow(ch11) != 1){
    stop("muliple rows")
  }

  dat = data.frame(LSOA21CD = ch21$LSOA21CD[1],
                   year = c(2011,2021),
                   oil = c(ch11$oil, ch21$oil),
                   solid_fuel = c(ch11$solid_fuel,
                                  sum(ch21$solid_fuel,ch21$wood)),
                   other = c(ch11$other,
                            sum(ch21$heat_network,ch21$other_central_heating, ch21$renewable_energy)
                   ),
                   two_or_more = c(ch11$two_or_more,
                                   sum(ch21$two_types_inc_renewable_energy, ch21$two_types_no_renewable_energy)
                                   )
                   )

  dat_predict = data.frame(LSOA21CD = ch21$LSOA21CD[1], year = 2012:2020)
  dat_predict$oil = unname(round(predict(lm(oil ~ year, data = dat), newdata = dat_predict)))
  dat_predict$solid_fuel = unname(round(predict(lm(solid_fuel ~ year, data = dat), newdata = dat_predict)))
  dat_predict$other = unname(round(predict(lm(other ~ year, data = dat), newdata = dat_predict)))
  dat_predict$two_or_more = unname(round(predict(lm(two_or_more ~ year, data = dat), newdata = dat_predict)))

  dat_final = rbind(dat[c(1,1),],
                    dat_predict,
                    dat[c(2,2,2,2),]
                    )
  dat_final$year = 2010:2024

  dat_final


}
