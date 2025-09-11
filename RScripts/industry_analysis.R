library(targets)
library(sf)
library(dplyr)
library(tidyr)
library(tmap)
library(ggplot2)
tar_load(nondomestic_gas)
tar_load(nondomestic_electricity)

#dat - jobs data
#classif - classifications
bounds = read_sf("../inputdata/boundaries/MSOA_2021_EW_BSC_V2_-246042110782396072.gpkg")
bounds = bounds[,"MSOA21CD"]

dat = left_join(dat, classif, by = c("industry_name"))
dat = dat[!is.na(dat$category),]

dat$category = gsub("Not manufacturing industry - ","",dat$category)
dat$category = gsub("Not manufacturing industry","non_manufacturing",dat$category)
dat$category = gsub(" Power Needs","",dat$category)

dat_summary = dat |>
  group_by(MSOA21CD, category) |>
  summarise(count = sum(count))

dat_summary = pivot_wider(dat_summary, names_from = "category", values_from = "count")
dat_summary$all_jobs = rowSums(dat_summary[,2:ncol(dat_summary)])

# Latest year energy
nondomestic_gas = nondomestic_gas[,c("MSOA","metres","total_gas_kwh","year")]
nondomestic_electricity = nondomestic_electricity[,c("MSOA","metre_type","metres","total_elec_kwh","year")]

nondomestic_gas = nondomestic_gas[nondomestic_gas$year == 2021,]
nondomestic_electricity = nondomestic_electricity[nondomestic_electricity$year == 2021,]

nondomestic_gas = nondomestic_gas[nondomestic_gas$MSOA != "All MSOAs",]
nondomestic_electricity = nondomestic_electricity[nondomestic_electricity$MSOA != "All MSOAs",]


dat_summary2 = left_join(dat_summary, nondomestic_electricity[,c("MSOA","total_elec_kwh")], by = c("MSOA21CD" = "MSOA"))
dat_summary2 = left_join(dat_summary2, nondomestic_gas[,c("MSOA","total_gas_kwh")], by = c("MSOA21CD" = "MSOA"))

# Job ratios
dat_summary2$Agriculture = dat_summary2$Agriculture / dat_summary2$all_jobs
dat_summary2$Extractives = dat_summary2$Extractives / dat_summary2$all_jobs
dat_summary2$Flexible = dat_summary2$Flexible / dat_summary2$all_jobs
dat_summary2$Inflexible = dat_summary2$Inflexible / dat_summary2$all_jobs
dat_summary2$Unclear = dat_summary2$Unclear / dat_summary2$all_jobs
dat_summary2$non_manufacturing = dat_summary2$non_manufacturing / dat_summary2$all_jobs

#flexible use
dat_summary2$flex_elec_mwh = dat_summary2$total_elec_kwh * dat_summary2$Flexible / 1000
dat_summary2$flex_gas_mwh = dat_summary2$total_gas_kwh * dat_summary2$Flexible / 1000

dat_summary2 = dat_summary2[order(dat_summary2$flex_elec_mwh, decreasing = TRUE),]

dat_summary2$flex_elec_mwh_cum = cumsum(dat_summary2$flex_elec_mwh)
dat_summary2$elec_rank = 1:nrow(dat_summary2)

ggplot(dat_summary2, aes(x = elec_rank, y = flex_elec_mwh_cum)) +
  geom_line() +
  xlab("MSOA") +
  ylab("Cumulative Electricity MWh")

ggsave("plots/industrial_cumulative_flex_electricity.png")

dat_summary2 = dat_summary2[order(dat_summary2$flex_gas_mwh, decreasing = TRUE),]
dat_summary2$flex_gas_mwh_cum = cumsum(dat_summary2$flex_gas_mwh)
dat_summary2$gas_rank = 1:nrow(dat_summary2)

ggplot(dat_summary2, aes(x = gas_rank, y = flex_gas_mwh_cum)) +
  geom_line() +
  xlab("MSOA") +
  ylab("Cumulative Gas MWh")

ggsave("plots/industrial_cumulative_flex_gas.png")



plot_data = left_join(bounds, dat_summary2, by = c("MSOA21CD"))

plot_data$flex_percent = plot_data$Flexible * 100

m_elec = tm_shape(plot_data) +
  tm_fill(fill = "flex_elec_mwh",
          fill.scale = tm_scale_intervals(values = "-tableau.classic_orange_blue",
                                          style = "quantile",
                                          n = 10),
          fill.legend = tm_legend(
            "Annual Total Electricity MWh (2021)\nFlexible Industries",
            position = tm_pos_on_top(pos.h = "left", pos.v = "top"),
            bg.color = "white"))

tmap_save(m_elec,"plots/industrial_flex_electricity.png")


m_gas = tm_shape(plot_data) +
  tm_fill(fill = "flex_gas_mwh",
          fill.scale = tm_scale_intervals(values = "-tableau.classic_orange_blue",
                                          style = "quantile",
                                          n = 10),

                                          #style = "fixed",
                                          #breaks = c(0, 10, 50, 100, 200, 400, 800, 1000, 5000, 110000)),
          fill.legend = tm_legend(
            "Annual Total Gas MWh (2021)\nFlexible Industries",
            position = tm_pos_on_top(pos.h = "left", pos.v = "top"),
            bg.color = "white"))

tmap_save(m_gas,"plots/industrial_flex_gas.png")


m_jobs = tm_shape(plot_data) +
  tm_fill(fill = "flex_percent",
          fill.scale = tm_scale_intervals(values = "-tableau.classic_orange_blue",
                                          style = "fixed",
                                          breaks = c(0,1,2,5,10,20,30,40,43)),

          #style = "fixed",
          #breaks = c(0, 10, 50, 100, 200, 400, 800, 1000, 5000, 110000)),
          fill.legend = tm_legend(
            "Percentage of jobs in\nFlexible Industries",
            position = tm_pos_on_top(pos.h = "left", pos.v = "top"),
            bg.color = "white"))

tmap_save(m_jobs,"plots/industrial_flex_jobs.png")

# Sector maps

sectors = classif$industry_name[classif$category == "Flexible Power Needs"]

for(i in 1:length(sectors)){
  dat_sub = dat[dat$industry_name == sectors[i], ]
  plot_data = left_join(bounds, dat_sub, by = c("MSOA21CD"))

  sec_lable = sectors[i]
  sec_lable = substr(sec_lable,3,nchar(sec_lable))
  sec_lable = gsub(" Manufacture of ","",sec_lable)
  if(nchar(sec_lable) > 30){
    sec_lable = substr(sec_lable,1,30)
  }


  m_sector = tm_shape(plot_data) +
    tm_fill(fill = "count",
            fill.scale = tm_scale_intervals(values = "-tableau.classic_orange_blue",
                                            style = "fisher",
                                            n = 10),
            fill.legend = tm_legend(
              paste0("Jobs in ",sec_lable),
              position = tm_pos_on_top(pos.h = "left", pos.v = "top"),
              bg.color = "white"))

  tmap_save(m_sector,paste0("plots/industrial_jobs_",gsub(" ","-",sec_lable),".png"))

}

