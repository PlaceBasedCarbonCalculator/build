library(targets)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
tar_load(synth_households_lcfs_2020)
tar_load(bounds_lsoa_GB_super_generalised)


syth_summary = synth_households_lcfs_2020 |>
  group_by(LSOA, Gorx) |>
  summarise(count = n())

syth_summary2 = pivot_wider(syth_summary, names_from = "Gorx", values_from = "count", id_cols = "LSOA", values_fill  = 0)

syth_summary2$all_households = rowSums(syth_summary2[,2:ncol(syth_summary2)], na.rm = TRUE)

syth_summary2$p_london = syth_summary2$London / syth_summary2$all_households * 100

bounds = left_join(bounds_lsoa_GB_super_generalised, syth_summary2, by = c("LSOA21CD" = "LSOA"))

bounds = bounds[!substr(bounds$LSOA21CD,1,1) == "S",]

bounds$p_yorkshire = bounds$`Yorkshire and the Humber` / syth_summary2$all_households * 100
bounds$p_wales = bounds$`Wales` / syth_summary2$all_households * 100
bounds$p_southeast = bounds$`South East` / syth_summary2$all_households * 100

m1 = tm_shape(bounds) +
  tm_fill("p_london",
          fill.scale = tm_scale_intervals(breaks = seq(0,100,10)),
          fill.legend = tm_legend("% Households from\n London", position = tm_pos_on_top(pos.h = "left", pos.v = "top"))
          )
tmap_save(m1,"plots/percentage_london.png", dpi = 600)

m2 = tm_shape(bounds) +
  tm_fill("p_yorkshire",
          fill.scale = tm_scale_intervals(breaks = seq(0,100,10)),
          fill.legend = tm_legend("% Householdsfrom\n Yorkshire & Humber", position = tm_pos_on_top(pos.h = "left", pos.v = "top"))
  )
tmap_save(m2,"plots/percentage_yorkshire.png", dpi = 600)

m3 = tm_shape(bounds) +
  tm_fill("p_wales",
          fill.scale = tm_scale_intervals(breaks = seq(0,100,10)),
          fill.legend = tm_legend("% Households from\n Wales", position = tm_pos_on_top(pos.h = "left", pos.v = "top"))
  )
tmap_save(m3,"plots/percentage_wales.png", dpi = 600)

m4 = tm_shape(bounds) +
  tm_fill("p_southeast",
          fill.scale = tm_scale_intervals(breaks = seq(0,100,10)),
          fill.legend = tm_legend("% Householdsfrom\n South East", position = tm_pos_on_top(pos.h = "left", pos.v = "top"))
  )
tmap_save(m4,"plots/percentage_southeast.png", dpi = 600)


extract_anne = synth_households_lcfs_2020[synth_households_lcfs_2020$LSOA == "E01004051",]
extract_anne$household_id = sapply(extract_anne$household_id, paste, collapse = " ")

write.csv(extract_anne,"data/E01004051_SynthPop.csv", row.names = FALSE)
