library(targets)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(tmap)

tar_load(lsoa_emissions_all)
tar_load(area_classifications_11_21)
tar_load(vehicle_summary)
tar_load(pt_frequency)
tar_load(income_lsoa_msoa)
tar_load(domestic_electricity)
tar_load(domestic_gas)

bounds = read_sf("data/GB_LSOA21_for_plots.gpkg")
islandBox = read_sf("data/island_boxes.gpkg")
borders = bounds[bounds$hectares > 2000,]

cols = c("Cosmopolitan student neighbourhoods" ='#955123',
         "Ageing rural neighbourhoods" ='#007f42',
         "Prospering countryside life" ='#3ea456',
         "Remoter communities" ='#8aca8e',
         "Rural traits" ='#cfe8d1',
         "Achieving neighbourhoods" ='#00498d',
         "Asian traits" ='#2967ad',
         "Highly qualified professionals" ='#7b99c7',
         "Households in terraces and flats" ='#b9c8e1',
         "Challenged white communities" ='#e3ac20',
         "Constrained renters" ='#edca1a',
         "Hampered neighbourhoods" ='#f6e896',
         "Hard-pressed flat dwellers" ='#fcf5d8',
         "Ageing urban communities" ='#e64c2b',
         "Aspiring urban households" ='#ec773c',
         "Comfortable neighbourhoods" ='#faa460',
         "Endeavouring social renters" ='#fcc9a0',
         "Primary sector workers" ='#fee4ce',
         "Inner city cosmopolitan" = '#f79ff0',
         "Urban cultural mix" ='#6a339a',
         "Young ethnic communities" ='#9f84bd',
         "Affluent communities" ='#576362',
         "Ageing suburbanites" ='#a1a2a1',
         "Comfortable suburbia" ='#e5e4e3')

# Fig 1: LSOA Classifications



f1 = left_join(bounds, area_classifications_11_21, by = "LSOA21CD")


# set factor levels in the desired order
f1$lsoa_class_name <- factor(f1$lsoa_class_name, levels = names(cols))

m1 = tm_shape(f1) +
  tm_fill(fill = "lsoa_class_name",
          fill.scale = tm_scale_ordinal(values = cols),
          fill.legend = tm_legend(
              title = "LSOA Classification 2011")) +
  tm_shape(borders) +
  tm_borders(lwd = 0.1, col = "black") +
  tm_layout(
    frame = FALSE,                 # no frame
    inner.margins = c(0, 0, 0, 0), # top, left, bottom, right inside the frame
    outer.margins = c(0, 0, 0, 0), # outside the frame
    legend.outside = FALSE         # keep legend inside to avoid extra space
  ) +
  tm_shape(islandBox) +
  tm_borders(lwd = 1, col = "black")


tmap_save(m1,"plots/eceee_f1_lsoa_classifications.png", dpi = 600, width = 8, height = 8.5)



# pivot longer tph_Sat_Midday_2018_3 in tph _ day _ time _ year _mode
names(pt_frequency) = gsub("Afternoon_Peak","AfternoonPeak",names(pt_frequency))
names(pt_frequency) = gsub("Morning_Peak","MorningPeak",names(pt_frequency))
pt_frequency2 = pivot_longer(pt_frequency,
                             cols = names(pt_frequency)[2:ncol(pt_frequency)],
                             names_to = c("tph2","day","time","year","mode"),
                             values_to = "tph", names_sep = "_")

pt_frequency2 = pt_frequency2[pt_frequency2$time == "avg",]

pt_frequency2 = pt_frequency2 |>
  group_by(zone_id,year ) |>
  summarise(tph = sum(tph, na.rm = TRUE))
pt_frequency2 = pt_frequency2[pt_frequency2$year >= 2010,]
names(pt_frequency2)[1] = "LSOA21CD"
pt_frequency2$year = as.integer(pt_frequency2$year)




area_classifications_11_21 = area_classifications_11_21[c("LSOA21CD","lsoa_class_name")]
area_classifications_11_21$lsoa_class_name = factor(area_classifications_11_21$lsoa_class_name,
                                            levels = names(cols))

pt_frequency2 = left_join(pt_frequency2,
                          area_classifications_11_21, by = "LSOA21CD")


lsoa_emissions_all = left_join(lsoa_emissions_all,
                               area_classifications_11_21, by = "LSOA21CD")



lsoa_emissions_summary = lsoa_emissions_all |>
  group_by(year, lsoa_class_name) |>
  summarise(dom_gas_kgco2e_percap = median(dom_gas_kgco2e_percap[dom_gas_kgco2e_percap > 100], na.rm = TRUE),
            dom_elec_kgco2e_percap = median(dom_elec_kgco2e_percap, na.rm = TRUE),
            car_kgco2e_percap = median(car_kgco2e_percap, na.rm = TRUE),
            van_kgco2e_percap = median(van_kgco2e_percap, na.rm = TRUE),
            company_bike_kgco2e_percap = median(company_bike_kgco2e_percap, na.rm = TRUE),
            heating_other_kgco2e_percap   = median(heating_other_kgco2e_percap, na.rm = TRUE),
            food_kgco2e_percap = median(food_kgco2e_percap, na.rm = TRUE),
            alcohol_kgco2e_percap   = median(alcohol_kgco2e_percap, na.rm = TRUE),
            clothing_kgco2e_percap = median(clothing_kgco2e_percap, na.rm = TRUE),
            communication_kgco2e_percap = median(communication_kgco2e_percap, na.rm = TRUE),
            housing_other_kgco2e_percap = median(housing_other_kgco2e_percap, na.rm = TRUE),
            furnish_kgco2e_percap    = median(furnish_kgco2e_percap, na.rm = TRUE),
            recreation_kgco2e_percap = median(recreation_kgco2e_percap, na.rm = TRUE),
            transport_optranequip_other_kgco2e_percap = median(transport_optranequip_other_kgco2e_percap, na.rm = TRUE),
            transport_vehiclepurchase_kgco2e_percap = median(transport_vehiclepurchase_kgco2e_percap, na.rm = TRUE),
            transport_pt_kgco2e_percap = median(transport_pt_kgco2e_percap, na.rm = TRUE),
            health_kgco2e_percap = median(health_kgco2e_percap, na.rm = TRUE),
            education_kgco2e_percap   = median(education_kgco2e_percap, na.rm = TRUE),
            restaurant_kgco2e_percap = median(restaurant_kgco2e_percap, na.rm = TRUE),
            misc_kgco2e_percap      = median(misc_kgco2e_percap, na.rm = TRUE),
            flights_kgco2e_percap = median(flights_kgco2e_percap, na.rm = TRUE),
            goods_services_combined_kgco2e_percap   = median(goods_services_combined_kgco2e_percap, na.rm = TRUE),
            total_kgco2e_percap = median(total_kgco2e_percap, na.rm = TRUE),
  )

vehicle_summary = vehicle_summary[,c("LSOA21CD","year","pBEV_COMPANY",
                                     "pBEV_PRIVATE","pULEV_COMPANY","pULEV_PRIVATE",
                                     "vehiclesPPers","vehiclesPAdult","vehiclesPHousehold")]




vehicle_summary = left_join(vehicle_summary,
                            area_classifications_11_21, by = "LSOA21CD")

vehicle_summary$country = substr(vehicle_summary$LSOA21CD,1,1)
#vehicle_summary = vehicle_summary[vehicle_summary$country != "S",] # Missing recent scotland data

vehicle_summary2 = vehicle_summary |>
  filter(country != "S") |>
  group_by(year, lsoa_class_name) |>
  summarise(pBEV_COMPANY = median(pBEV_COMPANY, na.rm = TRUE),
            pBEV_PRIVATE = median(pBEV_PRIVATE, na.rm = TRUE),
            pULEV_COMPANY = median(pULEV_COMPANY, na.rm = TRUE),
            pULEV_PRIVATE = median(pULEV_PRIVATE, na.rm = TRUE),
            vehiclesPPers = median(vehiclesPPers, na.rm = TRUE),
            vehiclesPAdult = median(vehiclesPAdult, na.rm = TRUE),
            vehiclesPHousehold = median(vehiclesPHousehold, na.rm = TRUE)
  )

pt_summary = pt_frequency2 |>
  group_by(year, lsoa_class_name) |>
  summarise(tph = median(tph, na.rm = TRUE)
  )
pt_summary = pt_summary[!pt_summary$year %in% 2012:2017,] # Missing London Data


oac_plot = function(
    var  = dom_elec_kgco2e_percap,
    name = "Per person Electricty Emissions",
    dat  = lsoa_emissions_summary,
    xlims = c(min(dat$year), max(dat$year))
){

  # wrap legend labels to ~15 characters
  wrapped_labels <- stringr::str_wrap(unique(dat$lsoa_class_name), width = 20)
  names(wrapped_labels) <- unique(dat$lsoa_class_name)

  plot = ggplot(dat) +
    geom_line(aes(x = year,
                  y = {{var}},
                  colour = lsoa_class_name),
              linewidth = 1) +
    ylab(name) +
    xlab("Year") +
    scale_x_continuous(
      breaks  = seq(xlims[1], xlims[2], 2),
      expand  = c(0, 0),
      limits  = c(xlims[1], xlims[2])
    ) +
    scale_y_continuous(
      expand  = c(0, 0),
      limits  = c(0, NA)
    ) +
    guides(
      color = guide_legend(
        title = "Area classification",
        ncol  = 1,
        byrow = TRUE,    # ensures spacing applies cleanly
        override.aes = list(
          size = 4.5  # artificially large point/line increases vertical spacing
        )

      )
    ) +
    scale_color_manual(
      values = cols,
      labels = wrapped_labels
    ) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title.x = element_text(size = 8),   # x-axis label size
      axis.title.y = element_text(size = 8),   # y-axis label siz
      legend.spacing.y = unit(4, "mm"),   # increase vertical gap
      legend.key.size   = unit(0.3, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.key.width  = unit(0.3, "cm"),
      legend.title = element_text(size = 8),
      legend.text  = element_text(size = 7),
      legend.box.margin = margin(0, 2, 0, 2)  # compress legend box
    )

  plot
}

lsoa_plot = function(var = "dom_elec_kgco2e_percap",
                     title = "Change in Electrcity Emissions",
                     dat = lsoa_emissions_all,
                     bounds = bounds,
                     borders = borders,
                     islandBox = islandBox){

  yr1 = min(dat$year)
  yr2 = max(dat$year)

  tmap_options(component.autoscale = FALSE)

  dat = dat[,c("LSOA21CD","year",var)]
  dat = dat[dat$year %in% c(yr1, yr2),]
  dat = dat |>
    pivot_wider(id_cols = "LSOA21CD", names_from = "year", values_from = var)
  dat$change = (dat[[as.character(yr2)]] - dat[[as.character(yr1)]]) / dat[[as.character(yr1)]] * 100
  dat$change[is.infinite(dat$change)] = NA

  message(names(dat))

  dat2 = left_join(bounds, dat, by = "LSOA21CD")

  m0 = tm_shape(dat2) +
    tm_fill(
      fill = "change",
      fill.scale = tm_scale_intervals(
        style = "fixed",
        breaks = c(
          min(dat$change, na.rm = TRUE),
          quantile(dat$change,
                   probs = c(0.01,0.2,0.4,0.6,0.8,0.99),
                   na.rm = TRUE),
          max(dat$change, na.rm = TRUE)
        ),
        values = "-brewer.rd_yl_bu",
        label.style = "continuous",
        midpoint = 0
      ),
      fill.legend = tm_legend(
        title = paste0(title," ",yr1,"-",yr2),
        orientation = "landscape",
        position = tm_pos_out("center","bottom")
      )
    ) +
    tm_shape(islandBox) +
    tm_borders(lwd = 1, col = "black") +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0, 0, 0, 0),
      outer.margins = c(0, 0, 0, 0),
      scale = 0.5
    )

  m0



}



income_lsoa_msoa = left_join(income_lsoa_msoa, area_classifications_11_21, by = "LSOA21CD")
income_lsoa_msoa_summary = income_lsoa_msoa |>
  group_by(year, lsoa_class_name) |>
  summarise( total_annual_income = median(total_annual_income, na.rm = TRUE))

domestic_electricity = left_join(domestic_electricity, area_classifications_11_21, by = "LSOA21CD")
domestic_electricity_summary = domestic_electricity |>
  group_by(year, lsoa_class_name) |>
  summarise( median_elec_kwh = median(median_elec_kwh, na.rm = TRUE))


domestic_gas = left_join(domestic_gas, area_classifications_11_21, by = "LSOA21CD")
domestic_gas_summary = domestic_gas |>
  group_by(year, lsoa_class_name) |>
  summarise( median_gas_kwh = median(median_gas_kwh[median_gas_kwh > 100], na.rm = TRUE))


# Make Plots
# Fig 2
oac_plot(dom_elec_kgco2e_percap, expression("Per person Electricity Emissions (kgCO"[2]*"e)"))
ggsave("plots/eceee_fig2aelec.png", dpi = 600, width = 4, height = 6)
m2 = lsoa_plot("dom_elec_kgco2e_percap","% change in electrcity emissions",lsoa_emissions_all,
               bounds,borders,islandBox)
tmap_save(m2,
          "plots/eceee_fig2belec.png", dpi = 600, width = 4, height = 6)

#Fig 3
oac_plot(median_elec_kwh , "Household electricity consumption kWh/yr", domestic_electricity_summary  )
ggsave("plots/eceee_fig3aelec.png", dpi = 600, width = 4, height = 6)
m2 = lsoa_plot("median_elec_kwh","% change in electrcity consumption",domestic_electricity,
               bounds,borders,islandBox)
tmap_save(m2,
          "plots/eceee_fig3belec.png", dpi = 600, width = 4, height = 6)

#Fig 4
# oac_plot(dom_gas_kgco2e_percap, expression("Per person gas emissions (kgCO"[2]*"e)"))
# ggsave("plots/eceee_fig4a_gas.png", dpi = 600, width = 4, height = 6)
# m2 = lsoa_plot("median_gas_kwh","% change in gas consumption",domestic_gas,
#                bounds,borders,islandBox)
# tmap_save(m2,
#           "plots/eceee_fig4b_gas.png", dpi = 600, width = 4, height = 6)

# Fig 4
oac_plot(median_gas_kwh, expression("Household gas consumption kWh/yr"), lsoa_emissions_all)
ggsave("plots/eceee_fig4a_gas.png", dpi = 600, width = 4, height = 6)
m2 = lsoa_plot("median_gas_kwh","% change in gas consumption",domestic_gas,
               bounds,borders,islandBox)
tmap_save(m2,
          "plots/eceee_fig4b_gas.png", dpi = 600, width = 4, height = 6)


# Fig 6 Added
oac_plot(car_kgco2e_percap, expression("Per person car emissions (kgCO"[2]*"e)"))
ggsave("plots/eceee_fig6a_caremissions.png", dpi = 600, width = 4, height = 6)
m6 = lsoa_plot("car_kgco2e_percap","% change in car driving emissions",lsoa_emissions_all,
               bounds,borders,islandBox)
tmap_save(m6,
          "plots/eceee_fig6b_caremissions.png", dpi = 600, width = 4, height = 6)



#Fig 6
oac_plot(vehiclesPHousehold, name = "Vehicles/Household", vehicle_summary2, xlims = c(2010,2024))
ggsave("plots/eceee_fig6a_vehicles.png", dpi = 600, width = 4, height = 6)
m6 = lsoa_plot(var = "vehiclesPHousehold",title = "% change in vehicles per household", dat = vehicle_summary[vehicle_summary$year <= 2022,],
               bounds,borders,islandBox)
tmap_save(m6,
          "plots/eceee_fig6b_vehicles.png", dpi = 600, width = 4, height = 6)

#Fig 7
# Link vehicle onwership to PT frequnecy
vehil_pt = full_join(pt_summary, vehicle_summary2, by = c("year", "lsoa_class_name"))
vehil_pt = vehil_pt[,c("year","lsoa_class_name","tph","vehiclesPHousehold")]
vehil_pt = vehil_pt[!is.na(vehil_pt$lsoa_class_name),]

vehil_pt2 = vehil_pt |> arrange(lsoa_class_name, year)

# Fig 8
oac_plot(pBEV_PRIVATE, name = "Percentage of Vehciles that are Battery Electric", vehicle_summary2,xlims = c(2015,2025))
ggsave("plots/eceee_fig8a_bev.png", dpi = 600, width = 4, height = 6)

tmap_options(component.autoscale = FALSE)

dat = vehicle_summary[,c("LSOA21CD","year","pBEV_PRIVATE")]
dat = dat[dat$year == 2025,]
message(names(dat))

dat2 = left_join(bounds, dat, by = "LSOA21CD")

m8 = tm_shape(dat2) +
  tm_fill(
    fill = "pBEV_PRIVATE",
    fill.scale = tm_scale_intervals(
      style = "fixed",
      breaks = c(
        #min(dat$pBEV_PRIVATE, na.rm = TRUE),
        quantile(dat$pBEV_PRIVATE,
                 probs = c(0.01,0.2,0.4,0.6,0.8,0.99),
                 na.rm = TRUE),
        max(dat$pBEV_PRIVATE, na.rm = TRUE)
      ),
      values = "brewer.rd_yl_bu",
      label.style = "continuous"#,
      #midpoint = 0
    ),
    fill.legend = tm_legend(
      title = "Percentage of private vehicles that are BEVs",
      orientation = "landscape",
      position = tm_pos_out("center","bottom")
    )
  ) +
  tm_shape(islandBox) +
  tm_borders(lwd = 1, col = "black") +
  tm_layout(
    frame = FALSE,
    inner.margins = c(0, 0, 0, 0),
    outer.margins = c(0, 0, 0, 0),
    scale = 0.5
  )
tmap_save(m8,
          "plots/eceee_fig8b_bev.png", dpi = 600, width = 4, height = 6)

#Fig 9
oac_plot(flights_kgco2e_percap, name = expression("Per person flights emissions (kgCO"[2]*"e)"))
ggsave("plots/eceee_fig9a_flights.png", dpi = 600, width = 4, height = 6)
m9 = lsoa_plot("flights_kgco2e_percap","% change in per person flight emissions",lsoa_emissions_all,
               bounds,borders,islandBox)
tmap_save(m9,
          "plots/eceee_fig9b_flights.png", dpi = 600, width = 4, height = 6)


#Fig 10
oac_plot(goods_services_combined_kgco2e_percap, name = expression("Per person goods and services emissions (kgCO"[2]*"e)"))
ggsave("plots/eceee_fig10a_goods_services.png", dpi = 600, width = 4, height = 6)
m10 = lsoa_plot("goods_services_combined_kgco2e_percap","% change in per person goods and services emissions",lsoa_emissions_all,
               bounds,borders,islandBox)
tmap_save(m10,
          "plots/eceee_fig10b_goods_services.png", dpi = 600, width = 4, height = 6)


# oac_plot(heating_other_kgco2e_percap, name = "Per person other heating Emissions")

# oac_plot(van_kgco2e_percap, name = "Per person van Emissions")
# oac_plot(company_bike_kgco2e_percap, name = "Per person bike comany car Emissions")
# oac_plot(food_kgco2e_percap, name = "Per person food Emissions")
# oac_plot(alcohol_kgco2e_percap, name = "Per person alchol Emissions")
# oac_plot(clothing_kgco2e_percap, name = "Per person clothing Emissions")
# oac_plot(communication_kgco2e_percap, name = "Per person communication Emissions")
# oac_plot(housing_other_kgco2e_percap, name = "Per person housing other Emissions")
# oac_plot(furnish_kgco2e_percap, name = "Per person funshing Emissions")
# oac_plot(recreation_kgco2e_percap, name = "Per person recreation Emissions")
# oac_plot(transport_optranequip_other_kgco2e_percap, name = "Per person transport Emissions")
# oac_plot(transport_vehiclepurchase_kgco2e_percap, name = "Per person vehicle purchase Emissions")
# oac_plot(transport_pt_kgco2e_percap, name = "Per person publiv trnasport Emissions")
# oac_plot(health_kgco2e_percap, name = "Per person health Emissions")
# oac_plot(education_kgco2e_percap, name = "Per person education Emissions")
# oac_plot(restaurant_kgco2e_percap, name = "Per person restaurant Emissions")
# oac_plot(misc_kgco2e_percap, name = "Per person misc Emissions")

# oac_plot(goods_services_combined_kgco2e_percap, name = "Per person goods services Emissions")
oac_plot(total_kgco2e_percap, name = "Per person total Emissions")


oac_plot(pBEV_COMPANY, name = "pBEV_COMPANY", vehicle_summary2, xlims = c(2015,2025))

oac_plot(pULEV_COMPANY, name = "pULEV_COMPANY", vehicle_summary2)
oac_plot(pULEV_PRIVATE, name = "pULEV_PRIVATE", vehicle_summary2)
oac_plot(vehiclesPPers, name = "vehiclesPPers", vehicle_summary2, xlims = c(2010,2024))
oac_plot(vehiclesPAdult, name = "vehiclesPAdult", vehicle_summary2, xlims = c(2010,2024))
oac_plot(vehiclesPHousehold, name = "vehiclesPHousehold", vehicle_summary2, xlims = c(2010,2024))

oac_plot(tph, name = "Trips per hour", pt_summary)

oac_plot(total_annual_income, name = "Income", income_lsoa_msoa_summary )










library(tidyverse)
library(grid) # for unit()



# 1) Clean and order
vehil_pt2 <- vehil_pt %>%
  # Drop rows where x or y missing, otherwise ggplot breaks the path
  filter(!is.na(tph), !is.na(vehiclesPHousehold)) %>%
  mutate(
    year = as.integer(year),
    lsoa_class_name = as.factor(lsoa_class_name)
  ) %>%
  arrange(lsoa_class_name, year)

# 2) Build observed step pairs (t -> next observed)
vehil_steps <- vehil_pt2 %>%
  group_by(lsoa_class_name) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    tph_next = lead(tph),
    vph_next = lead(vehiclesPHousehold),
    year_next = lead(year)
  ) %>%
  ungroup() %>%
  filter(!is.na(tph_next), !is.na(vph_next))

# 3) Compute a short segment centered at the midpoint (for the arrow)
#    The small segment (length factor f) is centered at the midpoint,
#    oriented in the forward direction.
f <- 0.22  # fraction of the full step length to draw around the midpoint

vehil_steps_mid <- vehil_steps %>%
  mutate(
    dx = tph_next - tph,
    dy = vph_next - vehiclesPHousehold,
    mx = (tph_next + tph)/2,
    my = (vph_next + vehiclesPHousehold)/2,
    x_start_mid = mx - f * dx,
    y_start_mid = my - f * dy,
    x_end_mid   = mx + f * dx,
    y_end_mid   = my + f * dy
  )

# 4) Plot: full path + points + mid-segment arrows
ggplot(vehil_pt2, aes(x = tph, y = vehiclesPHousehold, colour = lsoa_class_name)) +
  # Full path (no arrows) connects observed points in time order
  geom_path(aes(group = lsoa_class_name),
            linewidth = 0.9, lineend = "round", na.rm = TRUE, alpha = 0.55) +
  geom_point(size = 1) +
  # Arrow in the middle of each observed step
  geom_segment(
    data = vehil_steps_mid,
    aes(x = x_start_mid, y = y_start_mid, xend = x_end_mid, yend = y_end_mid),
    arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
    linewidth = 0.9
  ) +
  labs(
    x = "Average accessible public transport\ntrips per hour during daytime",
    y = "Vehicles per household",
    colour = "LSOA class",
    #title = "Progression over time by LSOA class",
    subtitle = "Arrows show progress over time"
  ) +
  guides(color=guide_legend(title="Area classification", ncol =1)) +
  scale_color_manual(values=cols) +

  geom_smooth(
    data = vehil_pt2,
    mapping = aes(x = tph, y = vehiclesPHousehold),
    inherit.aes = FALSE,
    method = "lm",
    formula = y ~ log(x),  # log
    se = FALSE,
    colour = "black",
    linetype = "dashed",
    linewidth = 1
  )

ggsave("plots/eceee_fig8a_pt_vs_car.png", dpi = 600, width = 8, height = 6)
