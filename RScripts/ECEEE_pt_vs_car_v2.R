# Updated version of ECEEE figure 8a: public transport frequency against
# vehicle ownership, one trajectory per ONS area classification.
#
# The figure is the one built at the end of RScripts/ECEEE_paper.R
# ("plots/eceee_fig8a_pt_vs_car.png"). This script is a standalone rerun of
# just that figure on the current data, and writes to a NEW file name -
# eceee_fig8a_pt_vs_car_v2.png - so the published version is untouched.
#
# Why it is worth rerunning rather than reusing the May 2026 render: both
# inputs have changed underneath it.
#
#   pt_frequency    the whole 2004-2025 timetable archive was reprocessed in
#                   2026 with a substantially improved UK2GTFS. The largest
#                   single change is better duplicate-journey detection in the
#                   NPTDR-era files, which removed roughly a quarter of the
#                   counted stop calls in the mid-2000s; the series also now
#                   extends to 2025 and takes coach from the BODS coach
#                   dataset from 2024 (TNDS stopped carrying it).
#   vehicle_summary now runs to 2025.
#
# So the trajectories are both longer at the recent end and differently
# anchored at the old end than in the published figure. They are NOT
# interchangeable - do not mix the two renders in one document.
#
# Everything from the palette down to the arrow geometry is kept exactly as in
# ECEEE_paper.R. This is the same figure with newer data, not a redesign.
#
# ONE INHERITED CAVEAT, carried through deliberately rather than silently
# fixed: the two axes cover different geographies. vehicle_summary2 drops
# Scotland (DVLA registrations for Scotland lag), pt_summary does not, so the
# x value for a class is a GB median and the y value an England-and-Wales one.
# That is how the published figure was built. Changing it here would make this
# render incomparable with the published one, which defeats the point of a v2.

library(targets)
library(dplyr)
library(ggplot2)
library(tidyr)
library(grid) # for unit()

setwd("f:/GitHub/PlaceBasedCarbonCalculator/build")

tar_load(vehicle_summary)
tar_load(pt_frequency)
tar_load(area_classifications_11_21)

# ---------------------------------------------------------------------------
# Palette (verbatim from ECEEE_paper.R - the ONS area classification colours)
# ---------------------------------------------------------------------------
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
         "Constrained renters" ='#eac364',
         "Hampered neighbourhoods" ='#f2dca6',
         "Hard-pressed flat dwellers" ='#f7ebd0',
         "Ageing urban communities" ='#6f3d79',
         "Aspiring urban households" ='#8e6494',
         "Comfortable neighbourhoods" ='#ad8bb0',
         "Endeavouring social renters" ='#ccb2cc',
         "Primary sector workers" ='#e9d9e9',
         "Inner city cosmopolitan" ='#d0021b',
         "Urban cultural mix" ='#e0505f',
         "Aspirational techies" ='#ef9fa5')

# ---------------------------------------------------------------------------
# Data prep (verbatim from ECEEE_paper.R)
# ---------------------------------------------------------------------------

# Peak names contain an underscore, which would give those columns six
# name parts instead of five and break the names_sep split below.
names(pt_frequency) = gsub("Afternoon_Peak","AfternoonPeak",names(pt_frequency))
names(pt_frequency) = gsub("Morning_Peak","MorningPeak",names(pt_frequency))
pt_frequency2 = pivot_longer(pt_frequency,
                             cols = names(pt_frequency)[2:ncol(pt_frequency)],
                             names_to = c("tph2","day","time","year","mode"),
                             values_to = "tph", names_sep = "_")

pt_frequency2 = pt_frequency2[pt_frequency2$time == "avg",]

pt_frequency2 = pt_frequency2 |>
  group_by(zone_id, year) |>
  summarise(tph = sum(tph, na.rm = TRUE))
pt_frequency2 = pt_frequency2[pt_frequency2$year >= 2010,]
names(pt_frequency2)[1] = "LSOA21CD"
pt_frequency2$year = as.integer(pt_frequency2$year)

area_classifications_11_21 = area_classifications_11_21[c("LSOA21CD","lsoa_class_name")]
area_classifications_11_21$lsoa_class_name = factor(area_classifications_11_21$lsoa_class_name,
                                                    levels = names(cols))

pt_frequency2 = left_join(pt_frequency2,
                          area_classifications_11_21, by = "LSOA21CD")

vehicle_summary = vehicle_summary[,c("LSOA21CD","year","pBEV_COMPANY",
                                     "pBEV_PRIVATE","pULEV_COMPANY","pULEV_PRIVATE",
                                     "vehiclesPPers","vehiclesPAdult","vehiclesPHousehold")]

vehicle_summary = left_join(vehicle_summary,
                            area_classifications_11_21, by = "LSOA21CD")

vehicle_summary$country = substr(vehicle_summary$LSOA21CD,1,1)

# Drop the years where vehiclesPHousehold is not yet populated.
#
# This is NOT in the original script, and it has to be here. vehicle_summary
# now extends to 2025, and in 2025 vehiclesPHousehold is exactly 0.000 for all
# 35,672 English and Welsh LSOAs - the household denominator for that year is
# missing, not the vehicles. A median of a column of zeros is zero, so without
# this filter every class gets a spurious 2025 point on the axis and the figure
# grows twenty vertical lines plunging to zero. (Scotland is in the same state
# from 2023, which is why the main pipeline pins Scotland to 2022 and England
# and Wales to 2024 in select_transport_vars().)
#
# The test is "was any zone in this year non-zero", not a hard-coded year, so
# 2025 reappears by itself once the household figures land. Genuine per-zone
# zeros - 81 LSOAs in 2010, falling to none by 2019 - are untouched, because
# the filter drops whole empty years rather than individual zero rows.
years_with_vehicle_data = vehicle_summary |>
  filter(country != "S") |>
  group_by(year) |>
  summarise(has_data = any(vehiclesPHousehold > 0, na.rm = TRUE)) |>
  filter(has_data) |>
  pull(year)

message("vehicle years used: ", paste(range(years_with_vehicle_data), collapse = " - "))

vehicle_summary2 = vehicle_summary |>
  filter(country != "S", year %in% years_with_vehicle_data) |>
  group_by(year, lsoa_class_name) |>
  summarise(vehiclesPHousehold = median(vehiclesPHousehold, na.rm = TRUE))

pt_summary = pt_frequency2 |>
  group_by(year, lsoa_class_name) |>
  summarise(tph = median(tph, na.rm = TRUE))
pt_summary = pt_summary[!pt_summary$year %in% 2012:2017,] # Missing London Data

vehil_pt = full_join(pt_summary, vehicle_summary2, by = c("year", "lsoa_class_name"))
vehil_pt = vehil_pt[,c("year","lsoa_class_name","tph","vehiclesPHousehold")]
vehil_pt = vehil_pt[!is.na(vehil_pt$lsoa_class_name),]

# ---------------------------------------------------------------------------
# Figure (verbatim from ECEEE_paper.R)
# ---------------------------------------------------------------------------

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

ggsave("plots/eceee_fig8a_pt_vs_car_v2.png", dpi = 600, width = 8, height = 6)

message("years plotted: ", paste(range(vehil_pt2$year), collapse = " - "))
message("classes plotted: ", length(unique(vehil_pt2$lsoa_class_name)))
message("points: ", nrow(vehil_pt2))
