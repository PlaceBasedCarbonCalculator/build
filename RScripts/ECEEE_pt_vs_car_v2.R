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
# TWO DELIBERATE DEPARTURES from ECEEE_paper.R, both fixing defects rather
# than restyling. Each is explained in full at the point it happens:
#
#   1. The palette is rebuilt from the classification's own group codes. The
#      original covers 21 of the 24 groups and includes one name that is not in
#      the 2021 classification, which silently deleted a fifth of the country
#      and shifted colours within supergroup 7.
#   2. Scotland is included. The original compares a GB x-axis against an
#      England-and-Wales y-axis; this costs two years at the recent end but
#      makes both axes GB.
#
# Because of 2 the period is shorter than the published figure's, and because
# of 1 there are more trajectories. This is a corrected figure, not a restyled
# one.

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
# Palette
# ---------------------------------------------------------------------------
# The palette in ECEEE_paper.R is incomplete, and that is why the first render
# of this figure was missing classifications and mis-assigning colours.
#
# The 2021 ONS area classification has 8 supergroups and 24 groups (1a-8c).
# The original `cols` vector has 21 entries: it runs correctly from 1a to 7a,
# then ends with "Aspirational techies", which is not a group name in the 2021
# classification at all - it appears to be from an earlier draft. So the
# palette was:
#
#   - missing 7b Young ethnic communities (1,744 LSOAs)
#   - missing the whole of supergroup 8, Suburban living:
#       8a Affluent communities  (2,012)
#       8b Ageing suburbanites   (2,924)
#       8c Comfortable suburbia  (2,384)
#   - carrying one entry, "Aspirational techies", matching no data
#
# Those four groups are ~9,000 LSOAs, a fifth of the country. Because
# ECEEE_paper.R sets the factor levels to names(cols), every one of them became
# NA and was then dropped by the !is.na() filter - silently, with no warning.
#
# Keyed by group CODE rather than name below, so a future rename cannot
# reintroduce this class of bug quietly: an unmatched code stops the script.
#
# Colours 1a-7a are the original values, unchanged, so anything already correct
# stays byte-identical to the published figure. The shading follows the
# original's own scheme - one hue per supergroup, dark to light within it.
#
# NEW, and worth a look before publication:
#   7b  reuses #ef9fa5, the light pink that sat in the 7-family slot under the
#       wrong name. Same supergroup, same position, so this is a relabel.
#   8a-8c  are a teal ramp that had to be invented, because supergroup 8 has no
#       colour anywhere in the original. Teal was chosen as the nearest unused
#       hue - brown, green, blue, amber, purple, red and pink are all taken. If
#       the paper is meant to match an official ONS colour set, override these
#       three; they are the only values here not traceable to the original.
class_cols = c(
  "1a" = '#955123',                                                  # Cosmopolitan student neighbourhoods
  "2a" = '#007f42', "2b" = '#3ea456', "2c" = '#8aca8e', "2d" = '#cfe8d1',  # Countryside living
  "3a" = '#00498d', "3b" = '#2967ad', "3c" = '#7b99c7', "3d" = '#b9c8e1',  # Ethnically diverse professionals
  "4a" = '#e3ac20', "4b" = '#eac364', "4c" = '#f2dca6', "4d" = '#f7ebd0',  # Hard-pressed communities
  "5a" = '#6f3d79', "5b" = '#8e6494', "5c" = '#ad8bb0', "5d" = '#ccb2cc',
  "5e" = '#e9d9e9',                                                  # Industrious communities
  "6a" = '#d0021b',                                                  # Inner city cosmopolitan
  "7a" = '#e0505f', "7b" = '#ef9fa5',                                # Multicultural living
  "8a" = '#00767d', "8b" = '#4aa8ae', "8c" = '#a5d5d8'               # Suburban living  (NEW)
)

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

# Map every group code in the data to a colour, and fail loudly if any code has
# none - the failure mode this replaces was silent deletion of a fifth of the
# country. Legend order follows the code (1a...8c), so the colour families read
# in order rather than alphabetically.
area_classifications_11_21 = area_classifications_11_21[c("LSOA21CD","lsoa_class_code","lsoa_class_name")]

class_lookup = area_classifications_11_21 |>
  distinct(lsoa_class_code, lsoa_class_name) |>
  arrange(lsoa_class_code)

unknown = setdiff(class_lookup$lsoa_class_code, names(class_cols))
if (length(unknown) > 0) {
  stop("No colour defined for area classification code(s): ", paste(unknown, collapse = ", "),
       ". Add them to class_cols.")
}

# Named by the label that will appear in the legend.
cols = setNames(class_cols[class_lookup$lsoa_class_code], class_lookup$lsoa_class_name)

area_classifications_11_21$lsoa_class_name = factor(area_classifications_11_21$lsoa_class_name,
                                                    levels = class_lookup$lsoa_class_name)
area_classifications_11_21$lsoa_class_code = NULL

pt_frequency2 = left_join(pt_frequency2,
                          area_classifications_11_21, by = "LSOA21CD")

vehicle_summary = vehicle_summary[,c("LSOA21CD","year","pBEV_COMPANY",
                                     "pBEV_PRIVATE","pULEV_COMPANY","pULEV_PRIVATE",
                                     "vehiclesPPers","vehiclesPAdult","vehiclesPHousehold")]

vehicle_summary = left_join(vehicle_summary,
                            area_classifications_11_21, by = "LSOA21CD")

vehicle_summary$country = substr(vehicle_summary$LSOA21CD,1,1)

# Scotland is INCLUDED here, unlike in ECEEE_paper.R.
#
# The original drops it ("Missing recent scotland data") and so compares a GB
# median on the x-axis against an England-and-Wales median on the y-axis. The
# vehicle data does cover Scotland - 7,392 Data Zones, fully classified - it
# just stops earlier, so the fix is to shorten the period rather than to drop a
# country. Both axes are now GB.
#
# Which years survive is derived from the data, not hard-coded, because
# vehiclesPHousehold is present but *zero* in the years where the household
# denominator is missing rather than being NA - the trap that put twenty
# vertical lines through the first render of this figure. A year is kept only
# if EVERY country in it has at least one non-zero zone:
#
#   England & Wales   real to 2024, all 35,672 zones exactly 0.000 in 2025
#   Scotland          real to 2022, all 7,392 zones exactly 0.000 from 2023
#
# so the common window is 2010-2022. That is two years shorter than the
# England-and-Wales-only version, which is the price of a consistent
# geography. Genuine per-zone zeros (81 LSOAs in 2010, none by 2019) are
# untouched - whole empty country-years are dropped, not individual rows - and
# the window extends itself as each country's figures land.
years_with_vehicle_data = vehicle_summary |>
  group_by(year, country) |>
  summarise(has_data = any(vehiclesPHousehold > 0, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  summarise(all_countries = all(has_data)) |>
  filter(all_countries) |>
  pull(year)

message("vehicle years used (GB): ", paste(range(years_with_vehicle_data), collapse = " - "))

vehicle_summary2 = vehicle_summary |>
  filter(year %in% years_with_vehicle_data) |>
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
