library(targets)
tar_load(vehicle_cenus21)
tar_load(vehicle_registrations)

vehicle_registrations = vehicle_registrations[vehicle_registrations$quarter == "2021 Q1",]

# Only LSOA in 2021 and 2011 Cenus and England/ Wales
vehicle_registrations = vehicle_registrations[vehicle_registrations$LSOA11CD %in% vehicle_cenus21$LSOA21, ]


vr_all = dplyr::left_join(vehicle_cenus21, vehicle_registrations, by = c("LSOA21" = "LSOA11CD"))
summary(vr_all$total_carvan_est)
summary(vr_all$Cars_Private_Licensed)

library(ggplot2)
ggplot(vr_all, aes(total_carvan_est, Cars_Private_Licensed)) +
  geom_point(size = 0.5) +
  xlab("Total Cars/Vans in 2021 Cenus") +
  ylab("Total Privately Licenced Cars in Q1 2021 DfT Stats")


ggplot(vr_all, aes(Cars_Private_Licensed, total_carvan_est)) +
  geom_point(size = 0.3) +
  ylab("Total Cars/Vans in 2021 Cenus") +
  xlab("Total Privately Licenced Cars in Q1 2021 DfT Stats") +
  xlim(0, 2300) +
  ylim(0, 2300) +
  geom_abline(slope=1, intercept=0, colour = "red")
