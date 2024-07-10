library(targets)
library(ggplot2)
library(dplyr)
library(sf)
tar_load(access_poi_circle_30min )
tar_load(access_poi_iso_30min )
tar_load(lookup_oa2021_lsoa2011)
tar_load(area_classifications)

acc = access_poi_iso_30min
prox = access_poi_circle_30min
lookup_oa2021_lsoa2011
# Convert from OA2021 to LSOA
acc = dplyr::left_join(acc, lookup_oa2021_lsoa2011, by = c("OA21CD" = "nearest_OA2021"), relationship = "many-to-many")
acc$OA21CD = NULL
acc$nearest_OA2021_dist = NULL

locs = c(
"E01026984" ,#Corby
"E01032739" ,#City of London
"E01032501" ,#Affluent Leeds Suburb
"E01018944") #Rural Cornwall

acc_sub = acc[acc$LSOA11CD %in% locs,]
prox_sub = prox[prox$LSOA11CD %in% locs,]

acc_sub = acc_sub[,c("LSOA11CD","groupname","categoryname","classname","sp10kp_diff_SD")]
prox_sub = prox_sub[,c("LSOA11CD","groupname","categoryname","classname","sp10kp_diff_SD")]
names(acc_sub)[5] = "access"
names(prox_sub)[5] = "proximity"

ap = dplyr::full_join(prox_sub, acc_sub, by = c("LSOA11CD","groupname","categoryname","classname"))

# Add Missing combincations
miss = prox[!duplicated(prox$classname),]
miss = miss[,c("groupname","categoryname","classname")]
miss = miss[rep(1:nrow(miss), 4),]
miss$LSOA11CD = rep(locs, each = 385)

ap = dplyr::full_join(ap, miss, by = c("LSOA11CD","groupname","categoryname","classname"))

cols = read.csv("data/access_plots_colours.csv")
#cols[1:2] = lapply(cols[1:2], function(x){gsub("<ff>")})
ap = left_join(ap, cols, by = c("groupname","categoryname"))
ap$LSOA_name  = ap$LSOA11CD
ap$LSOA_name[ap$LSOA_name == "E01026984"] = "E01026984: Corby"
ap$LSOA_name[ap$LSOA_name == "E01032739"] = "E01032739: City of London"
ap$LSOA_name[ap$LSOA_name == "E01032501"] = "E01032501: Suburb of Leeds"
ap$LSOA_name[ap$LSOA_name == "E01018944"] = "E01018944: Rural Cornwall"

lim = 3
#Crop
ap_crop = ap
ap_crop$proximity[is.na(ap_crop$proximity)] = -lim
ap_crop$access[is.na(ap_crop$access)] = -lim
ap_crop$proximity[ap_crop$proximity < -lim] = -lim
ap_crop$proximity[ap_crop$proximity >lim] = lim
ap_crop$access[ap_crop$access < -lim] = -lim
ap_crop$access[ap_crop$access >lim] = lim

ap_crop$categoryname = factor(ap_crop$categoryname, levels = cols$categoryname)

pal = cols$colour
names(pal) = cols$categoryname

ggplot(ap_crop, aes(x = proximity, y = access, color = categoryname)) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_point(shape = 19) +
  scale_x_continuous(limits = c(-lim,lim), expand = c(0,0)) +
  scale_y_continuous(limits = c(-lim,lim), expand = c(0,0)) +
  xlab("Proximity, services per 10,000 people, SD from mean") +
  ylab("Accessibility by public transport, services per 10,000 people, SD from mean") +
  guides(color=guide_legend(ncol=1,keyheight=0.5,title="Categories of Destination")) +
  scale_color_manual(values = pal) +
  facet_wrap(vars(LSOA_name),ncol = 2)
ggsave("plots/access_proximity_factet_15min_3pmh.png", width = 10, height = 6)

# Just pubs

acc_sub = acc[acc$classname %in% c("Fast Food and Takeaway Outlets","Bus Stops","Designated Scenic Features","Sports Clubs and Associations"),]
prox_sub = prox[prox$classname %in% c("Fast Food and Takeaway Outlets","Bus Stops","Designated Scenic Features","Sports Clubs and Associations"),]

acc_sub = acc_sub[,c("LSOA11CD","groupname","categoryname","classname","sp10kp_diff_SD")]
prox_sub = prox_sub[,c("LSOA11CD","groupname","categoryname","classname","sp10kp_diff_SD")]
names(acc_sub)[5] = "access"
names(prox_sub)[5] = "proximity"

# Add Missing combincations
miss = prox_sub[!duplicated(prox_sub$classname),]
miss = miss[,c("groupname","categoryname","classname")]
miss = miss[rep(1:nrow(miss), 34753),]
miss$LSOA11CD = rep(unique(prox$LSOA11CD), each = 4)


ap = dplyr::full_join(prox_sub, acc_sub, by = c("LSOA11CD","groupname","categoryname","classname"))
ap = dplyr::full_join(ap, miss, by = c("LSOA11CD","groupname","categoryname","classname"))

ap_crop = ap
ap_crop$proximity[is.na(ap_crop$proximity)] = -lim
ap_crop$access[is.na(ap_crop$access)] = -lim
ap_crop$proximity[ap_crop$proximity < -lim] = -lim
ap_crop$proximity[ap_crop$proximity >lim] = lim
ap_crop$access[ap_crop$access < -lim] = -lim
ap_crop$access[ap_crop$access >lim] = lim

ap_crop$categoryname = factor(ap_crop$categoryname, levels = cols$categoryname)

ap_crop = left_join(ap_crop, area_classifications, by = c("LSOA11CD" = "LSOACD11"))

pal = c("Cosmopolitan student neighbourhoods" ='#955123',
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

#cols = factor(cols, levels = unname(cols))
ap_crop$lsoa_class_name = factor(ap_crop$lsoa_class_name, levels = names(pal))

ggplot(ap_crop, aes(x = proximity, y = access, color = lsoa_class_name)) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_point(size = 0.1) +
  scale_x_continuous(limits = c(-lim,lim), expand = c(0,0)) +
  scale_y_continuous(limits = c(-lim,lim), expand = c(0,0)) +
  xlab("Proximity, services per 10,000 people, SD from mean") +
  ylab("Accessibility by public transport, services per 10,000 people, SD from mean") +
  guides(color=guide_legend(ncol=1,keyheight=1,title="LSOA Area Classification", override.aes = list(size=2))) +
  theme(legend.key.size = unit(4, 'mm')) +
  scale_color_manual(values = pal) +
  facet_wrap(vars(classname),ncol = 2)
ggsave("plots/access_proximity_LSOAs_15min_3mph.png", width = 10, height = 6)

# Map


acc_sub = acc[acc$classname %in% c("Swimming Pools"),]
prox_sub = prox[prox$classname %in% c("Swimming Pools"),]

acc_sub = acc_sub[,c("LSOA11CD","groupname","categoryname","classname","sp10kp_diff_SD")]
prox_sub = prox_sub[,c("LSOA11CD","groupname","categoryname","classname","sp10kp_diff_SD")]
names(acc_sub)[5] = "access"
names(prox_sub)[5] = "proximity"

# Add Missing combincations
miss = prox_sub[!duplicated(prox_sub$classname),]
miss = miss[,c("groupname","categoryname","classname")]
miss = miss[rep(1:nrow(miss), 34753),]
miss$LSOA11CD = unique(prox$LSOA11CD)

ap = dplyr::full_join(prox_sub, acc_sub, by = c("LSOA11CD","groupname","categoryname","classname"))
ap = dplyr::full_join(ap, miss, by = c("LSOA11CD","groupname","categoryname","classname"))

ap_crop = ap
ap_crop$proximity[is.na(ap_crop$proximity)] = -lim
ap_crop$access[is.na(ap_crop$access)] = -lim
ap_crop$proximity[ap_crop$proximity < -lim] = -lim
ap_crop$proximity[ap_crop$proximity >lim] = lim
ap_crop$access[ap_crop$access < -lim] = -lim
ap_crop$access[ap_crop$access >lim] = lim

quantile(ap_crop$access, probs = seq(0,1,0.1))
quantile(ap_crop$proximity, probs = seq(0,1,0.1))

ap_crop = ap_crop %>%
  mutate(biv_access = case_when(
         access < -1 ~ "A",
         between(access, -1, -0.5) ~ "B",
         between(access, -0.5,  0.5) ~ "C",
         between(access,  0.5,  1) ~ "D",
         access > 1 ~ "E"
         ),
         biv_prox = case_when(
           proximity < -1 ~ "A",
           between(proximity, -1, -0.5) ~ "B",
           between(proximity, -0.5,  0.5) ~ "C",
           between(proximity,  0.5,  1) ~ "D",
           proximity > 1 ~ "E"
         )
         )
table(ap_crop$biv_access)
table(ap_crop$biv_prox)
ap_crop$biv_all = paste0(ap_crop$biv_access,ap_crop$biv_prox)

cols = data.frame(id = c("EA","EB","EC","ED","EE",
                         "DA","DB","DC","DD","DE",
                         "CA","CB","CC","CD","CE",
                         "BA","BB","BC","BD","BE",
                         "AA","AB","AC","AD","AE"),
                  col = c("#5950A1","#7A8BB9","#9BC8D2","#51A788","#0D8943",
                         "#9D7CB8","#BCB3D2","#D6E7EB","#AAD2B0","#7BBC6D",
                         "#CC9BC8","#E3CBE1","#FAFAFA","#E8F0CC","#CEE28D",
                         "#D959A6","#E991BB","#F8CBD1","#EDBD9D","#DEB15C",
                         "#E90C7E","#EF5091","#F597A5","#F3896D","#F07621"))


ap_crop = left_join(ap_crop, cols, by = c("biv_all" = "id"))

bounds = readRDS("D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_super_generalised.Rds")
bounds = left_join(bounds, ap_crop, by = c("code" = "LSOA11CD"))
bounds = bounds[!is.na(bounds$categoryname),]

library(tmap)

m1 = tm_shape(bounds) +
  tm_fill(col = "col") +
  tm_scale_bar()
  #tm_borders(col = "black", lwd = 0.01)
tmap_save(m1, "plots/bivariate_swimming_30min_3mph.png", dpi = 300, height = 6, width = 4)

#
# library(ggplot2)
# foo = ap[!is.na(ap$proximity) & !is.na(ap$access), ]
# neglog = function(x){
#   if(x>0){
#     return(log(x))
#   } else{
#     return(-log(-x))
#   }
# }
#
# foo$log_prox = sapply(foo$proximity, neglog)
# foo$log_acc = sapply(foo$access, neglog)
#
# ggplot(foo, aes(x = log_prox, y = log_acc, color = colour, shape = shape)) +
#   geom_point() +
#   ylim(-9,9) +
#   xlim(-9,9) +
#   xlab("Proximity: log(SD) from mean") +
#   ylab("Accessability by public tranport: log(SD) from mean") +
#   guides(color=guide_legend(ncol=2)) +
#   scale_color_identity() +
#   scale_shape_identity()
#
#
# sumtab = access_poi_iso_30min %>%
#   group_by(groupname,categoryname) %>%
#   summarise(count = n())
#
