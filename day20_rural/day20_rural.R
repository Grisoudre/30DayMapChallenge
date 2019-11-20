# 30DayMapChallenge - Day 19 | Rural : Médecins généralistes dans le Nord

# Packages ----
library(tidyverse)
library(sf)
library(ggspatial)
library(viridis)

# Imports ----

bpe <- read.csv2("data/bpe18_ensemble_xy.csv", stringsAsFactors = F) %>% 
  filter(DEP == "59")
nord <- st_read("data/communes-20150101-50m.shp")
diff <- st_read("data/diff_nord.shp")

# Mises en forme ----

nord <- nord %>% filter(substr(insee,1,2)=="59")
bpe <- bpe %>% filter(LAMBERT_X !="" & LAMBERT_Y!= "")
bpe_st <- st_as_sf(bpe, coords = c("LAMBERT_X","LAMBERT_Y"), crs=2154)
nord <- st_transform(nord, crs=2154)

medecins <- bpe_st %>% filter(TYPEQU =="D201")
boulangeries <- bpe_st %>% filter(TYPEQU =="B203")
gares <- bpe_st %>% filter(TYPEQU =="E01G"|
                             TYPEQU =="E107"|
                             TYPEQU =="E108"|
                             TYPEQU =="E109")


boulangeriesd <- st_centroid(boulangeries$geometry)
boulangeriesc <- data.frame(st_coordinates(boulangeriesd))

# Carte -----
 
ggplot()+ geom_sf(data=nord, fill=viridis(2, option="A")[1], col="transparent") +
  stat_density_2d(data=boulangeriesc,aes(x=X, y=Y,
                                     fill = ..level..), 
                  geom="polygon", colour="transparent",
                  bins=40) + 
  scale_fill_viridis(option="A") +
   geom_sf(data=diff, fill="white", col="transparent") +
   geom_sf(data=diff, fill="white", col="transparent") +
  geom_sf(data=boulangeries, fill = "white", col = "transparent") +
  geom_sf(data=boulangeries, col="white", shape = 1, size=.5)+
  coord_sf(crs = st_crs(nord), datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        title=element_text(size=16))+
  labs(x = "", y = "", fill="densité", title = "Boulangeries dans le Nord",
       caption = "Source : Base Permanente des Equipements, Insee, 2018") 

# Output ----

ggsave(  path ="output", "30daymapchallenge_day120.png",width=12, height=8, dpi=300)

 