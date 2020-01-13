# 30DayMapChallenge - Day 28 | Funny : Joker / psychiatrie dans le Nord

# Packages ----
library(tidyverse)
library(sf)

# Imports ----

bpe <- read.csv2("data/bpe18_ensemble_xy.csv", stringsAsFactors = F) %>% 
  filter(DEP == "59")
nord <- st_read("data/Nord_COM.shp")

# Mises en forme ----

bpe <- bpe %>% filter(LAMBERT_X !="" & LAMBERT_Y!= "")
bpe_st <- st_as_sf(bpe, coords = c("LAMBERT_X","LAMBERT_Y"), crs=2154)
nord <- st_transform(nord, crs=2154)
joker <- bpe_st %>% filter(TYPEQU =="D104"|TYPEQU =="D109"|
                            TYPEQU =="D207")
joker$type <- ifelse(joker$TYPEQU == "D104", "Avec hébergement","Sans hébergement")
joker$type <- factor(joker$type , c("Sans hébergement","Avec hébergement"))

# Carte -----

ggplot()+ geom_sf(data=nord, fill="gray80", col="white", size=.4) +
  geom_sf(data=joker, aes(col=type), show.legend = "point")+
  scale_color_manual (values = c("gray20","chocolate2"))+
  coord_sf(datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill = "white"),
        title=element_text(size=16))+
  labs(col="", title = "Structures et spécialistes en psychiatrie dans le Nord",
       caption = "Source : Base Permanente des Equipements, Insee, 2018") 

# output ----
ggsave( path ="output", "30daymapchallenge_28.png",width=16, height=8,dpi=300)
