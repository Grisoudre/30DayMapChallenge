# 30DayMapChallenge - day 25 - Climate | Lignes RFN non exploitées ----

# https://www.data.gouv.fr/fr/datasets/fichier-de-formes-des-lignes-du-reseau-ferre-national/

# Packages ----

library(sf)
library(tidyverse)
library(questionr)

# Imports -----

sf <- st_read("data/formes-des-lignes-du-rfn.shp")
nord <- st_read("data/Nord_COM.shp")

# Mise en forme ----
sf$ex <- ifelse (sf$mnemo =="EXPLOITE" , "Exploitées",
                 ifelse ((sf$mnemo !="EXPLOITE" &
                           sf$mnemo !="PROJET"  &
                           sf$mnemo !="VS" ), "Neutralisées,
fermées, 
déclassées ou
retranchées",""))
sf <- sf %>% filter(ex!="")
sf_n <- st_intersection(nord, sf)

# Cartes ----

ggplot() +
  geom_sf(data=nord, col = "grey90",fill="transparent", size=.4)+
  geom_sf(data=sf_n,aes(color=ex), size=2, show.legend="line")+
  scale_color_manual(values=c("grey80","red"))+
  coord_sf( datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect("transparent"),
        title=element_text(size=16))+
  labs(color="",title = "Lignes du Réseau Ferré National non-exploitées - Nord", 
       caption="Source : SNCF, 2019")

ggsave("output/day25_climate_1.png", width = 12, height = 9, dpi = 300)

ggplot() +
  geom_sf(data=sf, aes(color=ex), size=2, show.legend="line")+
  scale_color_manual(values=c("grey80","red"))+
  coord_sf( datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect("transparent"),
        title=element_text(size=16))+
  labs(color="",title = "Lignes du Réseau Ferré National non-exploitées", 
       caption="Source : SNCF, 2019")
  

ggsave("output/day25_climate_2.png", width = 12, height = 10, dpi = 300)


