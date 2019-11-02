# 30 day map challenge - jour 1 - points / bars de lille -----------
# sources : https://overpass-turbo.eu/
# + MEL : https://opendata.lillemetropole.fr/explore/dataset/metropole-cyclable-2020-schema-cyclable/map/?flg=fr&location=13,50.62932,3.05729&basemap=jawg.streets

# Packages ----
library(tidyverse)
library(sf)
library(geojsonio)
library(viridis)
library(ggspatial)

# imports ----
data <- st_read("data/highway.geojson")
mel <- st_read("data/metropole-cyclable-2020-schema-cyclable.geojson")
lille <- st_read("data/Lille_commune.shp")

# mise en forme --------

data <- st_transform(data,st_crs(lille))
data <- st_join(data, lille) %>%
  filter(!is.na(nom))
mel <- st_transform(mel,st_crs(lille))
mel <- mel %>% filter(commune =="LILLE"|commune =="LOMME"|commune =="HELLEMMES") %>% 
  filter(etat=="PROJET"|etat=="EXISTANT")
mel.proj <- mel %>% filter(commune =="LILLE"|commune =="LOMME"|commune =="HELLEMMES") %>% 
  filter(etat=="PROJET")
mel.ex <- mel %>% filter(commune =="LILLE"|commune =="LOMME"|commune =="HELLEMMES") %>% 
  filter(etat=="EXISTANT")
mel$etat <- str_to_title(mel$etat)

names(data)[str_detect(names(data),"cycle")]
data.cycle <- data %>% filter(str_detect(highway,"cycle") ==T |
                           bicycle=="yes"|
                           cycleway=="yes"|
                           oneway.bicycle=="yes"|
                           oneway.bicycle=="designated"|
                           oneway.bicycle=="lane")

# carte ----

# OSM :
 # ggplot() +
 #  annotation_map_tile(zoom=13,cachedir = system.file("rosm.cache", package = "ggspatial"),
 #                      type="cartodark") +
 #  geom_sf(data = st_geometry(data), fill="transparent", color="grey50")+
 #  geom_sf(data = st_geometry(data.cycle),color="aquamarine1", size=2.1) +
 #  geom_sf(data = st_geometry(data.cycle),color="aquamarine3", size=1.4) +
 #  geom_sf(data = st_geometry(data.cycle),color="aquamarine4", size=.7) +
 #  geom_sf(data = st_geometry(lille),fill="transparent", color="yellow", size = .3)+
 #  labs(title = "Pistes et voies cyclables à Lille", caption="Source : OpenStreetMap, fond : OSM")+ 
 #  theme(panel.grid.major = element_line(colour = 'transparent'), title=element_text(size=16))+
 #  coord_sf( datum = NA)

# MEL :

ggplot() +
  annotation_map_tile(zoom=13,cachedir = system.file("rosm.cache", package = "ggspatial"),
                      type="cartodark") +
  geom_sf(data = st_geometry(mel),aes(color=mel$etat), size=1.4, fill="transparent") + 
  scale_color_manual(values=c("chocolate3","aquamarine4"),aesthetics = "color") +
  geom_sf(data = st_geometry(data), fill="transparent", color="grey50")+
  geom_sf(data = st_geometry(mel.proj),color="chocolate1", size=1.4) +
  geom_sf(data = st_geometry(mel.proj),color="chocolate3", size=.7) +
  geom_sf(data = st_geometry(mel.ex),color="aquamarine1", size=2.1) +
  geom_sf(data = st_geometry(mel.ex),color="aquamarine3", size=1.4) +
  geom_sf(data = st_geometry(mel.ex),color="aquamarine4", size=.7) +
  geom_sf(data = st_geometry(lille),fill="transparent", color="yellow", size = .3)+
  labs(title = "Pistes et voies cyclables à Lille", caption="Source : MEL, fond : OSM",
       color="Etat")+ 
  theme(panel.grid.major = element_line(colour = 'transparent'), title=element_text(size=16))+
  coord_sf( datum = NA)

# output ----
ggsave( path ="output", "30daymapchallenge_2.png",width=16, height=8,dpi=300)
