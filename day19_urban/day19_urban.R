# 30DayMapChallenge - Day 19 | Urban : lignes et arrêts de bus dans la MEL

# Packages ----
library(tidyverse)
library(sf)
library(ggspatial)

# imports ----
arrets <- st_read("data/ilevia-physicalstop.shp")
lignes <- st_read("data/ilevia-traceslignes.shp")

# Carte ----

ggplot() +
   annotation_map_tile(zoom=12,cachedir = system.file("rosm.cache", package = "ggspatial"),
                       type="cartodark") +
  geom_sf(data = st_geometry(lignes), size=1, col="chocolate3") + 
  geom_sf(data = st_geometry(arrets), shape=4, size=2, color="burlywood") +
  labs(title = "Lignes et arrêts de bus dans la MEL", caption="Sources : MEL, ilevia, fond : OSM",
       color="Etat")+ 
  theme(panel.grid.major = element_line(colour = 'transparent'), title=element_text(size=16))+
  coord_sf( datum = NA)

# output ----

ggsave(  path ="output", "30daymapchallenge_day19.png",width=10, height=10, dpi=300)
