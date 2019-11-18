# 30DayMapChallenge - day 17 - Zones commerciales ----


# Packages ----
library(tidyverse)
library(sf)
library(ggspatial)

# Imports ----

land <- st_read("data/gis_osm_landuse_a_free_1.shp")
mel <- st_read("data/MEL_IRIS.shp")

# Mise en forme ----

land <- st_transform(land,st_crs(mel))

land <- st_join(land, mel) %>%
  filter(!is.na(IRIS))

land <- land %>% filter(fclass == "retail" |
                          fclass == "commercial" )
mel <- mel %>% group_by(LIBEPCI) %>% summarise(n =n())
  
# Cartes et sorties ----


ggplot()+
  annotation_map_tile(zoom=11,cachedir = system.file("rosm.cache", package = "ggspatial"),
                      type="cartolight") +
  geom_sf(data=mel, fill="transparent", size=.2)+
  geom_sf(data=land, fill="grey40",col="grey20", size=.4)+
  labs(title = "Zones commerciales et zones de bureaux dans la MEL", 
       caption="Source : OSM, Landuse = commercial ou retail, fond : OSM")+ 
  theme(panel.grid.major = element_line(colour = 'transparent'),
        title=element_text(size=16))+
  coord_sf( datum = NA)

# out-put-------
ggsave(  path ="output", "30daymapchallenge_day17.png",width=9, height=9, dpi=300)

