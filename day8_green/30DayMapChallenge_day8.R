# 30DayMapChallenge - day 7 - Red - Vote communiste dans le Nord aux municipales----

# https://www.data.gouv.fr/fr/posts/les-donnees-des-elections/

# Packages ----
library(tidyverse)
library(sf)
library(questionr)
library(ggspatial)

# Imports ----

land <- st_read("data/gis_osm_landuse_a_free_1.shp")
natural <- st_read("data/gis_osm_natural_free_1.shp")
lille <- st_read("data/Lille_commune.shp")

# Mise en forme ----

land <- st_transform(land,st_crs(lille))
natural <- st_transform(natural,st_crs(lille))

land <- st_join(land, lille) %>%
  filter(!is.na(insee))
natural <- st_join(natural, lille) %>%
  filter(!is.na(insee))

land <- land %>% filter(fclass == "forest" |
                          fclass == "grass" |
                          fclass == "park")
natural <- natural %>% filter(fclass == "tree")

# Cartes et sorties ----

ggplot()+
  annotation_map_tile(zoom=13,cachedir = system.file("rosm.cache", package = "ggspatial"),
                      type="cartodark") +
  geom_sf(data=land$geometry, fill="aquamarine4",
          col="aquamarine1", size=.4)+
  labs(title = "Espaces verts à Lille", 
       caption="Source : OSM, Landuse = forest, grass ou park, fond : OSM")+ 
  theme(panel.grid.major = element_line(colour = 'transparent'), title=element_text(size=16))+
  coord_sf( datum = NA)


ggsave(  path ="output", "30daymapchallenge_day8_1.png",width=12, height=9, dpi=300)

ggplot()+
  annotation_map_tile(zoom=13,cachedir = system.file("rosm.cache", package = "ggspatial"),
                      type="cartodark") +
  geom_sf(data=land$geometry, fill="aquamarine4",
          col="aquamarine1", size=.4)+
  geom_sf(data=natural$geometry,
          col="white", shape =3)+
  labs(title = "Espaces verts et arbres à Lille", 
       caption="Source : OSM, Landuse = forest, grass ou park et Natural = tree, fond : OSM")+ 
  theme(panel.grid.major = element_line(colour = 'transparent'),
        title=element_text(size=16))+
  coord_sf( datum = NA)

ggsave(  path ="output", "30daymapchallenge_day8_2.png",width=12, height=9, dpi=300)

