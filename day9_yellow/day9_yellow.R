# Packages ----

library(tidyverse)
library(sf)
library(geojsonio)
library(ggspatial)

# Imports ----

rond <- geojson_read("data/Overpass_turbo_junction_roundabout.geojson",
                     what = "sp")
lille <- st_read("data/Lille_commune.shp")
roads <- st_read("data/gis_osm_roads_free_1.shp")

# Mise en forme ----

rond <- st_as_sf(rond)
rond <- st_transform(rond, st_crs(lille))
roads <- st_transform(roads, st_crs(lille))
plot(rond$geometry)
rond <- st_join(rond, lille) %>%
  filter(!is.na(insee))
roads <- st_join(roads, lille) %>%
  filter(!is.na(insee))

# Carte ----
ggplot()+
  annotation_map_tile(zoom=13,cachedir = system.file("rosm.cache", package = "ggspatial"),
                      type="cartodark") +
  geom_sf(data=lille$geometry, fill="transparent",
          col="yellow", size=.2)+
  geom_sf(data=roads$geometry, fill="transparent",
          col="grey30", size=.4)+
  geom_sf(data=rond$geometry,fill = "transparent",
          col="darkgoldenrod3",size = 2, shape =3)+
  geom_sf(data=rond$geometry,fill = "transparent",
          col="darkgoldenrod1",size = 1, shape =3)+
  labs(title = "Ronds-Points à Lille", 
       caption="Source : OSM | https://overpass-turbo.eu/, Junction = roundabout, fond : OSM")+ 
  theme(panel.grid.major = element_line(colour = 'transparent'),
        title=element_text(size=16))+
  coord_sf( datum = NA)

# Output ----

ggsave(  path ="output", "30daymapchallenge_day9.png",width=12, height=9, dpi=300)
