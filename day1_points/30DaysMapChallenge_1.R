# 30 day map challenge - jour 1 - points / bars de lille -----------
# sources : https://overpass-turbo.eu/ + RP 2016

# Packages ----
library(tidyverse)
library(sf)
library(geojsonio)
library(viridis)
library(ggspatial)

# imports ----
data <- geojson_read("data/lille_bar_cafe_pub_restaurant.geojson",
                                what = "sp")
lille <- st_read("data/Lille_IRIS.shp")
lille_pop_16 <- read.csv2("data/Lille_IRIS_pop_16.csv")

# mise en forme ----
data <- st_as_sf(data)
data <- st_transform(data,st_crs(lille))
data <- data %>% filter(amenity!='restaurant')
lille$IRIS <- paste0(lille$INSEE_COM, lille$IRIS)
lille <- merge(lille, lille_pop_16, by="IRIS")
rm(lille_pop_16)
lille$nb_amenity <- lengths(st_intersects(lille, data))
lille <- lille %>% mutate(P16_18P=P16_POP1824+P16_POP2539+
                             P16_POP4054+P16_POP5564+P16_POP6579+P16_POP80P,
                          hab_par_bar = P16_18P/nb_amenity)
lille$hab_par_bar[lille$hab_par_bar=="Inf"]<-NA
data <- st_join(data, lille) %>%
  filter(!is.na(NOM_IRIS))

# carte ----
 ggplot() +
  annotation_map_tile(zoom=13,cachedir = system.file("rosm.cache", package = "ggspatial"),
                      type="cartolight") +
  # geom_sf(data = st_geometry(lille), aes(fill=lille$hab_par_bar), color="snow3")+
  # scale_fill_gradient(high="#677478", low="#a9bcc2", na.value = "grey90") +
  geom_sf(data = st_geometry(lille), fill="transparent", color="snow3")+
  geom_sf(data = st_geometry(data), aes(color=data$amenity,size=data$hab_par_bar),show.legend = 'point') +
  theme_minimal() + scale_color_viridis(discrete=T)+
  labs(title = "Bars, pubs et cafés à Lille",  color="Type d'établissement",
       size="Nb hab.18ans+/établissement\ndans l'IRIS", caption="Sources : OpenStreetMap, Recensement 2016 Insee, fond : OSM")+ 
  theme(panel.grid.major = element_line(colour = 'transparent'), title=element_text(size=16))+
coord_sf(crs = st_crs(lille), datum = NA)

# output ----
ggsave( path ="output", "30daymapchallenge_1.png",width=16, height=8,dpi=300)
