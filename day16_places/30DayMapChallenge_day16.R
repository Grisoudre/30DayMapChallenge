# 30DayMapChallenge - day 16 - Places - Places de parking voiture et vélo à lille ----

# Geofabrik

# Packages ----
library(tidyverse)
library(sf)
library(viridis)

# Imports ----

traffic <- st_read("data/gis_osm_traffic_a_free_1.shp")
trafficb <- st_read("data/gis_osm_traffic_free_1.shp")

lille <- st_read("data/Lille_commune.shp")

# Mise en forme -----
# Lille :
traffic <- st_transform(traffic,st_crs(lille))
trafficb <- st_transform(trafficb,st_crs(lille))


traffic <- st_join(traffic, lille) %>%
  filter(!is.na(insee))
trafficb <- st_join(trafficb, lille) %>%
  filter(!is.na(insee))


diff <- st_difference(st_as_sfc(st_bbox(lille)), lille$geometry)

# Voitures :
voitures <- traffic %>% filter(fclass=="parking"|
                                 fclass=="parking_multistorey"|
                                 fclass=="parking_underground")
voituresb <- trafficb %>% filter(fclass=="parking"|
                                   fclass=="parking_multistorey"|
                                   fclass=="parking_underground")
voituresd <- rbind(voitures, voituresb)
voituresd <- st_centroid(voituresd$geometry)
voituresc <- data.frame(st_coordinates(voituresd))

# Vélos :

velos <- traffic %>% filter(fclass=="parking_bicycle")
velosb <- trafficb %>% filter(fclass=="parking_bicycle")


velosd <- rbind(velos, velosb)
velosd <- st_centroid(velosd$geometry)
velosc <- data.frame(st_coordinates(velosd))


# Cartes -----

a <- ggplot()+
  geom_sf(data=lille, fill=viridis(2, option="E")[1], col="transparent") +
  stat_density_2d(data=voituresc,aes(x=X, y=Y,
                                     fill = ..level..), 
                  geom="polygon", colour="transparent",
                  bins=20) + 
  scale_fill_viridis(option="E") +
  geom_sf(data=diff, fill="white", col="transparent") +
  geom_sf(data=diff, fill="white", col="transparent") +
  geom_sf(data=voitures, fill = "black", col = "transparent") +
  geom_sf(data=voituresb, col="black", shape = 1, size=.5)+
  coord_sf(crs = st_crs(lille), datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        title=element_text(size=16))+
  labs(x = "", y = "", fill="densité", title = "Places de parking à Lille",
       caption = "Source : OpenStreetMap") 

b <-  ggplot()+
  geom_sf(data=lille, fill=viridis(2, option="E")[1], col="transparent") +
  stat_density_2d(data=velosc,aes(x=X, y=Y,
                                     fill = ..level..), 
                  geom="polygon", colour="transparent",
                  bins=20) +
  scale_fill_viridis(option="E") +
  geom_sf(data=diff, fill="white", col="transparent") +
  geom_sf(data=diff, fill="white", col="transparent") +
  geom_sf(data=velos, fill = "black", col = "transparent") +
  geom_sf(data=velosb, col="black", shape = 1, size=.5)+
  coord_sf(crs = st_crs(lille), datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        title=element_text(size=16))+
  labs(x = "", y = "", fill="densité", title = "Places de parking vélo à Lille",
       caption = "Source : OpenStreetMap") 

gridExtra::grid.arrange(a, b, nrow = 2)

# output ------------

ggsave(  path ="output", "30daymapchallenge_day16.png",width=12, height=18, dpi=300)

