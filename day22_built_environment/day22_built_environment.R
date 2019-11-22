# 30DayMapChallenge - Day 22 | Built environment | Cadastre Bâti

# https://www.data.gouv.fr/fr/datasets/cadastre/#_

# Packages ----
library(tidyverse)
library(sf)
library(ggspatial)
library(geojsonio)
library(questionr)

# Imports ----

bati <- st_read("data/batiments.shp") %>% filter(commune == "59350")
lille <- st_read("data/Lille_commune.shp")


bati <- st_transform(bati, crs=2154)
lille <- st_transform(lille, crs=2154)

bati$type <- fct_recode(factor
                          (bati$type),
                        "dur"="01",
                        "léger"="02")

# Carte ----

ggplot()+ 
  annotation_map_tile(zoom=15,cachedir = system.file("rosm.cache", package = "ggspatial"),
                      type="cartodark") +
  geom_sf(data=lille, col = "grey20", fill="transparent", size=1.5) +
  geom_sf(data=bati, aes(fill = type), col ="transparent", show.legend = F) +
  scale_color_manual(values=c("grey60","grey30"),aesthetics = "fill") +
  coord_sf(crs = st_crs(lille), datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        title=element_text(size=20))+
  labs(col="", title = "Bâti à Lille",
       caption = "Source : Cadastre, Ministère de l'économie et des finances, mise en forme Etalab, 2019 | Fond : OSM")


# Output ----

ggsave(  path ="output", "30daymapchallenge_day22.png",width=12, height=8, dpi=300)

 