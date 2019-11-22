# 30DayMapChallenge - Day 21 | Sites Seveso dans le Nord

# ICPE Installation Classées pour la Protection de l'Environnement :
# http://www.georisques.gouv.fr/dossiers/telechargement

# Packages ----

library(sf)
library(ggspatial)
library(geojsonio)

# Imports ----

icpe <- st_read("data/InstallationsClassees_France.shp")
nord <- st_read("data/shpNord.shp")

icpe <- st_transform(icpe, crs=2154)
nord <- st_transform(nord, crs=2154)

icpe$seveso <- fct_recode(factor(icpe$seveso),
                          "Non seveso" = "NS",
                          "Seveso seuil bas" = "SB",
                          "Seveso seuil haut" = "SH")

# Carte ----

ggplot()+ 
  annotation_map_tile(zoom=9,cachedir = system.file("rosm.cache", package = "ggspatial"),
                      type="cartodark") +
  geom_sf(data=nord, col = "grey20", fill="transparent") +
  geom_sf(data=icpe, aes(col = seveso),  size=1.7, show.legend = "point")+ 
  scale_color_manual(values=c("grey50","chocolate2", "brown3"),aesthetics = "color") +
  coord_sf(crs = st_crs(nord), datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        title=element_text(size=16))+
  labs(col="", title = "Installations Classées pour la Protection de l'Environnement dans le Nord",
       caption = "Source : Ministère de la transition écologique et solidaire, 2019 | Fond : OSM")


# Output ----

ggsave(  path ="output", "30daymapchallenge_day21.png",width=12, height=8, dpi=300)

 