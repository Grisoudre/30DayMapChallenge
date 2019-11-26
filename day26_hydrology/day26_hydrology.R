# 30DayMapChallenge - day 26 - Hydrology | cours d'eau dans le Nord ----

# Packages ----
library(tidyverse)
library(sf)
library(ggspatial)

# Imports ----

lic <- read.csv2("data/lic-data-2016.csv", fileEncoding = "WINDOWS-1252",
                 stringsAsFactors = F)
water <- st_read("data/gis_osm_waterways_free_1.shp")
nord <- st_read("data/Nord_COM.shp")
pdc <- st_read("data/Pdc_COM.shp")

# Mise en forme ----


lic <- lic %>% 
  filter(substr(code_commune,1,2) %in% c("59","62")) %>%
  filter(str_detect(nom_fed,"canoë|kayak")) %>% 
  group_by( code_commune) %>% 
  summarise(l_2016 = sum(l_2016)) %>% 
   arrange(desc(l_2016)) %>% 
  select(code_commune, l_2016)

nord <- merge(nord, lic, by.x="insee",by.y="code_commune",all.x=T)
pdc <- merge(pdc, lic, by.x="insee",by.y="code_commune",all.x=T)

# Carte --------

ggplot()+
  annotation_map_tile(zoom=10,cachedir = system.file("rosm.cache", package = "ggspatial"),
                      type="cartodark") +
  geom_sf(data=nord, aes(fill = l_2016), col="transparent") + 
  geom_sf(data=pdc, aes(fill = l_2016), col="transparent") + 
  scale_fill_gradient(low = "cadetblue4", high = "cadetblue1", na.value="transparent")+
  geom_sf(data=water$geometry, 
        col="lightcyan4", size=.8)+
  geom_sf(data=water$geometry, 
          col="lightcyan3", size=.6)+
  geom_sf(data=water$geometry, 
          col="lightcyan2", size=.4)+
  geom_sf(data=water$geometry, 
          col="lightcyan1", size=.2)+
  labs(title = "Cours d'eau et licences de canoë-kayak dans le Nord et le Pas-de-Calais", 
       caption="Source : Injep, fond : OSM",
       fill="Nombre de\nlicences (2016)")+ 
  theme(panel.grid.major = element_line(colour = 'transparent'),
        title=element_text(size=16))+
  coord_sf( datum = NA)

# Export -----

ggsave(  path ="output", "30daymapchallenge_day26.png",width=12, height=9, dpi=300)

