# Packages ----
library(tidyverse)
library(sf)
library(geogrid)

# Imports ----
mel_com <- st_read("data/MEL_IRIS.shp")
pauv_15 <- read.csv2("data/FILO_DEC_COM.csv", stringsAsFactors = F)


# Mise en forme ----

mel_com$EPCI <- as.integer(mel_com$EPCI)
mel_com <- aggregate(mel_com[,"EPCI"], by=list(mel_com$INSEE_COM), FUN="sum") %>% 
  rename("INSEE_COM"=Group.1) %>% select(INSEE_COM, geometry)

mel_com.sp <- as(mel_com, "Spatial")
hexa <- calculate_grid(mel_com.sp, grid_type = "hexagonal", seed=4)
hexa <- assign_polygons(mel_com.sp, hexa) # très très long
hexa <- st_as_sf(hexa)

hexa <- merge(hexa, pauv_15, by.x="INSEE_COM", by.y="CODGEO", all.x=T)

# Carte ----

per_mille<-scales::number_format(accuracy = 1)

ggplot()+
  geom_sf(data=st_geometry(hexa), aes(fill = hexa$D915), col="grey60") +
  viridis::scale_fill_viridis(na.value = "grey80",labels =per_mille)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill= "white"),title=element_text(size=16) )+
  coord_sf(crs = st_crs(hexa), datum = NA)+
  labs(fill = "9ème décile de\nrevenu déclaré (€)",
       title="Hauts revenus dans les communes de la\nMétropole Européenne de Lille",
       caption = "Source : FiLoSoFi 2015")

# Output ----

ggsave( path ="output", "30daymapchallenge_4.png",width=16, height=8,dpi=300)
