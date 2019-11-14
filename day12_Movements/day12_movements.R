# Packages  ----

library(tidyverse)
library(sf)
library(cartogram)
library(viridis)

# Imports ----

shpNord <- st_read("data/shpNord.shp")
mob <- read.csv2("data/base-excel-flux-mobilite-domicile-lieu-travail-2015.csv")

# Fonds de carte -----------

shpNord <- merge(shpNord, mob %>% filter(substr(CODGEO,1,2)=="59") %>% 
                   select(CODGEO, C15_ACTOCC15P),
                 by.x ="insee", by.y="CODGEO", all.x=T)

# shpNord <- cartogram_cont(shpNord,"C15_ACTOCC15P" , itermax=7)
# /!\ peut être long

# Mise en forme ----

mob <- mob %>% 
  filter(substr(CODGEO,1,2)=="59") %>%
  mutate(nExt=C15_ACTOCC15P_ILTAUT/ C15_ACTOCC15P*100,
         nLocal = C15_ACTOCC15P_ILT1/C15_ACTOCC15P*100) 

shpNord <- merge(shpNord, mob %>% select(CODGEO, 
                                           nExt, nLocal) ,
                 by.x="insee",by.y="CODGEO",all.x=T)

# Carte --------


ggplot(shpNord, aes(fill=nLocal))+
  geom_sf(col="transparent")+
  scale_fill_viridis(option="E")+
  labs(title ="Mobilités professionnelles dans le Nord",
       fill="Part des 15 ans et plus 
en emploi travaillant dans
leur commune de résidence (%)",
       caption = "Cartogramme selon le nombre de personnes 
de 15 ans et plus en emploi résidant dans la commune.
Source : Mobilités professionnelles des individus, Insee, 2015")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill="transparent"),
        title=element_text(size=16))+
  coord_sf( datum = NA)


# Export ----

ggsave("output/day12_Movements.png", width = 11, height = 9, dpi = 300)
