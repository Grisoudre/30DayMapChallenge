# Packages  ----

library(tidyverse)
library(sf)
library(viridis)
library(questionr)
# Imports ----

shpNord <- st_read("data/shpNord.shp")
edition <- read.csv2("data/etablissements (1).csv",sep=",", stringsAsFactors = F)

# Mise en forme ----

editionnord.nb <- edition %>% 
  filter(substr(codeCommuneEtablissement,1,2)=="59") %>% 
  group_by(codeCommuneEtablissement) %>% 
  summarise(nbed = n())

shpNord <- merge(shpNord, editionnord.nb, by.x = "insee",
                 by.y = "codeCommuneEtablissement", all.x=T)

# Carte ----

ggplot(shpNord) + 
  geom_sf(aes(fill = nbed))+
  scale_fill_viridis(na.value = "snow2")+
  labs(title ="Edition musicale dans le Nord",
       fill="Nombre d'établissements 
d'enregistrement sonore 
et d'édition musicale",
       caption = "Source : SIRENE, Insee, 2017")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill="transparent"),
        title=element_text(size=16))+
  coord_sf( datum = NA)

# Export ----

ggsave("output/day13_Tracks.png", width = 11, height = 9, dpi = 300)
