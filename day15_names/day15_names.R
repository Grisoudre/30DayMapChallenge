# Day 15 - Names
# Noms du Registre National des Elus

# Packages  ----

library(tidyverse)
library(sf)
library(viridis)

# Imports ----

rne <- read.table("data/1-rne-cm.csv",fileEncoding = "WINDOWS-1252",
                  sep="\t",stringsAsFactors = F, header=T) 
shpNord <- st_read("data/shpNord.shp")

# Mise en forme ----

rne <- rne %>% filter(dep =="59")
rne$comp <- ifelse(str_detect(rne$prenom,"-")==T,1,0)

rne$com <- ifelse(nchar(rne$com)==1, paste0("00",rne$com),
                  ifelse(nchar(rne$com)==2, paste0("0", rne$com),
                         rne$com))
rne <- rne %>%  
  group_by(com) %>% 
  summarise(membres = n(),
            comp=sum(comp)) %>% 
  mutate(comp_perc = round(comp/membres*100,1),
         com= paste0("59",com))

shpNord <- merge(shpNord, rne,
                 by.x ="insee", by.y="com", all.x=T)

# Carte --------


ggplot(shpNord, aes(fill=comp_perc))+
  geom_sf(col="transparent")+
  scale_fill_viridis(option="E")+
  labs(title ="Prénoms composés dans les conseils municipaux",
       fill="Part de membres des
conseils municipaux
avec un prénom composé",
       caption = "Source : Répertoire National des Elus, 2019")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill="transparent"),
        title=element_text(size=16))+
  coord_sf( datum = NA)

# Export ----

ggsave("output/day15_name.png", width = 11, height = 9, dpi = 300)
