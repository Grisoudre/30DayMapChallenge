# 30DayMapChallenge - Day 31 | Mobilisations ESR

# Packages ----
library(tidyverse)
library(sf)
library(geojsonio)
library(questionr)

# Imports ----
mob <- read.csv2("data/Mobilisation.csv", stringsAsFactors = F) 
com <- st_read("data/communes-20150101-50m.shp")
dep <- st_read("data/departements-20140306-50m.shp")

# Mises en forme ----
# France métro
com <- com %>% filter(substr(com$insee,1,2) !="97") %>% 
  st_transform(crs=2154)
dep <- dep %>% filter(substr(dep$code_insee,1,2) !="97") %>% 
  st_transform(crs=2154)

# Ajout COM-DOM / voir package cartelette / A FAIRE

# Villes en tant que points
comd <- st_centroid(com %>% filter(is.na(n)==F) %>% 
                      select("geometry", "insee"))
comc <- data.frame(st_coordinates(comd), comd$insee)
comc <-  st_as_sf(comc, coords = c("X", "Y"), crs=st_crs(com))

mobb_u <- mob %>% filter(Echelle !="Revue" & Echelle !="Laboratoire") %>% 
  group_by(Ville, Insee) %>%  summarise(n = n())
mobb_r <- mob %>% filter(Echelle =="Revue") %>% 
  group_by(Ville, Insee) %>%  summarise(n = n())
mobb_l <- mob %>% filter(Echelle =="Laboratoire") %>% 
  group_by(Ville, Insee) %>%  summarise(n = n())

# Jointure table mobb et fonds
com_u <- merge(comc, mobb_u, by.x = "comd.insee", by.y = "Insee",
               all.x=T) %>% filter(is.na(n)==F)
com_r <- merge(comc, mobb_r, by.x = "comd.insee", by.y = "Insee",
               all.x=T) %>% filter(is.na(n)==F)
com_l <- merge(comc, mobb_l, by.x = "comd.insee", by.y = "Insee",
               all.x=T) %>% filter(is.na(n)==F)

# Carte -----
# Départements, UFR et universités :
ggplot()+ 
  geom_sf(data=dep, col = "gray10", size=.2)+
  geom_sf(data=com_u, aes(size=n),col="firebrick", show.legend = "point")+
  coord_sf(datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill = "white"),
        title=element_text(size=16))+
  labs(size="Nombre de composantes\n déclarées", 
       title = "Départements, UFR et universités en lutte",
       caption = "Source : Collectif de mobilisation") 

ggsave( path ="output", "Carte_ESR_Lutte_univ.png",width=16, height=8,dpi=300)

# Revues
ggplot()+ 
  geom_sf(data=dep, col = "gray10", size=.2)+
  geom_sf(data=com_r, aes(size=n),col="seagreen", show.legend = "point")+
  coord_sf(datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill = "white"),
        title=element_text(size=16))+
  labs(size="Nombre de revues\n déclarées", 
       title = "Revues en lutte",
       caption = "Source : Collectif de mobilisation") 


ggsave( path ="output", "Carte_ESR_Lutte_revues.png",width=16, height=8,dpi=300)

# Laboratoires
ggplot()+ 
  geom_sf(data=dep, col = "gray10", size=.2)+
  geom_sf(data=com_l, aes(size=n),col="steelblue", show.legend = "point")+
  coord_sf(datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill = "white"),
        title=element_text(size=16))+
  labs(size="Nombre de laboratoires\n déclarées", 
       title = "Laboratoires en lutte",
       caption = "Source : Collectif de mobilisation") 

ggsave( path ="output", "Carte_ESR_Lutte_labo.png",width=16, height=8,dpi=300)
