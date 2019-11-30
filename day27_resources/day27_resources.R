# 30 day map challenge - day 27 - resources | bibliothèques dans la MEL -----------
# sources : open data MEL : https://opendata.lillemetropole.fr/explore/dataset/bibliotheques-mel/export/

# Packages ----
library(tidyverse)
library(sf)

# imports ----
bbq <- st_read("data/bibliotheques-mel.shp")
mel <- st_read("data/MEL_IRIS.shp")
mel <- aggregate(mel, by = list(mel$INSEE_COM), FUN = mean)
bbq <- st_transform(bbq, crs = st_crs(mel))
bbq$type <- ifelse(str_detect(bbq$libelle, "Mediatheque"),
                  "Médiathèques","Bibliothèques")

# Carte -----

ggplot()+ geom_sf(data=mel, fill="gray70", col="snow1", size=.4) +
  geom_sf(data=bbq, aes(col=type), show.legend = "point")+
  scale_color_manual (values = c("gray20","chocolate2"))+
  coord_sf(datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill = "white"),
        title=element_text(size=16))+
  labs(col="", title = "Bibliothèques dans la Métropole Européenne de Lille",
       caption = "Source : opendata.lillemetropole, 2019") 

# output ----
ggsave( path ="output", "30daymapchallenge_27.png",width=16, height=8,dpi=300)
