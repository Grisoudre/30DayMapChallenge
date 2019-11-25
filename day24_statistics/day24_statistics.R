# 30DayMapChallenge - day 23 - Statistics | Carte choroplète bivariée ----

# https://www.data.gouv.fr/fr/posts/les-donnees-des-elections/
# https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/
# https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html

# Packages ----
library(tidyverse)
library(sf)
library(viridis)
library(biscale)
library(ggspatial)
library(cowplot)

# Imports ----

leg17 <- read.csv2("data/Leg_2017_Resultats_communes_T1_c (1).csv",
                   stringsAsFactors = F)
pre17 <- read.csv2("data/Presidentielle_2017_Resultats_Communes_Tour_1_c.csv",
                   stringsAsFactors = F)
nord <- st_read("data/Nord_COM.shp")


# Mise en forme --------

nord$insee[nord$insee=="59404"] <- "59588"
nord$insee[nord$insee=="59154"] <- "59260"

leg17$Code.de.la.commune <- ifelse(nchar(leg17$Code.de.la.commune)==1,
                                   paste0("00", leg17$Code.de.la.commune),
                                   ifelse(nchar(leg17$Code.de.la.commune)==2,
                                          paste0("0", leg17$Code.de.la.commune),
                                          leg17$Code.de.la.commune))

pre17$Code.de.la.commune <- ifelse(nchar(pre17$Code.de.la.commune)==1,
                                   paste0("00", pre17$Code.de.la.commune),
                                   ifelse(nchar(pre17$Code.de.la.commune)==2,
                                          paste0("0", pre17$Code.de.la.commune),
                                          pre17$Code.de.la.commune))
leg17 <- leg17 %>% 
  filter(Code.du.département=="59") %>% 
  group_by(Code.de.la.commune) %>% 
  summarise(Inscrits =sum(Inscrits),
            Abstentions = sum(Abstentions) ) %>% 
  mutate(ab.leg17 = round(Abstentions/Inscrits*100,1),
         cog = paste0("59",Code.de.la.commune )) %>% 
  select(cog, ab.leg17)


pre17 <- pre17 %>% 
  filter(Code.du.département=="59") %>% 
  group_by(Code.de.la.commune) %>% 
  summarise(Inscrits =sum(Inscrits),
            Abstentions = sum(Abstentions) ) %>% 
  mutate(ab.pre17 = round(Abstentions/Inscrits*100,1),
         cog = paste0("59",Code.de.la.commune )) %>% 
  select(cog, ab.pre17)

nord <- merge(nord, leg17, by.x="insee",by.y="cog", all.x=T)
nord <- merge(nord, pre17, by.x="insee",by.y="cog", all.x=T)

data <- bi_class(nord, x = ab.leg17, y = ab.pre17, style = "fisher", dim = 3)

# Carte -------


pal <- "DkViolet"
map <- ggplot()+ 
  annotation_map_tile(zoom=9,
                      cachedir = system.file("rosm.cache",
                                             package = "ggspatial"),
                      type="cartolight")+
  geom_sf(data=data,
          aes(fill=bi_class),
          color="snow2",
          size=.1,
          show.legend = F) +
  bi_scale_fill(pal = pal, dim = 3)+
labs(title = "Abstention aux élections présidentielles et législatives de 2017", 
     caption="Source : Ministère de l'Intérieur, package : slu-openGIS/biscale - style = fisher, fond : OSM")+ 
  theme(panel.grid.major = element_line(colour = 'transparent'),
        title=element_text(size=18))+
  coord_sf( datum = NA)
legend <- bi_legend(pal = pal,
                      dim = 3,
                      xlab = "Legislatives",
                      ylab = "Présidentielles",
                      size = 14)
  
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.17, .1, 0.2, 0.2)

# Output----
ggsave(  path ="output", "30daymapchallenge_day24.png",width=12, height=9, dpi=300)
