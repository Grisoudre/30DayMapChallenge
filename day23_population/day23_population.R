# Packages  ----

library(tidyverse)
library(sf)
library(cartogram)
library(viridis)

# Imports ----

pop <- read.csv2("data/base-ic-evol-struct-pop-2016.csv")
shpNord <- st_read("data/shpNord.shp")

# Mise en forme -----------
pop <- pop %>% filter(DEP=="59")
popd <- pop %>% group_by(COM) %>% summarise(M2ans = round(sum(P16_POP0002)/sum(P16_POP)*100,1),
                                            P75ans = round(sum(P16_POP75P)/sum(P16_POP)*100,1),
                                            P65ans = round(sum(P16_POP65P)/sum(P16_POP)*100,1),
                                            pop = sum(P16_POP))

# Fonds de carte -----------

shpNord <- merge(shpNord, popd,
                 by.x ="insee", by.y="COM", all.x=T)

shpNordB <- cartogram_cont(shpNord,"pop" , itermax=4)

ggplot() + 
  geom_sf(data=shpNordB, aes(fill=P75ans), color ="transparent")+
  #scale_fill_gradient(low='black',high="white")
   viridis::scale_fill_viridis(option ="E") +
  coord_sf(crs = st_crs(shpNord), datum = NA)+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect("transparent"),
        title=element_text(size=16))+
  labs(col="", fill="%",title = "Part de population de 75 ans et plus dans les communes du Nord
Anamorphose selon la population totale",
       caption = "Source : Recensement de la population, Insee, 2016 | Fond : OSM")

# Export ----

ggsave("output/day23_population.png", width = 11, height = 9, dpi = 300)
