# 30DayMapChallenge | Dat 14 - Boundaries | Travailleurs transfrontaliers dans le Nord,
# les ardennes, le doubs, la haute savoie / 59, 08, 25, 74

# Données : Mobilités professionnelles en 2017, Insee

# Packages  ----

library(tidyverse)
library(sf)

# Imports ----
hsavoie <- st_read("data/communes-20150101-50m.shp") %>% 
  filter(substr(insee, 1, 2)=="74")
mob <- read.csv2("data/base-excel-flux-mobilite-domicile-lieu-travail-2015.csv", encoding = "WINDOWS-1252")
fl <- read.csv2("data/base-excel-flux-mobilite-domicile-lieu-travail-2015_flux.csv", encoding = "WINDOWS-1252")

# Mise en forme -----

# travaille en Suisse
fl74SU <- fl %>% filter(substr(CODGEO,1,2)=="74" & substr(DCLT,1,2)=="SU") %>% 
  group_by(CODGEO) %>% summarise(Su = sum(NBFLUX_C15_ACTOCC15P))

fl74SU <- merge(fl74SU, mob %>% select(CODGEO, C15_ACTOCC15P), by.x = "CODGEO",by.y = "CODGEO", all.x=T)
fl74SU$Su_perc <- round(fl74SU$Su/fl74SU$C15_ACTOCC15P*100,1)

hsavoie <- merge(hsavoie, fl74SU, by.x="insee",by.y="CODGEO", all.x=T)
hsavoie$Su_perc[is.na(hsavoie$Su_perc)]<-0

# Carte ----

ggplot(hsavoie) +
  geom_sf (aes(fill=Su_perc))+
  labs(title ="Travailleurs en Suisse vivant en Haute-savoie",
       fill="Part des 15 ans et plus en
emploi travaillant en Suisse (%)",
       caption = "Source : Mobilités professionnelles des individus, Insee, 2015")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill="transparent"),
        title=element_text(size=16))+
  coord_sf( datum = NA)

# Export ----

ggsave("output/day14_boundaries.png", width = 11, height = 9, dpi = 300)
