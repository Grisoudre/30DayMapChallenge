# Pays des villes jumelées avec les HdF

# Packages ------
library(tidyverse)
library(questionr)
library(sf)

# Imports ------
ju <- read.csv2("data/villes_jumelees.csv", stringsAsFactors = F, header=F)
iso <- read.csv2("data/iso3_wiki.csv", stringsAsFactors = F, header=T)
world <- st_read("data/ne_110m_admin_0_countries.shp")

# Mise en forme -------
ju$pays <- ifelse (str_detect(ju$V1, "\\d{4}"),
                   gsub("^.*\\(|\\).*$","", ju$V1), "")

ju$pays[ju$pays=="REPUBLIQUE TCHEQUE"] <- "TCHEQUIE"
ju$pays[ju$pays=="PAYS-BAS"] <- "PAYS BAS"
ju$pays[ju$pays=="PALESTINE"] <- "ETAT DE PALESTINE"
ju$pays[ju$pays=="ISRAËL"] <- "ISRAEL"
ju$pays[ju$pays=="RUSSIE"] <- "RUSSIE, FEDERATION DE"

iso$pays <- iconv(iso$nomb,to="ASCII//TRANSLIT")
ju <- merge(ju, iso %>% select(alpha.3, pays), by= "pays", all.x=T)
ju$date <- str_split_fixed(ju$V1, " - ",2)[,2]
ju$date <- gsub("NORD\\)","",ju$date)
ju$dec <- cut(as.integer(ju$date), seq(1920,2020,10), right = F)
ju <- ju %>% filter(pays!="") %>% group_by(alpha.3) %>% summarise(nbvilles = n())
world <- merge(world, ju, by.x= "ADM0_A3", by.y = "alpha.3", all.x=T)

# Carte--------

ggplot(world)+geom_sf(aes(fill=nbvilles), col="transparent")+
  scale_fill_continuous(na.value="snow3")+
  labs(fill = "Nombre de villes \njumelées",
       caption = 'Source : Association Française du Conseil des Communes et Régions d\'Europe
Fond : naturalearthdata.com',
       title="Pays des villes jumelées avec les communes des Hauts-de-France \ndepuis 1920")+
theme(title=element_text(size=16))+ theme_light()

# output --------

ggsave(  path ="output", "30daymapchallenge_day18.png",width=9, height=6, dpi=300)
