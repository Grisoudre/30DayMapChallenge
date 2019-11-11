# Day 10 - Black and white - Echecs et stratégie de Parcoursup 2018

# Packages ----

library(sf)
library(tidyverse)
library(questionr)
library(ggspatial)

# Imports ----

ps <- st_read("data/fr-esr-parcoursup.shp")
dep <- st_read("data/departements-20140306-50m.shp")

# Mise en forme ----

dep <- dep %>% filter(nchar(as.character(code_insee))==2)
ps$geometry <- NULL
ps <- ps %>% 
  filter (fili == "Licence" & (str_detect(form_lib_vo, "Sciences humaines et sociales")))
psdep <- ps %>% group_by(dep) %>% summarise(capa_fin=sum(capa_fin),
                                              voe_tot = sum(voe_tot),
                                              prop_tot = sum(prop_tot),
                                              acc_tot=sum(acc_tot),
                                            acc_neobac = sum(acc_neobac),
                                            acc_brs = sum(acc_brs))
psdep$prop_p <- psdep$prop_tot/psdep$voe_tot*100

dep <- merge(dep,psdep, by.x ="code_insee", by.y="dep", all.x=T)
dep$prop_p.cl <- cut(dep$prop_p, c(0,40,50,60,70,80,90,101), right=F)
dep$prop_p.cl <- fct_explicit_na(dep$prop_p.cl,
                                 "Pas de formation")
dep$prop_p.cl <- relevel(dep$prop_p.cl,
                                 "Pas de formation")

ggplot()+
  geom_sf(data = dep$geometry, aes(fill = dep$prop_p.cl), col = "black", size=.2)+
  scale_fill_manual(values=c("white","grey70","grey60",
                             "grey50","grey40","grey30",
                             "grey20","black"))+
labs(title = "Propositions d'admission dans les licences \nde sciences humaines et sociales en 2018", 
     caption="Source : Ministère de l'Enseignement supérieur, de la Recherche et de l'Innovation",
     fill = "Part des candidats ayant reçu \nune proposition d'admission (%)")+ 
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill="transparent"),
        title=element_text(size=16))+
  coord_sf( datum = NA)

# Export ----

ggsave("output/day10_BlackAndWhite.png", width = 13, height = 15, dpi = 300)