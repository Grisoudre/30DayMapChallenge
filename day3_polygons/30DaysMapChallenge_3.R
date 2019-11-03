# 30 day map challenge - jour 3 - polygones / Nb de ventes immo / CA et CU du Nord -----------
# sources : DVF 

# Packages ----
library(tidyverse)
library(sf)
library(geojsonio)
library(viridis)
library(magick)
library(gganimate)

# imports ----
dvf14 <- read.csv("data/dvf_2014.csv", stringsAsFactors = F, sep=",")
dvf15 <- read.csv("data/dvf_2015.csv", stringsAsFactors = F, sep=",")
dvf16 <- read.csv("data/dvf_2016.csv", stringsAsFactors = F, sep=",")
dvf17 <- read.csv("data/dvf_2017.csv", stringsAsFactors = F, sep=",")
dvf18 <- read.csv("data/dvf_2018.csv", stringsAsFactors = F, sep=",")
mel <- st_read("data/MEL_IRIS.shp")
dk <- st_read("data/dk_IRIS.shp")
val <- st_read("data/val_IRIS.shp")
hai <- st_read("data/hai_IRIS.shp")
mau <- st_read("data/mau_IRIS.shp")
dou <- st_read("data/dou_IRIS.shp")
cam <- st_read("data/cam_IRIS.shp")
cau <- st_read("data/cau_IRIS.shp")

# Mise en forme -------
dvf14 <- dvf14 %>% filter(substr(code_commune,1,2)=="59"
                          & (type_local=="Maison"|
                               type_local=="Appartement")&
                            is.na(valeur_fonciere)==F &
                            is.na(surface_reelle_bati)==F&
                            is.na(longitude)==F & is.na(latitude)==F &
                            surface_reelle_bati > 1)
dvf14_sf = st_as_sf(dvf14, coords = c("longitude", "latitude"), crs=4326)
dvf14_sf <- st_transform(dvf14_sf,st_crs(mel))

dvf15 <- dvf15 %>% filter(substr(code_commune,1,2)=="59"
                          & (type_local=="Maison"|
                               type_local=="Appartement")&
                            is.na(valeur_fonciere)==F &
                            is.na(surface_reelle_bati)==F&
                            is.na(longitude)==F & is.na(latitude)==F &
                            surface_reelle_bati > 1)
dvf15_sf = st_as_sf(dvf15, coords = c("longitude", "latitude"), crs=4326)
dvf15_sf <- st_transform(dvf15_sf,st_crs(mel))

dvf16 <- dvf16 %>% filter(substr(code_commune,1,2)=="59"
                          & (type_local=="Maison"|
                               type_local=="Appartement")&
                            is.na(valeur_fonciere)==F &
                            is.na(surface_reelle_bati)==F&
                            is.na(longitude)==F & is.na(latitude)==F &
                            surface_reelle_bati > 1)
dvf16_sf = st_as_sf(dvf16, coords = c("longitude", "latitude"), crs=4326)
dvf16_sf <- st_transform(dvf16_sf,st_crs(mel))

dvf17 <- dvf17 %>% filter(substr(code_commune,1,2)=="59"
                          & (type_local=="Maison"|
                               type_local=="Appartement")&
                            is.na(valeur_fonciere)==F &
                            is.na(surface_reelle_bati)==F&
                            is.na(longitude)==F & is.na(latitude)==F &
                            surface_reelle_bati > 1)
dvf17_sf = st_as_sf(dvf17, coords = c("longitude", "latitude"), crs=4326)
dvf17_sf <- st_transform(dvf17_sf,st_crs(mel))

dvf18 <- dvf18 %>% filter(substr(code_commune,1,2)=="59"
                          & (type_local=="Maison"|
                               type_local=="Appartement")&
                            is.na(valeur_fonciere)==F &
                            is.na(surface_reelle_bati)==F&
                            is.na(longitude)==F & is.na(latitude)==F &
                            surface_reelle_bati > 1)
dvf18_sf = st_as_sf(dvf18, coords = c("longitude", "latitude"), crs=4326)
dvf18_sf <- st_transform(dvf18_sf,st_crs(mel))

# Nb de ventes :
mel$nb_dvf14 <-  lengths(st_intersects(mel, dvf14_sf))
mel$nb_dvf15 <-  lengths(st_intersects(mel, dvf15_sf))
mel$nb_dvf16 <-  lengths(st_intersects(mel, dvf16_sf))
mel$nb_dvf17 <-  lengths(st_intersects(mel, dvf17_sf))
mel$nb_dvf18 <-  lengths(st_intersects(mel, dvf18_sf))

dk$nb_dvf14 <-  lengths(st_intersects(dk, dvf14_sf))
dk$nb_dvf15 <-  lengths(st_intersects(dk, dvf15_sf))
dk$nb_dvf16 <-  lengths(st_intersects(dk, dvf16_sf))
dk$nb_dvf17 <-  lengths(st_intersects(dk, dvf17_sf))
dk$nb_dvf18 <-  lengths(st_intersects(dk, dvf18_sf))

val$nb_dvf14 <-  lengths(st_intersects(val, dvf14_sf))
val$nb_dvf15 <-  lengths(st_intersects(val, dvf15_sf))
val$nb_dvf16 <-  lengths(st_intersects(val, dvf16_sf))
val$nb_dvf17 <-  lengths(st_intersects(val, dvf17_sf))
val$nb_dvf18 <-  lengths(st_intersects(val, dvf18_sf))

mau$nb_dvf14 <-  lengths(st_intersects(mau, dvf14_sf))
mau$nb_dvf15 <-  lengths(st_intersects(mau, dvf15_sf))
mau$nb_dvf16 <-  lengths(st_intersects(mau, dvf16_sf))
mau$nb_dvf17 <-  lengths(st_intersects(mau, dvf17_sf))
mau$nb_dvf18 <-  lengths(st_intersects(mau, dvf18_sf))

cam$nb_dvf14 <-  lengths(st_intersects(cam, dvf14_sf))
cam$nb_dvf15 <-  lengths(st_intersects(cam, dvf15_sf))
cam$nb_dvf16 <-  lengths(st_intersects(cam, dvf16_sf))
cam$nb_dvf17 <-  lengths(st_intersects(cam, dvf17_sf))
cam$nb_dvf18 <-  lengths(st_intersects(cam, dvf18_sf))

cau$nb_dvf14 <-  lengths(st_intersects(cau, dvf14_sf))
cau$nb_dvf15 <-  lengths(st_intersects(cau, dvf15_sf))
cau$nb_dvf16 <-  lengths(st_intersects(cau, dvf16_sf))
cau$nb_dvf17 <-  lengths(st_intersects(cau, dvf17_sf))
cau$nb_dvf18 <-  lengths(st_intersects(cau, dvf18_sf))


hai$nb_dvf14 <-  lengths(st_intersects(hai, dvf14_sf))
hai$nb_dvf15 <-  lengths(st_intersects(hai, dvf15_sf))
hai$nb_dvf16 <-  lengths(st_intersects(hai, dvf16_sf))
hai$nb_dvf17 <-  lengths(st_intersects(hai, dvf17_sf))
hai$nb_dvf18 <-  lengths(st_intersects(hai, dvf18_sf))


dou$nb_dvf14 <-  lengths(st_intersects(dou, dvf14_sf))
dou$nb_dvf15 <-  lengths(st_intersects(dou, dvf15_sf))
dou$nb_dvf16 <-  lengths(st_intersects(dou, dvf16_sf))
dou$nb_dvf17 <-  lengths(st_intersects(dou, dvf17_sf))
dou$nb_dvf18 <-  lengths(st_intersects(dou, dvf18_sf))

melt <- rbind(mel %>% select(CODE_IRIS, nb_dvf14, geometry) %>% rename(nb_dvf=nb_dvf14) %>% mutate(annee="2014"),
              mel %>% select(CODE_IRIS, nb_dvf15, geometry) %>% rename(nb_dvf=nb_dvf15) %>% mutate(annee="2015"),
              mel %>% select(CODE_IRIS, nb_dvf16, geometry) %>% rename(nb_dvf=nb_dvf16) %>% mutate(annee="2016"),
              mel %>% select(CODE_IRIS, nb_dvf17, geometry) %>% rename(nb_dvf=nb_dvf17) %>% mutate(annee="2017"),
              mel %>% select(CODE_IRIS, nb_dvf18, geometry) %>% rename(nb_dvf=nb_dvf18) %>% mutate(annee="2018"))

dkt <- rbind(dk %>% select(CODE_IRIS, nb_dvf14, geometry) %>% rename(nb_dvf=nb_dvf14) %>% mutate(annee="2014"),
             dk %>% select(CODE_IRIS, nb_dvf15, geometry) %>% rename(nb_dvf=nb_dvf15) %>% mutate(annee="2015"),
             dk %>% select(CODE_IRIS, nb_dvf16, geometry) %>% rename(nb_dvf=nb_dvf16) %>% mutate(annee="2016"),
             dk %>% select(CODE_IRIS, nb_dvf17, geometry) %>% rename(nb_dvf=nb_dvf17) %>% mutate(annee="2017"),
             dk %>% select(CODE_IRIS, nb_dvf18, geometry) %>% rename(nb_dvf=nb_dvf18) %>% mutate(annee="2018"))


valt <- rbind(val %>% select(CODE_IRIS, nb_dvf14, geometry) %>% rename(nb_dvf=nb_dvf14) %>% mutate(annee="2014"),
              val %>% select(CODE_IRIS, nb_dvf15, geometry) %>% rename(nb_dvf=nb_dvf15) %>% mutate(annee="2015"),
              val %>% select(CODE_IRIS, nb_dvf16, geometry) %>% rename(nb_dvf=nb_dvf16) %>% mutate(annee="2016"),
              val %>% select(CODE_IRIS, nb_dvf17, geometry) %>% rename(nb_dvf=nb_dvf17) %>% mutate(annee="2017"),
              val %>% select(CODE_IRIS, nb_dvf18, geometry) %>% rename(nb_dvf=nb_dvf18) %>% mutate(annee="2018"))


hait <- rbind(hai %>% select(CODE_IRIS, nb_dvf14, geometry) %>% rename(nb_dvf=nb_dvf14) %>% mutate(annee="2014"),
              hai %>% select(CODE_IRIS, nb_dvf15, geometry) %>% rename(nb_dvf=nb_dvf15) %>% mutate(annee="2015"),
              hai %>% select(CODE_IRIS, nb_dvf16, geometry) %>% rename(nb_dvf=nb_dvf16) %>% mutate(annee="2016"),
              hai %>% select(CODE_IRIS, nb_dvf17, geometry) %>% rename(nb_dvf=nb_dvf17) %>% mutate(annee="2017"),
              hai %>% select(CODE_IRIS, nb_dvf18, geometry) %>% rename(nb_dvf=nb_dvf18) %>% mutate(annee="2018"))


camt <- rbind(cam %>% select(CODE_IRIS, nb_dvf14, geometry) %>% rename(nb_dvf=nb_dvf14) %>% mutate(annee="2014"),
              cam %>% select(CODE_IRIS, nb_dvf15, geometry) %>% rename(nb_dvf=nb_dvf15) %>% mutate(annee="2015"),
              cam %>% select(CODE_IRIS, nb_dvf16, geometry) %>% rename(nb_dvf=nb_dvf16) %>% mutate(annee="2016"),
              cam %>% select(CODE_IRIS, nb_dvf17, geometry) %>% rename(nb_dvf=nb_dvf17) %>% mutate(annee="2017"),
              cam %>% select(CODE_IRIS, nb_dvf18, geometry) %>% rename(nb_dvf=nb_dvf18) %>% mutate(annee="2018"))


caut <- rbind(cau %>% select(CODE_IRIS, nb_dvf14, geometry) %>% rename(nb_dvf=nb_dvf14) %>% mutate(annee="2014"),
              cau %>% select(CODE_IRIS, nb_dvf15, geometry) %>% rename(nb_dvf=nb_dvf15) %>% mutate(annee="2015"),
              cau %>% select(CODE_IRIS, nb_dvf16, geometry) %>% rename(nb_dvf=nb_dvf16) %>% mutate(annee="2016"),
              cau %>% select(CODE_IRIS, nb_dvf17, geometry) %>% rename(nb_dvf=nb_dvf17) %>% mutate(annee="2017"),
              cau %>% select(CODE_IRIS, nb_dvf18, geometry) %>% rename(nb_dvf=nb_dvf18) %>% mutate(annee="2018"))

maut <- rbind(mau %>% select(CODE_IRIS, nb_dvf14, geometry) %>% rename(nb_dvf=nb_dvf14) %>% mutate(annee="2014"),
              mau %>% select(CODE_IRIS, nb_dvf15, geometry) %>% rename(nb_dvf=nb_dvf15) %>% mutate(annee="2015"),
              mau %>% select(CODE_IRIS, nb_dvf16, geometry) %>% rename(nb_dvf=nb_dvf16) %>% mutate(annee="2016"),
              mau %>% select(CODE_IRIS, nb_dvf17, geometry) %>% rename(nb_dvf=nb_dvf17) %>% mutate(annee="2017"),
              mau %>% select(CODE_IRIS, nb_dvf18, geometry) %>% rename(nb_dvf=nb_dvf18) %>% mutate(annee="2018"))

dout <- rbind(dou %>% select(CODE_IRIS, nb_dvf14, geometry) %>% rename(nb_dvf=nb_dvf14) %>% mutate(annee="2014"),
              dou %>% select(CODE_IRIS, nb_dvf15, geometry) %>% rename(nb_dvf=nb_dvf15) %>% mutate(annee="2015"),
              dou %>% select(CODE_IRIS, nb_dvf16, geometry) %>% rename(nb_dvf=nb_dvf16) %>% mutate(annee="2016"),
              dou %>% select(CODE_IRIS, nb_dvf17, geometry) %>% rename(nb_dvf=nb_dvf17) %>% mutate(annee="2017"),
              dou %>% select(CODE_IRIS, nb_dvf18, geometry) %>% rename(nb_dvf=nb_dvf18) %>% mutate(annee="2018"))

# Cartes ----------

a <- 
  ggplot()+
  geom_sf(data = st_geometry(melt),  aes(fill=melt$nb_dvf), col="transparent")+
  viridis::scale_fill_viridis(option="E") + 
  transition_states(melt$annee,
                    wrap = T,
                    state_length = .01,
                    transition_length = .005)+
  labs(title ="Ventes de maisons et appartements\n\nMétropole Européenne de Lille : {closest_state}", fill="Nb de ventes")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill= "white"),title=element_text(size=16))+
  coord_sf(crs = st_crs(melt), datum = NA)

b <- ggplot()+
  geom_sf(data = st_geometry(obj = dkt),  aes(fill=dkt$nb_dvf), col="transparent")+
  viridis::scale_fill_viridis(option="E") + 
  transition_states(dkt$annee,
                    wrap = T,
                    state_length = .05,
                    transition_length = .01)+
  labs(title ="CU de Dunkerque : {closest_state}", fill="Nb de ventes")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill= "white"),title=element_text(size=16))+
  coord_sf(crs = st_crs(melt), datum = NA)

c <- ggplot()+
  geom_sf(data = st_geometry(obj = maut),  aes(fill=maut$nb_dvf), col="transparent")+
  viridis::scale_fill_viridis(option="E") + 
  transition_states(maut$annee,
                    wrap = T,
                    state_length = .05,
                    transition_length = .01)+
  labs(title ="CA Maubeuge Val de Sambre : {closest_state}", fill="Nb de ventes")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill= "white"),title=element_text(size=16))+
  coord_sf(crs = st_crs(melt), datum = NA)

d <- ggplot()+
  geom_sf(data = st_geometry(obj = camt),  aes(fill=camt$nb_dvf), col="transparent")+
  viridis::scale_fill_viridis(option="E") + 
  transition_states(camt$annee,
                    wrap = T,
                    state_length = .05,
                    transition_length = .01)+
  labs(title ="CA de Cambrai : {closest_state}", fill="Nb de ventes")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill= "white"),title=element_text(size=16))+
  coord_sf(crs = st_crs(melt), datum = NA)


e <- ggplot()+
  geom_sf(data = st_geometry(obj = hait),  aes(fill=hait$nb_dvf), col="transparent")+
  viridis::scale_fill_viridis(option="E") + 
  transition_states(hait$annee,
                    wrap = T,
                    state_length = .05,
                    transition_length = .01)+
  labs(title ="CA de la Porte du Hainaut : {closest_state}", fill="Nb de ventes")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill= "white"),title=element_text(size=16))+
  coord_sf(crs = st_crs(melt), datum = NA)


f <- ggplot()+
  geom_sf(data = st_geometry(obj = dout),  aes(fill=dout$nb_dvf), col="transparent")+
  viridis::scale_fill_viridis(option="E") + 
  transition_states(dout$annee,
                    wrap = T,
                    state_length = .05,
                    transition_length = .01)+
  labs(title ="CA du Douaisis : {closest_state}", fill="Nb de ventes")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill= "white"),title=element_text(size=16))+
  coord_sf(crs = st_crs(melt), datum = NA)


g <- ggplot()+
  geom_sf(data = st_geometry(obj = valt),  aes(fill=valt$nb_dvf), col="transparent")+
  viridis::scale_fill_viridis(option="E") + 
  transition_states(valt$annee,
                    wrap = T,
                    state_length = .05,
                    transition_length = .01)+
  labs(title ="CA Valenciennes Métropole : {closest_state}", fill="Nb de ventes")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill= "white"),title=element_text(size=16))+
  coord_sf(crs = st_crs(melt), datum = NA)


h  <- ggplot()+
  geom_sf(data = st_geometry(obj = caut),  aes(fill=caut$nb_dvf), col="transparent")+
  viridis::scale_fill_viridis(option="E") + 
  transition_states(caut$annee,
                    wrap = T,
                    state_length = .05,
                    transition_length = .01)+
  labs(title ="CA du Caudrésis et du Catésis : {closest_state}", fill="Nb de ventes",
       caption = "Source : Données de valeur foncière, DGFiP")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill= "white"),title=element_text(size=16))+
  coord_sf(crs = st_crs(melt), datum = NA)

# gif ----------
# https://github.com/thomasp85/gganimate/wiki/Animation-Composition

a_gif<-animate(a, width = 400, height = 300)
b_gif<-animate(b, width =400, height = 300)
c_gif<-animate(c, width = 400, height = 300)
d_gif<-animate(d, width =400, height = 300)
e_gif<-animate(e, width = 400, height = 300)
f_gif<-animate(f, width =400, height = 300)
g_gif<-animate(g, width = 400, height = 300)
h_gif<-animate(h, width =400, height = 300)

a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)
c_mgif <- image_read(c_gif)
d_mgif <- image_read(d_gif)
e_mgif <- image_read(e_gif)
f_mgif <- image_read(f_gif)
g_mgif <- image_read(g_gif)
h_mgif <- image_read(h_gif)

new_gif <- image_append(c(
  image_append(c(a_mgif[1], b_mgif[1], c_mgif[1])),
  image_append(c(d_mgif[1], e_mgif[1], f_mgif[1])),
  image_append(c(g_mgif[1], h_mgif[1]))), stack=T)


for(i in 2:100){
  combined <- image_append(c(
    image_append(c(a_mgif[i], b_mgif[i], c_mgif[i])),
    image_append(c(d_mgif[i], e_mgif[i], f_mgif[i])),
    image_append(c(g_mgif[i], h_mgif[i]))), stack=T)
  new_gif <- c(new_gif, combined)
}

# Output----
image_write(new_gif, "output/dvf_agglo_nord.gif")
