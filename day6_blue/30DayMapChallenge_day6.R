# 30DayMapChallenge / day 6 - Blue : Accidents constatés par la police impliquant
# des piétons ou cyclistes
# Source : BAAC 2009 - 2018

# Packages ----

library(tidyverse)
library(sf)
library(questionr)
library(magick)
library(gganimate)


# Données ----

d_18 <- read.csv2("data/caracteristiques-2018.csv", sep=",", stringsAsFactors = F)
v_18 <- read.csv2("data/vehicules-2018.csv", sep=",", stringsAsFactors = F)
u_18 <- read.csv2("data/usagers-2018.csv", sep=",", stringsAsFactors = F)

d_17 <- read.csv2("data/caracteristiques-2017.csv", sep=",", stringsAsFactors = F)
v_17 <- read.csv2("data/vehicules-2017.csv", sep=",", stringsAsFactors = F)
u_17 <- read.csv2("data/usagers-2017.csv", sep=",", stringsAsFactors = F)

d_16 <- read.csv2("data/caracteristiques_2016.csv", sep=",", stringsAsFactors = F)
v_16 <- read.csv2("data/vehicules_2016.csv", sep=",", stringsAsFactors = F)
u_16 <- read.csv2("data/usagers_2016.csv", sep=",", stringsAsFactors = F)

d_15 <- read.csv2("data/caracteristiques_2015.csv", sep=",", stringsAsFactors = F)
v_15 <- read.csv2("data/vehicules_2015.csv", sep=",", stringsAsFactors = F)
u_15 <- read.csv2("data/usagers_2015.csv", sep=",", stringsAsFactors = F)

d_14 <- read.csv2("data/caracteristiques_2014.csv", sep=",", stringsAsFactors = F)
v_14 <- read.csv2("data/vehicules_2014.csv", sep=",", stringsAsFactors = F)
u_14 <- read.csv2("data/usagers_2014.csv", sep=",", stringsAsFactors = F)

d_13 <- read.csv2("data/caracteristiques_2013.csv", sep=",", stringsAsFactors = F)
v_13 <- read.csv2("data/vehicules_2013.csv", sep=",", stringsAsFactors = F)
u_13 <- read.csv2("data/usagers_2013.csv", sep=",", stringsAsFactors = F)

d_12 <- read.csv2("data/caracteristiques_2012.csv", sep=",", stringsAsFactors = F)
v_12 <- read.csv2("data/vehicules_2012.csv", sep=",", stringsAsFactors = F)
u_12 <- read.csv2("data/usagers_2012.csv", sep=",", stringsAsFactors = F)

d_11 <- read.csv2("data/caracteristiques_2011.csv", sep=",", stringsAsFactors = F)
v_11 <- read.csv2("data/vehicules_2011.csv", sep=",", stringsAsFactors = F)
u_11 <- read.csv2("data/usagers_2011.csv", sep=",", stringsAsFactors = F)

d_10 <- read.csv2("data/caracteristiques_2010.csv", sep=",", stringsAsFactors = F)
v_10 <- read.csv2("data/vehicules_2010.csv", sep=",", stringsAsFactors = F)
u_10 <- read.csv2("data/usagers_2010.csv", sep=",", stringsAsFactors = F)

d_09 <- read.csv2("data/caracteristiques_2009.csv", sep="\t", stringsAsFactors = F)
v_09 <- read.csv2("data/vehicules_2009.csv", sep=",", stringsAsFactors = F)
u_09 <- read.csv2("data/usagers_2009.csv", sep=",", stringsAsFactors = F)

d <- rbind(d_18,d_17,d_16,d_15,d_14,d_13,d_12,d_11,d_10,d_09)
rm(d_18,d_17,d_16,d_15,d_14,d_13,d_12,d_11,d_10,d_09)
u <- rbind(u_18,u_17,u_16,u_15,u_14,u_13,u_12,u_11,u_10,u_09)
rm(u_18,u_17,u_16,u_15,u_14,u_13,u_12,u_11,u_10,u_09)
v <- rbind(v_18,v_17,v_16,v_15,v_14,v_13,v_12,v_11,v_10,v_09)
rm(v_18,v_17,v_16,v_15,v_14,v_13,v_12,v_11,v_10,v_09)

nord <- st_read("data/Nord_COM.shp")
mel <- st_read("data/MEL_IRIS.shp")

# Mise en forme --------
# BAAC : hospitalisation et ou mortel :
mortel <- u %>% 
  filter(grav=="2" | grav=="3") %>% 
  select(Num_Acc, num_veh) %>% 
  mutate(mortel ="oui")

# Piétons
# 18 : catu  = 3, avant catu = 3 ou 4
u$an <- substr(u$Num_Acc, 3,4)
u <- u %>% 
  filter((catu=="3" & an == "18") |
           ((catu=="3" | catu =="4") & an != "18")) %>%
  select(Num_Acc, num_veh) %>%
  unique() %>% 
  mutate(pieddr="oui")

# Vélo et "autres" en 18, juste vélo avant
v$an <- substr(v$Num_Acc, 3,4)
v <- v %>%
  filter(((catv=="1" |catv=="99") & an =="18")|
  (catv=="1" & an != "18")) %>%
  select(Num_Acc, num_veh) %>% 
  unique() %>% 
  mutate(pieddr ="oui")

v <- merge(v, mortel, by= c("Num_Acc","num_veh"), all.x = T)
u <- merge(u, mortel, by= c("Num_Acc","num_veh"), all.x = T)
pietons <- rbind(v, u)
blesse <- pietons %>% filter(mortel=="oui") %>% select(Num_Acc, mortel) %>% unique()
pieddr <- pietons %>% filter(pieddr=="oui") %>% select(Num_Acc, pieddr) %>% unique()
pietons <- merge(blesse, pieddr, by="Num_Acc", all=T)
pietons$mortel[is.na(pietons$mortel)]<-"non"
d <- merge(d,pietons, by="Num_Acc", all.x=T)
d$pieddr[is.na(d$pieddr)]<-"non"
d$mortel[is.na(d$mortel)]<-"non"
d$nomoto <- ifelse (d$mortel=="oui","Non-motorisé·e hospitalisé·e ou tué·e",
                    ifelse(d$pieddr=="oui",
                           "Non-motorisé·e impliqué·e", "Pas de non-motorisé·e impliqué·e"))
# Département 59
d <- d %>% filter(dep=="590" & is.na(lat)==F & is.na(long)==F & long<423636 & long>0 & lat>5000000)

# géo :
mel$EPCI <- as.integer(mel$EPCI)
mel <- aggregate(mel[,"EPCI"], by=list(mel$INSEE_COM), FUN="sum") %>% 
  rename("INSEE_COM"=Group.1) %>% select(INSEE_COM, geometry)

d$lat <- paste(substr(d$lat,1,2), substr(d$lat,3,nchar(d$lat)), sep=".")
d$long <- paste(substr(d$long,1,1), substr(d$long,2,nchar(d$long)), sep=".")
d.sf <-  st_as_sf(d, coords = c("long", "lat"), crs=st_crs(nord))
mel <- st_transform(mel,st_crs(nord))
data <- st_join( d.sf, mel) %>%
  filter(!is.na(INSEE_COM))

data$nomoto <- factor(data$nomoto, levels =c(
  "Pas de non-motorisé·e impliqué·e", "Non-motorisé·e impliqué·e",
  "Non-motorisé·e hospitalisé·e ou tué·e"
))

# heure de la journée
data$hrmn <- ifelse (nchar(data$hrmn)==2,paste0("00",data$hrmn),
                     ifelse (nchar(data$hrmn)==3, paste0("0",data$hrmn),
                             ifelse(nchar(data$hrmn)==1,
                                    paste0("000",data$hrmn),data$hrmn)))

data$hr <- factor(paste0(substr(data$hrmn,1,2),":00-",
                  as.integer(substr(data$hrmn,1,2))+1,":00"))
data$hr2 <- substr(data$hrmn,1,2)
data$hr2 <- ifelse(as.integer(data$hr2)%%2 ==1,
                   as.integer(data$hr2)-1,as.integer(data$hr2))
data$hr2 <- paste0(data$hr2,":00-",as.integer(data$hr2)+2,":00")
data <- arrange(data, hr)
data$hr2 <- factor(data$hr2, levels =unique(data$hr2))

# Carte------
a<- ggplot() +
geom_sf(data=mel$geometry, color="snow3", fill="transparent")+
 geom_sf(data=data$geometry, aes(col = data$nomoto), size=2, show.legend = "point")+
  viridis::scale_color_viridis(discrete=T, end=0.8, option="B")+
  transition_states(data$hr2,
                    wrap = F,
                    state_length = 5,
                    transition_length = 0)+
  labs(title ="Accidents dans la MEL constatés par la police impliquant au moins un·e
piéton·ne (yc en roller ou trottinette) ou un·e cycliste par heure
{closest_state}h",
       col = "", caption = "Source : BAAC 2009 à 2018")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill= "white"),title=element_text(size=16),
        legend.text=element_text(size=14))+
  coord_sf(crs = st_crs(mel), datum = NA)

# Output-------

a_gif<-animate(a, width = 800, height = 500)

image_write(image_read(a_gif), "output/BAAC_MEL.gif")
