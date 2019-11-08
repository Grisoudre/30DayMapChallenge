# 30DayMapChallenge - day 7 - Red - Vote communiste dans le Nord aux municipales----

# https://www.data.gouv.fr/fr/posts/les-donnees-des-elections/

# Packages ----
library(tidyverse)
library(sf)
library(questionr)

# Imports ----

leg17 <- read.csv2("data/Leg_2017_Resultats_communes_T1_c (1).csv",
                   stringsAsFactors = F)
leg12 <- read.csv2("data/legislatives_12_Com.csv",
                   stringsAsFactors = F)
pre17 <- read.csv2("data/Presidentielle_2017_Resultats_Communes_Tour_1_c.csv",
                   stringsAsFactors = F)
pre12 <- read.csv2("data/Pres_12_Com.csv",
                   stringsAsFactors = F)
reg15 <-  read.csv2("data/Reg_15_Resultats_Communes_T1_c.csv",
                    stringsAsFactors = F)
eur19 <-  read.csv2("data/Eur_19_Com.csv",fileEncoding = "WINDOWS-1252",
                    stringsAsFactors = F)
eur14 <-  read.csv2("data/euro-2014-resultats-communes-c (1).csv",
                    stringsAsFactors = F)
nord_com <- st_read("data/Nord_COM.shp")

# Mise en forme --------

COG_nord <- eur14 %>% filter(Code.du.département =="59") %>% 
  select(Code.du.département,Code.de.la.commune)

# Eur14 :
eur14 <- eur14 %>% filter(Code.du.département =="59")
eur14_com <- eur14[,c(1:20,21:27)]
for (i in 1:30){
  
  bind <- eur14[,c(1:20,((20+(i*7+1)):(27+(i*7))))]
  names(bind) <- names(eur14_com[,c(1:20,21:27)])
  eur14_com <- rbind(eur14_com, bind)
  
}
eur14_com <-eur14_com %>% 
  filter(Nom.Tête.de.Liste == "GOURMELEN Louis-Daniel") 

# Eur19 :
eur19 <- eur19 %>% filter(Code.du.département=="59")
eur19_com <- eur19[,c(1:18,146:152)]
names(eur19_com) <- names(eur19[,c(1:18,20:26)])

# Leg17 :
leg17 <- leg17 %>% filter(Code.du.département=="59")
leg17_com <- leg17[,c(1:20,21:28)]
for (i in 1:25){
  bind <- leg17[,c(1:20,((20+(i*8+1)):(28+(i*8))))]
  names(bind) <- names(leg17_com[,c(1:20,21:28)])
  leg17_com <- rbind(leg17_com, bind)
  
}

# Leg12 :
leg12 <- leg12 %>% filter(Code.du.département=="59")
leg12_com <- leg12[,c(1:17,18:24)]
for (i in 1:22){
  bind <- leg12[,c(1:17,((17+(i*7+1)):(24+(i*7))))]
  
  names(bind)
  names(bind) <- names(leg12_com[,c(1:17,18:24)])
  leg12_com <- rbind(leg12_com, bind)
}

# pre17 :
pre17 <- pre17 %>% filter(Code.du.département=="59")
pre17_com <- pre17[,c(1:18,19:25)]

for (i in 1:10){
  bind <- pre17[,c(1:18,((18+(i*7+1)):(25+(i*7))))]
  
  names(bind)
  names(bind) <- names(pre17_com[,c(1:18,19:25)])
  pre17_com <- rbind(pre17_com, bind)
}

# pre12 :
pre12 <- pre12 %>% filter(Code.du.département=="59")
names(pre12)
freq(pre12$Nom.3)

pre12_com <- pre12[,c(1:15,34:39)]

# Reg15 :
reg15 <- reg15 %>% filter(Code.du.département=="59")
reg15_com <- reg15[,c(1:18,19:26)]

for (i in 1:12){
  bind <- reg15[,c(1:18,((18+(i*8+1)):(26+(i*8))))]
  
  names(bind)
  names(bind) <- names(reg15_com[,c(1:18,19:26)])
  reg15_com <- rbind(reg15_com, bind)
  
}


rm(pre12, pre17, leg12, leg17, eur14, eur19, reg15)

# Ensemble : E19, L17, P17, R15

data1 <- rbind(eur19_com %>% 
                select(Code.du.département,
                       Code.de.la.commune,
                       X..Voix.Exp,
                       Libellé.Abrégé.Liste) %>% 
                rename (CodeDep = Code.du.département,
                        CodeCom = Code.de.la.commune,
                        Voix = X..Voix.Exp,
                        Coul = Libellé.Abrégé.Liste) %>% 
                 merge(COG, by.x = c("CodeDep", "CodeCom"),
                       by.y = c("Code.du.département","Code.de.la.commune"), all.y=T)%>% 
                 mutate(election = "E19"),
              reg15_com %>% 
                filter(Nuance.Liste == "LCOM") %>% 
                select(Code.du.département,
                       Code.de.la.commune,
                       X..Voix.Exp,
                       Nom.Tête.de.Liste) %>% 
                rename (CodeDep = Code.du.département,
                        CodeCom = Code.de.la.commune,
                        Voix = X..Voix.Exp,
                        Coul = Nom.Tête.de.Liste) %>% 
                merge(COG, by.x = c("CodeDep", "CodeCom"),
                      by.y = c("Code.du.département","Code.de.la.commune"), all.y=T)%>% 
                mutate(election = "R15"),
              pre17_com %>% 
                filter(Nom == "MÉLENCHON") %>% 
                select(Code.du.département,
                       Code.de.la.commune,
                       X..Voix.Exp,
                       Nom) %>% 
                rename (CodeDep = Code.du.département,
                        CodeCom = Code.de.la.commune,
                        Voix = X..Voix.Exp,
                        Coul = Nom) %>% 
                merge(COG, by.x = c("CodeDep", "CodeCom"),
                      by.y = c("Code.du.département","Code.de.la.commune"), all.y=T)%>% 
                mutate(election = "P17"),
              leg17_com %>% 
                filter(Nuance == "COM" ) %>% 
                select(Code.du.département,
                       Code.de.la.commune,
                       X..Voix.Exp,
                       Nuance) %>% 
                rename (CodeDep = Code.du.département,
                        CodeCom = Code.de.la.commune,
                        Voix = X..Voix.Exp,
                        Coul = Nuance) %>% 
                merge(COG, by.x = c("CodeDep", "CodeCom"),
                      by.y = c("Code.du.département","Code.de.la.commune"), all.y=T) %>% 
                mutate(election = "L17"))
data1$Voix[is.na(data1$Voix)] <- 0

data1$CodeCom <- ifelse (nchar(data1$CodeCom)=="2",
                         paste0("0",data1$CodeCom),
                         ifelse (nchar(data1$CodeCom)=="1",
                                 paste0("00",data1$CodeCom),
                                 data1$CodeCom))
data1$COG <- paste0(data1$CodeDep,data1$CodeCom)


data1 <- data1 %>%
  select (COG, Voix, election) %>% 
  mutate(Voix = as.numeric(Voix)) %>% 
  group_by(COG, election) %>% summarise(Voix = sum(Voix))

# Ensemble : E14, L12, P12, 

data2 <- rbind(eur14_com %>% 
                 select(Code.du.département,
                        Code.de.la.commune,
                        X..Voix.Exp,
                        Libellé.Abrégé.Liste) %>% 
                 rename (CodeDep = Code.du.département,
                         CodeCom = Code.de.la.commune,
                         Voix = X..Voix.Exp,
                         Coul = Libellé.Abrégé.Liste)%>% 
                 merge(COG, by.x = c("CodeDep", "CodeCom"),
                       by.y = c("Code.du.département","Code.de.la.commune"), all.y=T) %>% 
                 mutate(election = "E14"),
               pre12_com %>% 
                 select(Code.du.département,
                        Code.de.la.commune,
                        X..Voix.Exp.3,
                        Nom.3) %>% 
                 rename (CodeDep = Code.du.département,
                         CodeCom = Code.de.la.commune,
                         Voix = X..Voix.Exp.3,
                         Coul = Nom.3)%>% 
                 merge(COG, by.x = c("CodeDep", "CodeCom"),
                       by.y = c("Code.du.département","Code.de.la.commune"), all.y=T) %>% 
                 mutate(election = "P12"),
               leg12_com %>% 
                 filter(Nuance == "FG") %>% 
                 select(Code.du.département,
                        Code.de.la.commune,
                        X..Voix.Exp,
                        Nuance) %>% 
                 rename (CodeDep = Code.du.département,
                         CodeCom = Code.de.la.commune,
                         Voix = X..Voix.Exp,
                         Coul = Nuance)%>% 
                 merge(COG, by.x = c("CodeDep", "CodeCom"),
                       by.y = c("Code.du.département","Code.de.la.commune"), all.y=T) %>% 
                 mutate(election = "L12"))

data2$Voix[is.na(data2$Voix)] <- 0

data2$CodeCom <- ifelse (nchar(data2$CodeCom)=="2",
                         paste0("0",data2$CodeCom),
                         ifelse (nchar(data2$CodeCom)=="1",
                                 paste0("00",data2$CodeCom),
                                 data2$CodeCom))
data2$COG <- paste0(data2$CodeDep,data2$CodeCom)


data1 <- data1 %>%
  select (COG, Voix, election) %>% 
  mutate(Voix = as.numeric(Voix)) %>% 
  group_by(COG, election) %>% summarise(Voix = sum(Voix))

data2 <- data2 %>%
  select (COG, Voix, election) %>% 
  mutate(Voix = as.numeric(Voix)) %>% 
  group_by(COG, election) %>% summarise(Voix = sum(Voix))

# Cartes :

nord_com1 <- merge(nord_com, data1 %>%
                     select(COG, Voix, election), by.x ="insee", by.y = "COG", 
                   all.x=T )

nord_com1$Voix[is.na(nord_com1$Voix)]<-0


nord_com1$election <- factor(nord_com1$election,
                             levels = c("E19","L17","P17", "R15"),
                             labels = c('Européennes 2019 : "Pour l\'Europe des gens"',
                                        "Législatives 2017 : PCF",
                                        "Présidentielles 2017 : J.-L. Mélenchon",
                                        "Régionales 2015 : F. Roussel"))
nord_com2 <- merge(nord_com, data2 %>%
                     select(COG, Voix, election), by.x ="insee", by.y = "COG", 
                   all.x=T )

nord_com2$Voix[is.na(nord_com2$Voix)]<-0

nord_com2$election <- factor(nord_com2$election,
                             levels = c("E14","L12","P12"),
                             labels = c('Européennes 2014 : L.-D. Gourmelen',
                                        "Législatives 2012 : FG",
                                        "Présidentielles 2012 : J.-L. Mélenchon"))

# Cartes et sorties ----------

ggplot()+
  geom_sf(data=nord_com1$geometry, aes(fill=nord_com1$Voix),
                 col="transparent")+
  facet_wrap(~nord_com1$election)+ scale_fill_gradient(low="#dbd5d5",
                                                         high = "#7b0000")+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.background = element_rect(fill= "transparent"),
        title=element_text(size=16),
        strip.text.x = element_text(size = 12))+
  coord_sf(crs = st_crs(nord_com1), datum = NA)+
    labs(title = "Résultats du PCF aux dernières élections",fill = "% des voix exprimées", caption ="Source : Ministère de l'Intérieur - https://www.data.gouv.fr/fr/posts/les-donnees-des-elections/")

ggsave(  path ="output", "30daymapchallenge_day7_1.png",width=12, height=9, dpi=300)


 ggplot()+
  geom_sf(data=nord_com2$geometry, aes(fill=nord_com2$Voix),
          col="transparent")+
  facet_wrap(~nord_com2$election, nrow=2, ncol=2)+ scale_fill_gradient(low="#dbd5d5",
                                                       high = "#7b0000")+
theme(panel.grid.major = element_line(colour = 'transparent'),
      panel.background = element_rect(fill= "transparent"),
      title=element_text(size=16),
      strip.text.x = element_text(size = 12))+
  coord_sf(crs = st_crs(nord_com2), datum = NA)+
    labs(title = "Résultats du PCF aux élections",fill = "% des voix exprimées", caption ="Source : Ministère de l'Intérieur - https://www.data.gouv.fr/fr/posts/les-donnees-des-elections/")
  
  
ggsave(  path ="output", "30daymapchallenge_day7_2.png",width=12, height=9, dpi=300)







