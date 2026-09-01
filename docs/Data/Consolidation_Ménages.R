#### CONSOLIDATION BASE DE DONNEES ####
library(here)
library(readxl)
library(tidyverse)
library(dplyr)
library(data.table)
library(lubridate)
library(labelled)
library(fs)


files <- fs::dir_ls(here("Data/01. Data"))
BDD_115<-files %>%
  map_dfr(read_xlsx, .id = "source")

BDD_115$EXTRACTION<-str_sub(BDD_115$source,89,97)

#### RENOMMAGE DES VARIABLES  -> dplyr ####
BDD_115 <- BDD_115 %>%
  rename(ID_DEMANDE = "ID de la demande",
         ID_MENAGE = "Identifiant de ménage",
         ID_PERSONNE = "Identifiant de personne",
         TYPE_DEMANDE = "Type de la demande",
         TYPOLOGIE = "Typologie de sous-ensemble de ménage",
         DATE_DEMANDE = "Date de la demande",
         RENOUVELLEMENT = "Demande issue d'un renouvellement",
         ANIMAL = "Présence animal",
         POURVUE = "Demande pourvue",
         REFUS_USAGER = "Motif de refus Usager",
         AGE = "Âge au moment de la demande",
         SITUATION = "Situation",
         DOMICILIATION = "Domiciliation",
         NATIONALITE = "Nationalité",
         SUIVI = "Suivi social",
         RESIDENTIELLE_1 = "Situation résidentielle de niveau 1 au moment de la demande",
         RESIDENTIELLE_2 = "Situation résidentielle de niveau 2 au moment de la demande",
         LOGEMENT_SOCIAL = "Demande de logement social",
         DAHO = "Passage en commission DAHO",
         PREMIERE_DEMANDE = "Date de la 1ère demande" )

#### AJOUT DES SEMAINES -> lubridate ####
BDD_115$SEMAINES <- floor_date(BDD_115$DATE_DEMANDE, unit = "week", week_start = 1)
BDD_115$SEMAINES_DEMANDES_1 <- floor_date(BDD_115$PREMIERE_DEMANDE, unit = "week", week_start = 1)

########################################################

#### INDICATEUR_115 1 ####

# Nombre hebdomadaire de ménages distincts qui ont formulé au moins une demande
# d'hébergement auprès du 115

Indicateur_1m <- BDD_115 %>%
  filter(RENOUVELLEMENT == "NON") %>%
  group_by(SEMAINES) %>%
  summarize(Men_dist_en_Dem = n_distinct(ID_MENAGE))

# Pour obtenir la valeur de la dernière semaine
Indicateur_1m_Last <- Indicateur_1m %>%
  filter(SEMAINES== max(SEMAINES))

########################################################

#### INDEICATEUR_115 2 ####

# Nombre hebdomadaire de ménages distincts dont au moins une demandes 
# d'hébergement a été pourvue par le 115

Indicateur_2m <- BDD_115 %>%
  filter(RENOUVELLEMENT == "NON") %>%
  filter(POURVUE == "OUI") %>%
  group_by(SEMAINES) %>% 
  summarize(m = n_distinct(ID_MENAGE)) 

# Pour obtenir la valeur de la dernière semaine
Indicateur_2m_Last <- Indicateur_2m %>%
  filter(SEMAINES== max(SEMAINES))

########################################################

#### INDICATEUR_115 3 ####

# Nombre hebdomadaire de ménages distincts dont aucune demande d'hébergement
# n'a été pourvue par le 115
BDD_115$DEMANDES_POURVUE_CODE <- ifelse(BDD_115$POURVUE == "OUI","1","null")
BDD_115$DEMANDES_POURVUE_CODE <- as.numeric(BDD_115$DEMANDES_POURVUE_CODE)
BDD_115$DEMANDES_NON_POURVUE_CODE <- ifelse(BDD_115$POURVUE != "OUI","0","null")
BDD_115$DEMANDES_NON_POURVUE_CODE <- as.numeric(BDD_115$DEMANDES_NON_POURVUE_CODE)
BDD_115$AUCUNE_DEMANDE <- rowSums(BDD_115[,c("DEMANDES_POURVUE_CODE","DEMANDES_NON_POURVUE_CODE")],na.rm = TRUE)

Indicateur_3.2m <- BDD_115 %>%
  group_by(SEMAINES, ID_MENAGE) %>%
  summarize(nb = sum(AUCUNE_DEMANDE))

Indicateur_3m <- Indicateur_3.2m %>%
  filter(nb == 0) %>%
  group_by(SEMAINES) %>%
  summarize(Men_Dist_Aucune_Pourv = n_distinct(ID_MENAGE))

# Pour obtenir la valeur de la dernière semaine
Indicateur_3m_Last <- Indicateur_3m %>%
  filter(SEMAINES== max(SEMAINES))

########################################################

#### INDICATEUR_115 4 ####

# Nombre hebdomadaire de ménages distincts qui ont formulé au moins une demande
# d'hébergement auprès du 115 et qui en étaient inconnues jusqu'alors

BDD_115$PRIMO <- ifelse(BDD_115$SEMAINES_DEMANDES_1 == BDD_115$SEMAINES, "Primo", "Non")

Indicateur_4m <- BDD_115 %>%
  filter(PRIMO == "Primo") %>%
  group_by(SEMAINES) %>%
  summarize(Men_Dist_Primo = n_distinct(ID_MENAGE))

# Pour obtenir la valeur de la dernière semaine
Indicateur_4m_Last <- Indicateur_4m %>%
  filter(SEMAINES== max(SEMAINES))

########################################################

#### TABLE COMPLETE : MENAGES ####
Dashboard_1m <- merge (Indicateur_1m,Indicateur_2m, by = "SEMAINES")
Dashboard_2m <- merge (Dashboard_1m, Indicateur_3m, by = "SEMAINES")
Dashboard_3m <- merge (Dashboard_2m, Indicateur_4m, by = "SEMAINES")

DASHBOARD_m <- Dashboard_3m %>%
  rename("Indicateur1" = "Men_dist_en_Dem",
         "Indicateur2" = "m",
         "Indicateur3" = "Men_Dist_Aucune_Pourv",
         "Indicateur4" = "Men_Dist_Primo")

########################################################
# L'arrondie peut etre fait directement dans la table (DT)

#### PERSONNES ####

DASHBOARD_m$Men1_ante <- shift(DASHBOARD_m$Indicateur1)
DASHBOARD_m$Men1_Evol <- ((DASHBOARD_m$Indicateur1 - DASHBOARD_m$Men1_ante)/DASHBOARD_m$Men1_ante)
DASHBOARD_m$Men1_Evol <- round(DASHBOARD_m$Men1_Evol, digits=2)

DASHBOARD_m$Men2_ante <- shift(DASHBOARD_m$Indicateur2)
DASHBOARD_m$Men2_Evol <- ((DASHBOARD_m$Indicateur2 - DASHBOARD_m$Men2_ante)/DASHBOARD_m$Men2_ante)
DASHBOARD_m$Men2_Evol <- round(DASHBOARD_m$Men2_Evol, digits=2)

DASHBOARD_m$Men3_ante <- shift(DASHBOARD_m$Indicateur3)
DASHBOARD_m$Men3_Evol <- ((DASHBOARD_m$Indicateur3 - DASHBOARD_m$Men3_ante)/DASHBOARD_m$Men3_ante)
DASHBOARD_m$Men3_Evol <- round(DASHBOARD_m$Men3_Evol, digits=2)

DASHBOARD_m$Men4_ante <- shift(DASHBOARD_m$Indicateur4)
DASHBOARD_m$Men4_Evol <- ((DASHBOARD_m$Indicateur4 - DASHBOARD_m$Men4_ante)/DASHBOARD_m$Men4_ante)
DASHBOARD_m$Men4_Evol <- round(DASHBOARD_m$Men4_Evol, digits=2)


DASHBOARD_m <- DASHBOARD_m[c(1,2,7,3,9,4,11,5,13)]
#View(DASHBOARD_m)

