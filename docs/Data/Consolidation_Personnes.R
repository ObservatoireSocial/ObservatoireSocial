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

################################################################################

#### INDICATEUR_115 1 ####

# Nombre hebdomadaire de personnes distinctes qui ont formulé au moins une demande
# d'hébergement auprès du 115

Indicateur_1 <- BDD_115 %>%
  filter(RENOUVELLEMENT == "NON") %>%
  group_by(SEMAINES) %>%
  summarize(Pers_dist_en_Dem = n_distinct(ID_PERSONNE))

# Pour obtenir la valeur de la dernière semaine
Indicateur_1_Last <- Indicateur_1 %>%
  filter(SEMAINES== max(SEMAINES))

################################################################################

#### INDEICATEUR_115 2 ####

# Nombre hebdomadaire de personnes distinctes dont au moins une demandes 
# d'hébergement a été pourvue par le 115

Indicateur_2 <- BDD_115 %>%
  filter(RENOUVELLEMENT == "NON") %>%
  filter(POURVUE == "OUI") %>%
  group_by(SEMAINES) %>% 
  summarize(n = n_distinct(ID_PERSONNE)) 

# Pour obtenir la valeur de la dernière semaine
Indicateur_2_Last <- Indicateur_2 %>%
  filter(SEMAINES== max(SEMAINES))

################################################################################

#### INDICATEUR_115 3 ####

# Nombre hebdomadaire de personnes distinctes dont aucune demande d'hébergement
# n'a été pourvue par le 115
BDD_115$DEMANDES_POURVUE_CODE <- ifelse(BDD_115$POURVUE == "OUI","1","null")
BDD_115$DEMANDES_POURVUE_CODE <- as.numeric(BDD_115$DEMANDES_POURVUE_CODE)
BDD_115$DEMANDES_NON_POURVUE_CODE <- ifelse(BDD_115$POURVUE != "OUI","0","null")
BDD_115$DEMANDES_NON_POURVUE_CODE <- as.numeric(BDD_115$DEMANDES_NON_POURVUE_CODE)
BDD_115$AUCUNE_DEMANDE <- rowSums(BDD_115[,c("DEMANDES_POURVUE_CODE","DEMANDES_NON_POURVUE_CODE")],na.rm = TRUE)

Indicateur_3.2 <- BDD_115 %>%
  group_by(SEMAINES, ID_PERSONNE) %>%
  summarize(nb = sum(AUCUNE_DEMANDE))

Indicateur_3 <- Indicateur_3.2 %>%
  filter(nb == 0) %>%
  group_by(SEMAINES) %>%
  summarize(Pers_Dist_Aucune_Pourv = n_distinct(ID_PERSONNE))

# Pour obtenir la valeur de la dernière semaine
Indicateur_3_Last <- Indicateur_3 %>%
  filter(SEMAINES== max(SEMAINES))

################################################################################

#### INDICATEUR_115 4 ####

# Nombre hebdomadaire de personnes distinctes qui ont formulé au moins une demande
# d'hébergement auprès du 115 et qui en étaient inconnues jusqu'alors

BDD_115$PRIMO <- ifelse(BDD_115$SEMAINES_DEMANDES_1 == BDD_115$SEMAINES, "Primo", "Non")

Indicateur_4 <- BDD_115 %>%
  filter(PRIMO == "Primo") %>%
  group_by(SEMAINES) %>%
  summarize(Pers_Dist_Primo = n_distinct(ID_PERSONNE))

# Pour obtenir la valeur de la dernière semaine
Indicateur_4_Last <- Indicateur_4 %>%
  filter(SEMAINES== max(SEMAINES))

################################################################################

#### INDICATUER_115 5 ####

# Nombre de demandes d'hébergement formulées auprès du 115

Indicateur_5 <- BDD_115 %>%
  filter(RENOUVELLEMENT=="NON")%>%
  group_by(SEMAINES) %>%
  summarize(Demandes_pers = n_distinct(ID_DEMANDE))

# Pour obtenir la valeur de la dernière semaine
Indicateur_5_Last <- Indicateur_5 %>%
  filter(SEMAINES== max(SEMAINES))

################################################################################

#### TABLE COMPLETE : PERSONNES ####
Dashboard_1 <- merge (Indicateur_1,Indicateur_2, by = "SEMAINES")
Dashboard_2 <- merge (Dashboard_1, Indicateur_3, by = "SEMAINES")
Dashboard_3 <- merge (Dashboard_2, Indicateur_4, by = "SEMAINES")
Dashboard_4 <- merge (Dashboard_3, Indicateur_5, by = "SEMAINES")

DASHBOARD <- Dashboard_4 %>%
  rename("Indicateur1" = "Pers_dist_en_Dem",
         "Indicateur2" = "n",
         "Indicateur3" = "Pers_Dist_Aucune_Pourv",
         "Indicateur4" = "Pers_Dist_Primo",
         "Indicateur5" = "Demandes_pers")

########################################################

#### TAUX D'EVOLUTION ####
# data.table = shift
# L'arrondie peut etre fait directement dans la table (DT)

DASHBOARD$Ind1_ante <- lag(DASHBOARD$Indicateur1)
DASHBOARD$Ind1_Evol <- ((DASHBOARD$Indicateur1 - DASHBOARD$Ind1_ante)/DASHBOARD$Ind1_ante)
DASHBOARD$Ind1_Evol <- round(DASHBOARD$Ind1_Evol, digits=2)

DASHBOARD$Ind2_ante <- lag(DASHBOARD$Indicateur2)
DASHBOARD$Ind2_Evol <- ((DASHBOARD$Indicateur2 - DASHBOARD$Ind2_ante)/DASHBOARD$Ind2_ante)
DASHBOARD$Ind2_Evol <- round(DASHBOARD$Ind2_Evol, digits=2)

DASHBOARD$Ind3_ante <- lag(DASHBOARD$Indicateur3)
DASHBOARD$Ind3_Evol <- ((DASHBOARD$Indicateur3 - DASHBOARD$Ind3_ante)/DASHBOARD$Ind3_ante)
DASHBOARD$Ind3_Evol <- round(DASHBOARD$Ind3_Evol, digits=2)

DASHBOARD$Ind4_ante <- lag(DASHBOARD$Indicateur4)
DASHBOARD$Ind4_Evol <- ((DASHBOARD$Indicateur4 - DASHBOARD$Ind4_ante)/DASHBOARD$Ind4_ante)
DASHBOARD$Ind4_Evol <- round(DASHBOARD$Ind4_Evol, digits=2)

DASHBOARD$Ind5_ante <- shift(DASHBOARD$Indicateur5)
DASHBOARD$Ind5_Evol <- ((DASHBOARD$Indicateur5 - DASHBOARD$Ind5_ante)/DASHBOARD$Ind5_ante)
DASHBOARD$Ind5_Evol <- round(DASHBOARD$Ind5_Evol, digits=2)

DASHBOARD <- DASHBOARD[c(1,2,8,3,10,4,12,5,14,6,16)]

DASHBOARD2 <- DASHBOARD #%>%
  #filter(SEMAINES >= "2026-01-05")

################################################################################



