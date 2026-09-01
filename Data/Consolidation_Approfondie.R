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

################################################################################
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

#### RETENIR UNIQUEMENT LA SITUATION DE LA DATE MAX
BDD_115 <- data.table(BDD_115, key = "ID_PERSONNE","DATE_DEMANDE","SEMAINES")
BDD_115[,TYPOLOGIE2:=max(TYPOLOGIE), by = key(BDD_115)]

#### RECODAGE DE LA TYPOLOGIE
BDD_115$TYPOLOGIE_gp <- as.character(BDD_115$TYPOLOGIE2)
BDD_115$TYPOLOGIE_gp[BDD_115$TYPOLOGIE2 %in% c("Couple avec enfant",
                                               "Femme seule avec enfant(s)",
                                               "Groupe avec enfant(s)",
                                               "Homme seul avec enfant(s)")] <- "Ménages avec enfant(s)"
BDD_115$TYPOLOGIE_gp[BDD_115$TYPOLOGIE2 %in% c("Couple sans enfant",
                                               "Enfant / Mineur isolé",
                                               "Enfants / Mineurs en groupe",
                                               "Femme seule",
                                               "Groupe d'adultes sans enfant",
                                               "Homme seul")] <- "Ménages sans enfant"

#### RECODAGE DE LA NATIONALITE
BDD_115$NATIONALITE_gp[BDD_115$NATIONALITE %in% c("APATRIDE",
                                               "HORS_UE")] <- "Hors UE"
BDD_115$NATIONALITE_gp[BDD_115$NATIONALITE %in% c("FRANCAISE")] <- "Française"
BDD_115$NATIONALITE_gp[BDD_115$NATIONALITE %in% c("UE")] <- "UE"
BDD_115$NATIONALITE_gp[BDD_115$NATIONALITE %in% c("NR")] <- "Non renseigné"
BDD_115$NATIONALITE_gp <-fct_relevel(BDD_115$NATIONALITE_gp, "Française","UE","Hors UE","Non renseigné")
################################################################################
#### INDICATEUR_115 1 ####

# Nombre hebdomadaire de personnes distinctes qui ont formulé au moins une demande
# d'hébergement auprès du 115

Indicateur_1_AP <- BDD_115 %>%
  filter(RENOUVELLEMENT == "NON") %>%
  group_by(SEMAINES,
           TYPOLOGIE_gp,
           NATIONALITE_gp#,
           #RESIDENTIELLE_1
           ) %>%
  summarize(Pers_dist_en_Dem = n_distinct(ID_PERSONNE))

# Pour obtenir la valeur de la dernière semaine
Indicateur_1_Last_AP <- Indicateur_1_AP %>%
  filter(SEMAINES== max(SEMAINES))

# CLE de jointure
Indicateur_1_AP$CLE <- paste(Indicateur_1_AP$SEMAINES,
                             Indicateur_1_AP$TYPOLOGIE_gp,
                             Indicateur_1_AP$NATIONALITE_gp,
                             #Indicateur_1_AP$RESIDENTIELLE_1,
                             sep="_")

# PIVOT
#Indicateur_1_AP_pivot <- Indicateur_1_AP[c(1:4)]
#Indicateur_1_AP_pivot <- pivot_longer(Indicateur_1_AP_pivot,
#                                      cols=c(TYPOLOGIE,
#                                             NATIONALITE ),
#                                      names_to="Parent",
#                                      values_to="Child")

################################################################################
#### INDICATEUR_115 2 ####

# Nombre hebdomadaire de personnes distinctes dont au moins une demandes 
# d'hébergement a été pourvue par le 115

Indicateur_2_AP <- BDD_115 %>%
  filter(RENOUVELLEMENT == "NON") %>%
  filter(POURVUE == "OUI") %>%
  group_by(SEMAINES,
           TYPOLOGIE_gp,
           NATIONALITE_gp,
           #RESIDENTIELLE_1
           ) %>% 
  summarize(n = n_distinct(ID_PERSONNE)) 

# Pour obtenir la valeur de la dernière semaine
Indicateur_2_Last_AP <- Indicateur_2_AP %>%
  filter(SEMAINES== max(SEMAINES))

# CLE de jointure
Indicateur_2_AP$CLE <- paste(Indicateur_2_AP$SEMAINES,
                             Indicateur_2_AP$TYPOLOGIE_gp,
                             Indicateur_2_AP$NATIONALITE_gp,
                             #Indicateur_2_AP$RESIDENTIELLE_1,
                             sep="_")

# Selection de la CLE et de l'indice
Indicateur_2_AP <- Indicateur_2_AP[c(5,4)]

################################################################################
#### INDICATEUR_115 3 ####

# Nombre hebdomadaire de personnes distinctes dont aucune demande d'hébergement
# n'a été pourvue par le 115
BDD_115$AUCUNE_DEMANDE <- ifelse(BDD_115$POURVUE == "OUI","1","0")
BDD_115$AUCUNE_DEMANDE <- as.numeric(BDD_115$AUCUNE_DEMANDE)

# Calcul du nombre de demande par personnes
Indicateur_3_nbDem_AP <- BDD_115 %>%
  group_by(SEMAINES, 
           ID_PERSONNE,
           TYPOLOGIE_gp,
           NATIONALITE_gp,
           #RESIDENTIELLE_1
           ) %>%
  summarize(nb = sum(AUCUNE_DEMANDE))

# Calcul de l'indicateur
Indicateur_3_AP <- Indicateur_3_nbDem_AP %>%
  filter(nb == 0) %>%
  group_by(SEMAINES,
           TYPOLOGIE_gp,
           NATIONALITE_gp#,
           #RESIDENTIELLE_1
           ) %>%
  summarize(Pers_Dist_Aucune_Pourv = n_distinct(ID_PERSONNE))

# Pour obtenir la valeur de la dernière semaine
Indicateur_3_Last_AP <- Indicateur_3_AP %>%
  filter(SEMAINES== max(SEMAINES))

# CLE de jointure
Indicateur_3_AP$CLE <- paste(Indicateur_3_AP$SEMAINES,
                             Indicateur_3_AP$TYPOLOGIE_gp,
                             Indicateur_3_AP$NATIONALITE_gp,
                             #Indicateur_3_AP$RESIDENTIELLE_1,
                             sep="_")

# Selection de la CLE et de l'indice
Indicateur_3_AP <- Indicateur_3_AP[c(5,4)]

################################################################################
#### INDICATEUR_115 4 ####

# Nombre hebdomadaire de personnes distinctes qui ont formulé au moins une demande
# d'hébergement auprès du 115 et qui en étaient inconnues jusqu'alors

BDD_115$PRIMO <- ifelse(BDD_115$SEMAINES_DEMANDES_1 == BDD_115$SEMAINES, "Primo", "Non")

Indicateur_4_AP <- BDD_115 %>%
  filter(PRIMO == "Primo") %>%
  group_by(SEMAINES,
           TYPOLOGIE_gp,
           NATIONALITE_gp,
           #RESIDENTIELLE_1
           ) %>%
  summarize(Pers_Dist_Primo = n_distinct(ID_PERSONNE))

# Pour obtenir la valeur de la dernière semaine
Indicateur_4_Last_AP <- Indicateur_4_AP %>%
  filter(SEMAINES== max(SEMAINES))

# CLE de jointure
Indicateur_4_AP$CLE <- paste(Indicateur_4_AP$SEMAINES,
                             Indicateur_4_AP$TYPOLOGIE_gp,
                             Indicateur_4_AP$NATIONALITE_gp,
                             #Indicateur_4_AP$RESIDENTIELLE_1,
                             sep="_")

# Selection de la CLE et de l'indice
Indicateur_4_AP <- Indicateur_4_AP[c(5,4)]

################################################################################
#### TABLE COMPLETE : PERSONNES ####
Dashboard_1_AP <- merge (Indicateur_1_AP,Indicateur_2_AP, by = "CLE", all = TRUE)
Dashboard_2_AP <- merge (Dashboard_1_AP ,Indicateur_3_AP, by = "CLE", all = TRUE)
Dashboard_3_AP <- merge (Dashboard_2_AP ,Indicateur_4_AP, by = "CLE", all = TRUE)

DASHBOARD_AP <- Dashboard_3_AP %>%
  rename("Indicateur1" = "Pers_dist_en_Dem",
         "Indicateur2" = "n",
         "Indicateur3" = "Pers_Dist_Aucune_Pourv",
         "Indicateur4" = "Pers_Dist_Primo")

########################################################






