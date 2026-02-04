# Analyses jeu de données LOING #

# Package
library(tidyverse)
library(readr)
library(cowplot)
library(ggtext)
library(ggbump)

# # Environnement de travail
setwd("C:/Users/r0bin/OneDrive - Profish/Projets Profish/Profish FR/EPAGE Le Loing/data")

# Données
detections_filter <- readRDS("data_clean/detections_filter_1h.rds")
detections_bio <- readRDS("data_clean/detections_filter_bio.rds")

# Analyses

#########################################
#### Nombre de détection par antenne ####
#########################################

  # Ordonner les antennes et mise au propre nom
nom_antennes <- c(
  "LA_RONCE_AVAL",
  "LA_RONCE_AMONT",
  "MILLERON",
  "DALOT",
  "LANCIERE")

nom_clean <- c(
  "LA_RONCE_AVAL"  = "La Ronce aval",
  "LA_RONCE_AMONT" = "La Ronce amont",
  "MILLERON"       = "Milleron",
  "DALOT"          = "Dalot",
  "LANCIERE"       = "La Lancière")

detections_antenne_filter <- detections_bio %>%
  filter(!is.na(espece)) %>%
  count(antenne, name = "n_detections") %>%
  right_join(tibble(antenne = nom_antennes), by = "antenne") %>%
  mutate(
    n_detections = replace_na(n_detections, 0),
    antenne = factor(antenne, levels = nom_antennes))

detection_sum <- detections_antenne_filter %>%
  summarise(total = sum(n_detections, na.rm = TRUE)) %>%
  pull(total)

detection_ant <- detections_filter %>%
  group_by(antenne) %>%
  summarise(n_detections = n(), .groups = "drop") %>%
  right_join(tibble(antenne = nom_antennes), by = "antenne") %>%
  mutate(
    n_detections = replace_na(n_detections, 0),
    antenne = factor(antenne, levels = nom_antennes))



#######################################
####      Nombre d'individu        ####
#######################################

ind_sum <- detections_bio %>%
  filter(!is.na(espece)) %>%
  summarise(n_id = n_distinct(id_tag), .groups = "drop") %>% 
  pull(n_id)
