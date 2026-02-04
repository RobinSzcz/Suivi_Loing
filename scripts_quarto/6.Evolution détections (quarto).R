# Packages
library(tidyverse)
library(readxl)
library(kableExtra)
library(webshot2)
library(purrr)

# Chargement des données
suivi_data <- read_excel("../data_raw/suivi_data.xlsx", 
                         col_types = c("numeric", "text", "date", 
                                       "date", "numeric", "date", "numeric"))

detections_bio <- readRDS("../data_clean/detections_filter_bio.rds") %>% 
  filter(!is.na(espece))


# Préparation des données
suivi_ord <- suivi_data %>%
  arrange(date_fin) %>%
  mutate(date_fin_prev = lag(date_fin))

# Tableau de suivi des téléchargements
tableau_suivi <- suivi_ord %>%
  mutate(
    stats = pmap(
      list(date_fin_prev, date_fin),
      function(debut, fin) {
        
        det_window <- detections_bio %>%
          filter(
            date_heure <= fin,
            is.na(debut) | date_heure > debut
          )
        
        det_before <- detections_bio %>%
          filter(
            !is.na(debut),
            date_heure <= debut
          )
        
        ids_window <- unique(det_window$id_tag)
        ids_before <- unique(det_before$id_tag)
        
        tibble(
          n_id_depuis_dernier = length(ids_window),
          n_nouveaux_id       = length(setdiff(ids_window, ids_before))
        )
      }
    )
  ) %>%
  unnest(stats)

# Transformer la liste des nouveaux ID en texte pour affichage
tableau_suivi_aff <- tableau_suivi %>%
  select(-heure_debut, -heure_fin, -date_dl, -date_fin_prev) %>%
  mutate(
    n_id_depuis_dernier = as.integer(n_id_depuis_dernier),
    n_id_depuis_dernier = ifelse(is.na(n_id_depuis_dernier),
                                 '<span style="color:red; font-style:italic;">NA</span>',
                                 as.character(n_id_depuis_dernier)),
    n_nouveaux_id = ifelse(is.na(n_nouveaux_id),
                           '<span style="color:red; font-style:italic;">NA</span>',
                           as.character(n_nouveaux_id)))

tableau_suivi_aff <- tableau_suivi_aff %>%
  mutate(across(everything(), ~ unlist(.x)))

# Créer le HTML 
html_file <- "tableaux/clean/tableau_suivi.html"
caption_text <- "<b style='color:black; font-size:13pt;'>Suivi des téléchargements</b><br>"

tableau_suivi_aff <- tableau_suivi_aff %>%
  kbl(
    format = "html",
    escape = FALSE,
    align = c("c", "l", "c", "c", "c"),
    col.names = c("N° DL", "Fichier", "Date début", "Date fin", "Nb ID", "Nb nouveaux ID"),
    caption = caption_text) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "snow3")
