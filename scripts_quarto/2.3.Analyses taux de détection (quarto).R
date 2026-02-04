# Analyses jeu de données LOING #

# Package
library(tidyverse)
library(readr)
library(cowplot)
library(ggtext)
library(ggpubr)
# Package
library(kableExtra)
library(webshot2)
library(formattable)

# Environnement de travail
## Ne jamais utiliser de setwd() dans un quarto ==> uniquement chemin relatif à data/

# Mise en forme tableaux
###############################################################################

# Données
taux_redetection_esp_lot <- read_delim("../tableaux/taux_redetection_esp_lot.csv", 
                                       delim = ",", escape_double = FALSE, trim_ws = TRUE)
taux_redetection_esp <- read_delim("../tableaux/taux_redetection_espece.csv", 
                                       delim = ",", escape_double = FALSE, trim_ws = TRUE)
taux_redetection_lot <- read_delim("../tableaux/taux_redetection_lot.csv", 
                                       delim = ",", escape_double = FALSE, trim_ws = TRUE)

# ESPECE #

taux_redetection_esp <- taux_redetection_esp %>%
  rename("Espèces" = espece,
         "N. lâchés" = n_laches,
         "N. détectés" = n_detectes,
         "Taux (%)" = taux_redetection,
         "Indicateur" = indicateur)

  # Personnalisation colonne Taux
taux_redetection_esp$`Taux (%)` <- round(taux_redetection_esp$`Taux (%)`, 1)

taux_redetection_esp$Couleur_barre <- case_when(
  taux_redetection_esp$`N. lâchés` < 5 ~ "grey",
  taux_redetection_esp$`Taux (%)` > 33.3 ~ "seagreen",
  taux_redetection_esp$`Taux (%)` >= 20 & taux_redetection_esp$`Taux (%)` <= 33.3 ~ "goldenrod",
  taux_redetection_esp$`Taux (%)` < 20 ~ "tomato",
  TRUE ~ "lightgrey")

# Appliquer color_bar sur la colonne entière avec la couleur correspondante
taux_redetection_esp$`Taux (%)` <- color_bar(taux_redetection_esp$Couleur_barre)(taux_redetection_esp$`Taux (%)`)

# Aligner le texte à gauche (HTML)
taux_redetection_esp$`Taux (%)` <- paste0('<div style="text-align: left;">', taux_redetection_esp$`Taux (%)`, '</div>')

# Personnalisation colonne Indicateur
taux_redetection_esp$Indicateur <- cell_spec(
  taux_redetection_esp$Indicateur,
  color = case_when(
    taux_redetection_esp$Indicateur == "Bon" ~ "seagreen",
    taux_redetection_esp$Indicateur == "Moyen" ~ "goldenrod",
    taux_redetection_esp$Indicateur == "Mauvais" ~ "tomato",
    taux_redetection_esp$Indicateur == "Données*" ~ "grey",
    TRUE ~ "black"),
  bold = TRUE)

# texte des titres
caption_text <- paste0(
  "<b style='color:black; font-size:13pt;'>Résumé des taux de redétection par espèce</b><br>",
  "<b><span style='font-size:10pt; color:grey;'>Indicateur : ",
  "<span style='color:seagreen;'>Bon (&gt;33.3)</span>, ",
  "<span style='color:orange;'>Moyen (20-33.3)</span>, ",
  "<span style='color:tomato;'>Mauvais (&lt;20)</span>",
  "</span></b>")

  # Création de la table 
table_esp <- taux_redetection_esp %>%
  select(`Espèces`, `N. lâchés`, `N. détectés`, `Taux (%)`, `Indicateur`) %>%
  kbl(
    caption = caption_text,
    digits = 1,
    align = "c",
    escape = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) %>% 
  column_spec(4, extra_css = "white-space: nowrap;") %>%
  row_spec(0, bold = TRUE, color = "black", background = "snow3") %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(nrow(taux_redetection_esp), 
           bold = TRUE, 
           color = "black", 
           background = "snow3",
           font_size = 14)  


# LOT #
taux_redetection_lot <- taux_redetection_lot %>%
  rename("Lot" = lot,
         "Site de lâcher" = site_lache,
         "N. lâchés" = n_laches,
         "N. détectés" = n_detectes,
         "Taux (%)" = taux_redetection,
         "Indicateur" = indicateur)

# Arrondir le taux
taux_redetection_lot$`Taux (%)` <- round(taux_redetection_lot$`Taux (%)`, 1)

# Couleur des barres selon le taux
taux_redetection_lot$Couleur_barre <- case_when(
  taux_redetection_lot$`N. lâchés` < 5 ~ "grey",
  taux_redetection_lot$`Taux (%)` > 33.3 ~ "seagreen",
  taux_redetection_lot$`Taux (%)` >= 20 & taux_redetection_lot$`Taux (%)` <= 33.3 ~ "goldenrod",
  taux_redetection_lot$`Taux (%)` < 20 ~ "tomato",
  TRUE ~ "lightgrey")

# Appliquer color_bar
taux_redetection_lot$`Taux (%)` <- color_bar(taux_redetection_lot$Couleur_barre)(taux_redetection_lot$`Taux (%)`)

# Aligner le texte à gauche
taux_redetection_lot$`Taux (%)` <- paste0('<div style="text-align: left;">', taux_redetection_lot$`Taux (%)`, '</div>')

# Personnalisation colonne Indicateur
taux_redetection_lot$Indicateur <- cell_spec(
  taux_redetection_lot$Indicateur,
  color = case_when(
    taux_redetection_lot$Indicateur == "Bon" ~ "seagreen",
    taux_redetection_lot$Indicateur == "Moyen" ~ "goldenrod",
    taux_redetection_lot$Indicateur == "Mauvais" ~ "tomato",
    taux_redetection_lot$Indicateur == "Données*" ~ "grey",
    TRUE ~ "black"),
  bold = TRUE)

# Texte du titre
caption_text_lot <- paste0(
  "<b style='color:black; font-size:13pt;'>Résumé des taux de redétection par lot</b><br>",
  "<b><span style='font-size:10pt; color:grey;'>Indicateur : ",
  "<span style='color:seagreen;'>Bon (&gt;33.3)</span>, ",
  "<span style='color:orange;'>Moyen (20-33.3)</span>, ",
  "<span style='color:tomato;'>Mauvais (&lt;20)</span>",
  "</span></b>")

# Création de la table
table_lot <- taux_redetection_lot %>%
  select(`Lot`, `Site de lâcher`, `N. lâchés`, `N. détectés`, `Taux (%)`, `Indicateur`) %>%
  kbl(
    caption = caption_text_lot,
    digits = 1,
    align = "c",
    escape = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center") %>%
  column_spec(2:4, extra_css = "white-space: nowrap;") %>%
  row_spec(0, bold = TRUE, color = "black", background = "snow3") %>%
  column_spec(2, bold = TRUE) %>%
  row_spec(nrow(taux_redetection_lot),
           bold = TRUE,
           color = "black",
           background = "snow3",
           font_size = 14) 

# ESPECE / LOT #
# Renommer les colonnes pour affichage
taux_redetection_esp_lot <- taux_redetection_esp_lot %>%
  rename("Lot" = lot,
         "Espèce" = espece,
         "N. lâchés" = n_laches,
         "N. détectés" = n_detectes,
         "Taux (%)" = taux_redetection,
         "Indicateur" = indicateur)

# Arrondir le taux
taux_redetection_esp_lot$`Taux (%)` <- round(taux_redetection_esp_lot$`Taux (%)`, 1)

# Couleur des barres selon le taux
taux_redetection_esp_lot$Couleur_barre <- case_when(
  taux_redetection_esp_lot$`N. lâchés` < 5 ~ "grey",
  taux_redetection_esp_lot$`Taux (%)` > 33.3 ~ "seagreen",
  taux_redetection_esp_lot$`Taux (%)` >= 20 & taux_redetection_esp_lot$`Taux (%)` <= 33.3 ~ "goldenrod",
  taux_redetection_esp_lot$`Taux (%)` < 20 ~ "tomato",
  TRUE ~ "lightgrey")

# Appliquer color_bar
taux_redetection_esp_lot$`Taux (%)` <- color_bar(taux_redetection_esp_lot$Couleur_barre)(taux_redetection_esp_lot$`Taux (%)`)

# Aligner le texte à gauche
taux_redetection_esp_lot$`Taux (%)` <- paste0('<div style="text-align: left;">', taux_redetection_esp_lot$`Taux (%)`, '</div>')

# Personnalisation colonne Indicateur
taux_redetection_esp_lot$Indicateur <- cell_spec(
  taux_redetection_esp_lot$Indicateur,
  color = case_when(
    taux_redetection_esp_lot$Indicateur == "Bon" ~ "seagreen",
    taux_redetection_esp_lot$Indicateur == "Moyen" ~ "goldenrod",
    taux_redetection_esp_lot$Indicateur == "Mauvais" ~ "tomato",
    taux_redetection_esp_lot$Indicateur == "Données*" ~ "grey",
    TRUE ~ "black"),
  bold = TRUE)

# Texte du titre
caption_text_esp_lot <- paste0(
  "<b style='color:black; font-size:13pt;'>Résumé des taux de redétection par lot et par espèce</b><br>",
  "<b><span style='font-size:10pt; color:grey;'>Indicateur : ",
  "<span style='color:seagreen;'>Bon (&gt;33.3)</span>, ",
  "<span style='color:orange;'>Moyen (20-33.3)</span>, ",
  "<span style='color:tomato;'>Mauvais (&lt;20)</span>, ",
  "<span style='color:grey;'>Données* (effectif &lt; 5)</span>",
  "</span></b>")

# Création de la table
tables_esp_lot <- taux_redetection_esp_lot %>%
  # garder toutes les colonnes pour le calcul du style, mais retirer Couleur_barre pour l'affichage
  split(.$Lot) %>%
  lapply(function(df) {
    df %>%
      select(-Couleur_barre) %>%  
      kbl(
        digits = 1,
        align = "c",
        escape = FALSE) %>%
      collapse_rows(columns = 1, valign = "middle") %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = FALSE,
        position = "center") %>%
      row_spec(0, bold = TRUE, color = "black", background = "snow3")
  })

# Préparer les données
table_filtrable <- taux_redetection_esp_lot %>%
  select(Lot, Espèce, `N. lâchés`, `N. détectés`, `Taux (%)`, Indicateur)

# Facteurs pour menus déroulants
table_filtrable$Lot <- as.factor(table_filtrable$Lot)
table_filtrable$Espèce <- as.factor(table_filtrable$Espèce)
