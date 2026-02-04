# Taux de franchissement brut

# Package
library(tidyverse)
library(knitr)
library(kableExtra)
library(htmltools)
library(webshot2)

# Données
detections_bio <- readRDS("../data_clean/detections_filter_bio.rds")
marquages_clean <- readRDS("../data_clean/marquages_clean.rds")

# Ordre des antennes et site de lâcher
nom_antennes <- c("LA_RONCE_AVAL", "LA_RONCE_AMONT", "MILLERON", "DALOT", "LANCIERE")

ordre_ant <- c(
  "RONCE AVAL"      = "Aval",
  "LA_RONCE_AVAL"   = "La Ronce aval",
  "LA_RONCE_AMONT"  = "La Ronce amont",
  "MILLERON"        = "Milleron",
  "CITY STADE"      = "City Stade",
  "DALOT AVAL"      = "Dalot aval",
  "DALOT"           = "Dalot",
  "LANCIERE AVAL"   = "La Lancière aval",
  "LANCIERE"        = "La Lancière")

ordre_antenne <- tibble(
  site = names(ordre_ant),
  ordre_pos = seq_along(ordre_ant),
  type = ifelse(names(ordre_ant) %in% nom_antennes, "antenne", "site_lache"))

marquages_ordres <- marquages_clean %>%
  left_join(
    ordre_antenne %>% 
      filter(type == "site_lache") %>% 
      rename(site_lache = site, ordre_lache = ordre_pos),
    by = "site_lache")

# Ajouter l'ordre à detection_bio
detection_antenne <- detections_bio %>%
  left_join(
    ordre_antenne %>% 
      filter(type == "antenne") %>% 
      rename(antenne = site, ordre_ant = ordre_pos),
    by = "antenne")

detection_antenne <- detection_antenne %>%
  left_join(
    marquages_ordres %>% select(id_tag = ID_HPR, ordre_lache),
    by = "id_tag")

# Calcul du nombre de poissons relâchés à l’aval de chaque antenne
# Créer toutes les combinaisons poisson × antenne
marquages_cross <- marquages_ordres %>%
  crossing(tibble(antenne = nom_antennes))

# Ajouter ordre_ant depuis ordre_antenne
marquages_cross <- marquages_cross %>%
  left_join(
    ordre_antenne %>%
      filter(type == "antenne") %>%
      rename(ordre_ant = ordre_pos, antenne = site),
    by = "antenne")

# Filtrer poissons relâchés à l'aval de chaque antenne et calculer N_laches_aval
N_laches_aval <- marquages_cross %>%
  filter(ordre_lache < ordre_ant) %>%
  group_by(antenne) %>%
  summarise(n_laches_aval = n_distinct(ID_HPR), .groups = "drop")

# Calcul du nombre de poissons détectés sur chaque antenne
detection_montaison <- detection_antenne %>%
  filter(!is.na(espece)) %>% 
  distinct(id_tag, antenne, ordre_lache, ordre_ant, .keep_all = TRUE) %>%
  filter(ordre_lache < ordre_ant)

detection_total <- detection_antenne %>%
  filter(!is.na(espece)) %>%
  distinct(id_tag, antenne) %>%
  group_by(antenne) %>%
  summarise(n_detectes_total = n_distinct(id_tag), .groups = "drop")

N_detectes_montaison <- detection_montaison %>%
  group_by(antenne) %>%
  summarise(n_detectes = n_distinct(id_tag), .groups = "drop")

# Calcul du taux brut des poissons en montaison
taux_brut_montaison <- N_laches_aval %>%
  left_join(N_detectes_montaison, by = "antenne") %>%
  # left_join(detection_total, by = "antenne") %>% 
  mutate(taux_brut = 100 * n_detectes / n_laches_aval)

nom_antennes_propres <- c(
  "LA_RONCE_AVAL"  = "La Ronce aval",
  "LA_RONCE_AMONT" = "La Ronce amont",
  "MILLERON"       = "Milleron",
  "DALOT"          = "Dalot",
  "LANCIERE"       = "La Lancière")

# Mise en forme du tableau
tableau <- taux_brut_montaison %>%
  mutate(
    antenne = nom_antennes_propres[antenne],
    antenne = factor(antenne, levels = nom_antennes_propres),
    taux_brut = round(taux_brut, 1),      # pourcentage arrondi
    n_laches_aval = as.integer(n_laches_aval),
    n_detectes = as.integer(n_detectes)) %>%
  arrange(desc(antenne)) %>%
  mutate(
    n_laches_aval = ifelse(is.na(n_laches_aval),
                           '<span style="color:red; font-style:italic;">NA</span>',
                           as.character(n_laches_aval)),
    n_detectes = ifelse(is.na(n_detectes),
                        '<span style="color:red; font-style:italic;">NA</span>',
                        as.character(n_detectes)),
    taux_brut = ifelse(is.na(taux_brut),
                       '<span style="color:red; font-style:italic;">NA</span>',
                       as.character(round(taux_brut, 1))))

# Création de la table

# Nom du fichier HTML
html_file <- "tableaux/clean/tableau_taux_franchissement.html"

caption_text <- paste0(
  "<b style='color:black; font-size:13pt;'>Taux de franchissement brut</b><br>")

tableau_brut <- tableau %>%
  kbl(
    format = "html",
    escape = FALSE,
    align = c("l", "c", "c", "c"),
    col.names = c("Antenne", "Poissons relâchés (aval)", "Poissons détectés", "Taux (%)"),
    caption = caption_text) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center") %>%
  row_spec(0, bold = TRUE, color = "black", background = "snow3")

############# TAUX CUMULATIF #########################


# Définir les ordres des sites et antennes
ordre_sites <- ordre_antenne %>%
  filter(type == "site_lache") %>%
  rename(site_lache = site, ordre_lache = ordre_pos)

ordre_ant <- ordre_antenne %>%
  filter(type == "antenne") %>%
  select(antenne = site, ordre_ant = ordre_pos)

# Ajouter ordre_lache aux marquage
marquages_ordres <- marquages_clean %>%
  mutate(ID_HPR = as.character(ID_HPR)) %>%
  left_join(ordre_sites, by = "site_lache")

# Ajouter ordre_ant et ordre_lache aux detections
detections_ordres <- detections_bio %>%
  mutate(id_tag = as.character(id_tag)) %>%
  left_join(ordre_ant, by = "antenne") %>%
  left_join(
    marquages_ordres %>%
      select(id_tag = ID_HPR, ordre_lache, site_lache_marquage = site_lache),
    by = "id_tag"
  ) %>%
  distinct(id_tag, antenne, ordre_ant, ordre_lache, site_lache_marquage, .keep_all = TRUE)

# Filtrer les poissons en montaison (ordre_ant > ordre_lache)
detections_montaison <- detections_ordres %>%
  filter(ordre_ant > ordre_lache)

# N_rel : nombre de poissons relâchés à l'aval de chaque antenne
N_rel <- marquages_ordres %>%
  mutate(id_tag = as.character(ID_HPR)) %>%
  crossing(ordre_ant) %>%
  filter(ordre_lache < ordre_ant) %>%
  group_by(site_lache, antenne, ordre_ant) %>%
  summarise(N_rel = n_distinct(ID_HPR), .groups = "drop")

# N_pass : nombre de poissons détectés à l'antenne
N_pass <- detections_montaison %>%
  group_by(site_lache_marquage, antenne, ordre_ant) %>%
  summarise(N_pass = n_distinct(id_tag), .groups = "drop") %>%
  rename(site_lache = site_lache_marquage)

# Combiner N_rel et N_pass et calculer proportions
taux_cumulatif_site <- N_rel %>%
  left_join(N_pass, by = c("site_lache", "antenne", "ordre_ant")) %>%
  mutate(
    N_pass = ifelse(is.na(N_pass), 0, N_pass),
    prop = N_pass / N_rel,
    prop_pct = round(prop * 100, 1))

# Mise en forme
# Préparation du df 
taux_cumulatif_site_clean <- taux_cumulatif_site %>%
  mutate(

    site_lache_nom = case_when(
      site_lache == "CITY STADE"    ~ "City Stade",
      site_lache == "DALOT AVAL"    ~ "Dalot Aval",
      site_lache == "LANCIERE AVAL" ~ "Lancière Aval",
      site_lache == "RONCE AVAL"    ~ "Ronce Aval",
      TRUE ~ site_lache
    ),

    antenne_nom = case_when(
      antenne == "LA_RONCE_AVAL" ~ "La Ronce aval",
      antenne == "LA_RONCE_AMONT" ~ "La Ronce amont",
      antenne == "MILLERON"       ~ "Milleron",
      antenne == "DALOT"          ~ "Dalot",
      antenne == "LANCIERE"       ~ "La Lancière",
      TRUE ~ antenne
    ),
    
    site_lache_nom = factor(site_lache_nom, levels = c("Lancière Aval", "Dalot Aval", "City Stade",  "Ronce Aval")),
    antenne_nom = factor(antenne_nom, levels = c( "La Lancière",  "Milleron", "Dalot", "La Ronce amont", "La Ronce aval"))
  ) %>%
  
  group_by(site_lache_nom) %>%
  arrange(antenne_nom, .by_group = TRUE) %>%
  
  ungroup() %>%
  
  select(site_lache_nom, antenne_nom, N_rel, N_pass, prop_pct)

# Préparation des séparations
sep_lines <- which(taux_cumulatif_site_clean$site_lache_nom !=
                     lag(taux_cumulatif_site_clean$site_lache_nom))
sep_lines <- sep_lines[!is.na(sep_lines)]

# Création du tableau
tableau_treated <- taux_cumulatif_site_clean %>%
  kbl(
    format = "html",
    escape = FALSE,
    align = c("l", "l", "c", "c", "c"),
    col.names = c("Site de lâcher", "Antenne",
                  "Poissons relâchés", "Poissons détectés",
                  "Taux (%)"),
    caption = "<b style='color:black; font-size:13pt;'>Taux cumulés des franchissements piscicoles</b><br>") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center") %>%
  collapse_rows(columns = 1, valign = "middle") %>%
  row_spec(0, bold = TRUE, color = "black", background = "snow3")

for (i in sep_lines) {
  tableau_treated <- tableau_treated %>%
    row_spec(i, extra_css = "border-top: 1.5px solid #555;")
}
