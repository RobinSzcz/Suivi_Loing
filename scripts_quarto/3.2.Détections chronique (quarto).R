# Package
library(tidyverse)
library(readr)
library(cowplot)
library(plotly)

# Chargement des données
detections_bio <- readRDS("../data_clean/detections_filter_bio.rds")
debit_amont <- read_csv("../débit/Débit Loing amont.csv")

# Préparation des données de débit
debit_amont <- debit_amont %>%
  mutate(`Date (TU)` = as.Date(`Date (TU)`)) %>%
  select(`Date (TU)`, `Valeur (en m³/s)`) %>%
  rename(date = `Date (TU)`, valeur = `Valeur (en m³/s)`) %>%
  group_by(date) %>%
  summarise(valeur = mean(valeur, na.rm = TRUE), .groups = "drop")

# Préparation des données de détections
ordre <- c(
  "LA_RONCE_AVAL"   = "Ronce aval",
  "LA_RONCE_AMONT"  = "Ronce amont",
  "MILLERON"        = "Milleron",
  "DALOT"           = "Dalot",
  "LANCIERE"        = "La Lancière")

couleurs_antennes <- c(
  "Ronce aval"  = "#F26386FF",
  "Ronce amont" = "#F588AFFF",
  "Milleron"       = "#A4D984FF",
  "Dalot"          = "#FCBC52FF",
  "La Lancière"    = "#FD814EFF")


id_par_jour <- detections_bio %>%
  filter(!is.na(espece)) %>% 
  mutate(date = as.Date(date_heure)) %>%
  group_by(date, antenne) %>%
  summarise(n_id = n_distinct(id_tag))

# Variable : Nouveau id cumulé
  # Date première détection
premiere_detection <- detections_bio %>%
  filter(!is.na(espece)) %>%
  mutate(date = as.Date(date_heure)) %>%
  group_by(id_tag, antenne) %>%
  summarise(date_premiere = min(date), .groups = "drop")
  # Nombre de nouveau id détéctés par jour
new_id_par_jour <- premiere_detection %>%
  group_by(date_premiere, antenne) %>%
  summarise(new_id = n(), .groups = "drop") %>%
  rename(date = date_premiere)
  # Nombre total id unique par jour et par antenne
id_par_jour <- detections_bio %>%
  filter(!is.na(espece)) %>%
  mutate(date = as.Date(date_heure)) %>%
  group_by(date, antenne) %>%
  summarise(n_id = n_distinct(id_tag), .groups = "drop") 

  # Jointure + cumul des id
id_par_jour <- id_par_jour %>%
  left_join(new_id_par_jour, by = c("date", "antenne")) %>%
  mutate(new_id = replace_na(new_id, 0)) %>%
  arrange(antenne, date) %>%
  group_by(antenne) %>%
  mutate(new_id_cumule = cumsum(new_id)) %>%
  ungroup()

  # Modifier le nom des antennes 
id_par_jour <- id_par_jour %>%
  mutate(
    antenne = recode(antenne, !!!ordre),
    antenne = factor(antenne, levels = names(couleurs_antennes)))


# Préparation finale : débit + détections
id_plot <- id_par_jour %>%
  left_join(debit_amont, by = "date") %>%
  mutate(antenne = factor(antenne, levels = names(couleurs_antennes)))


# Graphe

# --- Création des traces antenne ---
antennes <- levels(id_plot$antenne)

traces <- list()

for (a in antennes) {
  
  df_ant <- id_plot %>% filter(antenne == a)
  
  # bornes temporelles de l’antenne
  dmin <- min(df_ant$date)
  dmax <- max(df_ant$date)
  
  # débit restreint à la plage
  df_debit <- debit_amont %>%
    filter(date >= dmin & date <= dmax)
  
  # --- ID détectés
  traces <- append(traces, list(
    list(
      x = df_ant$date,
      y = df_ant$n_id,
      type = "bar",
      name = "ID détectés",
      marker = list(color = couleurs_antennes[a]),
      visible = (a == antennes[1])
    )
  ))
  
  # --- Nouveaux ID
  traces <- append(traces, list(
    list(
      x = df_ant$date,
      y = df_ant$new_id,
      type = "bar",
      name = "Nouveaux ID",
      marker = list(color = "black"),
      visible = (a == antennes[1])
    )
  ))
  
  # --- Débit
  traces <- append(traces, list(
    list(
      x = df_debit$date,
      y = df_debit$valeur,
      type = "scatter",
      mode = "lines",
      name = "",
      yaxis = "y2",
      line = list(color = "white", width = 10),
      hoverinfo = "skip",
      showlegend = FALSE,
      visible = (a == antennes[1])
    )
  ))
  
  # --- Débit (ligne principale)
  traces <- append(traces, list(
    list(
      x = df_debit$date,
      y = df_debit$valeur,
      type = "scatter",
      mode = "lines",
      name = "Débit",
      yaxis = "y2",
      line = list(color = "#1f77b4", width = 5),
      visible = (a == antennes[1])
    )
  ))
}

# Dropdown pour choisir l’antenne
buttons <- lapply(seq_along(antennes), function(i) {
  
  vis <- rep(FALSE, length(traces))
  vis[(4 * (i - 1) + 1):(4 * i)] <- TRUE
  
  list(
    method = "update",
    args = list(list(visible = vis)),
    label = antennes[i]
  )
})

# Création du graphique Plotly final
p_final <- plot_ly()

# Ajout des traces
for (tr in traces) {
  p_final <- add_trace(
    p_final,
    x = tr$x,
    y = tr$y,
    type = tr$type,
    mode = tr$mode %||% NULL,
    name = tr$name,
    marker = tr$marker %||% NULL,
    line = tr$line %||% NULL,
    yaxis = tr$yaxis %||% "y",
    visible = tr$visible
  )
}

# Mise en page
p_final <- layout(
  p_final,
  title = "Évolution quotidienne des individus détectés et du débit",
  barmode = "overlay",
  
  xaxis = list(
    type = "date",
    rangeselector = list(
      buttons = list(
        list(count = 7, label = "1w", step = "day", stepmode = "backward"),
        list(count = 1, label = "1m", step = "month", stepmode = "backward"),
        list(count = 1, label = "1y", step = "year", stepmode = "backward"),
        list(step = "all", label = "All")
      )
    )
  ),
  
  yaxis = list(title = "Nombre d'ID"),
  
  yaxis2 = list(
    title = "Débit (m³/s)",
    overlaying = "y",
    side = "right",
    showgrid = FALSE
  ),
  
  updatemenus = list(
    list(
      type = "dropdown",
      active = 0,
      buttons = buttons,
      x = 0.02,
      y = 1.15,
      xanchor = "left",
      yanchor = "top"
    )
  ),
  
  legend = list(
    orientation = "h",
    x = 0.1,
    y = -0.25
  )
)

p_final