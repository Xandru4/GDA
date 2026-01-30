library(plotly)
library(dplyr)
library(lubridate)

# 1. Chargement et préparation des dates
df <- read.csv("data/plot.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

age_evolution <- df %>%
  # FILTRE : On ignore les rangées sans e.date ou avec e.date vide
  filter(!is.na(e.date) & e.date != "") %>% 
  
  mutate(
    birth = as.Date(naissance),
    election = as.Date(e.date),
    death = as.Date(mort)
  ) %>%
  # On s'assure aussi que la conversion en date a fonctionné et qu'on a la naissance
  filter(!is.na(election) & !is.na(birth))

# 2. Création d'une séquence d'années (ex: tous les 5 ans)
years_to_check <- seq(1928, 2024, by = 5)

# 3. Calcul de l'âge des membres présents pour chaque année
boxplot_data <- do.call(rbind, lapply(years_to_check, function(y) {
  date_ref <- as.Date(paste0(y, "-01-01"))
  
  age_evolution %>%
    # Membre élu avant cette année ET (toujours en vie OU décédé après cette année)
    filter(election <= date_ref) %>%
    filter(is.na(death) | death >= date_ref) %>%
    mutate(
      annee_ref = y,
      age = as.numeric(difftime(date_ref, birth, units = "days")) / 365.25
    ) %>%
    select(annee_ref, age)
}))

# 4. Génération du Box Diagram interactif
fig_age <- plot_ly(boxplot_data, 
                   x = ~annee_ref, 
                   y = ~age, 
                   type = "box", 
                   boxpoints = "outliers", 
                   marker = list(color = '#2c3e50'),
                   line = list(color = '#2c3e50'),
                   fillcolor = 'rgba(52, 152, 219, 0.6)') %>%
  layout(
    title = "Évolution de l'âge des membres de l'Académie",
    xaxis = list(title = "Année", tickmode = "array", tickvals = years_to_check),
    yaxis = list(title = "Âge"),
    hovermode = "closest"
  )

fig_age