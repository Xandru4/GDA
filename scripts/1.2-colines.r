library(ggridges)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)

cronologia <- read.csv("data/specialites.csv", fileEncoding = "UTF-8")
df_plot <- read.csv("data/plot.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# Préparation pour le graphique de spécialités
cronologia$annee <- as.numeric(substr(cronologia$naissance, 1, 4))

# Préparation pour le graphique d'âge
age_data <- df_plot %>%
  filter(!is.na(e.date) & e.date != "") %>% 
  mutate(
    birth = as.Date(naissance),
    election = as.Date(e.date),
    death = as.Date(mort)
  ) %>%
  filter(!is.na(election) & !is.na(birth))

# I - Plot des spécialités
fig_spe <- ggplot(cronologia, aes(x = annee, 
                                  y = spe,    
                                  fill = spe)) + 
  geom_density_ridges(scale = 1.5, rel_min_height = 0.01) +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(title = "Évolution chronologique des spécialités",
       x = "Année de naissance",
       y = "Spécialité")

# II - Graphique des ages
years_to_check <- seq(1928, 2024, by = 5)

boxplot_df <- do.call(rbind, lapply(years_to_check, function(y) {
  date_ref <- as.Date(paste0(y, "-01-01"))
  age_data %>%
    filter(election <= date_ref) %>%
    filter(is.na(death) | death >= date_ref) %>%
    mutate(
      annee_ref = y,
      age = as.numeric(difftime(date_ref, birth, units = "days")) / 365.25
    ) %>%
    select(annee_ref, age)
}))

fig_age <- plot_ly(boxplot_df, x = ~annee_ref, y = ~age, type = "box", 
                   name = "Distribution des âges",
                   boxpoints = "outliers",
                   fillcolor = 'rgba(52, 152, 219, 0.6)') %>%
  layout(title = "Évolution de l'âge des membres au fil du temps",
         xaxis = list(title = "Année"),
         yaxis = list(title = "Âge"))

# Affichages
print(fig_spe)
fig_age