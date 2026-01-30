library(leaflet)
library(sf)
library(leaflet.extras)

data <- read.csv("data/coord.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# PARTIE 1 : Pop-up HTML avec des infos sur l'académicien
date_naiss <- format(as.Date(data$naissance), "%d/%m/%Y")
date_mort  <- format(as.Date(data$mort), "%d/%m/%Y")

content <- paste(sep = "<br/>",
  paste0("<div class='leaflet-popup-scrolled' style='max-width:250px;max-height:200px'>"),
  paste0("<b><big>", enc2utf8(as.character(data$name)), "</big></b>"),
  paste0("Occupation : Historien des sciences et ", enc2utf8(as.character(data$ocupation)), "."), # Strings texte pré-régigé dans le tableur
  paste0("Citoyenneté : ", enc2utf8(as.character(data$nation))),
  paste0("Né le : ", date_naiss),
  paste0("Décédé le : ", date_mort),
  paste0("Membre numéro : ", data$e),
  # liens Wikipédia avec un controle au cas ou il manque
  ifelse(!is.na(data$WP) & data$WP != "", paste0("<a href='", data$WP, "' target='_blank'>Consulter la page Wikipédia</a>"), ""),
  # Images en ligne
  ifelse(!is.na(data$link) & data$link != "", paste0("<img src='", data$link, "' style='height: 100%; width: 100%; object-fit: contain; margin-top: 10px;'/>"), ""),
  paste0("</div>")
)

# PARTIE 2 : Carte
url_icon <- "https://cdn.iconscout.com/icon/free/png-512/free-academy-icon-svg-download-png-444459.png?f=webp&w=512"
aihs_icon <- makeIcon(url_icon, url_icon, 40, 40)

map <- leaflet(data) %>%
  addProviderTiles(providers$Stadia.StamenToner) %>%
  addResetMapButton() %>%
  addMiniMap() %>%
  leafem::addMouseCoordinates() %>%
  setView(lng = 10, lat = 45, zoom = 2) %>%
  # Marqueurs des académiciens
  addMarkers(
    lng = ~lng, 
    lat = ~lat, 
    icon = aihs_icon,
    popup = content,
    clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE),
    group = "Académiciens"
  ) %>%   # <--- IL MANQUAIT CE PIPE ICI POUR LIER LA SUITE
  
  # Légende avec crédit
  addLegend(
    "bottomleft", 
    colors = c("transparent"), 
    labels = c("xandru.garcia@gmail.com"), 
    title = "AIHS- CAPHÉS (29 Rue d'Ulm)"
  ) # <--- PAS DE PIPE ICI (c'est la fin)

# Affichage
map