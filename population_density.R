# Load packages
packages <- c("sf", "dplyr", "leaflet", "htmlwidgets")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

library(sf)
library(leaflet)
library(dplyr)
library(htmlwidgets)

# Read shapefile
pop_density_germany <- st_read("pop_density_germany.shp")

# Categorize population density
pop_density_germany$pop_density_category <- cut(
  pop_density_germany$pop_dn,
  breaks = c(0, 50, 150, 500, 2000, 5000),
  labels = c("0-50", "50-150", "150-500", "500-2000", "2000-5000"),
  include.lowest = TRUE,
  right = FALSE
)

# Color palette
pop_density_colors <- colorFactor(
  palette = "YlOrRd",
  domain = pop_density_germany$pop_density_category
)

# Create the map
population_density_map <- leaflet(pop_density_germany) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 10.5, lat = 51.2, zoom = 6) %>%
  addPolygons(
    color = NA,
    weight = 0,
    smoothFactor = 0.5,
    opacity = 1,
    fillOpacity = 0.5,
    fillColor = ~pop_density_colors(pop_density_category),
    popup = ~paste0(
      "<b>District:</b> ", GEN, "<br/>",
      "<b>Population Density:</b> ", round(pop_dn, 1), " per km²"
    )
  ) %>%
  addLegend(
    pal = pop_density_colors,
    values = ~pop_density_category,
    title = "Inhabitants per sqkm",
    position = "bottomright"
  )

population_density_map

# Save the map
saveWidget(population_density_map, "www/population_density.html", selfcontained = TRUE)
