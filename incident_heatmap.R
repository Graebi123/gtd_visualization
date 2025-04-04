setwd("C:/Users/Lukas/iCloudDrive/HfP_TUM Bachelor+Master/Master/0 Master Thesis/R/gtd_visualization")
#setwd("/Users/lukas/Library/Mobile Documents/com~apple~CloudDocs/HfP_TUM Bachelor+Master/Master/0 Master Thesis/R/gtd_visualization")

# Load packages
packages <- c("readr", "leaflet", "sf", "dplyr", "htmlwidgets", "htmltools")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

library(readr)
library(leaflet)
library(sf)
library(dplyr)
library(htmlwidgets)
library(htmltools)

# Load the dataset
data <- read_delim("gtd_0522_germany.csv", delim = ";", locale = locale(decimal_mark = ",")) %>% 
  mutate(date = as.Date(date, format = "%d.%m.%Y")) %>%
  arrange(date)

# nwoundte was automatically read in as logical variable instead of numeric. Hence, values were changed to NA or TRUE. To prevent this, make sure nwoundte is read as numeric
data$nwoundte <- as.numeric(data$nwoundte)

# Load the shapefile for spatial visualization
pop_density_germany <- st_read("pop_density_germany.shp")

# Convert incident data to sf object using coordinates
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = st_crs(pop_density_germany))

# Spatial join: assign district attributes to each incident
joined_data <- st_join(data_sf, pop_density_germany, join = st_within)

# Count incidents per district
incident_counts <- joined_data %>%
  st_drop_geometry() %>%
  group_by(OBJID) %>%
  summarise(incident_count = n())

# Merge with district polygons
pop_density_with_counts <- pop_density_germany %>%
  left_join(incident_counts, by = "OBJID") %>%
  mutate(incident_count = ifelse(is.na(incident_count), 0, incident_count))

# Define custom white-to-dark-red palette with sqrt scaling
count_palette <- colorNumeric(
  palette = c("white", "darkorange", "red", "darkred"),
  domain = sqrt(pop_density_with_counts$incident_count)
)

# Build the map
incident_heatmap <- leaflet(pop_density_with_counts, height = "700px") %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 10.5, lat = 51.2, zoom = 6) %>%
  addPolygons(
    fillColor = ~count_palette(sqrt(incident_count)),
    opacity = 1,
    stroke = FALSE,
    fillOpacity = 0.5,
    popup = ~paste0(
      "<b>District:</b> ", GEN, "<br/>",
      "<b>Incidents:</b> ", incident_count, "<br/>",
      "<b>Population Density:</b> ", round(pop_dn, 1), " per km²"
    )
  ) %>%
  addLegend(
    pal = count_palette,
    values = sqrt(c(0, 250)),
    title = "Number of Incidents",
    position = "bottomright",
    labFormat = function(type, cuts) {
      c("0", "50", "100", "150", "200", "250")
    }
  )

# Save the leaflet map as a standalone HTML file
saveWidget(
  widget = incident_heatmap,
  file = "www/incident_heatmap.html",
  selfcontained = TRUE
)
