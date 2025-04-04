# Load packages under the condition that they have not been loaded before
packages <- c("shiny", "shinydashboard", "shinycssloaders", "dplyr", "readr", "tidyr", "leaflet", "plotly", "DT", "sf", "h3jsr", "randomForest", "lubridate")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# Initialize libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(readr)
library(tidyr)
library(leaflet)
library(plotly)
library(DT)
library(sf)
library(h3jsr)
library(randomForest)
library(lubridate)

# Load the dataset
data <- read_delim("gtd_0522_germany.csv", delim = ";", locale = locale(decimal_mark = ",")) %>% 
  mutate(date = as.Date(date, format = "%d.%m.%Y")) %>%
  arrange(date)

# nwoundte was automatically read in as logical variable instead of numeric. Hence, values were changed to NA or TRUE. To prevent this, make sure nwoundte is read as numeric
data$nwoundte <- as.numeric(data$nwoundte)

# Load the shapefile for spatial visualization
pop_density_germany <- st_read("pop_density_germany.shp")

# Define colors based on phenomenon_area as a named list
color_mapping <- c(
  "Right-Wing Extremism" = "chocolate",
  "Left-Wing Extremism" = "red",
  "Islamism" = "green",
  "Foreign Extremism" = "darkturquoise",
  "Uncategorized" = "darkgrey"
)

get_color <- function(phenomenon) {
  if (!is.null(color_mapping[[phenomenon]])) {
    return(color_mapping[[phenomenon]])
  } else {
    return("black")
  }
}

# Apply jitter to duplicate coordinates
apply_jitter <- function(df) {
  df <- df %>%
    mutate(latitude = round(latitude, 5), longitude = round(longitude, 5)) %>%
    group_by(latitude, longitude) %>%
    mutate(n = n()) %>%
    ungroup()
  
  df <- df %>%
    rowwise() %>%
    mutate(
      latitude = ifelse(n > 1, latitude + runif(1, -0.050, 0.050), latitude),
      longitude = ifelse(n > 1, longitude + runif(1, -0.050, 0.050), longitude)
    ) %>%
    ungroup() %>%
    select(-n)
  
  return(df)
}

data <- apply_jitter(data)

# Create cumulative data subsets (data up to the current year) for temporal data map
data <- data %>%
  mutate(year = as.integer(format(date, "%Y")))

# Full range of years for the slider
years <- min(data$year):max(data$year)

# Only years that exist in data
valid_data_years <- sort(unique(data$year))

# Precompute cumulative data for all valid data years
cumulative_by_valid_year <- lapply(valid_data_years, function(y) {
  data %>% filter(year <= y)
})
names(cumulative_by_valid_year) <- valid_data_years

# Create full cumulative list for slider years
yearly_data_cumulative <- list()

for (y in years) {
  # Get closest earlier year with data
  fallback_year <- max(valid_data_years[valid_data_years <= y], na.rm = TRUE)
  yearly_data_cumulative[[as.character(y)]] <- cumulative_by_valid_year[[as.character(fallback_year)]]
}
