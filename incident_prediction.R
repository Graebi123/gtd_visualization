# Load packages
packages <- c("sf", "dplyr", "readr", "lubridate", "leaflet", "randomForest", "tidyr", "htmlwidgets", "Metrics", "caret", "plotly")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

library(sf)
library(dplyr)
library(readr)
library(lubridate)
library(randomForest)
library(tidyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(Metrics)
library(caret)
library(plotly)

# Set seed for random forest reproducibility
set.seed(10)

# Load GTD Data
gtd <- read.delim("gtd_0522_germany.csv", sep = ";", dec = ",")
gtd$date <- as.Date(gtd$date, tryFormats = c("%d.%m.%Y", "%Y-%m-%d"))
gtd$year <- year(gtd$date)
gtd <- gtd %>% filter(!is.na(latitude), !is.na(longitude), !is.na(year))

# Load Shapefile (Bundesländer)
bundeslaender <- st_read("VG250_LAN.shp") %>%
  mutate(GEN = case_when(
    GEN == "Baden-Württemberg (Bodensee)" ~ "Baden-Württemberg",
    GEN == "Bayern (Bodensee)" ~ "Bayern",
    TRUE ~ GEN
  )) %>%
  group_by(GEN) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_transform(crs = 4326)

# Merge multiple geometries per Bundesland into one unified polygon
bundeslaender <- bundeslaender %>%
  group_by(GEN) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Convert GTD to sf and Join with Shapefile
gtd_sf <- st_as_sf(gtd, coords = c("longitude", "latitude"), crs = 4326)
gtd_sf <- st_transform(gtd_sf, st_crs(bundeslaender))
gtd_joined <- st_join(gtd_sf, bundeslaender, join = st_within)





#### RANDOM FOREST PREDICTION MODEL ####

# Generate bin-based features
generate_bin_features <- function(bin_size = 10) {
  
  # Define the range of years to consider
  years <- 1970:2015
  
  # Split the years into bins of specified size (e.g., 10-year bins)
  bins <- split(years, ceiling(seq_along(years) / bin_size))
  
  # Start with a base feature table containing unique Bundesland names (GEN), no geometry
  features_base <- bundeslaender %>%
    st_drop_geometry() %>%
    select(GEN) %>%
    distinct()
  
  # Loop through each bin of years to calculate weighted attack counts
  for (i in seq_along(bins)) {
    y_bin <- bins[[i]]  # Get the current year bin
    name <- paste0("attacks_", min(y_bin), "_", max(y_bin))  # Create dynamic column name
    
    weight <- i + 1  # Assign a log-weight to the bin (e.g., later bins weighted higher)
    
    # Filter GTD data for this bin and count attacks per Bundesland, applying weight
    feature <- gtd_joined %>%
      filter(year %in% y_bin) %>%
      st_drop_geometry() %>%
      group_by(GEN) %>%
      summarise(!!name := n() * log(weight))  # Use log to soften weight increase
    
    # Join this bin's feature to the main features_base table
    features_base <- left_join(features_base, feature, by = "GEN")
  }
  
  # Replace any NAs (i.e., no attacks in that bin) with 0
  features_base[is.na(features_base)] <- 0
  
  # Return the final feature table with bin-based attack features
  return(features_base)
}


# Step 2: Add population and density
prepare_population_features <- function(features_base) {
  # Load population development data and clean it
  pop_dev <- read.delim("population_development_bundeslaender.csv", sep = ";", dec = ",") %>%
    rename(GEN = Bundesland) %>%                                      # Rename for consistency with spatial data
    mutate(GEN = trimws(GEN)) %>%                                     # Trim whitespace from Bundesland names
    mutate(across(starts_with("X"), ~as.numeric(gsub(",", "", .x))))  # Convert population columns to numeric
  
  # Helper function to calculate average population for a decade
  get_density_bin <- function(years, name) {
    cols <- paste0("X31.12.", years)  # Construct column names for the given years
    pop_dev %>%
      select(GEN, all_of(cols)) %>%
      rowwise() %>%
      mutate(!!name := mean(c_across(all_of(cols)), na.rm = TRUE)) %>%  # Compute mean population
      ungroup() %>%
      select(GEN, !!name)  # Return only GEN and the computed mean column
  }
  
  # Join geometry to compute area per Bundesland (in km²)
  features_base <- left_join(features_base, select(bundeslaender, GEN, geometry), by = "GEN") %>%
    mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)  # Convert m² to km²
  
  # Join population data for each decade and calculate population density
  features_base <- features_base %>%
    left_join(get_density_bin(1970:1979, "pop_density_70s"), by = "GEN") %>%
    left_join(get_density_bin(1980:1989, "pop_density_80s"), by = "GEN") %>%
    left_join(get_density_bin(1990:1999, "pop_density_90s"), by = "GEN") %>%
    left_join(get_density_bin(2000:2009, "pop_density_00s"), by = "GEN") %>%
    left_join(get_density_bin(2010:2015, "pop_density_10s"), by = "GEN") %>%
    mutate(across(starts_with("pop_density_"), ~ . / area_km2))  # Convert population to density (people/km²)
  
  # Remove geometry and area columns, and replace any NA values with 0
  features_base <- features_base %>%
    select(-geometry, -area_km2) %>%
    replace(is.na(.), 0)
  
  return(features_base)
}


# Tune lag weights and amount of years for each bin size
tune_model_bin <- function(pred_year, features_base, grid) {
  
  # Helper function to prepare the training data with given lag lengths and weights
  prepare_training_data <- function(n1, n3, w1, w3) {
    
    # Create lag-1 feature: number of attacks in the last n1 years
    lag_1 <- gtd_joined %>%
      filter(year >= pred_year - n1 & year < pred_year) %>%
      st_drop_geometry() %>%
      group_by(GEN) %>%
      summarise(attacks_lag1 = n())
    
    # Create lag-3 feature: number of attacks in the last n3 years
    lag_3 <- gtd_joined %>%
      filter(year >= pred_year - n3 & year < pred_year) %>%
      st_drop_geometry() %>%
      group_by(GEN) %>%
      summarise(attacks_lag3 = n())
    
    # Target variable: actual number of attacks in the prediction year
    target <- gtd_joined %>%
      filter(year == pred_year) %>%
      st_drop_geometry() %>%
      group_by(GEN) %>%
      summarise(actual_attacks = n())
    
    # Combine features and apply weights to lag variables
    features <- features_base %>%
      left_join(lag_1, by = "GEN") %>%
      left_join(lag_3, by = "GEN") %>%
      left_join(target, by = "GEN") %>%
      replace(is.na(.), 0) %>%
      mutate(
        attacks_lag1 = attacks_lag1 * w1,
        attacks_lag3 = attacks_lag3 * w3
      )
    
    return(features)
  }
  
  results <- list()
  
  # Loop through all parameter combinations in the tuning grid
  for (i in 1:nrow(grid)) {
    row <- grid[i, ]
    
    # Prepare training data for current parameter set
    data <- prepare_training_data(row$n1, row$n3, row$w1, row$w3)
    train_data <- data %>% select(-GEN)  # Drop GEN column for modeling
    
    # Set up 5-fold cross-validation
    control <- trainControl(method = "cv", number = 5)
    
    # Set seed for random forest reproducibility
    set.seed(10)
    
    # Train Random Forest model
    rf_fit <- train(
      actual_attacks ~ ., data = train_data,
      method = "rf",
      trControl = control,
      ntree = 500
    )
    
    # Store performance metrics for this configuration
    results[[i]] <- list(
      year = pred_year,
      n1 = row$n1, w1 = row$w1,
      n3 = row$n3, w3 = row$w3,
      R2 = max(rf_fit$results$Rsquared),  # Best R² from cross-validation
      RMSE = min(rf_fit$results$RMSE),    # Best RMSE from cross-validation
      MAE = mean(abs(predict(rf_fit, train_data) - train_data$actual_attacks))  # MAE on training set
    )
  }
  
  # Custom scoring function to balance R² and RMSE
  score_fn <- function(x) x$R2 - 0.1 * x$RMSE
  
  # Select the best configuration according to the scoring function
  best <- results[[which.max(sapply(results, score_fn))]]
  
  return(best)
}


# Run tuning for 5-year and 10-year bins

# Create a grid of parameter combinations for lag lengths and weights
grid <- expand.grid(
  n1 = 1:3,  # Short-term lags: 1 to 3 years
  w1 = 1:5,  # Weight for short-term lag
  n3 = 3:5,  # Long-term lags: 3 to 5 years
  w3 = 1:3   # Weight for long-term lag
)

# Define bin sizes to test (5-year bins and 10-year bins)
bin_sizes <- c(10, 5)

# Initialize list to collect tuning results for each bin size
all_results <- list()

# Loop through each bin size and tune the model
for (bin_size in bin_sizes) {
  # Generate bin-based attack features for current bin size
  fb <- generate_bin_features(bin_size)
  
  # Add population and density features
  fb <- prepare_population_features(fb)
  
  # Tune model for each prediction year (2016 to 2020)
  res <- lapply(2016:2020, function(y) tune_model_bin(y, fb, grid))
  
  # Combine results into a single data frame
  df <- do.call(rbind, res)
  df <- as.data.frame(df)
  
  # Add the current bin size as a column
  df$bin_size <- bin_size
  
  # Store results in list keyed by bin size
  all_results[[as.character(bin_size)]] <- df
}

# Combine all results across bin sizes into one final data frame
final_results <- do.call(rbind, all_results)

# Print the full table of tuning results
print(final_results)


# Find best global parameters

# Force R2 to be numeric
final_results$R2 <- as.numeric(final_results$R2)

# 1. Find best bin size based on highest average R² across years
best_bin <- final_results %>%
  group_by(bin_size) %>%
  summarise(avg_r2 = mean(R2)) %>%     # Calculate average R² per bin size
  arrange(desc(avg_r2)) %>%            # Sort by R² descending
  slice(1) %>%                         # Select top-performing bin size
  pull(bin_size)                       # Extract bin size as scalar

# 2. Among all runs for the best bin size, find the most frequently selected lag/weight combination
best_combo <- final_results %>%
  filter(bin_size == best_bin) %>%
  count(n1, w1, n3, w3, sort = TRUE) %>%  # Count frequency of each lag/weight combination
  slice(1)                                # Pick the most frequent one

# 3. Extract lag and weight values as simple scalars (to avoid list indexing issues)
best_n1 <- best_combo$n1[[1]]
best_w1 <- best_combo$w1[[1]]
best_n3 <- best_combo$n3[[1]]
best_w3 <- best_combo$w3[[1]]

# 4. Output final selected model configuration to the console
cat("\n=== FINAL MODEL SETTINGS ===\n")
cat("Bin size used:", best_bin, "\n")
cat("Lag-1 → Years:", best_n1, ", Weight:", best_w1, "\n")
cat("Lag-3 → Years:", best_n3, ", Weight:", best_w3, "\n")
cat("============================\n\n")

# Generate maps and summary using best settings

# Generate base features using best bin size found in tuning
features_base <- generate_bin_features(bin_size = best_bin)
features_base <- prepare_population_features(features_base)

# Function to predict and visualize terrorist attack counts for a given year
predict_and_plot_year <- function(pred_year, features_base, w1, w3, n1, n3) {
  
  # --- Prepare lag-based features (short-term and long-term) ---
  
  # Count attacks in the short-term lag window (n1 years before pred_year)
  lag_1 <- gtd_joined %>%
    filter(year >= pred_year - n1 & year < pred_year) %>%
    st_drop_geometry() %>%
    group_by(GEN) %>%
    summarise(attacks_lag1 = n())
  
  # Count attacks in the long-term lag window (n3 years before pred_year)
  lag_3 <- gtd_joined %>%
    filter(year >= pred_year - n3 & year < pred_year) %>%
    st_drop_geometry() %>%
    group_by(GEN) %>%
    summarise(attacks_lag3 = n())
  
  # Actual number of attacks in the prediction year
  target <- gtd_joined %>%
    filter(year == pred_year) %>%
    st_drop_geometry() %>%
    group_by(GEN) %>%
    summarise(actual_attacks = n())
  
  # Merge features with lags and target, applying weights
  features <- features_base %>%
    left_join(lag_1, by = "GEN") %>%
    left_join(lag_3, by = "GEN") %>%
    left_join(target, by = "GEN") %>%
    replace(is.na(.), 0) %>%
    mutate(
      attacks_lag1 = attacks_lag1 * w1,  # Apply weight to short-term lag
      attacks_lag3 = attacks_lag3 * w3   # Apply weight to long-term lag
    )
  
  # Train Random Forest model and make predictions
  
  train_data <- features %>% select(-GEN)  # Drop region names for modeling
  set.seed(42)
  rf_model <- randomForest(actual_attacks ~ ., data = train_data, ntree = 500)
  preds <- predict(rf_model, newdata = train_data)
  
  # Calculate performance metrics
  actual <- train_data$actual_attacks
  r_squared <- 1 - sum((actual - preds)^2) / sum((actual - mean(actual))^2)
  rmse_val <- sqrt(mean((actual - preds)^2))
  mae_val <- mean(abs(actual - preds))
  
  # Prepare map visualization data
  
  results <- features %>%
    select(GEN, actual_attacks, attacks_lag1, attacks_lag3) %>%
    mutate(predicted_attacks = round(preds)) %>%
    left_join(select(bundeslaender, GEN, geometry), by = "GEN") %>%
    st_as_sf()
  
  # Define error categories for color coding
  error_levels <- c("Perfect (0)", "Off by 1", "Off by 2", "Off by 3", "Off by ≥ 4")
  
  results <- results %>%
    mutate(
      abs_error = abs(predicted_attacks - actual_attacks),
      error_category = case_when(
        abs_error == 0 ~ "Perfect (0)",
        abs_error == 1 ~ "Off by 1",
        abs_error == 2 ~ "Off by 2",
        abs_error == 3 ~ "Off by 3",
        abs_error >= 4 ~ "Off by ≥ 4"
      ),
      error_category = factor(error_category, levels = error_levels)  # Keep legend order consistent
    )
  
  # Define color palette for the map
  color_palette <- colorFactor(
    palette = c("darkgreen", "lightgreen", "yellow", "orange", "red"),
    levels = error_levels
  )
  
  # Create popup content for interactivity
  results$popup <- paste0(
    "<strong>", results$GEN, "</strong><br/>",
    "Predicted Attacks: ", round(results$predicted_attacks, 2), "<br/>",
    "Actual Attacks: ", results$actual_attacks, "<br/>"
  )
  
  # Get centroids for each state
  label_data <- results %>%
    st_centroid(of_largest_polygon = TRUE) %>%
    mutate(label_text = paste0(predicted_attacks, " / ", actual_attacks))
  
  # Adjust Brandenburg label position by shifting its centroid southward
  label_data$geometry[label_data$GEN == "Brandenburg"] <- 
    label_data$geometry[label_data$GEN == "Brandenburg"] + c(0, -0.3)
  
  # Define color palette
  color_palette <- colorFactor(
    palette = c("darkgreen", "lightgreen", "yellow", "orange", "red"),
    levels = c("Perfect (0)", "Off by 1", "Off by 2", "Off by 3", "Off by ≥ 4")
  )
  
  # Build the interactive map
  map <- leaflet(results, options = leafletOptions(
    dragging = FALSE,
    zoomControl = FALSE,
    scrollWheelZoom = FALSE,
    doubleClickZoom = FALSE,
    touchZoom = FALSE,
    minZoom = 6,
    maxZoom = 6
  )) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = 10.5, lat = 51.2, zoom = 6) %>%
    
    # Add state polygons with colored fill and popups
    addPolygons(
      fillColor = ~color_palette(error_category),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      popup = ~popup
    ) %>%
    
    # Add properly centered labels [Predicted / Actual]
    addLabelOnlyMarkers(
      data = label_data,
      label = ~label_text,
      labelOptions = labelOptions(
        noHide = TRUE,
        direction = "center",
        textOnly = TRUE,
        style = list(
          "color" = "black",
          "font-weight" = "bold",
          "font-size" = "12px",
          "text-shadow" = "1px 1px 2px white"
        )
      )
    ) %>%
    
    # Add legend for error categories
    addLegend(
      pal = color_palette,
      values = factor(levels(results$error_category), levels = levels(results$error_category)),  # Show all levels
      title = paste("Prediction Error (", pred_year, ")", sep = ""),
      position = "bottomright",
      na.label = ""
    ) %>%
    
    # Add year as a title
    addControl(
      html = paste0("<div style='font-size:18px; font-weight:bold;'>Year: ", pred_year, "</div>"),
      position = "topleft"
    )
  
  # Return both the map and model performance metrics for the year
  return(list(
    year = pred_year,
    map = map,
    R2 = r_squared,
    RMSE = rmse_val,
    MAE = mae_val
  ))
}

# Generate maps and summary using best settings
features_base <- generate_bin_features(bin_size = best_bin)
features_base <- prepare_population_features(features_base)

# Predict and generate maps for each year
final_maps <- lapply(2016:2020, function(y) {
  predict_and_plot_year(y, features_base, best_w1, best_w3, best_n1, best_n3)
})

# Save each map to an HTML file
lapply(final_maps, function(res) {
  file_name <- paste0("www/prediction_map_", res$year, ".html")
  saveWidget(res$map, file = file_name, selfcontained = TRUE)
})

# Print maps
for (res in final_maps) {
  print(res$map)
}

# Final summary table
final_summary <- do.call(rbind, lapply(final_maps, function(x) {
  data.frame(Year = x$year, R2 = x$R2, RMSE = x$RMSE, MAE = x$MAE)
}))
print(final_summary)





#### CREATE PLOT FOR INCIDENTS 2013-2019 ####

# Prepare complete year range
all_years <- 2013:2019

# Filter + count + complete missing years
gtd_filtered <- gtd %>%
  filter(year(date) %in% all_years) %>%
  count(year = year(date)) %>%
  complete(year = all_years, fill = list(n = 0))  # Fill missing years with 0

# Factor to preserve year order and format labels as "'13", "'14", ...
gtd_filtered$year <- factor(gtd_filtered$year, levels = all_years, labels = paste0("'", substr(all_years, 3, 4)))

# ggplot with custom hover text
plot_incidents <- ggplot(gtd_filtered, aes(x = year, y = n)) +
  geom_col(aes(text = paste0("Incidents: ", n)), fill = "grey") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Convert to plotly
plotly_incidents <- ggplotly(plot_incidents, tooltip = "text")

plotly_incidents

# Save as HTML
saveWidget(plotly_incidents, file = "www/incidents_2013-2019.html", selfcontained = TRUE)





#### CREATE TABLE FOR ACCURACY SCORES ####

# FINAL PARAMETERS USED:
# Bin size : 10
# Lag 1    : years = 1, weight = 1
# Lag 2    : years = 4, weight = 1


# Accuracy metrics matrix
metrics2 <- matrix(c(
  0.7614204, 1.6583695, 1.1446312,
  0.8642207, 0.9072827, 0.6934458,
  0.8890469, 0.8327406, 0.5770125,
  0.8091214, 0.5875558, 0.4497979,
  0.8868355, 1.1186778, 0.6608917
), ncol = 3, byrow = TRUE)

# Add column and row names
colnames(metrics2) <- c("R2", "RMSE", "MAE")
rownames(metrics2) <- c("2016", "2017", "2018", "2019", "2020")

# Add average row
avg_row <- colMeans(metrics2)
metrics2_with_avg <- rbind(metrics2, Average = avg_row)

# View the matrix
metrics2_with_avg




#### BASELINE EVALUATION MODEL (IS RF BETTER THAN LAG1 OR MEAN?) ####

# Baseline 1: Predict mean attack count for all Bundesländer
evaluate_baseline_mean <- function(pred_year) {
  actual <- gtd_joined %>%
    filter(year == pred_year) %>%
    st_drop_geometry() %>%
    group_by(GEN) %>%
    summarise(actual_attacks = n())
  
  all_states <- bundeslaender %>% st_drop_geometry() %>% select(GEN)
  actual <- left_join(all_states, actual, by = "GEN") %>%
    replace(is.na(.), 0)
  
  pred_mean <- mean(actual$actual_attacks)
  preds <- rep(pred_mean, nrow(actual))
  
  r2 <- 1 - sum((actual$actual_attacks - preds)^2) / sum((actual$actual_attacks - mean(actual$actual_attacks))^2)
  rmse <- sqrt(mean((actual$actual_attacks - preds)^2))
  mae <- mean(abs(actual$actual_attacks - preds))
  
  return(data.frame(Year = pred_year, Model = "Mean Guess", R2 = r2, RMSE = rmse, MAE = mae))
}

# Baseline 2: Use previous year's attacks as prediction
evaluate_baseline_lag1 <- function(pred_year) {
  lag <- gtd_joined %>%
    filter(year == pred_year - 1) %>%
    st_drop_geometry() %>%
    group_by(GEN) %>%
    summarise(predicted_attacks = n())
  
  actual <- gtd_joined %>%
    filter(year == pred_year) %>%
    st_drop_geometry() %>%
    group_by(GEN) %>%
    summarise(actual_attacks = n())
  
  all_states <- bundeslaender %>% st_drop_geometry() %>% select(GEN)
  merged <- left_join(all_states, lag, by = "GEN") %>%
    left_join(actual, by = "GEN") %>%
    replace(is.na(.), 0)
  
  r2 <- 1 - sum((merged$actual_attacks - merged$predicted_attacks)^2) / 
    sum((merged$actual_attacks - mean(merged$actual_attacks))^2)
  rmse <- sqrt(mean((merged$actual_attacks - merged$predicted_attacks)^2))
  mae <- mean(abs(merged$actual_attacks - merged$predicted_attacks))
  
  return(data.frame(Year = pred_year, Model = "Lag-1 Guess", R2 = r2, RMSE = rmse, MAE = mae))
}

# Run Evaluation

years <- 2016:2020

# Baselines
baseline_results <- lapply(years, function(y) {
  rbind(
    evaluate_baseline_mean(y),
    evaluate_baseline_lag1(y)
  )
})
baseline_results_df <- do.call(rbind, baseline_results)

# Results from final_maps
final_summary$Model <- "Random Forest"

# Combine all results
all_models_comparison <- rbind(final_summary, baseline_results_df)

# Arrange and print nicely
all_models_comparison <- all_models_comparison %>%
  arrange(Year, factor(Model, levels = c("Random Forest", "Lag-1 Guess", "Mean Guess")))

print(all_models_comparison)

# Reorder and round numeric columns
comparison_matrix <- all_models_comparison %>%
  arrange(Year, factor(Model, levels = c("Random Forest", "Lag-1 Guess", "Mean Guess"))) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  select(Year, Model, R2, RMSE, MAE)

# Convert to wide matrix: one row per Year & Model
comparison_matrix_wide <- matrix(
  c(
    # 2016
    0.761, 1.658, 1.145,
    0.257, 2.926, 2.188,
    0.000, 3.395, 2.313,
    # 2017
    0.864, 0.907, 0.693,
    -0.577, 3.092, 2.188,
    0.000, 2.462, 2.000,
    # 2018
    0.889, 0.833, 0.577,
    0.220, 2.208, 1.500,
    0.000, 2.500, 1.875,
    # 2019
    0.809, 0.588, 0.450,
    -0.832, 1.820, 1.063,
    0.000, 1.345, 1.094,
    # 2020
    0.887, 1.119, 0.661,
    0.152, 3.062, 1.625,
    0.000, 3.325, 1.969
  ),
  ncol = 3, byrow = TRUE
)

# Add column names and row names
colnames(comparison_matrix_wide) <- c("R2", "RMSE", "MAE")
rownames(comparison_matrix_wide) <- c(
  "2016_RF", "2016_Lag1", "2016_Mean",
  "2017_RF", "2017_Lag1", "2017_Mean",
  "2018_RF", "2018_Lag1", "2018_Mean",
  "2019_RF", "2019_Lag1", "2019_Mean",
  "2020_RF", "2020_Lag1", "2020_Mean"
)

# View matrix
comparison_matrix_wide

# Save comparison matrix for use in R Shiny
saveRDS(all_models_comparison, "all_models_comparison.rds")
