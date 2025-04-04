server <- function(input, output, session) {
  
  #### POP-UP ####
  create_popup <- function(row) {
    
    # Define a named vector mapping variable names to custom descriptions
    var_descriptions <- c(
      "eventid" = "ID",
      "date" = "Date",
      "city" = "City",
      "gname" = "Perpetrator Group/Category Name",
      "nkill" = "Number of Victims Killed",
      "nwound" = "Number of Victims Wounded",
      "nkillter" = "Number of Perpetrators Killed",
      "nwoundte" = "Number of Perpetrators Wounded",
      "attacktype1_txt" = "Attack Type",
      "attacktype2_txt" = "Attack Type 2",
      "targtype1_txt" = "Target Type",
      "targtype2_txt" = "Target Type 2",
      "targtype3_txt" = "Target Type 3",
      "target1" = "Target",
      "target2" = "Target 2",
      "target3" = "Target 3",
      "corp1" = "Targeted Corporation",
      "corp2" = "Targeted Corporation 2",
      "corp3" = "Targeted Corporation 3",
      "weaptype1_txt" = "Weapon Type",
      "weapsubtype1_txt" = "Weapon Subtype",
      "weaptype2_txt" = "Weapon Type 2",
      "weapsubtype2_txt" = "Weapon Subtype 2",
      "weaptype3_txt" = "Weapon Type 3",
      "weapsubtype3_txt" = "Weapon Subtype 3",
      "weapdetail" = "Weapon Details"
    )
    
    # Define a custom display order
    display_order <- c(
      "eventid", "date", "city", "gname",
      "nkill", "nwound", "nkillter", "nwoundte",
      "attacktype1_txt", "attacktype2_txt",
      "targtype1_txt", "targtype2_txt", "targtype3_txt",
      "target1", "target2", "target3",
      "corp1", "corp2", "corp3",
      "weaptype1_txt", "weapsubtype1_txt", "weaptype2_txt", "weapsubtype2_txt", "weaptype3_txt", "weapsubtype3_txt", "weapdetail"
    )
    
    # Filter 'row' to keep only the variables that have corresponding descriptions in 'var_descriptions'
    selected_row <- row[names(row) %in% names(var_descriptions)]  
    
    # Remove NA values
    selected_row <- selected_row[!is.na(selected_row)]
  
    # Construct the popup content in the specified order
    content <- paste(
      sapply(display_order[display_order %in% names(selected_row)],
             function(var) paste0(var_descriptions[[var]], ": ", as.character(selected_row[[var]]))),
      collapse = "<br>"
    )
    
    # Add header for Incident Data
    content <- paste("<b>Incident Data:</b>", content)
    
    # Initialize an empty vector to store additional information in a specific order
    additional_info <- c()
    
    # Append additional information in a specific order
    if ("doubtterr" %in% names(row)) {
      if (as.numeric(row[["doubtterr"]]) == 1) {
        if ("alternative_txt" %in% names(row) && !is.na(row[["alternative_txt"]])) {
          alternative_text <- paste0(" (", row[["alternative_txt"]], ")")
        } else {
          alternative_text <- ""
        }
        
        additional_info <- c(additional_info, paste0("Unclear if Terrorism", alternative_text))
      }
    }

    if ("dayunclear" %in% names(row) && row[["dayunclear"]] == 1) {
      additional_info <- c(additional_info, "Exact day is unclear")
    }
    
    if ("locationunclear" %in% names(row) && row[["locationunclear"]] == 1) {
      additional_info <- c(additional_info, "Exact location is unclear")
    }
    
    if ("success" %in% names(row)) {
      if (row[["success"]] == 1) {
        additional_info <- c(additional_info, "Attack was successful")
      } else {
        additional_info <- c(additional_info, "Attack was not successful")
      }
    }
    
    if ("suicide" %in% names(row) && row[["suicide"]] == 1) {
      additional_info <- c(additional_info, "Suicide Attack")
    }
    
    # Combine additional information with main content
    if (length(additional_info) > 0) {
      content <- paste(content, "<br><br><b>Comments:</b>", paste(additional_info, collapse = "<br>"))
    }
    
    # Wrap in a styled div to adjust dimensions
    paste0(
      "<div style='max-height: 400px; width: 350px; overflow-y: auto; padding: 10px;
               background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 5px;
               box-sizing: border-box; display: flex; flex-direction: column;'>",
      content,
      "</div>"
    )
  }
  
  
  
  
  
  #### OVERVIEW ####

  # Render overview map
  output$overview <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10.5, lat = 51.2, zoom = 6)
  })
  
  # Reactive filtered data based on selected groups
  phenomenon_area_data <- reactive({
    
    # Filter data based on the selected phenomenon_area
    data %>% filter(phenomenon_area %in% input$selected_groups_overview)
  })
  
  # Observe changes in the selected phenomenon areas
  observe({
    
    # Fetch the filtered data based on selected groups
    phenomenon_area_points <- phenomenon_area_data()
    
    # Create the leafletProxy to update markers dynamically
    leafletProxy("overview") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = phenomenon_area_points,
        lng = ~longitude,
        lat = ~latitude,
        color = "transparent",
        fillColor = ~unname(sapply(phenomenon_area, get_color)),
        fillOpacity = 1,
        radius = 3,
        popup = apply(phenomenon_area_points, 1, create_popup),
        popupOptions = popupOptions(
          maxWidth = 400,
          minWidth = 300, 
          maxHeight = 500, 
          autoPan = TRUE, 
          closeOnClick = TRUE,
          keepInView = TRUE
        )
      )
  })
  
  # Text before incident counter
  observe({
    output$overview_incident_counter <- renderUI({
      HTML(
        paste0("<b>Number of Incidents:</b> <b>", nrow(phenomenon_area_data()), "</b>"
      ))
    })
  })
  
  # Text above checkboxes
  observe({
    output$overview_checkboxes_text <- renderUI({
      HTML(
        paste("<b>Selected Phenomenon Areas:</b>")
      )
    })
  })
  
  # Render horizontal proportional barplot for group (colored) vs. individual attacks
  output$overview_perpetrator_plot <- renderPlotly({
    
    # Summarize data and calculate proportions
    perpetrator_summary <- data %>%
      filter(!is.na(individual)) %>%
      group_by(phenomenon_area, individual) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(phenomenon_area) %>%
      mutate(
        proportion = count / sum(count),
        individual_label = ifelse(individual == 1, "Individual", "Group"),
        color = case_when(
          individual_label == "Individual" ~ "beige",
          phenomenon_area == "Right-Wing Extremism" ~ "chocolate",
          phenomenon_area == "Left-Wing Extremism" ~ "red",
          phenomenon_area == "Islamism" ~ "darkgreen",
          phenomenon_area == "Foreign Extremism" ~ "darkturquoise",
          phenomenon_area == "Uncategorized" ~ "darkgrey"
        )
      )
    
    # Set factor levels for consistent stacking
    perpetrator_summary$phenomenon_area <- factor(
      perpetrator_summary$phenomenon_area,
      levels = c("Right-Wing Extremism", "Left-Wing Extremism", "Islamism", 
                 "Foreign Extremism", "Uncategorized")
    )
    
    # Create plot
    plot <- plot_ly(data = perpetrator_summary, 
                    x = ~proportion, 
                    y = ~phenomenon_area, 
                    color = ~I(color),
                    type = 'bar', 
                    orientation = 'h', 
                    hoverinfo = 'text',
                    hovertext = ~paste(individual_label, ": ", scales::percent(proportion, accuracy = 0.1))
    ) %>%
      layout(
        barmode = 'stack',
        xaxis = list(title = "", tickformat = ".0%"),
        yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
        showlegend = FALSE,
        margin = list(l = 10, r = 30, t = 10, b = 30)
      )
  })
  
  
  
  
  
  #### TEMPORAL DATA ####
  
  # Render temporal data map
  output$temporal_data_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10.5, lat = 51.2, zoom = 6)
  })
  
  # Reactive value to track the incidents to display (cumulative data)
  plotted_data_cumulative <- reactiveVal(data.frame())
  
  # Keep track of the last year selected
  last_year_value <- reactiveVal(min(years))
  
  # Observe changes in the timeline slider (input$date_max)
  observeEvent(input$temporal_data_animation, {
    selected_year <- input$temporal_data_animation 
    previous_year <- last_year_value()
    
    # Update the last year value
    last_year_value(selected_year)
    
    if (selected_year > previous_year) {
      # Moving forward
      new_data_cumulative <- yearly_data_cumulative[[as.character(selected_year)]]
      plotted_data_cumulative(new_data_cumulative)
      
      # Check if the data exists and is not empty
      if (!is.null(new_data_cumulative) && nrow(new_data_cumulative) > 0) {
        tryCatch({
          leafletProxy("temporal_data_map") %>%
            addCircleMarkers(
              data = new_data_cumulative,  
              ~longitude, ~latitude,
              color = "transparent",
              fillColor = ~unname(sapply(phenomenon_area, get_color)),
              radius = 3,
              fillOpacity = 1,
              popup = apply(new_data_cumulative, 1, create_popup),
              popupOptions = popupOptions(
                maxWidth = 400,
                minWidth = 300,
                maxHeight = 500,
                autoPan = TRUE,
                closeOnClick = TRUE,
                keepInView = TRUE
              )
            )
        }, error = function(e) {
          cat("Error in adding markers (forward):", e$message, "\n")
        })
      } else {
        cat("No data to plot for year:", selected_year, "\n")
      }
      
    } else if (selected_year < previous_year) {
      # Moving backward
      cumulative_data_for_year <- yearly_data_cumulative[[as.character(selected_year)]]
      plotted_data_cumulative(cumulative_data_for_year)
      
      if (!is.null(cumulative_data_for_year) && nrow(cumulative_data_for_year) > 0) {
        tryCatch({ 
          leafletProxy("temporal_data_map") %>%
            clearMarkers() %>%
            addCircleMarkers(
              data = cumulative_data_for_year,  
              ~longitude, ~latitude,
              color = "transparent",
              fillColor = ~unname(sapply(phenomenon_area, get_color)),
              radius = 3,
              fillOpacity = 1,
              popup = apply(cumulative_data_for_year, 1, create_popup),
              popupOptions = popupOptions(
                maxWidth = 400,
                minWidth = 300,
                maxHeight = 500,
                autoPan = TRUE,
                closeOnClick = TRUE,
                keepInView = TRUE
              )
            )
        }, error = function(e) {
          cat("Error in adding markers (backward):", e$message, "\n")
        })
      } else {
        cat("No data to plot for year:", selected_year, "\n")
      }
    }
  })
  
  # Incidents over time plot
  output$attacks_over_time <- renderPlotly({
    
    # Ensure date format
    data$date <- as.Date(data$date)
    
    # Define order for phenomenon_area
    phenomenon_order <- c("Right-Wing Extremism", "Left-Wing Extremism", "Islamism", "Foreign Extremism", "Uncategorized")
    data$phenomenon_area <- factor(data$phenomenon_area, levels = phenomenon_order)
    
    # Full date sequence
    all_dates <- seq(min(data$date), max(data$date), by = "day")
    
    # Aggregate cumulative data
    data_cumulative <- data %>%
      group_by(date, phenomenon_area) %>%
      summarise(incident_count = n(), .groups = 'drop') %>%
      complete(date = all_dates, phenomenon_area, fill = list(incident_count = 0)) %>%
      arrange(phenomenon_area, date) %>% 
      group_by(phenomenon_area) %>%
      mutate(cumulative_incidents = cumsum(incident_count))
    
    # Optional total check
    total_cumulative_incidents <- data_cumulative %>%
      filter(date == max(date)) %>% 
      group_by(phenomenon_area) %>% 
      summarise(cumulative_sum = max(cumulative_incidents)) %>% 
      summarise(total = sum(cumulative_sum)) %>% 
      pull(total)
    
    # Create plot
    plot_ly(data_cumulative, x = ~date, y = ~cumulative_incidents, type = 'scatter', mode = 'lines',
            color = ~phenomenon_area, colors = color_mapping,
            name = ~phenomenon_area,
            line = list(width = 1.2)) %>%
      layout(
        title = "", 
        xaxis = list(title = "", range = c(min(data$date), max(data$date))), 
        yaxis = list(title = "", range = c(0, 650)), 
        hovermode = "x unified", 
        hoverdistance = 10, 
        showlegend = FALSE,
        hoverlabel = list(  
          bgcolor = "rgba(255, 255, 255, 0.8)",
          bordercolor = "black",
          font = list(family = "Arial, sans-serif", size = 14, color = "black"),
          namelength = -1
        )
      )
  })
  
  
  
  
  
  #### SPATIAL DATA ####
  
  # Render violin plot
  output$population_density_distribution <- renderPlotly({
    
    # Convert data to sf object using latitude and longitude
    data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = st_crs(pop_density_germany))
    
    # Perform spatial join: Find which points lie in which polygon
    incidents_with_density <- st_join(data_sf, pop_density_germany, join = st_within)
    
    # Check for NAs in population density and filter them out
    incidents_with_density <- incidents_with_density %>% filter(!is.na(pop_dn))
    
    # Order the factor levels for phenomenon_area explicitly
    incidents_with_density$phenomenon_area <- factor(
      incidents_with_density$phenomenon_area,
      levels = c("Right-Wing Extremism", 
                 "Left-Wing Extremism", 
                 "Islamism", 
                 "Foreign Extremism", 
                 "Uncategorized")
    )
    
    # Statistics for hover text
    stats_data <- incidents_with_density %>%  
      group_by(phenomenon_area) %>%
      summarise(
        min_dn = min(pop_dn, na.rm = TRUE),
        max_dn = max(pop_dn, na.rm = TRUE),
        median_dn = median(pop_dn, na.rm = TRUE),
        mean_dn = mean(pop_dn, na.rm = TRUE)
      ) %>%
      ungroup() 
    
    # Create violin plots for each phenomenon area
    plot <- plot_ly()
    
    # Loop through each phenomenon area and create a violin plot
    for (area in levels(incidents_with_density$phenomenon_area)) {
      area_data <- incidents_with_density %>% filter(phenomenon_area == area)
      area_stats <- stats_data %>% filter(phenomenon_area == area)
      
      plot <- plot %>%
        add_trace(
          data = area_data,
          x = ~phenomenon_area,
          y = ~pop_dn,
          type = 'violin',
          name = area,
          box = list(visible = TRUE),
          meanline = list(visible = TRUE),
          points = "all",
          jitter = 0.3, 
          scalemode = "count",
          line = list(color = "black"), 
          fillcolor = color_mapping[[area]], 
          opacity = 0.7, 
          marker = list(
            color = color_mapping[[area]],
            size = 4,
            opacity = 0.8 
          ),
          hoverinfo = 'text',
          text = paste( 
            "<b>", area, "</b><br>",
            "Min: ", area_stats$min_dn, "<br>",
            "Max: ", area_stats$max_dn, "<br>",
            "Median: ", area_stats$median_dn, "<br>",
            "Mean: ", round(area_stats$mean_dn, 2)
          )
        )
    }
    
    # Customize layout
    plot <- plot %>%
      layout(
        title = NULL,
        xaxis = list(title = "", showticklabels = FALSE),
        yaxis = list(title = "", range = c(0, 5000)),
        showlegend = FALSE, 
        annotations = list(
          list(
            x = 0.5,
            y = -0.15,
            text = "",
            showarrow = FALSE,
            xref = 'paper',
            yref = 'paper',
            xanchor = 'center',
            yanchor = 'top'
          )
        )
      )
    
    return(plot)
    
  })
  
  # Reactive value for current map
  current_map <- reactiveVal("heatmap")
  
  # Toggle map view
  observeEvent(input$switch_map, {
    current <- current_map()
    current_map(ifelse(current == "heatmap", "population_density", "heatmap"))
  })
  
  # Render iframe with appropriate map
  output$map_iframe <- renderUI({
    map_file <- switch(current_map(),
                       "heatmap" = "incident_heatmap.html",
                       "population_density" = "population_density.html")
    
    tags$iframe(
      src = map_file,
      width = "100%",
      height = "700px",
      frameborder = "0",
      style = "border: none;"
    )
  })
  
  
  
  
  
  #### WEAPONS & VICTIMS ####
  
  create_plotly_popup <- function(row) {
    
    # Define variable descriptions
    var_descriptions <- c(
      "eventid" = "ID",
      "date" = "Date",
      "city" = "City",
      "gname" = "Perpetrator Group/Category Name",
      "nkill" = "Number of Victims Killed",
      "nwound" = "Number of Victims Wounded",
      "nkillter" = "Number of Perpetrators Killed",
      "nwoundte" = "Number of Perpetrators Wounded",
      "attacktype1_txt" = "Attack Type",
      "attacktype2_txt" = "Attack Type 2",
      "targtype1_txt" = "Target Type",
      "targtype2_txt" = "Target Type 2",
      "targtype3_txt" = "Target Type 3",
      "target1" = "Target",
      "target2" = "Target 2",
      "target3" = "Target 3",
      "corp1" = "Targeted Corporation",
      "corp2" = "Targeted Corporation 2",
      "corp3" = "Targeted Corporation 3",
      "weaptype1_txt" = "Weapon Type",
      "weapsubtype1_txt" = "Weapon Subtype",
      "weaptype2_txt" = "Weapon Type 2",
      "weapsubtype2_txt" = "Weapon Subtype 2",
      "weaptype3_txt" = "Weapon Type 3",
      "weapsubtype3_txt" = "Weapon Subtype 3",
      "weapdetail" = "Weapon Details"
    )
    
    # Define display order for variables
    display_order <- c(
      "eventid", "date", "city", "gname",
      "nkill", "nwound", "nkillter", "nwoundte",
      "attacktype1_txt", "attacktype2_txt",
      "targtype1_txt", "targtype2_txt", "targtype3_txt",
      "target1", "target2", "target3",
      "corp1", "corp2", "corp3",
      "weaptype1_txt", "weapsubtype1_txt", "weaptype2_txt", "weapsubtype2_txt", "weaptype3_txt", "weapsubtype3_txt", "weapdetail"
    )
    
    # Keep only fields with defined descriptions and remove NAs
    selected_row <- row[names(row) %in% names(var_descriptions)]
    selected_row <- selected_row[!is.na(selected_row)] 
    
    # Build the main content text using newline (\n) as the separator
    content <- paste(
      sapply(display_order[display_order %in% names(selected_row)], 
             function(var) paste0(var_descriptions[[var]], ": ", as.character(selected_row[[var]]))),
      collapse = "\n"
    )
    
    # Add header for incident data with bold formatting
    content <- paste("<b>Incident Data:</b>\n", content, sep = "")
    
    # Additional info
    additional_info <- c() 
    
    # Append additional information in a specific order
    if ("doubtterr" %in% names(row)) { 
      if (as.numeric(row[["doubtterr"]]) == 1) {
        if ("alternative_txt" %in% names(row) && !is.na(row[["alternative_txt"]])) { 
          alternative_text <- paste0(" (", row[["alternative_txt"]], ")") 
        } else {
          alternative_text <- ""
        }

        additional_info <- c(additional_info, paste0("Unclear if Terrorism", alternative_text))
      }
    }
    
    if ("dayunclear" %in% names(row) && row[["dayunclear"]] == 1) { 
      additional_info <- c(additional_info, "Exact day is unclear") 
    }
    
    if ("locationunclear" %in% names(row) && row[["locationunclear"]] == 1) {
      additional_info <- c(additional_info, "Exact location is unclear") 
    }
    
    if ("success" %in% names(row)) {
      if (row[["success"]] == 1) { 
        additional_info <- c(additional_info, "Attack was successful") 
      } else { 
        additional_info <- c(additional_info, "Attack was not successful") 
      }
    }
    
    if ("suicide" %in% names(row) && row[["suicide"]] == 1) {
      additional_info <- c(additional_info, "Suicide Attack") 
    }
    
    # Combine additional information with main content
    if (length(additional_info) > 0) {
      content <- paste(content, "<br><br><b>Comments:</b><br>", paste(additional_info, collapse = "<br>"))
    }
    
    return(content)
  } 
  
  # Victims killed dotplot with click-only popup
  output$victims_killed_plot <- renderPlotly({
    req(input$selected_groups_victims_killed)
    
    df_killed <- data %>% 
      filter(!is.na(nkill), phenomenon_area %in% input$selected_groups_victims_killed) 
    
    # Generate popup text for each row
    df_killed$hover_text <- apply(df_killed, 1, create_plotly_popup)
    
    plot_ly(
      data = df_killed,
      x = ~date,
      y = ~nkill,
      color = ~phenomenon_area,
      colors = color_mapping,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 6),
      customdata = ~hover_text,
      hoverinfo = "none",
      source = "killed"
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "", range = c("1969-01-01", "2020-12-31")),
        yaxis = list(title = "", range = c(0, 17)),
        hovermode = "closest",
        showlegend = FALSE,
        hoverlabel = list(align = "left", bgcolor = "#F5F5F5")
      )
  })
  
  # Victims wounded dotplot with click-only popup
  output$victims_wounded_plot <- renderPlotly({
    req(input$selected_groups_victims_wounded)
    
    df_wounded <- data %>% 
      filter(!is.na(nwound), phenomenon_area %in% input$selected_groups_victims_wounded)
    
    # Generate popup text for each row
    df_wounded$hover_text <- apply(df_wounded, 1, create_plotly_popup)
    
    plot_ly(
      data = df_wounded,
      x = ~date,
      y = ~nwound,
      color = ~phenomenon_area,
      colors = color_mapping,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 6),
      customdata = ~hover_text,
      hoverinfo = "none",
      source = "wounded"
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "", range = c("1969-01-01", "2020-12-31")),
        yaxis = list(title = "", range = c(0, 250)),
        hovermode = "closest",
        showlegend = FALSE,
        hoverlabel = list(align = "left", bgcolor = "#F5F5F5")
      )
  })
  
  # Observe click events for the victims killed plot
  observeEvent(event_data("plotly_click", source = "killed"), {
    click_data <- event_data("plotly_click", source = "killed")
    
    if (!is.null(click_data)) {
      showModal(modalDialog(
        title = "Incident Details",
        HTML(click_data$customdata),
        easyClose = TRUE 
      ))
    }
  })
  
  # Observe click events for the victims wounded plot
  observeEvent(event_data("plotly_click", source = "wounded"), {
    click_data <- event_data("plotly_click", source = "wounded")
    
    if (!is.null(click_data)) {
      showModal(modalDialog(
        title = "Incident Details",
        HTML(click_data$customdata),
        easyClose = TRUE
      ))
    }
  })
  
  # Reactive filtering for victims killed
  filtered_killed <- reactive({
    data %>% 
      filter(!is.na(nkill), phenomenon_area %in% input$selected_groups_victims_killed)
  })
  
  # Total number of victims killed
  output$killed_sum_counter <- renderText({
    total_killed <- sum(filtered_killed()$nkill, na.rm = TRUE)
    paste("Total Victims Killed:", total_killed)
  })
  
  # Count of incidents where at least one victim was killed
  output$killed_incident_counter <- renderText({
    incidents_killed <- sum(filtered_killed()$nkill > 0)
    paste("Incidents With Victims Killed:", incidents_killed)
  })
  
  # Reactive filtering for victims wounded
  filtered_wounded <- reactive({
    data %>% 
      filter(!is.na(nwound), phenomenon_area %in% input$selected_groups_victims_wounded)
  })
  
  # Total number of victims wounded (sum of nwound)
  output$wounded_sum_counter <- renderText({
    total_wounded <- sum(filtered_wounded()$nwound, na.rm = TRUE)
    paste("Total Victims Wounded:", total_wounded)
  })
  
  # Count of incidents where at least one victim was wounded
  output$wounded_incident_counter <- renderText({
    incidents_wounded <- sum(filtered_wounded()$nwound > 0)
    paste("Incidents With Victims Wounded:", incidents_wounded)
  })
  
  # Render victim ratio plot
  output$victim_ratio_plot <- renderPlotly({
    
    # Data preparation: Calculate total incidents, total killed, total wounded, and ratios by phenomenon area
    summary_data <- data %>%
      group_by(phenomenon_area)  %>%
      summarise(
        total_incidents = n(),
        total_killed = sum(nkill, na.rm = TRUE),
        total_wounded = sum(nwound, na.rm = TRUE),
        ratio_killed = sum(nkill, na.rm = TRUE) / n(),
        ratio_wounded = sum(nwound, na.rm = TRUE) / n()
      ) %>%
      mutate(color = color_mapping[phenomenon_area])
    
    # Explicitly set the order for phenomenon_area
    summary_data$phenomenon_area <- factor(
      summary_data$phenomenon_area, 
      levels = c("Right-Wing Extremism", 
                 "Left-Wing Extremism", 
                 "Islamism", 
                 "Foreign Extremism", 
                 "Uncategorized")
    )
    
    # Plot 1: Ratio of killed per incident by phenomenon area
    plot1 <- plot_ly(
      summary_data,
      x = ~phenomenon_area,
      y = ~ratio_killed,
      type = 'bar',
      hoverinfo = 'text',
      text = ~round(ratio_killed, 2),
      textposition = 'outside',
      hovertext = ~paste(
        "Total Attacks: ", total_incidents,
        "<br>Total Killed: ", total_killed
      ),
      marker = list(color = ~color),
      hoverlabel = list(align = "left")
    ) %>%
      layout(
        title = list(
          text = "",
          x = 0.5,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 20)
        ),
        xaxis = list(showticklabels = FALSE, title = ""),
        yaxis = list(title = "", showgrid = TRUE, zeroline = TRUE)
      )
    
    # Plot 2: Ratio of wounded per incident by phenomenon area
    plot2 <- plot_ly(
      summary_data,
      x = ~phenomenon_area,
      y = ~ratio_wounded,
      type = 'bar', 
      hoverinfo = 'text',
      text = ~round(ratio_wounded, 2),
      textposition = 'outside',
      hovertext = ~paste(
        "Total Attacks: ", total_incidents,
        "<br>Total Wounded: ", total_wounded
      ),
      marker = list(color = ~color),
      hoverlabel = list(align = "left")
    ) %>%
      layout(
        title = list(
          text = "",
          x = 0.5,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 20)
        ),
        xaxis = list(showticklabels = FALSE, title = ""),
        yaxis = list(title = "", showgrid = TRUE, zeroline = TRUE)
      )
    
    # Arrange the two plots side by side
    subplot(plot1, plot2, nrows = 1, shareX = TRUE, titleX = FALSE, titleY = FALSE) %>%
      layout(showlegend = FALSE)  # Hide the legend
  })
  
  # Render weapon usage over time graph
  output$weapon_types_graph <- renderPlotly({
    
    # Filter dataset based on selected phenomenon areas
    phenomenon_area_data <- data %>%  
      filter(phenomenon_area %in% selected_phenomenon_areas())
    
    # Select relevant columns and reshape data to long format
    weapon_data <- phenomenon_area_data %>%  
      select(date, weaptype1_txt, weaptype2_txt, weaptype3_txt) %>%
      pivot_longer(cols = c(weaptype1_txt, weaptype2_txt, weaptype3_txt),
                   names_to = "weapon_column", values_to = "weapon_type") %>%
      drop_na(weapon_type) %>%
      group_by(date, weapon_type) %>%
      summarise(count = n(), .groups = "drop")
    
    # Validate the min and max dates to ensure they are finite
    min_date <- min(weapon_data$date, na.rm = TRUE)
    max_date <- max(weapon_data$date, na.rm = TRUE)
    
    if (is.finite(min_date) && is.finite(max_date)) {
      full_dates <- tibble(date = seq(min_date, max_date, by = "day"))
    } else {
      stop("Invalid date range: min or max date is non-finite.")
    }
    
    # Get a list of all unique weapon types in the 'weapon_data'
    all_weapon_types <- unique(weapon_data$weapon_type)
    
    # Expand grid to include all weapon types for all dates
    expanded_data <- expand_grid(date = full_dates$date, weapon_type = all_weapon_types) %>% 
      left_join(weapon_data, by = c("date", "weapon_type")) %>%
      replace_na(list(count = 0))
    
    # Calculate cumulative counts over time for each weapon type
    expanded_data <- expanded_data %>%
      arrange(date) %>% 
      group_by(weapon_type) %>%
      mutate(cumulative_count = cumsum(count))
    
    # Define a color mapping for each weapon type, based on RColorBrewer palette "Paired"
    color_mapping <- c(
      "Chemical" = "#2ca02c",
      "Explosives" = "#ff7f0e",
      "Fake Weapons" = "#1f77b4",
      "Firearms" = "#9467bd",
      "Incendiary" = "#d62728",
      "Melee" = "#8c564b",
      "Other" = "#7f7f7f",
      "Sabotage Equipment" = "#e377c2",
      "Unknown" = "#bcbd22",
      "Vehicle" = "#17becf"
    )
    
    # Create the plot with fixed y-axis scale and custom color mapping
    plot_ly(
      data = expanded_data,
      x = ~date,
      y = ~cumulative_count,
      color = ~weapon_type,
      colors = color_mapping,
      type = 'scatter',
      mode = 'lines',
      hoverinfo = "x+text",
      text = ~paste("", weapon_type, "", cumulative_count),
      line = list(width = 1.2)
    ) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = "", range = c(0, 700)),
        hovermode = "x unified",
        hoverdistance = 10,
        showlegend = TRUE,
        legend = list(
          x = 0,
          y = 1,
          xanchor = "left",
          yanchor = "top"
        ),
        hoverlabel = list(
          bgcolor = "rgba(255, 255, 255, 0.8)",
          bordercolor = "black",
          font = list(family = "Arial, sans-serif", size = 14, color = "black"),
          namelength = -1
        )
      )
  })
  
  # Define a reactive value to hold the selected phenomenon areas
  selected_phenomenon_areas <- reactiveVal(
    c("Right-Wing Extremism", "Left-Wing Extremism", "Islamism", "Foreign Extremism", "Uncategorized")
  )
  
  # Sync checkboxes with selected phenomenon areas, prevent loops
  observeEvent(input$selected_groups_weapon_types, ignoreInit = TRUE, {
    if (!identical(selected_phenomenon_areas(), input$selected_groups_weapon_types)) {
      selected_phenomenon_areas(input$selected_groups_weapon_types)
    }
  })
  
  # Update checkboxes based on selected phenomenon areas, prevent loops
  observe({
    if (!identical(input$selected_groups_weapon_types, selected_phenomenon_areas())) {
      updateCheckboxGroupInput(session, "selected_groups_weapon_types",
                               selected = selected_phenomenon_areas())
    }
  })
  
  
  
  
  
  #### PREDICTION MODEL ####
  
  # Accuracy metrics matrix
  metrics2 <- matrix(c(
    0.7614204, 1.6583695, 1.1446312,
    0.8642207, 0.9072827, 0.6934458,
    0.8890469, 0.8327406, 0.5770125,
    0.8091214, 0.5875558, 0.4497979,
    0.8868355, 1.1186778, 0.6608917
  ), ncol = 3, byrow = TRUE)
  
  # Add column and row names
  colnames(metrics2) <- c("R²", "RMSE", "MAE")
  rownames(metrics2) <- c("2016", "2017", "2018", "2019", "2020")
  
  # Add average row
  avg_row <- colMeans(metrics2)
  metrics2_with_avg <- rbind(metrics2, Average = avg_row)
  
  # Table output
  output$model_accuracy_table <- DT::renderDataTable({
    df <- as.data.frame(round(metrics2_with_avg, 3))
    df <- tibble::rownames_to_column(df, var = "Year")
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = 't',
        ordering = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE,
        responsive = TRUE,
        rowCallback = JS(
          "function(row, data, index) {",
          "  if (data[0] === 'Average') {",
          "    $('td', row).css('font-style', 'italic');",
          "  }",
          "}"
        )
      ),
      class = "compact stripe hover"
    )
  })
  
  # Load comparison results from RDS file
  all_models_comparison <- readRDS("all_models_comparison.rds")
  
  # Clean and format all_models_comparison for use
  comparison_matrix <- all_models_comparison %>%
    arrange(Year, factor(Model, levels = c("Random Forest", "Lag-1 Guess", "Mean Guess"))) %>%
    mutate(across(where(is.numeric), ~ round(., 3))) %>%
    select(Year, Model, R2, RMSE, MAE)
  
  # Manually define metric values for each model and year (used in the bar plot)
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
  
  # Assign column and row names for clarity
  colnames(comparison_matrix_wide) <- c("R2", "RMSE", "MAE")
  rownames(comparison_matrix_wide) <- c(
    "2016_RF", "2016_Lag1", "2016_Mean",
    "2017_RF", "2017_Lag1", "2017_Mean",
    "2018_RF", "2018_Lag1", "2018_Mean",
    "2019_RF", "2019_Lag1", "2019_Mean",
    "2020_RF", "2020_Lag1", "2020_Mean"
  )
  
  # Create dataframe version of the comparison matrix
  years <- rep(2016:2020, each = 3)
  models <- rep(c("Random Forest", "Lag-1 Guess", "Mean Guess"), times = 5)
  
  comparison_df <- data.frame(
    Year = years,
    Model = models,
    comparison_matrix_wide
  )
  
  # Convert to long format for plotting with plotly
  long_df <- comparison_df %>%
    pivot_longer(cols = c(R2, RMSE, MAE), names_to = "Metric", values_to = "Value")
  
  # Define custom color palette for model bars
  model_colors <- c(
    "RF"   = "#BC8DBF",
    "Lag1" = "#8DBFBF",
    "Mean" = "#F2C57C"
  )
  
  # Function to generate the baseline evaluation bar plot
  baseline_evaluation_barplot <- function(metric_name, show_legend = FALSE, custom_title = NULL) {
    plot_data <- long_df %>%
      filter(Metric == metric_name) %>%
      mutate(
        Model = recode(Model,
                       "Random Forest" = "RF",
                       "Lag-1 Guess"   = "Lag1",
                       "Mean Guess"    = "Mean"),
        Model = factor(Model, levels = c("RF", "Lag1", "Mean")),
        Year = paste0("'", substr(as.character(Year), 3, 4)) 
      )
    
    plot_ly(
      data = plot_data,
      x = ~Year,
      y = ~Value,
      color = ~Model,
      colors = model_colors,
      type = "bar",
      name = ~Model,
      text = ~round(Value, 3),
      hoverinfo = "text",
      showlegend = show_legend
    ) %>%
      layout(
        barmode = "group",
        title = list(text = ifelse(is.null(custom_title), metric_name, custom_title), x = 0.5),
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        margin = list(t = 50)
      )
  }
  
  # Reactive value to track currently selected metric (R2, RMSE, or MAE)
  selected_metric <- reactiveVal("R2")
  
  # Handle "Next" button click to rotate to the next metric
  observeEvent(input$metric_next, {
    current <- selected_metric()
    next_metric <- switch(current,
                          "R2" = "RMSE",
                          "RMSE" = "MAE",
                          "MAE" = "R2")
    selected_metric(next_metric)
  })
  
  # Handle "Previous" button click to rotate to the previous metric
  observeEvent(input$metric_prev, {
    current <- selected_metric()
    prev_metric <- switch(current,
                          "R2" = "MAE",
                          "RMSE" = "R2",
                          "MAE" = "RMSE")
    selected_metric(prev_metric)
  })
  
  # Render the plotly bar chart based on the currently selected metric
  output$baseline_evaluation_plot <- renderPlotly({
    metric <- selected_metric()
    show_legend <- TRUE
    custom_title <- switch(metric,
                           "R2" = "R²",
                           "RMSE" = "RMSE",
                           "MAE" = "MAE")
    baseline_evaluation_barplot(metric, show_legend, custom_title)
  })
  
  
  
  
  
  #### DATASET ####
  
  # Render dataset (excluding variable "year", which was added for the temporal display)
  output$dataset <- renderDT({
    datatable(data %>% select(-year), 
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  
} # SERVER END
