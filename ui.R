ui <- dashboardPage(
  dashboardHeader(
    title = div(
      span("Terrorist Incidents in Germany (1970-2020)", 
           style = "font-size: 22px; font-weight: bold; text-align: center; width: 100%;")
    ),
    titleWidth = "100%"
  ),
  dashboardSidebar(
    collapsed = FALSE,
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Temporal Data", tabName = "temporal_data"),
      menuItem("Spatial Data", tabName = "spatial_data"),
      menuItem("Weapons & Victims", tabName = "weapons_victims"),
      menuItem("Prediction Model", tabName = "prediction_model"),
      menuItem("Dataset", tabName = "dataset"),
      menuItem("References", tabName = "references"),
      menuItem("Readme", tabName = "readme")
    )
  ),
  dashboardBody(
    tabItems(
      
      
      
      
      #### OVERVIEW ####
      
      tabItem(
        tabName = "overview",
        fluidRow(
          
          column(width = 6,
                 box(
                   title = "Incidents Filtered by Phenomenon Area",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   height = "900px",
                   
                   # Map output
                   withSpinner(
                     leafletOutput("overview", height = "700px"),
                     type = 5,
                     color = "#3c8dbc",
                     size = 1
                   ),
                   
                   # Incident counter
                   div(
                     style = "display: flex; justify-content: space-around; margin-top: 20px;",
                     uiOutput("overview_incident_counter")
                   ),
                   
                   # Checkboxes
                   div(
                     style = "display: flex; flex-wrap: wrap; gap: 0px; margin-top: 40px;",
                     checkboxGroupInput(
                       "selected_groups_overview", 
                       label = "Selected Phenomenon Areas:",
                       choices = c("Right-Wing Extremism", "Left-Wing Extremism", "Islamism", "Foreign Extremism", "Uncategorized"),
                       selected = c("Right-Wing Extremism", "Left-Wing Extremism", "Islamism", "Foreign Extremism", "Uncategorized"),
                       inline = TRUE
                     )
                   )
                 )
          ),
          
          column(width = 6,
                 align = "left",
                 
                 # Text output
                 div(
                   style = "min-height: 630px; overflow-y: auto; border: 1px solid #3c8dbc; padding: 20px; margin-bottom: 10px; background-color: #fff; font-family: Arial, sans-serif; font-size: 15px; line-height: 1.6;",
                   HTML("
                        <p><strong>Overview</strong></p>
                        <p>This interactive application was developed as part of a master thesis at the Technical University of Munich (TUM) with the aim of exploring and visualizing spatial and temporal patterns of terrorism in Germany. The project seeks to bridge academic research and intuitive data exploration by making complex incident-level data accessible through an interactive, web-based platform. Specifically, the application enables users to investigate ideological trends, geographic concentrations, and actor structures across five decades of political violence.</p>
                    
                        <p>The underlying data are drawn from the Global Terrorism Database (START, 2022), a comprehensive open-source dataset of terrorist incidents worldwide. All events recorded in Germany between 1970 and 2020 were extracted and extensively preprocessed to ensure consistency, reliability, and suitability for visualization. Temporal variables were unified, missing or vague data points were flagged, and spatial coordinates were adjusted to resolve overlaps in densely targeted areas. Incidents without precise geographic information were placed in the North Sea to signal spatial ambiguity without excluding them from the dataset.</p>
                    
                        <p>Central to the analysis was the manual classification of incidents into ideological categories. Each event was assigned to one of five phenomenon areas—Right-Wing Extremism, Left-Wing Extremism, Islamist Extremism, Foreign Extremism, or Uncategorized—based on the responsible group, where known. This process involved cross-referencing perpetrator names with public sources and applying a structured rule set for ambiguous cases. The ideological coding enables comparative analysis across threat types and supports a more nuanced understanding of actor dynamics in the German context.</p>
                    
                        <p>On this page, two foundational visualizations provide a starting point: an interactive overview map showing the geographic distribution of incidents by ideology, and a static figure that compares the share of attacks carried out by organized groups versus unaffiliated individuals. To ensure legibility, overlapping incident points were jittered on all maps in this application, and cases with missing coordinates were assigned placeholder positions in the North Sea to flag spatial uncertainty. The static figure illustrates a structural shift in Germany's terrorism landscape, with a growing role of lone-actor violence in recent years. Together, these elements offer insight into both the spatial footprint and organizational characteristics of terrorist activity in Germany, setting the stage for deeper exploration in the subsequent sections (Jost, 2022; LaFree, 2019; Berrebi & Lakdawalla, 2007).</p>
                    
                        <br>
                        <u>Throughout this application, the following color-coding is used for phenomenon areas:</u><br>
                        <b>
                          <span style='color: chocolate;'>Right-Wing Extremism</span> | 
                          <span style='color: red;'>Left-Wing Extremism</span> | 
                          <span style='color: green;'>Islamism</span> | 
                          <span style='color: darkturquoise;'>Foreign Extremism</span> | 
                          <span style='color: darkgrey;'>Uncategorized</span>
                        </b>
                      ")
                 ),
                 
                 # Group (colored) vs. individual plot
                 box(
                   title = "Responsibility: Group (Colored) vs. Individual",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   height = "260px",
                   plotlyOutput("overview_perpetrator_plot", height = "200px"),
                   
                   # Change position of modebar
                   tags$style(HTML("
                                .plotly .modebar {
                                left: 0 !important;
                                right: auto !important;
                              }
                            "))
                 )
          )
        )
      ),
      
      
      
      
      
      #### TEMPORAL DATA ####
      
      tabItem(
        tabName = "temporal_data",
        fluidRow(
          
          column(width = 6,
                 box(
                   title = "Animated Timeline Map",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   height = "950px",
                   
                   # Temporal data map
                   withSpinner(
                     leafletOutput("temporal_data_map", height = "700px"),
                     type = 5,
                     color = "#3c8dbc",
                     size = 1
                   ),
                   
                   # Slider below map
                   sliderInput("temporal_data_animation", "", 
                               min = min(years), max = max(years), 
                               value = min(years), step = 1, 
                               animate = TRUE,
                               sep = "")
                 ),
                 
                 # Timeline controls
                 tags$head(
                   tags$style(HTML("
                    /* Center the play button above the slider */
                    .slider-animate-container {
                        display: flex;
                        justify-content: center;  /* Centers the button horizontally */
                        margin-top: 10px;  /* Adds space between slider and button */
                    }
  
                    /* Adjust the play button itself */
                    .slider-animate-button {
                        font-size: 30px !important;  /* Increase button size */
                    }
                  "))
                 ),
          ),
          
          column(width = 6,
                 height = "950px",
                 
                 # Text output
                 div(
                   style = "min-height: 400px; overflow-y: auto; border: 1px solid #3c8dbc; padding: 20px; margin-bottom: 10px; background-color: #fff; font-family: Arial, sans-serif; font-size: 15px; line-height: 1.6;",
                   HTML("
                        <p><strong>Temporal Data</strong></p>
                        <p>Germany’s terrorism landscape has undergone notable ideological and structural shifts over time, reflected in clear temporal patterns. The 1970s and 1980s were dominated by left-wing extremism, marked by high-frequency campaigns from groups such as the Red Army Faction. The 1990s saw a dramatic rise in right-wing attacks, particularly after reunification, often targeting migrants and concentrated in eastern Germany. A second wave of right-wing violence emerged after 2014, aligned with increasing political polarization and digital radicalization. Islamist terrorism, while temporally concentrated, shows distinct spikes around major geopolitical events such as the rise of ISIS and the Syrian refugee crisis.</p>
                    
                        <p>These dynamics—visible both in the timeline map and the smoothed trend plot—reveal a broader evolution from coordinated group violence to more dispersed, lone-actor incidents, particularly among right-wing and Islamist perpetrators. Critical peak years such as 1977, 1992, and 2016 illustrate how domestic attacks often coincide with national and international crises. Overall, the temporal data highlights how terrorism in Germany is shaped by long-term ideological cycles, shifting perpetrator structures, and broader socio-political contexts (LaFree, 2019; Jost, 2022).</p>
                      ")
                 ),
                 
                 # Incident graph
                 box(
                   title = "Incidents Over Time",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   height = "540px",
                   withSpinner(
                     plotlyOutput("attacks_over_time", height = "475px"),
                     type = 5,
                     color = "#3c8dbc",
                     size = 1
                   )
                 )
          )
        )
      ),
      
      
      
      
      
      #### SPATIAL DATA ####
      
      tabItem(
        tabName = "spatial_data",
        fluidRow(
          
          column(
            width = 6,
            box(
              title = "Incident Heatmap & Population Density",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              height = "825px",
              
              # Output placeholder for iframe
              withSpinner(
                uiOutput("map_iframe"),
                type = 5,
                color = "#3c8dbc",
                size = 1
              ),
              
              br(),
              
              # Toggle Button
              div(
                style = "text-align: center;",
                actionButton("switch_map", "Switch Maps", icon = icon("refresh"))
              )
            )
          ),
          
          column(width = 6,
                 height = "825px",
                 
                 # Text output
                 div(
                   style = "min-height: 400px; overflow-y: auto; border: 1px solid #3c8dbc; padding: 20px; margin-bottom: 10px; background-color: #fff; font-family: Arial, sans-serif; font-size: 15px; line-height: 1.6;",
                   HTML("
                        <p><strong>Spatial Data</strong></p>
                        <p>This section explores regional patterns of terrorist activity across Germany using an interactive map and density-aware visualizations. The choropleth overlays incident data onto municipal population density, helping to contextualize both absolute and per-capita exposure. Urban hubs like Berlin, Hamburg, and Frankfurt emerge as key hotspots—particularly for left-wing and foreign extremist activity—while right-wing incidents appear more widely distributed, with notable clustering in eastern regions after reunification. Islamist incidents are more recent and spatially scattered, typically near large cities with symbolic or strategic relevance.</p>
                        
                        <p>A 'Switch Map' button allows users to toggle between the incident heatmap—showing the geographic concentration of attacks—and a view of population density across German municipalities. This feature supports comparative analysis between where people live and where political violence occurs, highlighting areas of disproportionate risk.</p>
                        
                        <p>The accompanying violin plots provide an additional lens on spatial distribution by showing how incidents vary across municipalities with differing population densities. While most attacks are concentrated in urban areas, right-wing violence shows a broader spread into rural and mid-sized towns—underscoring its decentralized and opportunistic character. Left-wing and Islamist incidents, by contrast, are more urban-centric. These visual tools collectively highlight that terrorism in Germany reflects not only ideological motives but also spatial strategies and demographic patterns (Berrebi & Lakdawalla, 2007; Farrell et al., 2019).</p>
                      ")
                 ),
                 
                 # Violin plots
                 box(
                   title = "Population Density Distribution of Incidents",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   withSpinner(
                     plotlyOutput("population_density_distribution", height = "354px"),
                     type = 5,
                     color = "#3c8dbc",
                     size = 1
                   )
                 )
          )
        )
      ),
      
      
      
      
      
      #### WEAPONS & VICTIMS ####
      
      tabItem(
        tabName = "weapons_victims",
        
        # TOP ROW: Victims Killed (Left) + Victims Wounded (Right)
        fluidRow(
          
          # Victims killed plot
          column(
            width = 6,
            box(
              title = "Victims Killed",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              
              div(
                style = "display: flex; justify-content: space-around; margin-top: 10px;",
                htmlOutput("killed_sum_counter"),
                textOutput("killed_incident_counter")
              ),
              
              withSpinner(
                plotlyOutput("victims_killed_plot", height = "300px"),
                type = 5,
                color = "#3c8dbc",
                size = 1
              ),
              
              checkboxGroupInput(
                "selected_groups_victims_killed", 
                label = "Selected Phenomenon Areas:",
                choices = c("Right-Wing Extremism", "Left-Wing Extremism", "Islamism", "Foreign Extremism", "Uncategorized"),
                selected = c("Right-Wing Extremism", "Left-Wing Extremism", "Islamism", "Foreign Extremism", "Uncategorized"),
                inline = TRUE
              )
            )
          ),
          
          # Victims wounded plot
          column(
            width = 6,
            box(
              title = "Victims Wounded",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              
              div(
                style = "display: flex; justify-content: space-around; margin-top: 10px;",
                textOutput("wounded_sum_counter"),
                textOutput("wounded_incident_counter")
              ),
              
              withSpinner(
                plotlyOutput("victims_wounded_plot", height = "300px"),
                type = 5,
                color = "#3c8dbc",
                size = 1
              ),
              
              checkboxGroupInput(
                "selected_groups_victims_wounded", 
                label = "Selected Phenomenon Areas:",
                choices = c("Right-Wing Extremism", "Left-Wing Extremism", "Islamism", "Foreign Extremism", "Uncategorized"),
                selected = c("Right-Wing Extremism", "Left-Wing Extremism", "Islamism", "Foreign Extremism", "Uncategorized"),
                inline = TRUE
              )
            )
          )
        ),
        
        # MIDDLE TEXT ROW: text output
        fluidRow(
          column(
            width = 12,
            div(
              style = "overflow-y: auto; border: 1px solid #3c8dbc; padding: 20px; margin-bottom: 20px; background-color: #fff; font-family: Arial, sans-serif; font-size: 15px; line-height: 1.6;",
              HTML("
                  <p><strong>Weapons & Victims</strong></p>
                  <p>This section examines the evolution of weapon use and the impact on victims across ideological categories. The cumulative timeline chart shows that incendiary devices and explosives have been the most frequently used weapons throughout the observation period, with incendiary attacks sharply increasing during the 1980s and remaining dominant thereafter. Explosive use grew steadily and leveled off in the early 2000s. Firearm usage, while less frequent overall, exhibits a nearly linear increase, particularly over the past three decades—indicating a gradual shift toward more direct and potentially lethal forms of attack, often associated with right-wing and Islamist incidents.</p>
              
                  <p>The victim analysis compares the average number of fatalities and injuries per incident by ideology. Islamist attacks, while fewer in number, stand out for their disproportionate lethality—causing on average 0.88 deaths and 4.29 injuries per attack. Right-wing violence ranks second in terms of victim impact, with 0.25 fatalities and 2.36 injuries per incident. Left-wing attacks, by contrast, show relatively low victim counts, aligning with their historically symbolic or infrastructure-focused tactics. These metrics highlight stark differences in intent, operational capability, and consequences across ideological movements, underscoring the complexity of terrorism’s human toll (LaFree, 2019; Piazza, 2009).</p>
                ")
            )
          )
        ),
        
        # BOTTOM ROW: Text (Left) and Ratio Plot (Right)
        fluidRow(
          column(
            width = 6,
            
            # Weapon types graph
            box(
              title = "Weapon Usage Over Time",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              withSpinner(
                plotlyOutput("weapon_types_graph", height = "315px"),
                type = 5,
                color = "#3c8dbc",
                size = 1
              ),
              
              # Text above checkboxes
              div(
                style = "display: flex; align-items: center; gap: 0px; margin-top: 20px;",
                uiOutput("weapon_types_checkboxes_text"),
              ),
              
              # Checkboxes
              div(
                style = "display: flex; flex-wrap: wrap; gap: 0px; margin-top: 0px;",
                checkboxGroupInput(
                  "selected_groups_weapon_types", 
                  label = "Selected Phenomenon Areas:",
                  choices = c("Right-Wing Extremism", "Left-Wing Extremism", "Islamism", "Foreign Extremism", "Uncategorized"),
                  selected = c("Right-Wing Extremism", "Left-Wing Extremism", "Islamism", "Foreign Extremism", "Uncategorized"),
                  inline = TRUE
                )
              )
            )
          ),
          
          # Victim ratio plots
          column(
            width = 6,
            box(
              title = "Ratio of Victims per Incident Killed (Left) and Wounded (Right)",
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              withSpinner(
                plotlyOutput("victim_ratio_plot", height = "391px"),
                type = 5,
                color = "#3c8dbc",
                size = 1
              )
            )
          )
        ),
        
        # Ensure checkbox validation
        tags$script(HTML("
          $(document).ready(function() {
            function ensureAtLeastOneChecked(name) {
              $('input[name=\"' + name + '\"]').on('change', function() {
                if ($('input[name=\"' + name + '\"]:checked').length === 0) {
                  $(this).prop('checked', true);
                }
              });
            }
            ensureAtLeastOneChecked('selected_groups_victims_killed');
            ensureAtLeastOneChecked('selected_groups_victims_wounded');
          });
        "))
      ),
      
      
      
      
      
      #### PREDICTION MODEL ####
      
      tabItem(
        tabName = "prediction_model",
        fluidRow(
          
          column(width = 6,
                 box(
                   title = "Prediction Model (Predicted Incidents / Actual Incidents)",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   height = "825px",
                   
                   tags$div(
                     style = "text-align: center;",
                     
                     # The single iframe to load maps
                     tags$iframe(
                       id = "prediction_map_iframe",
                       src = "prediction_map_2016.html",  # default first map
                       width = "100%",
                       height = "700px",
                       frameborder = "0"
                     ),
                     
                     # Navigation buttons
                     
                     tags$div(
                       style = "text-align: center; margin-top: 20px;",
                       
                       # Navigation buttons
                       tags$button(id = "prev_btn", "← Previous", style = "margin-right: 20px;"),
                       tags$button(id = "next_btn", "Next →")
                     ),
                     
                     # JavaScript to handle map switching
                     tags$script(HTML("
                      let years = [2016, 2017, 2018, 2019, 2020];
                      let currentIndex = 0;
                  
                      function updateIframe() {
                        const iframe = document.getElementById('prediction_map_iframe');
                        iframe.src = 'prediction_map_' + years[currentIndex] + '.html';
                      }
                  
                      document.getElementById('prev_btn').onclick = function() {
                        if (currentIndex > 0) {
                          currentIndex--;
                          updateIframe();
                        }
                      }
                  
                      document.getElementById('next_btn').onclick = function() {
                        if (currentIndex < years.length - 1) {
                          currentIndex++;
                          updateIframe();
                        }
                      }
                    "))
                   )
                   
                 )
          ),
          
          # JavaScript to reload iframe on reset
          tags$script(HTML("
          Shiny.addCustomMessageHandler('reset_prediction_map', function(message) {
            var iframe = document.getElementById('prediction_map_iframe');
            if (iframe) {
              iframe.src = iframe.src; // Reload iframe to reset map view
            }
          });
        ")),
          
          column(width = 6,
                 
                 # Text output box (top)
                 div(
                   style = "min-height: 400px; overflow-y: auto; border: 1px solid #3c8dbc; padding: 20px; margin-bottom: 10px; background-color: #fff; font-family: Arial, sans-serif; font-size: 15px; line-height: 1.6;",
                   HTML("
                        <p><strong>Prediction Model</strong></p>
                        <p>This section presents a regression-based prediction model developed to estimate the number of terrorist attacks per year in each of Germany’s federal states from 2016 to 2020. The model uses a Random Forest algorithm, trained on spatially and temporally enriched GTD data, including lagged attack histories, decade-based temporal trends, and demographic context such as population density. Incidents were assigned to states using official shapefiles, and features were engineered to reflect both long-term structural patterns and short-term dynamics. Temporal bins were weighted logarithmically to emphasize recency, while lag features spanning 1–5 years captured regional attack history at multiple scales.</p>
                    
                        <p>Performance was optimized through grid search across lag lengths and weighting schemes, with evaluation conducted via five-fold cross-validation using a composite metric: R² minus 0.1 times the RMSE. This hybrid score balances overall explanatory power with sensitivity to large errors, making it especially appropriate for imbalanced, sparse datasets like regional terrorism in Germany. The model's results are visualized through interactive choropleth maps, where absolute prediction errors are color-coded to highlight regional accuracy.</p>
                    
                        <p>Across the five prediction years, the model demonstrates consistently strong performance, with R² values ranging from 0.76 to 0.89 and low average error rates (RMSE ≈ 1.02; MAE ≈ 0.71). Compared to naive baselines—such as mean-value prediction and lag-1 extrapolation—the Random Forest model substantially outperforms in every metric and year. Spatially, it achieves high accuracy in most states, particularly in those with stable historical trends like Niedersachsen and Bayern, though occasional underestimates occur in dynamic environments like Berlin or Saxony. These errors likely reflect sudden local escalations not captured by lag-based features.</p>
                    
                        <p>Importantly, this model demonstrates that even in stable democracies like Germany, where terrorism is rare and institutionally constrained, structured incident data and contextual features can support effective forecasting. The use of Random Forests enables robust learning from sparse and nonlinear data without relying on broad political indices, which often lack variance in such settings. As a result, the model offers a tailored, empirically grounded approach to regional terrorism prediction—well-suited for exploratory analysis, risk assessment, or policy planning.</p>
                      ")
                 ),
                 
                 fluidRow(
                   
                   # Incidents 2013-2019 plot
                   column(width = 3,
                          box(
                            title = "Incidents 2013–2019",
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL,
                            height = "260px",
                            tags$iframe(
                              src = "incidents_2013-2019.html",
                              width = "100%",
                              height = "208px",
                              frameborder = "0",
                              style = "border: none;"
                            )
                          )
                   ),
                   
                   # Model accuracy table
                   column(width = 3,
                          box(
                            title = "Model Accuracy",
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL,
                            DT::dataTableOutput("model_accuracy_table")
                          )
                   ),
                   
                   # Baseline evaluation
                   column(width = 6,
                          box(
                            title = "Baseline Evaluation (R² / RMSE / MAE)",
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL,
                            height = "260px",
                            div(
                              style = "display: flex; justify-content: space-between;",
                              
                              # Plot output
                              div(
                                style = "width: 400px;",
                                plotlyOutput("baseline_evaluation_plot", height = "208px")
                              ),
                              
                              # Button output
                              div(
                                style = "display: flex; flex-direction: column; justify-content: center; margin-left: 10px;",
                                actionButton("metric_next", "Next →", style = "margin-bottom: 5px;"),
                                actionButton("metric_prev", "← Previous")
                              )
                            )
                          )
                   )
                 )
          )
        )
      ),
      
      
      
      
      
      #### DATASET ####
      
      tabItem(
        tabName = "dataset",
        fluidRow(
          
          column(width = 12,
                 box(
                   title = "Dataset",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   
                   # Dataset output
                   withSpinner(
                     DTOutput("dataset", height = "894px"),
                     type = 5,
                     color = "#3c8dbc",
                     size = 1
                   ),
                 )
          )
        )
      ),
      
      
      
      
      
      #### REFERENCES ####
      tabItem(
        tabName = "references",
        fluidRow(
          
          # Text output
          column(width = 12,
                 div(
                   style = "min-height: 400px; overflow-y: auto; padding: 20px; background-color: #fff; font-family: Arial, sans-serif; font-size: 15px; line-height: 1.6;",
                   HTML("
                        <h2>References</h2>
                        <p>All sources cited in this application follow APA (7th edition) style and reflect literature used in the accompanying master's thesis.</p>
                    
                        <ul>
                          <li>Berrebi, C., & Lakdawalla, D. (2007). How does terrorism risk vary across space and time? <i>Defense and Peace Economics, 18</i>(2), 113–131. https://doi.org/10.1080/10242690600804257</li>
                          
                          <li>Buffa, F., Ferretti, S., & Grosso, A. (2022). Forecasting terrorist incidents with tree-based ensemble models. <i>Security Informatics, 11</i>(1), 1–15. https://doi.org/10.1186/s13388-022-00072-2</li>
                          
                          <li>Federal Agency for Cartography and Geodesy. (n.d.). <i>Administrative Areas 1:5,000,000, as of January 1st</i>. Retrieved from https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete/verwaltungsgebiete-1-5-000-000-stand-01-01-vg5000-01-01.html</li>
                          
                          <li>Federal Statistical Office of Germany (Destatis). (n.d.). <i>12411-0010: Population: Federal states, reference date</i>. Retrieved April 1, 2025, from https://www-genesis.destatis.de/genesis/online?operation=table&code=12411-0010&bypass=true&levelindex=0&levelid=1681993055600</li>
                    
                          <li>Farrell, G., Tilley, N., & Tseloni, A. (2019). <i>Reducing crime: A companion to crime prevention</i>. Routledge.</li>
                    
                          <li>Jost, J. T. (2022). A theory of system justification: History, implications, and future directions. <i>Nature Reviews Psychology, 1</i>, 383–396. https://doi.org/10.1038/s44159-022-00054-w</li>
                    
                          <li>Krieg, M., Glauner, P., & Grolinger, K. (2022). Predicting state-level terrorist activity using machine learning. <i>Journal of Computational Social Science, 5</i>(2), 565–583. https://doi.org/10.1007/s42001-022-00145-6</li>
                    
                          <li>LaFree, G. (2019). Terrorism and the internet: The global terrorism database perspective. <i>Journal of National Security Law & Policy, 10</i>(2), 231–254.</li>
                    
                          <li>Piazza, J. A. (2009). Is Islamist terrorism more deadly? An empirical study of group ideology, organization, and goal structure. <i>Terrorism and Political Violence, 21</i>(1), 62–88. https://doi.org/10.1080/09546550802544698</li>
                    
                          <li>START. (2022). <i>Global Terrorism Database [Data file]</i>. National Consortium for the Study of Terrorism and Responses to Terrorism (START). Retrieved from https://www.start.umd.edu/gtd</li>
                    
                          <li>Tuszyński, M. (2023). Temporal machine learning for terrorism forecasting: Performance of Random Forest and LSTM models. <i>Computational Intelligence, 39</i>(3), 892–909. https://doi.org/10.1111/coin.12457</li>
                        </ul>
                      ")
                 )
          )
        )
      ),
      
      
      
      
      
      #### README ####
      tabItem(
        tabName = "readme",
        fluidRow(
          
          # Text output
          column(width = 12,
                 div(
                   style = "min-height: 400px; overflow-y: auto; padding: 20px; background-color: #fff; font-family: Arial, sans-serif; font-size: 15px; line-height: 1.6;",
                   HTML("
                    <h2>GTD Visualization: Terrorism in Germany (1970–2020)</h2>
                    <p><strong>Access the App:</strong> <a href='https://lukasgraeb.shinyapps.io/gtd_visualization' target='_blank'>lukasgraeb.shinyapps.io/gtd_visualization</a></p>
                
                    <p>This interactive application was developed as part of a Master's thesis at the Technical University of Munich (TUM). It visualizes terrorist incidents in Germany between 1970 and 2020, based on the Global Terrorism Database (START, 2022), and includes advanced preprocessing and manual classification for detailed analysis.</p>
                
                    <h3>Purpose</h3>
                    <ul>
                      <li>Explore spatial and temporal trends in German terrorism.</li>
                      <li>Demonstrate predictive modeling using structured incident data.</li>
                    </ul>
                
                    <h3>Main Features</h3>
                    <ul>
                      <li><strong>Overview:</strong> Interactive map and a comparative plot of group vs. individual attacks.</li>
                      <li><strong>Spatial Data:</strong> Incident distribution map, population density view, and violin plots by municipality size. Toggle between maps using the <em>Switch Map</em> button.</li>
                      <li><strong>Temporal Data:</strong> Trends in attack frequency, ideology, and perpetrator structure across key years.</li>
                      <li><strong>Weapons & Victims:</strong> Distribution of weapon types and victim outcomes by ideology.</li>
                      <li><strong>Prediction Model:</strong> Random forest classifier trained to predict ideological categories with high accuracy.</li>
                    </ul>
                
                    <h3>Technologies Used</h3>
                    <ul>
                      <li>R & Shiny</li>
                      <li>Leaflet for mapping</li>
                      <li>ggplot2 & plotly for visualizations</li>
                      <li>randomForest for machine learning</li>
                      <li>Global Terrorism Database (START, 2022)</li>
                    </ul>
                
                    <h3>Notes</h3>
                    <ul>
                      <li>Unclear locations are visualized in the North Sea as placeholders.</li>
                      <li>Incidents at identical coordinates are slightly jittered for clarity.</li>
                      <li>Consistent color-coding is used throughout the app to represent ideological categories.</li>
                    </ul>
                
                    <h3>Citation</h3>
                    <p>Gräb, L. (2025). <em>Visualizing Spatial and Temporal Patterns of Terrorist Incidents in Germany Between 1970 and 2020 Using R Shiny</em> [Master's thesis, Technical University of Munich].</p>
                  ")
                 )
                 
          )
        )
      )
    )
  )
)