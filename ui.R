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
                          <p>This interactive application was developed as part of a master thesis at the Technical University of Munich (TUM) to explore and visualize spatial and temporal patterns of terrorism in Germany. It aims to bridge academic research and intuitive data exploration by making complex incident-level data accessible through a user-friendly, web-based platform. Users can investigate ideological trends, geographic concentrations, and actor structures across five decades of political violence.</p>
  
                          <p>The underlying data stem from the Global Terrorism Database (START, 2022), covering all recorded incidents in Germany from 1970 to 2020. These events were extensively cleaned and preprocessed: These events were extensively cleaned and preprocessed: date variables were consolidated into a single standardized column, missing values flagged, and spatial coordinates adjusted to resolve overlaps. Incidents lacking precise geographic data were assigned placeholder coordinates in the North Sea to preserve their visibility.</p>
                          
                          <p>A key component of the analysis is the manual classification of incidents into five ideological categories (phenomenon areas)—Right-Wing, Left-Wing, Islamist, Foreign, and Uncategorized—based on the responsible group, where identifiable. This classification was informed by public sources and a standardized rule set, enabling meaningful comparison across different forms of extremism.</p>
                          
                          <p>This page presents two core visualizations: an interactive map showing the ideological distribution of incidents and a static figure comparing attacks by organized groups versus individuals. Both visualizations reflect structural changes in Germany’s terrorism landscape, including a recent increase in lone-actor violence (Fielitz et al., 2023; Gräfe, 2017). Together, they provide a foundation for deeper exploration in the sections that follow.</p>
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
                          <p>This page highlights the evolving ideological landscape of terrorism in Germany from 1970 to 2020. Left-wing extremism dominated the 1970s and 1980s, with peak activity from groups like the RAF during the “Deutscher Herbst” and beyond (Pfahl-Traughber, 2020). After reunification, left-wing violence declined as major groups dissolved. In contrast, right-wing extremism surged in the 1990s, especially in East Germany, driven by reunification-related dislocation and rising xenophobia (Gräfe, 2017). A second wave of right-wing violence emerged after 2015, fueled by digital radicalization and political polarization (Siewert, 2023).</p>
  
                          <p>Foreign extremist groups such as the PKK and IRA were most active in the 1980s and early 1990s, often targeting diplomatic or military sites (Freytag, 2025; Lyon & Ucarer, 2001). Islamist terrorism rose later, with most incidents occurring after 2001 and peaking around 2015, reflecting the influence of ISIS and global jihadist networks (Goertz, 2023; Schneiders, 2014). These attacks often involved lone actors and higher casualty rates.</p>
  
                          <p>Overall, the visualizations confirm that terrorist activity in Germany follows distinct temporal arcs across ideologies, shaped by broader social and political developments. These trends provide not only historical insight but also the empirical foundation for this thesis’s predictive modeling component (Farrell et al., 2019).</p>
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
                          <p>The spatial distribution of terrorism-related incidents in Germany reflects a clear relationship between population density and ideological targeting patterns. This page illustrates that urban centers such as Berlin, Hamburg, Munich, and the Rhine-Ruhr region have the highest population densities. Terrorist incidents cluster strongly in these same areas. This supports Farrell et al.’s (2019) view that terrorism often follows a strategic spatial logic—targeting areas that are symbolically significant and infrastructurally dense.</p>
  
                          <p>Figure 7 further explores these dynamics by comparing population density across ideological categories. Left-wing, Islamist, and foreign extremist incidents are concentrated in highly urbanized regions, with median population densities exceeding 2,300 inhabitants/km². Groups such as the RAF and PKK strategically operated in cities to maximize impact (Pfahl-Traughber, 2020; Freytag, 2025). In contrast, right-wing extremist violence is more widely dispersed, often occurring in rural or peri-urban areas—particularly in post-reunification East Germany—where structural and social dislocation created fertile ground for radicalization (Gräfe, 2017; Fielitz et al., 2023).</p>
  
                          <p>Together, these spatial patterns underscore the importance of demographic and geographic context in terrorism research, offering a foundation for both historical interpretation and predictive modeling.</p>
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
                    <p>This page explores the human and tactical impact of terrorism in Germany between 1970 and 2020. The Victims Killed/Wounded plots show that both fatalities and injuries declined after 2000, followed by a noticeable uptick around 2015. This shift aligns with the resurgence of right-wing extremism and the rise of jihadist lone-actor attacks (Siewert, 2023; Goertz, 2023), underscoring how broader political developments, migration patterns, and digital radicalization influence the severity of attacks.</p>
  
                    <p>The Ratio of Victims per Incident comparison reveals striking differences in the lethality of attacks across phenomenon areas. Islamist extremism stands out with the highest average casualties, reflecting a strategy geared toward mass-casualty events (Goertz, 2023). Right-wing attacks show moderate lethality, consistent with the shift from paramilitary networks like the NSU to lone actors in cases such as Halle and Hanau (Fielitz et al., 2023; Gräfe, 2017). Left-wing extremism, by contrast, shows the lowest casualty rates, indicative of symbolic attacks or sabotage tactics (Pfahl-Traughber, 2020). Foreign extremist incidents—linked to groups like the IRA and PKK—also yield moderate casualty levels, typically involving targeted attacks on state or diplomatic institutions (Freytag, 2025).</p>
  
                    <p>In the Weapon Types Over Time section, incendiary devices and explosives emerge as the most frequently used weapons across decades, particularly among left-wing and foreign extremist groups in the 1970s–1990s (Pfahl-Traughber, 2020; Freytag, 2025). More recently, firearms have become increasingly prominent, especially in right-wing and Islamist contexts—often tied to lone-actor attacks (Gräfe, 2017; Goertz, 2023). These trends reflect not only ideological motives but also practical considerations such as access, visibility, and symbolic impact, reinforcing Farrell et al.’s (2019) argument that weapon choice is shaped by both strategic logic and contextual opportunity.</p>
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
          
          # Prediction Map 2016
          column(width = 6,
                 box(
                   title = "Prediction Model (Predicted Incidents / Actual Incidents)",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   height = "825px",
                   tags$div(
                     style = "text-align: center;",
                     tags$iframe(
                       id = "prediction_map_iframe",
                       src = "prediction_map_2016.html",
                       width = "100%",
                       height = "700px",
                       frameborder = "0"
                     ),
                     tags$div(
                       style = "text-align: center; margin-top: 20px;",
                       tags$button(id = "prev_btn", "← Previous", style = "margin-right: 20px;"),
                       tags$button(id = "next_btn", "Next →")
                     ),
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
          
          # Right side layout
          column(width = 6,
                 
                 # Top Row
                 fluidRow(
                   
                   # Incidents 2010-2020
                   column(width = 8,
                          box(
                            title = "Incidents 2010–2020",
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL,
                            height = "260px",
                            tags$iframe(
                              src = "incidents_2010-2020.html",
                              width = "100%",
                              height = "208px",
                              frameborder = "0",
                              style = "border: none;"
                            )
                          )
                   ),
                   
                   # Model Accuracy
                   column(width = 4,
                          box(
                            title = "Model Accuracy",
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL,
                            height = "260px",
                            DT::dataTableOutput("model_accuracy_table")
                          )
                   )
                 ),
                 
                 # Middle Row
                 fluidRow(
                   style = "margin-top: 0px;",
                   
                   # Feature Importance
                   column(width = 6,
                          box(
                            title = "Feature Importance (Mean Increase in Node Purity)",
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL,
                            height = "543px",
                            tags$iframe(
                              src = "feature_importance.html",
                              width = "100%",
                              height = "450px",
                              frameborder = "0",
                              style = "border: none;"
                            )
                          )
                   ),
                   
                   # Baseline Evaluation
                   column(width = 6,
                          box(
                            title = "Baseline Evaluation (R² / RMSE / MAE)",
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL,
                            height = "543px",
                            div(
                              style = "display: flex; justify-content: space-between;",
                              div(
                                style = "width: 400px;",
                                plotlyOutput("baseline_evaluation_plot", height = "450px")
                              ),
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
        ),
        
        # Full-width text box below everything
        fluidRow(
          column(width = 12,
                 div(
                   style = "min-height: 400px; overflow-y: auto; border: 1px solid #3c8dbc; padding: 20px; margin-top: 10px; background-color: #fff; font-family: Arial, sans-serif; font-size: 15px; line-height: 1.6;",
                   HTML("
                        <p>This page presents a machine learning model developed to estimate the number of terrorist incidents in each of Germany’s federal states between 2016 and 2020. Using a Random Forest regression algorithm, the model draws on spatially and temporally enriched data from the Global Terrorism Database (START, 2022), combined with population statistics from official German sources. It was specifically designed to work within the constraints of a low-frequency, high-stability environment like Germany.</p>

                        <p>To prepare the data, historical incidents were aggregated at the state level and enriched with demographic features such as population density. Temporal predictors included decade-based time bins and lag variables representing past incident counts (1–5 years), with logarithmic and linear weighting to balance long-term memory and short-term trends. Each incident was geographically linked to a state using shapefiles from the Bundesamt für Kartographie und Geodäsie (2025). A grid search was used to fine-tune model parameters across years, and performance was evaluated using five-fold cross-validation.</p>
                        
                        <p>The model achieves strong performance and low error margins. Compared to two baseline models—a naive average and a lag-1 approach—the Random Forest consistently delivers superior predictions. While it occasionally underestimates incidents in high-volatility regions (e.g., Berlin, Sachsen in 2020), its predictions are generally stable and accurate, even in states with sparse data.</p>
                        
                        <p>Feature importance analysis shows that the most influential variables are recent historical trends (e.g., attacks in 2010–2015 and lagged counts from the previous 1–3 years). This is particularly relevant given the irregular and volatile nature of incident patterns during this period, which makes lag variables effective in capturing short-term spikes and troughs. The model’s ability to respond to such non-linear trends reinforces its contextual sensitivity and practical utility. Long-term historical bins like the 1970s and 1990s also rank highly—corresponding to periods of elevated terrorism—whereas the 2000s, a relatively quiet decade, rank lowest. Several population-based features also play an important role, reaffirming that terrorism in Germany is often concentrated in urban areas (Farrell et al., 2019).</p>
                        
                        <p>The model evaluation uses a composite scoring metric: R² − 0.1 × RMSE, ensuring that both explanatory power and risk sensitivity are considered. This approach is well suited to Germany’s data environment, where major outliers (e.g., sudden spikes in attacks) must be taken seriously. Overall, the prediction model offers a context-aware, transparent, and reproducible tool for regional risk estimation.</p>
                       ")
                 )
          )
        ),
        
        # JavaScript: Reset Handler
        tags$script(HTML("
    Shiny.addCustomMessageHandler('reset_prediction_map', function(message) {
      var iframe = document.getElementById('prediction_map_iframe');
      if (iframe) {
        iframe.src = iframe.src;
      }
    });
  "))
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
                          <li>Farrell, M. M., Findley, M. G., & Young, J. (2019). Geographical approaches in the study of terrorism. In E. Chenoweth, R. English, A. Gofas, & S. N. Kalyvas (Eds.), <i>The Oxford handbook of terrorism</i> (pp. 237–250). Oxford University Press. https://doi.org/10.1093/oxfordhb/9780198732914.013.49</li>
                      
                          <li>Fielitz, M., Schwarz, K., & Quent, M. (2023). Die digitale Subkultur des Rechtsterrorismus. In J. Jost & J. Krause (Eds.), <i>Jahrbuch Terrorismus 2019–2021</i> (pp. 127–175). Verlag Barbara Budrich. https://doi.org/10.3224/84742401</li>
                      
                          <li>Freytag, J. (2025). The German campaign of the IRA and its resolution by Eberhard Spiecker and Alec Reid. <i>Small Wars & Insurgencies, 1</i>–18. https://doi.org/10.1080/09592318.2025.2480327</li>
                      
                          <li>Goertz, S. (2023). Aktuelle und zukünftige Bedrohungen durch Islamismus und islamistischen Terrorismus in Europa sowie das Bedrohungspotenzial von Foreign Fighters. In J. Jost & J. Krause (Eds.), <i>Jahrbuch Terrorismus 2019–2021</i> (pp. 149–175). Verlag Barbara Budrich. https://doi.org/10.3224/84742401</li>
                      
                          <li>Gräfe, S. (2017). <i>Rechtsterrorismus in der Bundesrepublik Deutschland: Zwischen erlebnisorientierten Jugendlichen, “Feierabendterroristen” und klandestinen Untergrundzellen</i>. Nomos Verlagsgesellschaft mbH & Co. KG. https://doi.org/10.5771/9783845287577</li>
                      
                          <li>Lyon, A. J., & Ucarer, E. M. (2001). Mobilizing ethnic conflict: Kurdish separatism in Germany and the PKK. <i>Ethnic and Racial Studies, 24</i>(6), 925–948. https://doi.org/10.1080/0141987012007792</li>
                      
                          <li>Pfahl-Traughber, A. (2020). <i>Linksextremismus in Deutschland: Eine kritische Bestandsaufnahme</i>. Springer Fachmedien Wiesbaden. https://doi.org/10.1007/978-3-658-30209-2</li>
                      
                          <li>Schneiders, T. G. (2014). Salafismus in Deutschland—Einleitung. In <i>Salafismus in Deutschland</i> (pp. 9–24). transcript Verlag.</li>
                      
                          <li>Siewert, N. (2023). „Gegenwehr oder Verschwinden!“ Apokalyptische Narrative in der extremen Rechten. In J. Jost & J. Krause (Eds.), <i>Jahrbuch Terrorismus 2019–2021</i> (pp. 51–87). Verlag Barbara Budrich. https://doi.org/10.3224/84742401</li>
                      
                          <li>START (National Consortium for the Study of Terrorism and Responses to Terrorism). (2022). <i>Global Terrorism Database, 1970—2020</i> [Dataset]. https://www.start.umd.edu/data-tools/GTD</li>
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
                          <li><strong>Temporal Data:</strong> Trends in attack frequency, phenomenon area, and perpetrator structure across key years.</li>
                          <li><strong>Weapons & Victims:</strong> Distribution of weapon types and victim outcomes by phenomenon area.</li>
                          <li><strong>Prediction Model:</strong> Random forest classifier trained to predict phenomenon areas with high accuracy.</li>
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
                          <li>Consistent color-coding is used throughout the app to represent phenomenon areas.</li>
                        </ul>
                      
                        <h3>Abbreviations</h3>
                        <ul>
                          <li><strong>IRA:</strong> Provisional Irish Republican Army</li>
                          <li><strong>ISIS:</strong> Islamic State of Iraq and Syria</li>
                          <li><strong>NSU:</strong> Nationalsozialistischer Untergrund (National Socialist Underground)</li>
                          <li><strong>PKK:</strong> Partiya Karkerên Kurdistanê (Kurdistan Workers’ Party)</li>
                          <li><strong>RAF:</strong> Rote Armee Fraktion (Red Army Faction)</li>
                        </ul>
                      
                        <h3>Code</h3>
                        <p>Code can be downloaded and reviewed at <a href='https://github.com/Graebi123/gtd_visualization.git' target='_blank'>github.com/Graebi123/gtd_visualization.git</a></p>
                      
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