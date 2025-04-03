# UI ----------------------------------------------------------------------
# This file constructs the user interface for the Shiny dashboard using the shinydashboard package.
# The UI is organized into three main components:
#   1. Header: Displays the dashboard title, logo, and key action buttons (e.g., announcement button).
#   2. Sidebar: Contains a navigational menu for switching between tabs (Home, Data Overview, Downloads, etc.)
#      and includes conditional panels that reveal additional input controls based on the active tab.
#   3. Body: Hosts the main content for each tab, including dynamic outputs (e.g., maps, data tables, plots),
#      instructional text, and detailed information sections (About, Instructions).
#
# Custom HTML elements and external CSS are incorporated to enhance styling and ensure a responsive layout.

ui <- tags$html(
  lang = "en",  # Set the language attribute for accessibility
  tags$head(
    # Include external HTML (e.g., Google Analytics tracking code)
    includeHTML("g-analytics.html"),
    # Set the favicon for the dashboard
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    # Set the page title
    tags$title("Cal-SuWers Dashboard")
  ),
  dashboardPage(
    # Dashboard header: Contains the title, logo, and special action buttons
    dashboardHeader(
      # Title with an image logo and custom text
      title = tags$span(
        tags$img(
          src = "favicon.png", # Path to the image file for the logo
          height = "30px"      # Adjust the height of the image
        ),
        "CDPH California Surveillance of Wastewaters (Cal-SuWers) Network 2",
        class = "dashboard-title"  # Class to apply custom styling
      ),
      # Additional header content: a dropdown list with a special announcement button
      tags$li(
        class = "dropdown",
        div(
          id = "my_special_button",
          style = "margin-right: 10px",
          actionButton("announce_button", "Announcement", class = "btn-primary")
        )
      )
    ),
    
    ## Sidebar -----------------------------------------------------------------
    dashboardSidebar(
      # Sidebar menu to navigate between different tabs/pages
      sidebarMenu(
        id = "sidebar",
        
        ### Home page ---------------------------------------------------------------
        menuItem("Home", tabName = "dashboard", icon = icon("home")),
        tags$hr(),  # Horizontal line divider
        
        ### Respiratory Virus Data ---------------------------------------------------------------
        menuItem("Respiratory Virus Data", tabName = "overview", icon = icon("viruses")),
        
        # Conditional panel to display additional input options when 'overview' is selected
        conditionalPanel(
          condition = "input.sidebar == 'overview'",
          # Dropdown to select pathogen; currently only one is available (target_choice)
          selectInput(
            inputId = "pathogen",
            label = "Select pathogen",
            choices = target_choice,
            multiple = FALSE,
            selected = target_choice[1]
          ),
          # Dropdown to choose data view: Region or Sewershed
          selectInput(
            inputId = "select_plot",
            label = "Show data by",
            choices = c("Region", "Sewershed"),
            multiple = FALSE,
            selected = "Region"
          ),
          # Dropdown to select region from available options
          selectInput(
            inputId = "HO",
            label = "Select region",
            choices = region_choice,
            multiple = FALSE,
            selected = region_choice[1]
          ),
          # Dynamic UI output for selecting wastewater treatment plant (wwtp)
          uiOutput("dynamic_wwtp_picker"),
          
          # Add vertical space between buttons
          div(style = "margin-top: 42px;"),
          # Action button for State Wastewater Overview with centered styling
          div(
            style = "text-align: center; margin-top: 10px;",
            actionButton(
              inputId = "state_summary",
              label = HTML("State Wastewater Overview​"),
              title = "Click to view"
            )
          ),
          
          # Space between buttons
          div(style = "margin-top: 40px;"),
          # Action button for Regional Wastewater Overview
          div(
            style = "text-align: center; margin-top: 10px;",
            actionButton(
              inputId = "regional_summary",
              label = HTML("Regional Wastewater Overview"),
              title = "Click to view"
            )
          )
        ),
        ### Data Download ---------------------------------------------------------------
        tags$hr(),
        menuItem("Data Download", tabName = "download", icon = icon("download")),
        
        # Conditional panel for Data Download tab
        conditionalPanel(
          condition = "input.sidebar == 'download'",
          # Nested conditional panels based on the selected dataset
          conditionalPanel(
            condition = "input.dataset == 'Wastewater Data'",
            # Checkbox group input to select columns for Wastewater Data
            div(
              style = "margin-top: -10px;",
              checkboxGroupInput("show_vars_wastewater", 
                                 h3("Select columns to view and download", 
                                    style = "color: #337ab7; margin-top: 0px; margin-bottom: 5px;"),
                                 choices = names(download_df1), 
                                 selected = names(download_df1))
            ),
            # Container with tooltip for download button
            div(
              id = "downloadButtonWithTooltip1",
              style = "margin-left: 11px;",
              actionButton('confirmDownload', 'Download Data', 
                           style = "font-size: 18px; padding: 10px 20px; background-color: #337ab7; color: white; border: none; border-radius: 5px;"),
              # Tooltip text displayed on hover
              span(class = "tooltip-text", "Click to download the selected data. Please note, the download progress may take a few moments to appear due to the large file size.")
            )
          ),
          
          # Conditional panel for Metrics Summary Data
          conditionalPanel(
            condition = "input.dataset == 'Metrics Summary Data'",
            div(
              style = "margin-top: -10px;",
              checkboxGroupInput("show_vars_metrics", 
                                 h3("Select columns to view and download", 
                                    style = "color: #337ab7; margin-top: 0px; margin-bottom: 5px;"),
                                 choices = names(download_df2), 
                                 selected = names(download_df2))
            ),
            div(
              id = "downloadButtonWithTooltip2",
              style = "margin-left: 11px;",
              downloadButton('downloadData2', 'Download Data', 
                             style = "font-size: 18px; padding: 10px 20px; background-color: #337ab7; color: white; border: none; border-radius: 5px;"),
              span(class = "tooltip-text", "Click to download the selected data")
            )
          )
        ),
        tags$hr(),
        ### About the Dashboard ---------------------------------------------------------------
        menuItem("About the Dashboard", tabName = "technical_notes", icon = icon("file-alt")),
        ### Instructions About the Dashboard ---------------------------------------------------------------
        menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
        # menuItem("Resources", tabName = "resources", icon = icon("book")),
        tags$hr(),
        # Contact link
        menuItem("Contact Us", href = "mailto:Wastewatersurveillance@cdph.ca.gov", icon = icon("envelope")),
        div(style = "margin-top: 0px;"),
        
        ### CDPH Logo ---------------------------------------------------------------
        # Display CDPH logo with custom styling
        div(
          style = "text-align: left; margin-top: 20px; margin-left: 5px;",
          tags$img(
            src = "cdph_logo_2024e.png",
            alt = "California Department of Public Health Logo",
            style = "max-width: 90%; height: auto; width: 280px;"
          )
        ),
        # Additional links and version information
        div(
          style = "text-align: left; margin-top: 20px; margin-left: 10px;",
          tags$p(
            tags$a(
              href   = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/COVID-19/Wastewater-Surveillance.aspx", 
              "CDPH Wastewater Surveillance Webpage", 
              target = "_blank",
              class  = "text16"
            )
          ),
          tags$p(
            tags$a(
              href   = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/RespiratoryVirusReport.aspx",
              "CDPH Respiratory Virus Report", 
              target = "_blank",
              class  = "text16"
            )
          ),
          tags$br(),
          tags$p(
            "Developed in ",
            tags$a(href = "https://shiny.posit.co", "R-Shiny", target = "_blank"),
            class = "text18"
          ),
          tags$p(
            "Version released on Oct 16, 2024",
            class = "text18"
          )
        )
      )
    ),
    
    ## DashboardBody -----------------------------------------------------------
    dashboardBody(
      # Include external CSS styles from styles.css located in the www folder
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      ### Home page ---------------------------------------------------------------
      tabItems(
        # Home/Dashboard tab
        tabItem(tabName = "dashboard",
                # Left column for main content text and dynamic updates
                column(8,
                       h2(style = "font-size: 35px; color:  #3c3d45", "Statewide Overview for the Last 21 Days"),
                       fluidRow(
                         div(class = "col-md-6 col-sm-10", uiOutput("home_covid"))
                       ),
                       fluidRow(
                         column(12,
                                h2(style = "font-size: 40px; color: #3c3d45", strong("Welcome to the California Wastewater Surveillance Dashboard")),
                                p(style = "font-size: 20px;", tags$i(tags$b("Note:"), "We recommend viewing this dashboard on a full computer screen. It has not yet been optimized for mobile devices.")),
                                br(),
                                p(style = "font-size: 22px;", "This dashboard provides an overview of data from testing wastewater for the SARS-CoV-2 virus in California. This data is generated by those participating in the California Department of Public Health (CDPH) California Surveillance of Wastewaters (Cal-SuWers) network."),
                                p(style = "font-size: 22px;", "Contributors to this network include the CDPH Cal-SuWers Program, WastewaterSCAN, the CDC National Wastewater Surveillance System (NWSS), wastewater utilities, and academic, laboratory, and other state and federal partners."),
                                p(style = "font-size: 22px;  margin-bottom: 30px;", "The CDPH Cal-SuWers program and network participate in the Centers for Disease Control and Prevention (CDC) National Wastewater Surveillance System (NWSS)."),
                                h2(style = "font-size: 27px; color: #3c3d45", strong("Data Available")),
                                tags$ul(
                                  tags$li(style = "font-size: 21px;", tags$b("Pathogens:"), " SARS-CoV-2 (COVID-19)"),
                                  tags$li(style = "font-size: 21px;", tags$b("Metrics:"), " Viral concentrations, levels, and trend analyses for SARS-CoV-2"),
                                  tags$li(style = "font-size: 21px;", tags$b("Coverage Areas:"), " Participating sewersheds and aggregates of participating sites across the state and for each health officer region.")
                                ),
                                h2(style = "font-size: 27px; color: #3c3d45", strong("Last Data Update")),
                                uiOutput("publish_note"),
                                p(
                                  style = "font-size: 22px;", 
                                  "For more information, please visit our ", 
                                  actionLink(inputId = "about_dashboard_link", "About the Dashboard", style = "color: #00008B;"), 
                                  " and ", 
                                  actionLink(inputId = "instructions_link", "Instructions", style = "color: #00008B;"), 
                                  " sections. If you have any questions, comments, or suggestions, please contact us at ", 
                                  a(href = "mailto:Wastewatersurveillance@cdph.ca.gov", "Wastewatersurveillance@cdph.ca.gov", style = "color: #00008B;"), 
                                  ".")
                         )
                       )
                ),
                column(width = 4,
                       tags$div(style = "margin-top: 70px;"),
                       div(
                         id = "dash_update_panel_container",
                         # Custom collapsible panel heading for dashboard updates
                         div(
                           class = "panel-header",
                           "DASHBOARD UPDATES",  # Panel title text
                           id = "custom_toggle"  # ID for JavaScript toggle control
                         ),
                         # Hidden toggle button (used by JavaScript)
                         actionButton("toggle_btn", "Collapsible Panel"),
                         # Conditional panel to show/hide the dashboard updates content
                         conditionalPanel(
                           condition = "input.toggle_btn % 2 != 1 || input.toggle_btn == 0",
                           div(
                             id = "scrollable_content",
                             uiOutput("dash_update")  # Dynamic dashboard update content
                           )
                         ),
                         # JavaScript to trigger the hidden toggle button when the header is clicked
                         tags$script(HTML("
                                  document.getElementById('custom_toggle').onclick = function() {
                                    $('#toggle_btn').click();
                                  };
                                "))
                       )
                )
        ),
        
        ### Respiratory Virus Data ---------------------------------------------------------
        tabItem(tabName = "overview",
                # Dynamic title for the Respiratory Virus Data page
                fluidRow(
                  div(class = "dynamic-title", uiOutput("dynamic_title"))
                ),
                br(),
                fluidRow(
                  column(
                    width = 12,
                    div(style = "background-color: #99b6cf; padding: 12px; border-radius: 5px;",
                        h3(uiOutput("overview_title"),
                           style = "text-align: left; margin: 0; font-weight: bold; color: #333;")
                    )
                  )
                ),
                br(),
                # Fluid row with two columns: one for the leaflet map and one for the level box/plot
                fluidRow(
                  tags$head(tags$style(HTML(".small-box {height: 160px}"))),
                  tags$style(HTML("
                                            .bg-custom-trendinfo { background-color: #e3dcdc !important; color: #000000 !important; border: 5px solid #3c3d45; border-radius: 8px; }
                                            .bg-custom-covid_high { background-color: #C8534A !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                                            .bg-custom-covid_medium { background-color: #FEC309 !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                                            .bg-custom-covid_low { background-color: #118987 !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                                            .bg-custom-covid_no_data { background-color: #969696 !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                                            .bg-custom-flu_decrease { background-color: #67a867 !important; color: #000000 !important; border: 5px solid #3c3d45; border-radius: 8px;}
                                            .bg-custom-flu_strong_increase { background-color: #FF7F7F !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                                            .bg-custom-flu_increase { background-color: #FFA07A !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                                            .bg-custom-flu_very_strong_increase { background-color: #CD5C5C !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                                            .bg-custom-flu_plateau { background-color: #d4ac77 !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                                            .bg-custom-flu_no_data { background-color: #969696 !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                                            .bg-custom-flu_sporadic_detections { background-color: #D3C7A0 !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                                            .bg-custom-flu_all_below_lod { background-color: #D3D3D3 !important; color: #000000 !important;  border: 5px solid #3c3d45; border-radius: 8px;}
                                          ")),
                  # Left column: Leaflet map with loading spinner and accessibility attributes
                  column(
                    6,
                    div(
                      class = "loading",
                      `data-loading` = "true",
                      tags$div(
                        role = "img",
                        `aria-label` = "Heatmap showing wastewater trend and level",
                        leafletOutput("heatmap_region", height = "780px") %>% withSpinner(color = "#5A789A")
                      )
                    )
                  ),
                  # Right column: Level box and dynamic wastewater plot
                  column(
                    6,
                    div(class = "level-box-container", uiOutput("level_box")),
                    uiOutput("level_box"),
                    div(
                      class = "loading",
                      `data-loading` = "true",
                      tags$div(
                        role = "img",
                        `aria-label` = "Plot showing normalized virus concentration over time in wastewater",
                        uiOutput("dynamic_wwtp_plot") %>% withSpinner(color = "#5A789A")
                      )
                    )
                  )
                ),
                br(),
                # Table section: Header and collapsible data table
                titlePanel(
                  uiOutput("table_header")
                ),
                bsCollapse(
                  id = "collapseTable",
                  open = "panel1",
                  bsCollapsePanel(
                    tags$span("Show/Hide Table", style = "font-size: 24px; font-weight: bold;"),
                    DT::dataTableOutput("covid_table") %>% withSpinner(color = "#5A789A"),
                    value = "panel1"
                  )
                )
        ),
        ### About this dashboard ----------------------------------------------------
        tabItem(tabName = "technical_notes",
                div(
                  class = "technical-notes-section", 
                  h2("About the Dashboard Page", style = "font-size: 35px;"),
                  p("This dashboard displays an overview of wastewater surveillance data for SARS-CoV-2 (COVID-19)."),
                  p("The California Surveillance of Wastewaters (Cal-SuWers) Network collects and analyzes wastewater samples. Partners include CDPH, utilities, and academic and laboratory groups."),
                  p("For more information on how wastewater surveillance works please visit our ",
                    tags$a(href = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/COVID-19/Wastewater-Surveillance.aspx", "page", target = "_blank", style = "color: #00008B;"),
                    "."),
                  h3("Concentrations"),
                  p('We measure how much of a pathogen (e.g., the SARS-CoV-2 virus) is present in a sample of wastewater; this is also known as the “concentration”.'),
                  h3("Sewersheds"),
                  p('Maps show outlines of wastewater treatment plant service areas (sewersheds) representing the collection areas.'),
                  h3("Levels"),
                  p('Wastewater concentrations are categorized into levels (Low, Medium, High) based on data distributions.'),
                  p("If there is insufficient data, a level of ", 
                    tags$b(style = "background-color: #969696; color: black; padding: 2px 4px;", "Not enough data"),
                    " is assigned."),
                  br(),
                  fluidRow(
                    column(
                      dataTableOutput("levelsTable"), width = 8, style = "font-size:18px"
                    )
                  ),
                  br(),
                  p(
                    "*If there has been no sample in the past 10 days OR the site does not have 6 months of data, ",
                    tags$b(style = "background-color: #969696; color: black; padding: 2px 4px;", "Not enough data"),
                    " will be assigned."
                  ),
                  h3("Trends"),
                  p("Trends are calculated from the average percent change in wastewater levels over the past 21 days."),
                  fluidRow(
                    column(
                      dataTableOutput("trendsTable"), width = 6, style = "font-size:18px"
                    )
                  ),
                  br(),
                  p(
                    "*If there has been insufficient recent data, trend icons (",
                    icon("times"),
                    ") indicate 'Not enough data' or 'Sporadic Detections' on the heatmap."
                  ),
                  h3("Regions"),
                  p("Wastewater data from counties are grouped into five public health officer regions."),
                  p(tags$a(href = "#", "Click here to see a list of California counties by region",
                           style = "color: #00008B;",
                           onclick = "$('#regionDetails').collapse('toggle'); return false;")),
                  div(
                    id = "regionDetails",
                    class = "collapse",
                    tags$ul(
                      lapply(seq_along(county_list), function(i) {
                        tags$li(
                          tags$b(paste(names(county_list)[i], ": ")), 
                          paste(county_list[[i]], collapse = ", ")
                        )
                      })
                    )
                  ),
                  h3("Data Sources"),
                  p("Data is contributed by multiple groups, including CDPH and WastewaterSCAN."),
                  tags$ul(
                    tags$li("CDPH Drinking Water and Radiation Lab (DWRL)"),
                    tags$li(tags$a(href = "https://wastewaterscan.org/", "Wastewater SCAN", target = "_blank", style = "color: #00008B;")),
                    tags$li(tags$a(href = "https://healthycvtogether.org/", "Healthy Central Valley Together", target = "_blank", style = "color: #00008B;")),
                    tags$li(tags$a(href = "https://www.cdc.gov/nwss/wastewater-surveillance.html", "CDC NWSS Contract", target = "_blank", style = "color: #00008B;"))
                  ),
                  h3("Dashboard Updates"),
                  p("This dashboard is updated daily, Monday–Friday, by 5:30pm."),
                  h3("Dashboard Turnaround Time"),
                  p("The turnaround time from sample collection to dashboard display is usually 3–10 days."),
                  h3("Data Limitations"),
                  p("Wastewater surveillance is a developing field. Please keep in mind the inherent variability in the data.")
                )
        ),
        
        ### Dashboard Instructions --------------------------------------------------
    
        tabItem(tabName = "instructions",
                # Container div with a custom class to apply specific styles for the instructions section
                div(class = "instructions-section", 
                    
                    # Main heading for the instructions page with a large font size
                    h2("Dashboard Instructions", style = "font-size: 35px;"),
                    
                    # Subheading for the first instruction group: Selecting the Data
                    h3("Selecting the Data", style = "font-size: 25px;"),
                    
                    # Step 1: Select pathogen
                    h4(strong("Step 1:"), " Select pathogen."),
                    tags$ul(
                      # List item explaining that the user should choose the pathogen from the dropdown;
                      # currently, only SARS-CoV-2 is available.
                      tags$li('Begin by selecting the pathogen from the "Select pathogen" dropdown menu. At this time, only data for SARS-CoV-2 is available. Additional pathogens will be added in the coming months.')
                    ),
                    
                    # Step 2: Choose the data view (site-specific or regional)
                    h4(strong("Step 2:"), " Show Data By."),
                    tags$ul(
                      # List item prompting the user to select the view type from the sidebar.
                      tags$li('Select your desired view from the "Show data by" dropdown menu on the left-hand column.'),
                      tags$ul(
                        # Nested list detailing the two available options:
                        tags$li('To view data at the site-specific level, select "Sewershed".'),
                        tags$li('To view data aggregated at the regional level, select "Region".')
                      )
                    ),
                    
                    # Step 3: Select a region for regional data
                    h4(strong("Step 3:"), " Select Region."),
                    tags$ul(
                      # First list item instructs the user to filter data using the "Select region" filter.
                      tags$li('Filter available data at the regional level using the "Select region" filter.'),
                      # Second list item explains the five public health officer regions in California.
                      tags$li('California has five (5) public health officer regions: Bay Area (ABAHO), Greater Sacramento, Central/San Joaquin Valley (SJVC), Northern CA (RANCHO), and Southern CA (SOCAL).'),
                      # Third list item includes an action link to the "About the dashboard page" for additional details.
                      tags$li(
                        'For a list of California counties by region, view the ',
                        actionLink(inputId = "about_dashboard_link", "About the dashboard page", style = "color: #00008B;"),
                        '.'
                      )
                    ),
                    
                    # Step 4: Choose a specific sewershed if viewing site-specific data
                    h4(strong("Step 4:"), " Choose a Wastewater Sewershed listed by County (Utility Name)."),
                    tags$ul(
                      # Instructs the user that if "Sewershed" view is selected, they can pick a specific sewershed.
                      tags$li('If you selected to show data by sewershed in Step 2, you now have the option to select a Sewershed by County (Utility Name) from the dropdown list.')
                    ),
                    
                    # A Tip section explaining alternative methods to view sewershed details
                    p(strong(em("Tip:")), em("You can also hover over a sewershed area on the map to see the name, level, and trend for that site. You may also select a sewershed by clicking on the map to view data. However, please note that the sites on the map may be fewer than those in the dropdown due to the lack of shapefiles for all sewersheds.")),
                    br(),
                    
                    # Section for "Viewing the Data" after selections have been made
                    h3("Viewing the Data", style = "font-size: 25px;"),
                    p("Once you have selected a site or region, the summary text and plot will populate on the right-hand of the dashboard."),
                    
                    # Step 1 for Viewing Data: Adjusting the date range on the custom timescale plot
                    h4(strong("Step 1:"), "Adjust the Custom Timescale Plot to a desired date range"),
                    tags$ul(
                      tags$li("The default view of the plot is to show the entire time series of available data for that site or region."),
                      tags$li("To adjust the date range, use the slider tool located under the plot to change the view to a desired time period.")
                    ),
                    
                    # Tip for resetting the plot's zoom and axis settings using icons
                    p(
                      strong(em("Tip:")), 
                      em("To reset the plot, click the reset icon ("), 
                      icon("refresh"),  # Reset icon
                      em(") below the zoom in ("), 
                      icon("plus"),      # Zoom in icon
                      em(") and zoom out ("), 
                      icon("minus"),     # Zoom out icon
                      em(") icons.")
                    ),
                    
                    # Step 2 for Viewing Data: Optionally toggling display of individual data points
                    h4(strong("Step 2:"), "For each sewershed site, you can choose to display individual data points by toggling the \"Include Data Points\" button."),
                    tags$ul(
                      tags$li("Horizontal dashed reference lines indicate the level cut-offs to determine low, medium, and high categories."),
                      tags$li("A vertical reference line labeled \"21 days ago\" shows trends calculated from data collected in the past 21 days."),
                      tags$li("The line represents an average of the concentrations of all samples that fall within 10 days of that date (center aligned)."),
                      tags$li("Points on the plot represent individual sample results:"),
                      tags$ul(
                        tags$li("Hovering over data points will display the normalized concentration."),
                        tags$li(HTML("Data points above the y-axis limit are marked with a closed upward-pointing triangle (&#9650;); otherwise, they are shown as circular symbols (&#9679;).")),
                        tags$li(HTML("Data points with low concentrations are represented by an open downward-pointing triangle (&#x25BD;), indicating that they are below the limit of detection for laboratory analysis."))
                      )
                    ),
                    
                    # Additional tip on how to view another sewershed
                    p(
                      tags$i(
                        tags$b("Tip:"), 
                        " To view another sewershed you can either select a new sewershed by clicking on the map or select another site from the \"Choose a Wastewater Sewershed by County (Utility Name)\" filter on the left-hand column."
                      )
                    ),
                    br(),
                    
                    # Section for "Downloading the Data"
                    h3("Downloading the Data", style = "font-size: 25px;"),
                    p(
                      "To download and view the entire dataset (default setting), simply select all columns, and avoid applying any filters in the data table. If you'd like to download and view a specific subset of the data, select the relevant columns, and apply the necessary filters before downloading."
                    ),
                    br(),
                    br()
                )
        ),
        
        ### Data Download UI --------------------------------------------------------
        tabItem(tabName = "download",
                fluidPage(
                  fluidRow(
                    # Main content area for dataset download
                    column(
                      width = 12,
                      tabsetPanel(
                        id = 'dataset',
                        # Wastewater Data tab panel
                        tabPanel("Wastewater Data",
                                 br(),
                                 DT::DTOutput("download_table1") %>% withSpinner(color = "#5A789A")
                        ),
                        # Metrics Summary Data tab panel
                        tabPanel("Metrics Summary Data",
                                 br(),
                                 DT::DTOutput("download_table2") %>% withSpinner(color = "#5A789A")
                        )
                      )
                    )
                  )
                )
        )
      )
    )
  )
)
