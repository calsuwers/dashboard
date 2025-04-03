server <- function(input, output, session) {
  
  # ----------------------- Technical Notes -----------------------
  # Render a DataTable for the "levels" information using the levels_data dataset.
  output$levelsTable <- renderDT({
    datatable(levels_data, 
              options = list(
                dom = 't',            # Only display table without extra controls
                ordering = FALSE,     # Disable ordering
                paging = FALSE,       # Disable pagination
                searching = FALSE,    # Disable searching
                columnDefs = list(
                  list(width = '80px', targets = 0),  # Set width for first column
                  list(width = '80px', targets = 1),  # Set width for second column
                  list(className = 'dt-center', targets = "_all")  # Center align all cells
                )
              ), 
              rownames = FALSE, 
              colnames = c('Current Concentration', 'Wastewater Level'),
              elementId = "note-level-table"  # Unique ID for the table
    ) %>%
      formatStyle(
        'Current.Concentration',
        target = 'cell',
        backgroundColor = styleEqual(names(covid_map_colors), covid_map_colors)
      ) %>%
      formatStyle(
        columns = names(levels_data),  # Apply style to all columns
        fontSize = '20px'  # Increase font size
      )
  })
  
  # Render a DataTable for the "trends" information using the trends_data dataset.
  output$trendsTable <- renderDT({
    datatable(trends_data, options = list(dom = 't', ordering = FALSE, paging = FALSE, searching = FALSE, 
                                          columnDefs = list(list(width = '50px', targets = "_all",
                                                                 className = 'dt-center')
                                          )
    ), 
    rownames = FALSE, 
    colnames = c('21-day Percent Change Estimate', 'Trend Category', 'Trend Symbol on the Map')
    ) %>%
      formatStyle(
        columns = names(trends_data),  # Apply styling to all columns
        fontSize = '20px'
      )
  })
  
  # ----------------------- Instructions Navigation -----------------------
  # Observe clicks on "About the Dashboard" link and update the sidebar to show technical notes.
  observeEvent(input$about_dashboard_link, {
    updateTabItems(session, "sidebar", selected = "technical_notes")
  })
  
  # Observe clicks on "Instructions" link and update the sidebar accordingly.
  observeEvent(input$instructions_link, {
    updateTabItems(session, "sidebar", selected = "instructions")
  })
  
  # Observe clicks on "Respiratory Virus Data" link and update the sidebar to show overview.
  observeEvent(input$virus_link, {
    updateTabItems(session, "sidebar", selected = "overview")
  })
  
  # ----------------------- ShinyAlert Section -----------------------
  # Set parameters for a Shiny alert modal.
  alert_params = list(
    title = HTML('<div style="font-size: 26px; color: #000;">Announcement</div>'),
    text = HTML('<div style="font-size: 20px; color: #000;">The updated dashboard is currently under review and may undergo additional changes over the coming weeks</div>'),
    type = "info",
    html = TRUE,
    closeOnClickOutside = FALSE,
    showConfirmButton = TRUE,
    confirmButtonText = "Close",
    className = 'alert'
  )
  
  # Show the alert modal when the announcement button is clicked.
  observeEvent(input$announce_button, {
    do.call(shinyalert, alert_params)
  })
  
  # ----------------------- Dashboard Update Panel -----------------------
  # Dynamically generate UI for dashboard update entries.
  output$dash_update <- renderUI({
    formatted_dates <- dash_update_data$date  # Format dates if needed
    # Create a list of update entries
    entries <- lapply(1:nrow(dash_update_data), function(i) {
      div(class = "entry",
          tags$b(formatted_dates[i]), tags$br(),
          tags$b(dash_update_data$title[i]), tags$br(),
          HTML(dash_update_data$message[i])
      )
    })
    # Return the entries as a tag list
    do.call(tagList, entries)
  })
  
  # ----------------------- Homepage Info Box -----------------------
  # Render a UI element for the home COVID info box.
  output$home_covid <- renderUI({
    border_color <- getStateColor(target = "n", category = covid_state_level)
    div(
      class = "small-box",
      style = paste("background-color:", border_color, ";  color: black; border: 5px solid #3c3d45; border-radius: 8px; height: 160px;"),
      div(class = "inner",
          h3(rename_pathogen("n")),
          tags$div(
            class = "covid-text",
            tags$b(paste0("Level: ", covid_state_level)), br(),
            tags$b(paste0("Trend: ", covid_state_trend)), br(),
            tags$b(paste0("Last Update: ", published_date_state$n))
          )
      )
    )
  })
  
  # ----------------------- Reactive Data Filters -----------------------
  # Create reactive expressions to filter data based on the selected pathogen.
  targetlist = setNames(c("SARS-CoV-2", "Flu A", "Flu B", "RSV"), c("n", "infa", "infb", "rsv"))
  target = reactive({ names(targetlist[targetlist == input$pathogen]) })
  
  # Render the dynamic title for the overview tab.
  output$overview_title = renderUI({
    paste("Trends and Levels by ", input$select_plot, " (Last Update: ", 
          published_date[[input$select_plot]][[target()]], ")")
  })
  
  # Reactive expressions to filter various datasets (c1, c2, c3, c4, c5)
  c11 <- reactive({
    datafilter(c1, value = target())
  })
  c22 <- reactive({
    datafilter(c2, value = target())
  })
  c33 <- reactive({
    datafilter(c3, value = target())
  })
  c44 <- reactive({
    datafilter(c4, value = target())
  })
  c55 <- reactive({
    datafilter(c5, value = target())
  })
  
  
  # Render the table header for the summary table.
  output$table_header <- renderUI({
    h2(strong(paste0("Trend and Level Summary Table for Each Sewershed (Last Update: ", 
                     published_date[["Sewershed"]][[target()]], ")")), align = "center")
  })
  
  # Render the publish note with update dates.
  output$publish_note <- renderUI({
    p(
      style = "font-size: 20px;",
      strong(rename_pathogen("n")), ": ", published_date_state$n, " for statewide; ", 
      published_date_region$n, " for regions; ", published_date_wwtp$n, " for sewersheds.",
      tags$br(),
      tags$i("Please note that the state metrics may be updated at a different time than the regional metrics.")
    )
  })
  
  # Reactive expression to calculate the factor for ordering wastewater treatment plants.
  wwtp_factor = reactive({
    as.data.frame(c33()) %>%
      select(wwtp_name) %>%
      distinct() %>% pull(wwtp_name)
      # select(wwtp_name, Shape_Area) %>%
      # select(wwtp_name, Shape_Area) %>%
      # distinct() %>%
      # arrange(-Shape_Area) %>% pull(wwtp_name)
  })
  
  # Render the dynamic title for the overview page.
  output$dynamic_title <- renderUI({
    req(target(), published_date_region, published_date_state, published_date_wwtp)
    titlePanel(strong(paste0(" Statewide Wastewater Surveillance ")))
  })
  
  filtered_labels <- reactive({
    req(input$HO)  # Ensure input$HO is available before rendering the UI
    sort(na.omit(unique(c22() %>% filter(region == regname(input$HO)) %>% .$Label_Name)))
  })
  
  # Render the dynamic wastewater treatment plant picker when in "Sewershed" mode.
  output$dynamic_wwtp_picker <- renderUI({
    if (input$select_plot == "Sewershed") {
      selectInput(
        inputId = "select_wwtp",
        label = span(
          "Choose a Wastewater Sewershed by County (Utility Name) ",
          div(id = "sewershed_dropdown",
              style = "display:inline-block;",
              title = "Sites on the map may be fewer than in the dropdown due to missing shapefiles",
              icon("info-circle"))
        ),
        choices = filtered_labels(),
        selected = filtered_labels()[1],
        multiple = FALSE
      )
    }
  })
  
  output$dynamic_wwtp_plot <- renderUI({
    
    if (input$select_plot == "Sewershed") {
      # Create the selectInput UI dynamically
      div(
        id = "sewershed-plot-tabset",
        tabsetPanel(
          id = "tabset",
          tabPanel("Wastewater Plot",
                   plotlyOutput("wwplot_metric_scale", height = "550px") %>% withSpinner(color = "#5A789A")
          )#,
          # tabPanel("Short/Long term Plot",
          #          plotlyOutput("wwplot_metric", height = "550px") %>% withSpinner(color = "#5A789A")
          # )
        ),
        div(
          class = "switch-container",
          span("Include Data Points:", class = "switch-label"),
          tags$input(id = "include_data", type = "checkbox", class = "switch-input")
        )
      )
    } else if (input$select_plot == "Region") {
      plotlyOutput(outputId = "wwplot_metric_scale", height = "600px") %>% withSpinner(color = "#5A789A")
    }
  })
  # Render dynamic wastewater plot UI based on selection mode.
  output$dynamic_wwtp_plot <- renderUI({
    if (input$select_plot == "Sewershed") {
      div(
        id = "sewershed-plot-tabset",
        tabsetPanel(
          id = "tabset",
          tabPanel("Wastewater Plot",
                   plotlyOutput("wwplot_metric_scale", height = "550px") %>% withSpinner(color = "#5A789A")
          )
        ),
        # Toggle switch for including data points
        div(
          class = "switch-container",
          span("Include Data Points:", class = "switch-label"),
          tags$input(id = "include_data", type = "checkbox", class = "switch-input")
        )
      )
    } else if (input$select_plot == "Region") {
      plotlyOutput(outputId = "wwplot_metric_scale", height = "600px") %>% withSpinner(color = "#5A789A")
    }
  })
  
  # ----------------------- Map and Plot Reactive Expressions -----------------------
  # Reactive expression to filter data for mapping based on selected view (Region or Sewershed).
  mapdf = reactive({
    if(input$select_plot == "Region"){
      state_df %>% filter(!is.na(wwtp_name), pcr_gene_target == target(), report_include == T)
    } else if (input$select_plot == "Sewershed")  {
      c33() %>% filter(region == regname(input$HO), !is.na(wwtp_name), pcr_gene_target == target(), report_include == T) %>%
        mutate(wwtp_name = factor(wwtp_name, levels = wwtp_factor())) %>%
        arrange(wwtp_name)
    }
  })
  
  # Additional reactive expressions compute trend counts, percentages, and summary strings.
  trend_vec <- reactive({
    table(c55()$trend)
  })
  
  names_trend <- reactive({
    names(trend_vec())
  })
  
  trend_decrease <- reactive({
    sum(trend_vec()[str_detect(names_trend(), regex("Decrease", ignore_case = T))])
  })
  
  trend_increase <- reactive({
    sum(trend_vec()[str_detect(names_trend(), regex("Increase", ignore_case = T))])
  })
  
  trend_plateau <- reactive({
    sum(trend_vec()[str_detect(names_trend(), regex("Plateau", ignore_case = T))])
  })
  
  trend_low <- reactive({
    sum(trend_vec()[str_detect(names_trend(), regex("(Sporadic Detections|All samples below LOD)", ignore_case = T))])
  })
  
  not_enough <- reactive({
    sum(trend_vec()[str_detect(names_trend(), "Not enough data")])
  })
  
  # Calculate total sites reporting data in the last 21 days.
  demon <- reactive({
    sum(trend_increase(), trend_decrease(), trend_plateau(), trend_low())
  })
  
  total_sites <- reactive({
    paste0("Number of sites reporting data in past 21 days: ", demon())
  })
  
  # Create summary strings for increasing, decreasing, plateauing, and insufficient data trends.
  increasing <- reactive({
    paste0("Increasing at ", trend_increase(), "/", demon(), " sites (", round(trend_increase()/demon()*100, 0), "%)")
  })
  
  decreasing <- reactive({
    paste0("Decreasing at ", trend_decrease(), "/", demon(), " sites (", round(trend_decrease()/demon()*100, 0), "%)")
  })
  
  plateauing <- reactive({
    paste0("Plateauing at ", trend_plateau(), "/", demon(), " sites (", round(trend_plateau()/demon()*100, 0), "%)")
  })
  
  not_enough_2 <- reactive({
    paste0("Concentrations too low to define trends ", trend_low(), "/", demon(), " sites (", round(trend_low()/demon()*100, 0), "%)")
  })

  # ----------------------- Map Legend and Marker Setup -----------------------
  # Reactive expressions to create icon lists and marker labels for the leaflet map.
  z = reactive({
    if(input$select_plot == "Region"){
      list("lng" = -120.3384, "lat" = 37.06523, "zoom" = 6)
    } else if (input$select_plot == "Sewershed")
    {
      list("lng" = zoomlist[[regname(input$HO)]][1],
           "lat" = zoomlist[[regname(input$HO)]][2],
           "zoom" = zoomlist[[regname(input$HO)]][3])
    }
  })
  
  iconList <- reactive({
    if(target() %in% "n"){
      awesomeIcons(
        icon = ifelse(str_detect(mapdf()$trend, "Increase"), "arrow-up",
                      ifelse(str_detect(mapdf()$trend, "Plateau"), "minus",
                             ifelse(str_detect(mapdf()$trend, "Decrease"), "arrow-down", "times"))),
        iconColor = "black",
        markerColor = ifelse(str_detect(mapdf()$level, "High"), "darkred",
                             ifelse(str_detect(mapdf()$level, "Medium"), "orange",
                                    ifelse(str_detect(mapdf()$level, "Low"), "green", "gray"))),
        library = "fa"
      )
    } else if (target() %in% c("infa", "rsv", "infb")) {
      awesomeIcons(
        icon = ifelse(str_detect(mapdf()$trend2, "Very Strong Increase"), "angle-double-up",
                      ifelse(str_detect(mapdf()$trend2, "Strong Increase"), "arrow-up",
                             ifelse(str_detect(mapdf()$trend2, "Increase"), "arrow-up",
                                    ifelse(str_detect(mapdf()$trend2, "Plateau"), "minus",
                                           ifelse(str_detect(mapdf()$trend2, "Decrease"), "arrow-down",
                                                  ifelse(str_detect(mapdf()$trend2, "Sporadic Detections"), "ellipsis-h",
                                                         ifelse(str_detect(mapdf()$trend2, "All Samples Below LOD"), "times", "question"))))))),
        iconColor = "white",
        markerColor = ifelse(str_detect(mapdf()$trend2, "Very Strong Increase"), "darkred",
                             ifelse(str_detect(mapdf()$trend2, "Strong Increase"), "red",
                                    ifelse(str_detect(mapdf()$trend2, "Increase"), "darkorange",
                                           ifelse(str_detect(mapdf()$trend2, "Plateau"), "darkorange",
                                                  ifelse(str_detect(mapdf()$trend2, "Decrease"), "green",
                                                         ifelse(str_detect(mapdf()$trend2, "Sporadic Detections"), "orange",
                                                                ifelse(str_detect(mapdf()$trend2, "All Samples Below LOD"), "white", "gray"))))))),
        library = "fa"
      )
    }
  })
  
  sewershed_label <- reactive({
    if(target() %in% "n"){
      sprintf("<strong>%s</strong><br/>%s<br/>%s",
              mapdf()$wwtp_name,
              paste0("Level: ", mapdf()$level),
              paste0("Trend: ", mapdf()$trend)) %>% lapply(htmltools::HTML)
    } else if (target() %in% c("infa", "rsv", "infb")) {
      sprintf("<strong>%s</strong><br/>%s",
              mapdf()$wwtp_name,
              paste0("Trend: ", mapdf()$trend)) %>% lapply(htmltools::HTML)
    }
  })
  
  pal_value <- reactive({
    if(target() %in% "n"){
      ~pal[[target()]](mapdf()$level)
    } else if (target() %in% c("infa", "rsv", "infb")) {
      ~pal[[target()]](mapdf()$trend2)
    }
  })
  
  map_legend <- reactive({
    if(target() %in% "n"){
      HTML('<div style="background: rgba(255, 255, 255, 0.4); padding: 9px; border-radius: 5px;">
               <strong>Trend</strong><br>
               <i class="fa fa-arrow-up" style="color:black;"></i> Increase<br>
               <i class="fa fa-minus" style="color:black;"></i> Plateau<br>
               <i class="fa fa-arrow-down" style="color:black;"></i> Decrease<br>
               <i class="fa fa-times" style="color:black;"></i> Insufficient Data<br><br>
               <strong>Level</strong><br>
               <div style="display: flex; align-items: center;">
                 <div style="background-color: darkred; width: 20px; height: 20px; margin-right: 8px;"></div> High<br>
               </div>
               <div style="display: flex; align-items: center;">
                 <div style="background-color: orange; width: 20px; height: 20px; margin-right: 8px;"></div> Medium<br>
               </div>
               <div style="display: flex; align-items: center;">
                 <div style="background-color: green; width: 20px; height: 20px; margin-right: 8px;"></div> Low<br>
               </div>
               <div style="display: flex; align-items: center;">
                 <div style="background-color: gray; width: 20px; height: 20px; margin-right: 8px;"></div> Insufficient Data
               </div>
               </div>')
    } else if (target() %in% c("infa", "rsv", "infb")) {
      HTML('<div style="background: rgba(255, 255, 255, 0.4); padding: 9px; border-radius: 5px;">
             <strong>Trend</strong><br>
             <div style="display: flex; align-items: center;">
                <div style="background-color: #CD5C5C; width: 20px; height: 20px; margin-right: 8px; display: flex; justify-content: center; align-items: center; padding-left: 4px;">
                   <i class="fa fa-angle-double-up" style="color:black;"></i>
                </div> Very Strong Increase<br>
             </div>
             <div style="display: flex; align-items: center;">
                <div style="background-color: #FF7F7F; width: 20px; height: 20px; margin-right: 8px; display: flex; justify-content: center; align-items: center; padding-left: 4px;">
                   <i class="fa fa-arrow-up" style="color:black;"></i>
                </div> Strong Increase<br>
             </div>
             <div style="display: flex; align-items: center;">
                <div style="background-color: #FFA07A; width: 20px; height: 20px; margin-right: 8px; display: flex; justify-content: center; align-items: center; padding-left: 4px;">
                   <i class="fa fa-arrow-up" style="color:black;"></i>
                </div> Increase<br>
             </div>
             <div style="display: flex; align-items: center;">
                <div style="background-color: #d4ac77; width: 20px; height: 20px; margin-right: 8px; display: flex; justify-content: center; align-items: center; padding-left: 4px;">
                   <i class="fa fa-minus" style="color:black;"></i>
                </div> Plateau<br>
             </div>
             <div style="display: flex; align-items: center;">
                <div style="background-color: #67a867; width: 20px; height: 20px; margin-right: 8px; display: flex; justify-content: center; align-items: center; padding-left: 4px;">
                   <i class="fa fa-arrow-down" style="color:black;"></i>
                </div> Decrease<br>
             </div>
             <div style="display: flex; align-items: center;">
                <div style="background-color: #D3C7A0; width: 20px; height: 20px; margin-right: 8px; display: flex; justify-content: center; align-items: center; padding-left: 4px;">
                   <i class="fa fa-ellipsis-h" style="color:black;"></i>
                </div> Sporadic Detections<br>
             </div>
             <div style="display: flex; align-items: center;">
                <div style="background-color: #D3D3D3; width: 20px; height: 20px; margin-right: 8px; display: flex; justify-content: center; align-items: center; padding-left: 4px;">
                   <i class="fa fa-times" style="color:black;"></i>
                </div> All Samples Below LOD<br>
             </div>
             <div style="display: flex; align-items: center;">
                <div style="background-color: #969696; width: 20px; height: 20px; margin-right: 8px; display: flex; justify-content: center; align-items: center; padding-left: 4px;">
                   <i class="fa fa-question" style="color:black;"></i>
                </div> Not Enough Data
             </div>
          </div>')
    }
  })
  
  output$heatmap_region <- renderLeaflet({
    # Start with the base map
    map <- leaflet(mapdf()) %>%
      setView(lng = z()$lng, lat = z()$lat, zoom = z()$zoom) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addEasyButton(easyButton(
        icon = "fa-refresh",
        title = "Reset View",
        onClick = JS(paste0(
          "function(btn, map) {
          map.setView([", z()$lat, ", ", z()$lng, "], ", z()$zoom, ");
        }"
        ))
      ))
    
    if (input$select_plot == "Region" && !is.null(pal_value())) {

      map <- map %>%
        addPolygons(
          fillColor = pal_value(),
          weight = 0.5,
          opacity = 1,
          color = "white",
          dashArray = "1",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = sewershed_label(),
          layerId = mapdf()$wwtp_name,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        )
    }
    
    # Continue with remaining layers
    map %>%
      addAwesomeMarkers(
        lat = ~lat,
        lng = ~lng,
        icon = iconList(),
        layerId = ~mapdf()$wwtp_name,
        label = sewershed_label(),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addControl(html = map_legend(), position = "topright")
  })
  
  # Render the leaflet map for regional/sewershed view if polygon data of both region and sewershed are present.
  # output$heatmap_region <- renderLeaflet({
  #   # Create marker labels using bold formatting.
  #   marker_label <- sprintf("<strong>%s</strong>", mapdf()$wwtp_name) %>% lapply(htmltools::HTML)
  #   browser()
  #   leaflet(mapdf()) %>%
  #     setView(lng = z()$lng, lat = z()$lat, zoom = z()$zoom) %>%
  #     addTiles() %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     addPolygons(
  #       fillColor = pal_value(),
  #       weight = 0.5,
  #       opacity = 1,
  #       color = "white",
  #       dashArray = "1",
  #       fillOpacity = 0.7,
  #       highlightOptions = highlightOptions(
  #         weight = 2,
  #         color = "#666",
  #         dashArray = "",
  #         fillOpacity = 0.7,
  #         bringToFront = TRUE
  #       ),
  #       label = sewershed_label(),
  #       layerId = mapdf()$wwtp_name,
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "normal", padding = "3px 8px"),
  #         textsize = "15px",
  #         direction = "auto"
  #       )
  #     ) %>%
  #     addEasyButton(easyButton(
  #       icon = "fa-refresh",
  #       title = "Reset View",
  #       onClick = JS(paste0("
  #         function(btn, map) {
  #           map.setView([", z()$lat, ", ", z()$lng, "], ", z()$zoom, ");
  #         }"))
  #     )) %>%
  #     addAwesomeMarkers(
  #       lat = ~lat,
  #       lng = ~lng,
  #       icon = iconList(),
  #       layerId = ~mapdf()$wwtp_name,
  #       label = sewershed_label(),
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "bold", padding = "3px 8px"),
  #         textsize = "15px",
  #         direction = "auto"
  #       )
  #     ) %>%
  #     addControl(html = map_legend(), position = "topright")
  # })
  
  # Map click events: Zoom in when a marker or polygon is clicked in Sewershed mode.
  observeEvent(input$heatmap_region_marker_click, {
    if(input$select_plot == "Sewershed"){
      click <- input$heatmap_region_marker_click
      loc <- mapdf() %>% filter(wwtp_name == click$id)
      leafletProxy("heatmap_region") %>% setView(lng = loc$lng, lat = loc$lat, zoom = 11)
    }
  })
  
  observeEvent(input$heatmap_region_shape_click, {
    if(input$select_plot == "Sewershed"){
      click <- input$heatmap_region_shape_click
      loc <- mapdf() %>% filter(wwtp_name == click$id)
      leafletProxy("heatmap_region") %>% setView(lng = loc$lng, lat = loc$lat, zoom = 11)
    }
  })
  
  # ----------------------- Main Plot Rendering -----------------------
  # Reactive value for selected sewershed (or region) based on UI interactions.
  rv <- reactiveVal(NULL)
  
  observeEvent(input$select_plot, {
    if(input$select_plot == "Sewershed"){
      req(input$select_wwtp)
      rv(input$select_wwtp)
    }
  })
  
  observeEvent(input$select_wwtp, {
    if(input$select_plot == "Sewershed"){
      req(input$select_wwtp)
      rv(input$select_wwtp)
    }
  })
  
  observeEvent(input$heatmap_region_shape_click, {
    if(input$select_plot == "Sewershed"){
      rv(gsub(" $", "", input$heatmap_region_shape_click$id))
    }
  })
  
  observeEvent(input$heatmap_region_marker_click, {
    if(input$select_plot == "Sewershed"){
      rv(gsub(" $", "", input$heatmap_region_marker_click$id))
    }
  })
  
  # Render the plot for a selected site.
  plot_site_data = reactive({
    req(rv(), target(), input$HO)
    w1 %>% filter(wwtp_name == rv(), region == regname(input$HO),
                  pcr_gene_target == target() ) %>%
      mutate(term = factor(term, c("long", "short")))
  })
  
  # Similar reactive values and observers for regional data.
  re <- reactiveVal(NULL)
  
  observeEvent(input$select_plot, {
    if(input$select_plot == "Region"){
      re(gsub(" $", "", regname(input$HO)))
    }
  })
  
  observeEvent(input$HO, {
    if(input$select_plot == "Region"){
      re(gsub(" $", "", regname(input$HO)))
    }
  })
  
  observeEvent(input$heatmap_region_shape_click, {
    if(input$select_plot == "Region"){
      re(gsub(" $", "", input$heatmap_region_shape_click$id))
    }
  })
  
  observeEvent(input$heatmap_region_marker_click, {
    re(gsub(" $", "", input$heatmap_region_marker_click$id))
  })
  
  # Reactive expression to generate data for metric plotting when the view is "Region"
  metric_plotdf = reactive({
    # Ensure that the region (re()) and target are available
    req(re(), target())
    # If the user selected the "Region" view, filter c1 dataset:
    #   - region must match the reactive value re()
    #   - sample_date must be within the last 365 days
    #   - pcr_gene_target must match the selected target
    if(input$select_plot == "Region"){
      c1 %>% filter(
        region == re(),
        sample_date > max(sample_date) - 365,
        pcr_gene_target == target()
      )
    }
  })
  
  # Conversion factor to scale values (e.g., to millions)
  conversion_factor = 1000000
  
  # Annotation list to add a custom text annotation below the plot.
  # This list is used when multiple annotations might be needed.
  combine_annotation_list <-
    list(
      list(
        x = 0.5,              # Center the text horizontally (50% of paper width)
        y = -0.1,             # Position slightly below the plot area
        yshift = -40,         # Shift text downward to clear the rangeslider area
        text = "Adjust the time range using the slider above",  # Annotation text
        showarrow = FALSE,    # No arrow drawn for this annotation
        xref = "paper",       # x-position relative to the entire plot area
        yref = "paper",       # y-position relative to the entire plot area
        xanchor = 'center',   # Center align the text horizontally
        yanchor = 'top',      # Anchor at the top to avoid overlapping with other elements
        font = list(size = 16, color = "black")  # Set font size and color
      )
    )
  
  # A simpler annotation list containing a single annotation with identical properties.
  single_annotation_list <-
    list(
      list(
        x = 0.5,              # Center text horizontally
        y = -0.1,             # Position just below the plot
        yshift = -40,         # Ensure enough space below the rangeslider
        text = "Adjust the time range using the slider above",  # Annotation text
        showarrow = FALSE,    # Do not display an arrow
        xref = "paper",       # Use paper coordinates for responsive positioning
        yref = "paper",
        xanchor = 'center',   # Center align the text
        yanchor = 'top',      # Anchor at the top to avoid overlap
        font = list(size = 16, color = "black")
      )
    )
  
  # Function to create a stacked bar plot using plotly.
  # It takes three "stack" values representing different levels and an optional legend flag.
  create_bar_plot <- function(stack1, stack2, stack3, show_legend = F,
                              color_low = "#118987",
                              color_med = "#FEC309",
                              color_high = "#C8534A",
                              low_base = 0) {
    bar_plot <- plot_ly() %>%
      # First trace: "Low" level bar
      add_trace(
        x = c("raw concentration normalized by pmmov"),  # Dummy x-axis label
        y = stack1 + low_base,  # Height of the low-level bar
        type = 'bar',
        name = 'Low',
        marker = list(color = color_low, line = list(color = 'white', width = 1.5)),
        base = -low_base,      # Negative base if a base offset is applied
        hoverinfo = "none",    # Disable hover info for the bar
        showlegend = show_legend
      ) %>%
      # Second trace: "Medium" level bar
      add_trace(
        x = c("raw concentration normalized by pmmov"),
        y = stack2,
        type = 'bar',
        name = 'Medium',
        marker = list(color = color_med, line = list(color = 'white', width = 1.5)),
        base = stack1,         # Medium starts on top of the low-level bar
        hoverinfo = "none",
        showlegend = show_legend
      ) %>%
      # Third trace: "High" level bar
      add_trace(
        x = c("raw concentration normalized by pmmov"),
        y = stack3,
        type = 'bar',
        name = 'High',
        marker = list(color = color_high, line = list(color = 'white', width = 1.5)),
        base = stack1 + stack2,  # High starts on top of both low and medium bars
        hoverinfo = "none",
        showlegend = show_legend
      ) %>%
      # Layout configuration: Hide tick labels, grids, and set margins
      layout(
        xaxis = list(showticklabels = FALSE, title = NULL, showgrid = FALSE, zeroline = FALSE),
        yaxis = list(showticklabels = FALSE, title = NULL, showgrid = FALSE, zeroline = FALSE),
        margin = list(l = 0, r = 0, t = 0, b = 50),
        barmode = 'stack'  # Stack bars on top of each other
      )
    
    return(bar_plot)
  }
  
  ### State Main Plot ---------------------------------------------------------------
  # Render an aggregated state-level plot using Plotly.
  output$state_plot = renderPlotly({
    
    # Filter and prepare the state-level data from c1 for the last 365 days
    plotdf = c1 %>% filter(
      region == "State",
      sample_date > max(sample_date) - 365,
      pcr_gene_target == target()
    ) %>%
      mutate(ww_aggregate = ww_aggregate * conversion_factor) %>%  # Scale aggregation values
      rename(`sample date` = sample_date,
             `raw concentration normalized by pmmov` = ww_aggregate) %>%
      arrange(`sample date`)
    
    # Determine horizontal line positions based on quantile values from the data
    h33 = na.omit(unique(plotdf$q33))[1] * conversion_factor
    h66 = na.omit(unique(plotdf$q66))[1] * conversion_factor
    
    # Generate the main line plot using the metric_plot function
    region_plot = metric_plot(
      data = plotdf,
      x_col = "sample date",
      y_col = "raw concentration normalized by pmmov",
      vline_date = max(plotdf$`sample date`) - 21,  # Vertical line 21 days ago
      vline_label_position = max(plotdf$`raw concentration normalized by pmmov`, na.rm = T) * 0.8,
      vline_label_font = list(size = 14),
      hline_y1 = h33,
      hline_y2 = h66,
      plot_title = "",
      y_label = paste0(input$pathogen, "/PMMOV (x1 million)"),
      x_label = "",
      show_h_lines = T,
      margins = list(l = 80, r = 50, t = 10, b = 0),
      show_rangeslider = T,
      ymax = T
    )
    
    # Calculate bar plot components based on quantile values
    stack3 <- max(plotdf[["raw concentration normalized by pmmov"]], na.rm = TRUE) * upper_y_plot_limit
    stack2 <- h66 - h33
    stack1 <- h33
    
    title_margin = list(l = 20, r = 20, t = 90, b = 35)
    
    # If the target is "n" (COVID) and both stack2 and stack3 are defined, combine a bar plot with the line plot.
    if (target() %in% "n" & !is.na(stack2) & !is.na(stack3)) {
      
      bar_plot = create_bar_plot(stack1 = stack1, stack2 = stack2, stack3 = stack3, show_legend = T)
      
      # Combine the bar plot and the region plot side-by-side using subplot.
      combined_plot <- subplot(bar_plot, region_plot, widths = c(0.04, 0.96), shareY = TRUE, margin = 0) %>%
        layout(
          legend = list(
            title = list(text = "    <b>Level</b>"),  # Set legend title
            x = 1.00,
            y = 1
          ),
          title = list(text = paste("State Aggregated Plot for", rename_pathogen(target())),
                       font = list(size = 24, family = "Arial", weight = "bold")),
          margin = title_margin,
          annotations = combine_annotation_list  # Add custom annotation below the plot
        )
      
    } else {
      # If conditions are not met, return the region plot with a single annotation.
      combined_plot <- region_plot %>%
        layout(
          title = list(text = paste("State Aggregated Plot for", rename_pathogen(target())),
                       font = list(size = 24, family = "Arial", weight = "bold")),
          margin = title_margin,
          annotations = single_annotation_list
        )
    }
    
    combined_plot  # Render the final plot
  })
  
  ### WWTP Scalable Plot ---------------------------------------------------------------
  # Render a scalable plot for sewershed or regional view.
  output$wwplot_metric_scale = renderPlotly({
    
    if(input$select_plot == "Region") {
      
      # For the regional view, get data from metric_plotdf reactive expression and scale it.
      plotdf = metric_plotdf() %>%
        mutate(ww_aggregate = ww_aggregate * conversion_factor) %>%
        rename(`sample date` = sample_date,
               `raw concentration normalized by pmmov` = ww_aggregate) %>%
        arrange(`sample date`)
      
      h33 = na.omit(unique(plotdf$q33))[1] * conversion_factor
      h66 = na.omit(unique(plotdf$q66))[1] * conversion_factor
      
      # Create the regional plot using metric_plot
      region_plot = metric_plot(
        data = plotdf,
        x_col = "sample date",
        y_col = "raw concentration normalized by pmmov",
        vline_date = max(plotdf$`sample date`) - 21,
        vline_label_position = max(plotdf$`raw concentration normalized by pmmov`, na.rm = T) * 0.8,
        vline_label_font = list(size = 14),
        hline_y1 = h33,
        hline_y2 = h66,
        plot_title = "",
        y_label = paste0(input$pathogen, "/PMMOV (x1 million)"),
        x_label = "Adjust the time range using the slider above",
        x_label_show = F,
        show_h_lines = T,
        margins = list(l = 80, r = 50, t = 10, b = 40),
        show_rangeslider = T,
        ymax = T,
        range_selector = T
      )
      
      # Calculate bar plot components for annotation (using quantile values)
      stack3 <- max(plotdf[["raw concentration normalized by pmmov"]], na.rm = TRUE) * upper_y_plot_limit
      stack2 <- h66 - h33
      stack1 <- h33
      
      # If the target is COVID ("n") and values are valid, combine a bar plot with the region plot.
      if (target() %in% "n" & !is.na(stack2) & !is.na(stack3)) {
        
        bar_plot = create_bar_plot(stack1 = stack1, stack2 = stack2, stack3 = stack3, show_legend = T)
        
        combined_plot <- subplot(bar_plot, region_plot, widths = c(0.04, 0.96), shareY = TRUE, margin = 0) %>%
          layout(
            legend = list(
              title = list(text = "    <b>Level</b>"),
              x = 1.00,
              y = 1
            ),
            margin = list(l = 0, r = 20, t = 30, b = 40),
            yaxis = list(
              automargin = TRUE  # Allow automatic adjustment of margins
            ),
            annotations = combine_annotation_list
          )
        
      } else {
        # Otherwise, display the region plot with a single annotation.
        combined_plot <- region_plot %>%
          layout(
            margin = list(l = 15, r = 15, t = 30, b = 40),
            annotations = single_annotation_list
          )
      }
      
      combined_plot  # Render the combined plot for the regional view
      
    } else if(input$select_plot == "Sewershed"){
      # For the sewershed view, ensure site-specific data is available.
      req(plot_site_data())
      
      # Create helper data frames for quantile values for the sewershed data.
      df33 <- data.frame(term = c("long", "short"), Z = c(unique(na.omit(plot_site_data()$q33)) * conversion_factor, NA))
      df66 <- data.frame(term = c("long", "short"), Z = c(unique(na.omit(plot_site_data()$q66)) * conversion_factor, NA))
      df21day =  data.frame(term = c("long", "short"), Z = c(as.numeric(max((plot_site_data()$sample_date)) - 21), NA))
      
      # Prepare the plot data by renaming columns and calculating the vertical line (vline)
      plotdf = plot_site_data() %>% rename(
        `sample date` = sample_date,
        `raw concentration normalized by pmmov` = norm_pmmov,
        `10 days rolling average of normalized concentration` = norm_pmmov_ten_rollapply
      )
      plotdf$vline = max(plotdf$`sample date`) - 21
      
      # Group data to compute label positions for annotations on the plot.
      plotdf = plotdf %>% filter(term == "long") %>% group_by(term) %>%
        summarise(label_pos = max(`10 days rolling average of normalized concentration`, na.rm = T) * 0.9) %>%
        bind_rows(
          plotdf %>% filter(term == "short") %>% group_by(term) %>% summarise(label_pos = max(`raw concentration normalized by pmmov`, na.rm = T) * 0.9)
        ) %>% right_join(plotdf)
      
      # Ensure the data's region matches the selected region.
      req(unique(plotdf$region) == regname(input$HO))
      
      # Filter to "long" term data and sort by sample date.
      long_plotdf = plotdf %>% filter(term == "long") %>% arrange(`sample date`)
      
      # Determine a y-axis lower limit based on the data and whether data points are included.
      if(input$include_data & max(long_plotdf[["10 days rolling average of normalized concentration"]], na.rm = T) > 100) {
        y_limit = -10
      } else {
        y_limit = -1
      }
      
      # Generate the main long-term plot using metric_plot
      long_plot =
        metric_plot(
          data = long_plotdf,
          x_col = "sample date",
          y_col = "10 days rolling average of normalized concentration",
          vline_col = "10 days rolling average of normalized concentration",
          vline_date = max(long_plotdf$`sample date`) - 21,
          vline_label_position = unique(long_plotdf$label_pos),
          vline_label_font = list(size = 14),
          hline_y1 = filter(df33, term == "long")$Z,
          hline_y2 = filter(df66, term == "long")$Z,
          plot_title = "",
          y_label = paste0(input$pathogen, "/PMMOV (x1 million)"),
          x_label = "Adjust the time range using the slider above",
          x_label_show = F,
          y_lower_limit = y_limit,
          show_scatter = input$include_data,
          scatter_col = "norm_pmmov_limit",
          point_size = 8,
          margins = list(l = 80, r = 50, t = 10, b = 0),
          show_rangeslider = T,
          ymax = T,
          scatter_hover_text_col = "raw concentration normalized by pmmov",
          scatter_type = "data_type"
        )
      
      # Calculate bar plot components for the sewershed data.
      stack3 <- max(long_plotdf[["10 days rolling average of normalized concentration"]], na.rm = TRUE) * upper_y_plot_limit
      stack2 <- filter(df66, term == "long")$Z - filter(df33, term == "long")$Z
      stack1 <- filter(df33, term == "long")$Z
      
      # Determine a base value for the low-level offset depending on detection limits.
      if(any(long_plotdf$wwtp_name %in% below_LOD_list[[target()]]) & input$include_data)
      {low_base_value2 = low_base_value} else {low_base_value2 = 10}
      
      # If target is COVID and bar plot values are valid, combine bar and line plots.
      if (target() %in% "n" & !is.na(stack2) & !is.na(stack3)) {
        
        bar_plot = create_bar_plot(stack1 = stack1, stack2 = stack2, stack3 = stack3, show_legend = T, low_base = low_base_value2)
        
        combined_plot <- subplot(bar_plot, long_plot, widths = c(0.04, 0.96), shareY = TRUE, margin = 0) %>%
          layout(
            legend = list(
              title = list(text = "    <b>Level</b>"),
              x = 1.00,
              y = 1
            ),
            margin = list(l = 0, r = 20, t = 30, b = 40),
            yaxis = list(
              range = c(-low_base_value2, max(long_plotdf[["10 days rolling average of normalized concentration"]], na.rm = TRUE) * upper_y_plot_limit),
              automargin = TRUE
            ),
            annotations = combine_annotation_list
          )
        
      } else {
        # If conditions are not met, simply layout the long plot with a single annotation.
        combined_plot <- long_plot %>%
          layout(
            margin = list(l = 15, r = 15, t = 30, b = 40),
            annotations = single_annotation_list
          )
      }
      
      combined_plot  # Render the final combined plot for sewershed view
    }
  })
  
  # Info Box ---------------------------------------------------------------
  ### Regional and Sewershed Info Box
  
  # Initialize a reactive value to store the selected sewershed (for Sewershed view)
  rv2 <- reactiveVal(NULL)
  
  # When the view is "Sewershed" and the user selects a wastewater treatment plant (WWTP) from the dropdown,
  # update rv2 with that selection.
  observeEvent(input$select_plot, {
    if(input$select_plot == "Sewershed"){
      req(input$select_wwtp)  # Ensure the input exists
      rv2(input$select_wwtp)
    }
  })
  
  observeEvent(input$select_wwtp, {
    if(input$select_plot == "Sewershed"){
      req(input$select_wwtp)
      rv2(input$select_wwtp)
    }
  })
  
  # Also update rv2 when a marker or shape on the map is clicked (in Sewershed view)
  observeEvent(input$heatmap_region_shape_click, {
    if(input$select_plot == "Sewershed"){
      # Remove any trailing spaces from the clicked marker ID and update rv2
      rv2(gsub(" $", "", input$heatmap_region_shape_click$id))
    }
  })
  
  observeEvent(input$heatmap_region_marker_click, {
    if(input$select_plot == "Sewershed"){
      rv2(gsub(" $", "", input$heatmap_region_marker_click$id))
    }
  })
  
  # Reactive expression to filter the dataset (c3) for a specific sewershed based on the selected region (HO) and WWTP.
  # It ensures that the selected WWTP (stored in rv2) is used to filter the data.
  site_value = reactive({
    if(input$select_plot == "Sewershed"){
      req(input$HO, target(), rv2())
      c3 %>% 
        filter(
          region == regname(input$HO),   # Filter for the selected region (converted to standardized name)
          !is.na(wwtp_name),              # Exclude missing WWTP names
          pcr_gene_target == target(),    # Filter for the selected pathogen
          report_include == T             # Only include data flagged for reporting
        ) %>%
        mutate(wwtp_name = factor(wwtp_name, levels = wwtp_factor())) %>%  # Order WWTPs based on a calculated factor
        arrange(wwtp_name) %>% 
        filter(wwtp_name == rv2())  # Further filter to only the selected WWTP
    }
  })
  
  # For the "Region" view, initialize a separate reactive value to store the selected region.
  rv3 <- reactiveVal(NULL)
  
  # When the view is "Region" and the region selector (HO) changes, update rv3.
  observeEvent(input$select_plot, {
    if(input$select_plot == "Region"){
      req(input$HO)
      rv3(gsub(" $", "", regname(input$HO)))  # Convert region name and remove trailing spaces
    }
  })
  
  observeEvent(input$HO, {
    if(input$select_plot == "Region"){
      req(input$HO)
      rv3(gsub(" $", "", regname(input$HO)))
    }
  })
  
  # Also update rv3 when the map is clicked (either marker or shape) in Region view.
  observeEvent(input$heatmap_region_shape_click, {
    if(input$select_plot == "Region"){
      rv3(gsub(" $", "", input$heatmap_region_shape_click$id))
    }
  })
  
  observeEvent(input$heatmap_region_marker_click, {
    if(input$select_plot == "Region"){
      rv3(gsub(" $", "", input$heatmap_region_marker_click$id))
    }
  })
  
  # For the Region view, create a reactive expression to filter the dataset (c22)
  # to only include aggregated regional data (where WWTP name is missing).
  value_df = reactive({
    if(input$select_plot == "Region"){
      req(rv3(), c22())
      c22() %>% filter(
        region == rv3(),  # Filter by the selected region stored in rv3
        is.na(wwtp_name)  # Only aggregated regional data (no specific WWTP)
      )
    }
  })
  
  # Create a reactive list (value_list) that formats and stores key summary values,
  # depending on whether the view is Region or Sewershed.
  value_list <- reactive({
    if(input$select_plot == "Region"){
      req(value_df())
      # Remove any text in parentheses from the region name
      region = gsub("\\s*\\(.*\\)", "", regname(value_df()$region, reverse = T))
      list(
        title = tags$p(paste0(region, " (Aggregated)"),
                       style = "font-size: 70%; white-space: normal; word-wrap: break-word;"),
        source = value_df()$data_source,
        trend_value = value_df()$trend,
        trend_call = paste0(value_df()$trend, " ", value_df()$model_pc, "% ", "[",
                            value_df()$model_pc_lwr, "%, ", value_df()$model_pc_upr, "%]"),
        level_call = value_df()$level
      )
    } else if(input$select_plot == "Sewershed"){
      req(site_value(), site_value()$Label_Name == rv2())
      list(
        title = tags$p(paste0(site_value()$Label_Name),
                       style = "font-size: 65%; white-space: normal; word-wrap: break-word;"),
        source = site_value()$data_source,
        trend_value = site_value()$trend2,
        trend_call = paste0(site_value()$trend2, " ", site_value()$model_pc, "% ", "[",
                            site_value()$model_pc_lwr, "%, ", site_value()$model_pc_upr, "%]"),
        level_call = site_value()$level
      )
    }
  })
  
  # Create a reactive expression (subtitle_value) that builds a subtitle text for the info box,
  # including level, trend, and data source details.
  subtitle_value <- reactive({
    req(target(), value_list())
    if(target() %in% "n"){
      req(target(), value_list())
      tags$div(
        "Level: ", strong(value_list()$level_call), br(),
        "Trend (compared to 21 days ago): ", strong(value_list()$trend_call), br(),
        "Data Source: ", strong(value_list()$source),
        style = "font-size: 120%; white-space: normal; word-wrap: break-word;"
      )
    } else if (target() %in% c("infa", "rsv", "infb")) {
      tags$div(
        "Trend (compared to 21 days ago): ", strong(value_list()$trend_call), br(),
        "Data Source: ", strong(value_list()$source),
        style = "font-size: 120%; white-space: normal; word-wrap: break-word;"
      )
    }
  })

  # Create a reactive expression (subtitle_value) that builds a subtitle text for the statewide info box,
  # including level, trend, and data source details.
  state_value_df = reactive({
    req(c22())
    c22() %>% filter(region == "State", is.na(wwtp_name))
  })

  state_value_list <- reactive({

    req(state_value_df())
    list(
      title = tags$p("State Aggregated Summary",
                     style = "font-size: 70%; white-space: normal; word-wrap: break-word;"),
      source = state_value_df()$data_source,
      trend_value = state_value_df()$trend,
      trend_call = paste0(state_value_df()$trend, " ", state_value_df()$model_pc, "% ", "[",
                          state_value_df()$model_pc_lwr, "%, ", state_value_df()$model_pc_upr, "%]"),
      level_call = state_value_df()$level
    )
  })

  state_subtitle_value <- reactive({
    req(target(), state_value_list())

    if(target() %in% "n"){
      req(target(), state_value_list())
      tags$div(
        "Level: ", strong(state_value_list()$level_call), br(),
        "Trend (compared to 21 days ago): ", strong(state_value_list()$trend_call), br(),
        "Data Source: ", strong(state_value_list()$source),
        style = "font-size: 120%; white-space: normal; word-wrap: break-word;")

    } else if (target() %in% c("infa", "rsv", "infb")) {

      tags$div(
        "Trend (compared to 21 days ago): ", strong(state_value_list()$trend_call), br(),
        "Data Source: ", strong(state_value_list()$source),
        style = "font-size: 120%; white-space: normal; word-wrap: break-word;")
    }
  })
  
  ### ----------------------- Render Summary Info Boxes -----------------------
  # Render UI elements for site and state info boxes using reactive filtered data.
  output$level_box <- renderUI({
    req(target(), value_list(), subtitle_value())
    target_value <- target()
    value <- value_list()
    box_class <- get_box_class(target_value, value)
    div(
      class = paste("small-box", box_class),
      div(class = "inner",
          h3(value_list()$title),
          p(subtitle_value())
      )
    )
  })
  
  output$state_info_box <- renderUI({
    req(target(), state_value_list(), state_subtitle_value())
    target_value <- target()
    state_values <- state_value_list()
    box_class <- get_box_class(target_value, state_values)
    div(
      class = paste("small-box", box_class),
      div(class = "inner",
          h3(state_value_list()$title),
          p(state_subtitle_value())
      )
    )
  })
  
  output$trend_box <- renderUI({
    # Render a trend summary box with overall site trends.
    box_class <- "bg-custom-trendinfo"
    div(
      class = paste("small-box", box_class),
      div(class = "inner",
          h3(tags$p("21 day Trend Summary", style = "font-size: 65%; white-space: normal; word-wrap: break-word;")),
          p(tags$div(total_sites(), br(), increasing(), br(), decreasing(), br(),
                     plateauing(), br(), not_enough_2(),
                     style = "font-size: 100%; white-space: normal; word-wrap: break-word;")
          )
      )
    )
  })
  
  # ----------------------- Render Summary Table -----------------------
  report_table = reactive({
    req(target())
    target_value <- target()
    data = summary_table %>% 
      filter(`PCR Gene Target` == target_value) %>% 
      select(-`PCR Gene Target`) %>%
      as.data.frame() %>% 
      select("Region", "County" ,"County (City/Utility)", "Level",
             "21 day Trend", "Percent Change [95% CI]", "Data displayed on map",
             "Data Source")
    if (!target_value %in% "n") {
      data <- data %>% as.data.frame() %>% select(-Level)
    }
    return(data)
  })
  
  output$covid_table <- renderDT({
    req(target())
    column_name <- if (target() %in% "n") "Level" else "21 day Trend"
    DT::datatable(
      report_table() %>% as.data.frame() %>% arrange(Region, County, `County (City/Utility)`),
      filter = "top",
      caption = tags$caption("Enter your search criteria in the boxes below to filter the table", style = "color:#AD302C"),
      options = list(pageLength = 50, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatStyle(
        column_name,
        backgroundColor = styleEqual(names(state_threshold_colors_transparent[[target()]]),
                                     state_threshold_colors_transparent[[target()]]),
        color = "black"
      )
  })
  
  # ----------------------- State Summary Modal -----------------------
  # Show a modal with state summary info when the state_summary button is clicked.
  observeEvent(input$state_summary, {
    showModal(
      tags$div(id = "modal_state",
               modalDialog(
                 title = h2(strong(paste0("State Summary of California for ", rename_pathogen(target()), 
                                          " (Last Update: ",published_date_state[[target()]], ")")), 
                 style = "color: white;"),
                 fluidRow(
                   column(width = 5, class = "state-info-container",
                          fluidRow(
                            div(style = "margin-top: 120px;"),
                            uiOutput("state_info_box", width = 10),
                            uiOutput("trend_box", width = 10)
                          )
                   ),
                   column(width = 7,
                          plotlyOutput("state_plot", height = "700px") %>% withSpinner(color = "#5A789A"),
                          style = "padding-right: 15px; padding-left: 45px;"
                   )
                 ),
                 footer = tagList(actionButton("closeBtn2", "Close")),
                 easyClose = TRUE
               )
      )
    )
  })
  
  observeEvent(input$closeBtn2, {
    removeModal()
  })
  
  # ----------------------- Regional Summary Modal -----------------------
  # Show a modal with regional summary info when the regional_summary button is clicked.
  observeEvent(input$regional_summary, {
    showModal(
      tags$div(id = "modal_region",
               modalDialog(
                 title = h2(strong(paste("Regional Summary of California for", rename_pathogen(target()), " (Last Update: ", published_date_region[[target()]], ")")),
                 style = "color: white;"),
                 fluidRow(
                   column(4, plotOutput("region_plot1", height = "900px") %>% withSpinner(color = "#5A789A")),
                   column(8,
                          tagList(
                            div(class = "region-plot2-container", plotlyOutput("region_plot2") %>% withSpinner(color = "#5A789A"))
                          )
                   )
                 ),
                 footer = tagList(actionButton("closeBtn", "Close")),
                 easyClose = TRUE
               )
      )
    )
  })
  
  observeEvent(input$closeBtn, {
    removeModal()
  })
  
  # ----------------------- Regional Plot Rendering -----------------------
  # Render static and interactive regional plots.
  output$region_plot1 <- renderPlot({
    
    req(target())

    # For COVID, use level-based mapping; for others, use trend-based mapping.
    if(target() %in% "n"){
      list1 = list(geom_sf(aes(fill = factor(level, levels = c("Low", "Medium", "High", "Not enough data"))), lwd = 0.3, color= "black"))
      to_bind =  tibble(level = c("Low", "Medium", "High", "Not enough data"))
      plot_title = "Current Regional Levels"
    } else if(target() %in% c("infa", "rsv", "infb")){
      list1 = list(geom_sf(aes(fill = factor(trend2, levels = c("Decrease", "Plateau", "Increase",  "Strong Increase",
                                                                "Very Strong Increase", "Sporadic Detections", "All Samples Below LOD",  "Not enough data"))), lwd = 0.3, color= "black"))
      to_bind = tibble((trend2 = c("Very Strong Increase", "Strong Increase", "Increase", "Plateau", "Decrease", "Not enough data", "Sporadic Detections", "All Samples Below LOD")))
      plot_title = "Current Regional Trends"
    }
    plotdata = state_df %>% filter(pcr_gene_target == target()) %>% bind_rows(to_bind)
    
    ggplot(plotdata) +
      list1 +
      scale_fill_manual(values = state_threshold_colors[[target()]], drop = FALSE, name = "Current Wastewater Level") +
      labs(title = plot_title) +
      geom_sf_label(data = plotdata, aes(label = region), size = 5) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(
          size = 24, 
          margin = margin(t = 10, r = 0, b = 0, l = 0)
        )
      )
  })
  
  output$region_plot2 <- renderPlotly({
    req(target())
    # Prepare plot data for the regional plot.
    plotdf = state_region_plot_df %>%
      filter(pcr_gene_target == target()) %>% 
      mutate(ww_aggregate = ww_aggregate* conversion_factor) %>%
      rename(`sample date` = sample_date, `raw concentration normalized by pmmov` = ww_aggregate)
    d33 <-  plotdf %>% group_by(region) %>% summarise(q33 = mean(q33, na.rm = T)*conversion_factor)
    d66 <-  plotdf %>% group_by(region) %>% summarise(q66 = mean(q66, na.rm = T)*conversion_factor)
    plotdf$vline = max(plotdf$`sample date`) - 21
    plotdf = plotdf %>% group_by(region) %>% summarise(label_pos = max(`raw concentration normalized by pmmov`, na.rm = T)*0.8) %>% right_join(plotdf)
    df = as.data.frame(state_df %>% filter(pcr_gene_target == target())) %>% select(wwtp_name, level, trend) %>% filter(!is.na(wwtp_name))
    region_name = d33$region
    bar_level = plotdf %>% group_by(region) %>% summarise(max_conc = max(`raw concentration normalized by pmmov`, na.rm = T)) %>%
      left_join(d33) %>% left_join(d66) %>% mutate(stack2 = q66-q33) %>% rename(stack3 = max_conc, stack1 = q33) %>% mutate(show_legend = c(T, F, F, F, F, F)) %>% select(stack1, stack2, stack3, show_legend)
    update_list = c("", "", "", "", "", paste0(" (<span style='color:dark gray;'>Updated: ", published_date_state[[target()]], "*</span>)"))
    if(target() %in% "n"){
      line_plotlist = lapply(c(1:5), function(v){
        metric_plot(data = plotdf %>% filter(region == region_name[v]) %>% arrange(`sample date`),
                    x_col = "sample date",
                    y_col = "raw concentration normalized by pmmov",
                    vline_date = unique(plotdf$vline),
                    vline_label_position = plotdf %>% filter(region == region_name[v]) %>% pull(label_pos) %>% unique(),
                    hline_y1 = filter(d33, region == region_name[v])$q33,
                    hline_y2 = filter(d66, region == region_name[v])$q66,
                    plot_title = paste0(regname(region_name[v], reverse = T), ":\nTrend: ", filter(df, wwtp_name == region_name[v])$trend, " | Level: ", filter(df, wwtp_name == region_name[v])$level, update_list[v]),
                    y_label = paste0(rename_pathogen(target(), choice = 2), "/PMMOV (x1 million)"),
                    x_label = "",
                    show_h_lines = T,
                    show_h_label = F,
                    range_selector = F)
      })
      bar_plotlist = pmap(bar_level, create_bar_plot)
      plotlist <- lapply(seq_along(line_plotlist), function(i) {
        subplot(bar_plotlist[[i]], line_plotlist[[i]], widths = c(0.04, 0.96), shareY = TRUE, margin = 0)
      })
      screen_width <- session$clientData$output_region_plot2_width
      nrows_value <- ifelse(screen_width <= 768, 5, 3)
      subplot(plotlist, nrows = nrows_value, shareX = F, shareY = FALSE, margin = 0.04) %>%
        layout(title = list(text = "State regional plot", font = list(size = 24, family = "Arial", weight = "bold")),
               legend = list(title = list(text = "    <b>Level</b>"), x = 1.0, y = 1),
               margin = list(t = 100))
    } else if(target() %in% c("infa", "rsv", "infb")){
      plotlist = lapply(c(1:5), function(v){
        metric_plot(data = plotdf %>% filter(region == region_name[v]) %>% arrange(`sample date`),
                    x_col = "sample date",
                    y_col = "raw concentration normalized by pmmov",
                    vline_date = unique(plotdf$vline),
                    vline_label_position = plotdf %>% filter(region == region_name[v]) %>% pull(label_pos) %>% unique(),
                    hline_y1 = filter(d33, region == region_name[v])$q33,
                    hline_y2 = filter(d66, region == region_name[v])$q66,
                    plot_title = paste0(regname(region_name[v], reverse = T), ":\nTrend: ", filter(df, wwtp_name == region_name[v])$trend),
                    y_label = paste0(rename_pathogen(target(), choice = 2), "/PMMOV (x1 million)"),
                    x_label = "",
                    show_h_lines = T,
                    range_selector = F)
      })
      subplot(plotlist, nrows = 3, shareX = F, shareY = FALSE, margin = 0.04) %>%
        layout(title = list(text = "State regional plot", font = list(size = 24, family = "Arial", weight = "bold")),
               annotations = list(list(x = 0, y = 0.5, xref = "paper", yref = "paper", text = paste0(input$pathogen, "/PMMOV (x1 million)"), showarrow = FALSE, textangle = -90, font = list(size = 18), xshift = -70)))
    }
  })
  
  
  # ----------------------- Data Download Section -----------------------
  # Initialize reactiveValues for storing filtered download datasets.
  reactive_vals_download1 <- reactiveValues(filtered_data = NULL)
  
  output$download_table1 <- DT::renderDT({
    reactive_vals_download1$filtered_data <- download_df1 %>% select(input$show_vars_wastewater)
    DT::datatable(
      reactive_vals_download1$filtered_data,
      filter = "top",
      rownames = FALSE,
      options = list(lengthMenu = c(20, 50, 100), pageLength = 10, scrollX = TRUE),
      class = 'download-datatable-1'
    ) %>%
      formatSignif(columns = intersect(download1_num_col, input$show_vars_wastewater), digits = 3)
  })
  
  observeEvent(input$confirmDownload, {
    showModal(
      modalDialog(
        title = "Confirm Download",
        tags$p("Are you sure you want to download the wastewater data? The file size may be quite large."),
        footer = tagList(modalButton("Cancel"), downloadButton("downloadData1", "Yes, Download", class = "btn-success")),
        easyClose = TRUE
      )
    )
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("wastewater-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_vals_download1$filtered_data, file, row.names = FALSE)
    }
  )
  
  reactive_vals_download2 <- reactiveValues(filtered_data = NULL)
  
  output$download_table2 <- DT::renderDT({
    reactive_vals_download2$filtered_data <- download_df2 %>% select(input$show_vars_metrics)
    DT::datatable(
      reactive_vals_download2$filtered_data, 
      filter = "top",
      rownames = FALSE,
      options = list(lengthMenu = c(20, 50, 100), pageLength = 10, scrollX = TRUE),
      class = 'download-datatable-2'
    )
  })
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("wastewater-metrics-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_vals_download2$filtered_data, file, row.names = FALSE)
    }
  )
}
