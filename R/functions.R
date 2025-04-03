# Function ----------------------------------------------------------------
# This section contains custom functions for data wrangling, renaming,
# selecting data sources, plotting, and other tasks used in the Shiny app.


# Rename regions to standardized names
rename_region = function(df){
  # df: input data frame with a 'region' column.
  # Returns a data frame with updated region names.
  df2 = df %>%
    mutate(region = ifelse(region == "SOCAL",
                           "Southern California",
                           ifelse(region == "SACRAMENTO",
                                  "Sacramento",
                                  ifelse(region == "SJVC",
                                         "San Joaquin Valley",
                                         region)))
    )
  return(df2)
}

# Create a metric plot using plotly
metric_plot <- function(data,
                        x_col = "sample date",
                        y_col = "raw concentration normalized by pmmov",
                        vline_date,
                        vline_label_position,
                        vline_label_font = list(size = 12),
                        vline_col = "raw concentration normalized by pmmov",
                        hline_y1, hline_y2,
                        plot_title = "",
                        y_label = "",
                        y_label_font = list(size = 18),
                        x_label = "Adjust the time range using the slider above",
                        x_label_show = F,
                        y_lower_limit = 0,
                        show_scatter = F,  # Option to show or hide scatter points
                        scatter_col = "",
                        scatter_hover_text_col = "",
                        scatter_type = "",  # Type of scatter point shape (e.g., "regular", "limited", "below LOD")
                        point_size = 20,  # Size for scatter points
                        show_h_lines = T,
                        show_h_label = F,
                        show_rangeslider = F,  # Toggle for range slider
                        rangeslider_thickness = 0.08,  # Height of the range slider
                        range_selector = T,  # Toggle for range selector buttons
                        ymax = FALSE,  # Option to set ymax dynamically
                        margins = list(l = 80, r = 50, t = 50, b = 50),
                        upper_y_plot_limit = 1.2) {
  # data: data frame containing the data to plot
  # Other parameters control plot appearance and annotations
  
  # Prepare hover text for plotly
  hover_text <- paste("<b>Sample Date:</b>", data[[x_col]], "<br><b>Normalized Rolling Average:</b>",
                      round(data[[y_col]], digits = 1))
  
  # Calculate maximum Y value to determine upper limit if ymax is TRUE
  max_y_value <- max(data[[y_col]], na.rm = TRUE)
  y_axis_upper_limit <- if (ymax) max_y_value * upper_y_plot_limit else NULL
  
  # Define x-axis limits
  y_axis_lower_limit <- y_lower_limit
  min_x <- min(data[[x_col]], na.rm = TRUE) - 1
  max_x <- max(data[[x_col]], na.rm = TRUE) + 1
  num_day_diff <- as.numeric(as.Date(max_x) - as.Date(min_x)) + 2
  
  # Define vertical line shape for annotation
  shapelist <- list(
    list(
      type = 'line',
      x0 = as.Date(vline_date), x1 = as.Date(vline_date),
      y0 = 0, y1 = max(data[[vline_col]], na.rm = TRUE) * upper_y_plot_limit,
      line = list(color = 'gray', dash = 'dash')
    )
  )
  
  # Define color codes for horizontal lines
  Low = "#118987"
  Medium = "#FEC309"
  High = "#C8534A"
  
  # Add horizontal lines if specified
  if (show_h_lines & !is.na(hline_y1) & !is.na(hline_y2)) {
    shapelist <- c(shapelist,
                   list(
                     list(
                       type = 'line',
                       x0 = min(as.Date(data[[x_col]], na.rm = TRUE)),
                       x1 = max(as.Date(data[[x_col]], na.rm = TRUE)),
                       y0 = hline_y1, y1 = hline_y1,
                       line = list(color = Low, dash = 'dash')
                     ),
                     list(
                       type = 'line',
                       x0 = min(as.Date(data[[x_col]], na.rm = TRUE)),
                       x1 = max(as.Date(data[[x_col]], na.rm = TRUE)),
                       y0 = hline_y2, y1 = hline_y2,
                       line = list(color = Medium, dash = 'dash')
                     )
                   )
    )
  }
  
  # Build the initial plotly plot with lines
  plot <- plot_ly(data) %>%
    add_lines(
      x = ~get(x_col), y = ~get(y_col),
      text = hover_text, hoverinfo = 'text',
      line = list(color = 'black'), showlegend = FALSE
    ) %>%
    layout(
      xaxis = list(
        title = if (x_label_show) x_label else "",
        tickformat = "%m/%Y",
        showgrid = FALSE,
        range = c(min_x, max_x),
        rangeslider = list(
          visible = show_rangeslider,
          range = c(min_x, max_x),
          borderwidth = 2,
          thickness = rangeslider_thickness,
          bordercolor = "#5A789A",
          bgcolor = "rgba(211, 211, 211, 0.4)"
        ),
        rangeselector = if (range_selector) list(
          buttons = list(
            list(count = 45, label = "45 days", step = "day", stepmode = "backward"),
            list(count = 6, label = "6m", step = "month", stepmode = "backward"),
            list(count = 1, label = "1 yr", step = "year", stepmode = "backward"),
            list(count = num_day_diff, label = "All", step = "day", stepmode = "backward")
          ),
          y = 1.01,
          x = 0.1,
          bgcolor = "#635e5e",
          activecolor = "#141414",
          borderwidth = 1,
          font = list(color = "white")
        ) else NULL
      ),
      yaxis = list(
        title = y_label,
        titlefont = y_label_font,
        range = c(y_axis_lower_limit, y_axis_upper_limit),
        showgrid = FALSE
      ),
      shapes = shapelist,
      margin = margins
    )
  
  # Add annotations for the vertical line and horizontal lines
  annotations <- list(
    list(
      x = as.Date(vline_date),
      y = vline_label_position,
      text = "21 days ago",
      showarrow = FALSE,
      xanchor = "left",
      yanchor = "middle",
      ax = 20,
      ay = -40,
      textangle = -90,
      font = vline_label_font,
      bgcolor = "rgba(255, 255, 255, 0.8)",
      borderpad = 2
    )
  )
  
  # Add labels for horizontal lines if enabled
  if (show_h_lines & !is.na(hline_y1) & !is.na(hline_y2) & show_h_label) {
    annotations <- c(annotations,
                     list(
                       list(
                         x = min(as.Date(data[[x_col]], na.rm = TRUE)),
                         y = hline_y1,
                         text = "33rd percentile",
                         showarrow = FALSE,
                         xanchor = "left",
                         yanchor = "middle",
                         font = list(size = 12, color = 'black'),
                         bgcolor = "rgba(17, 137, 135, 0.8)",
                         borderpad = 2
                       ),
                       list(
                         x = min(as.Date(data[[x_col]], na.rm = TRUE)),
                         y = hline_y2,
                         text = "66th percentile",
                         showarrow = FALSE,
                         xanchor = "left",
                         yanchor = "middle",
                         font = list(size = 12, color = 'black'),
                         bgcolor = "rgba(254, 195, 9, 0.8)",
                         borderpad = 2
                       )
                     )
    )
  }
  
  # Apply annotations and add plot title
  plot <- plot %>% layout(annotations = annotations) %>%
    add_annotations(
      text = plot_title,
      x = 0.5,
      y = 1.1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 0,
      showarrow = FALSE,
      font = list(size = 15, family = "Arial", color = "black")
    )
  
  # Optionally add scatter points based on different data types
  if (show_scatter && scatter_col != "") {
    # Markers for regular data points
    regular_data <- subset(data, get(scatter_type) == "regular")
    scatter_hover_text_regular <- paste("<b>Sample Date:</b>", regular_data[[x_col]],
                                        "<br><b>Normalized Concentration:</b>", regular_data[[scatter_hover_text_col]])
    
    plot <- plot %>%
      add_markers(
        data = regular_data,
        x = ~get(x_col),
        y = ~get(scatter_col),
        text = scatter_hover_text_regular,
        hoverinfo = 'text',
        marker = list(size = point_size, color = 'rgba(153, 182, 207, 0.7)', symbol = "circle"),
        name = "Regular",
        showlegend = F
      )
    
    # Markers for high values (limited)
    limited_data <- subset(data, get(scatter_type) == "limited")
    scatter_hover_text_limited <- paste(
      "<b>High value, above y-axis limit</b><br>",
      "<b>Sample Date:</b>", limited_data[[x_col]], "<br>",
      "<b>Normalized Concentration:</b>", limited_data[[scatter_hover_text_col]]
    )
    
    plot <- plot %>%
      add_markers(
        data = limited_data,
        x = ~get(x_col),
        y = ~get(scatter_col),
        text = scatter_hover_text_limited,
        hoverinfo = 'text',
        marker = list(size = point_size, color = 'rgba(0, 0, 102, 0.7)', symbol = "triangle-up"),
        name = "High value",
        showlegend = F
      )
    
    # Markers for non-detected values
    no_detect_data <- subset(data, get(scatter_type) == "below LOD")
    scatter_hover_text_below <- paste(
      "<b>Not detected</b><br>",
      "<b>Sample Date:</b>", no_detect_data[[x_col]]
    )
    
    plot <- plot %>%
      add_markers(
        data = no_detect_data,
        x = ~get(x_col),
        y = ~get(scatter_col),
        text = scatter_hover_text_below,
        hoverinfo = 'text',
        marker = list(size = point_size, color = 'rgba(32, 32, 32, 0.7)', symbol = "triangle-down-open"),
        name = "Non-detected",
        showlegend = F
      )
  }
  
  return(plot)
}

# Read the latest CSV file from a given directory
get_latest_csv <- function(path) {
  # path: directory containing CSV files.
  # Returns the most recently modified CSV file read into a data frame.
  files <- list.files(path, full.names = TRUE, pattern = "csv")
  latest_file <- files[which.max(file.info(files)$mtime)]
  read.csv(latest_file)
}

# Convert region names to standardized or reversed formats
regname <- function(region, reverse = FALSE) {
  # region: a region name to convert.
  # reverse: logical flag; if TRUE, perform reverse mapping.
  # Returns the mapped region name.
  region_map <- c(
    "Statewide" = "State",
    "Bay Area (ABAHO)" = "ABAHO",
    "Northern CA (RANCHO)" = "RANCHO",
    "Greater Sacramento" = "SACRAMENTO",
    # "Greater Sacramento (Sacramento)" = "Sacramento",
    "San Joaquin Valley (SJVC)" = "SJVC",
    "Southern CA (SOCAL)" = "SOCAL"
  )
  
  if (reverse) {
    # Reverse the mapping: formatted names back to original codes.
    reverse_map <- setNames(names(region_map), region_map)
    return(ifelse(region %in% names(reverse_map), reverse_map[region], region))
  } else {
    # Forward mapping: original codes to formatted names.
    return(ifelse(region %in% names(region_map), region_map[region], region))
  }
}

# Rename pathogen values using a lookup vector
rename_pathogen <- function(input_value, choice = 2) {
  # input_value: the pathogen name or code to convert.
  # choice: numeric flag to select between two mapping options.
  # Returns the converted pathogen name/code.
  if (choice == 1){
    pathogen <- c(
      "SARS-CoV-2" = "n",
      "Flu A" = "infa",
      "Flu B" = "infb",
      "RSV" = "rsv"
    )
  } else if (choice == 2) {
    pathogen <- c(
      "SARS-CoV-2" = "n",
      "Influenza A" = "infa",
      "Influenza B" = "infb",
      "RSV" = "rsv")
  }
  
  # Check if input_value exists in the names or values of pathogen and return the corresponding mapping.
  if (input_value %in% names(pathogen)) {
    return(pathogen[[input_value]])
  }
  
  if (input_value %in% pathogen) {
    return(names(pathogen)[pathogen == input_value])
  }
  
  return(input_value)
}

# Filter a data frame based on a specified column and value
datafilter <- function(data, column_name = "pcr_gene_target", value) {
  # data: input data frame.
  # column_name: column to filter on.
  # value: value to filter for.
  # Returns the filtered data frame.
  if (!column_name %in% colnames(data)) {
    stop(paste("Column", column_name, "does not exist in the dataset"))
  }
  
  filtered_data <- data[data[[column_name]] == value, ]
  
  return(filtered_data)
}

# Determine a CSS class for a box based on target and state values
get_box_class <- function(target, state_value_list) {
  # target: pathogen target (e.g., "n", "infa", etc.).
  # state_value_list: list containing state-level values like 'level_call' or 'trend_value'.
  # Returns a CSS class name corresponding to the state.
  if (target %in% "n") {
    value <- state_value_list[["level_call"]]
    
    if (value == "High") {
      return("bg-custom-covid_high")
    } else if (value == "Medium") {
      return("bg-custom-covid_medium")
    } else if (value == "Low") {
      return("bg-custom-covid_low")
    } else if (value == "Not enough data") {
      return("bg-custom-covid_no_data")
    }
  } else if (target %in% c("infa", "rsv", "infb")) {
    value <- state_value_list[["trend_value"]]
    
    if (value == "Increase") {
      return("bg-custom-flu_increase")
    } else if (value == "Decrease") {
      return("bg-custom-flu_decrease")
    } else if (value == "Not enough data") {
      return("bg-custom-flu_no_data")
    } else if (value == "Plateau") {
      return("bg-custom-flu_plateau")
    } else if (value == "Very Strong Increase") {
      return("bg-custom-flu_very_strong_increase")
    } else if (value == "Strong Increase") {
      return("bg-custom-flu_strong_increase")
    } else if (value == "All Samples Below LOD") {
      return("bg-custom-flu_all_below_lod")
    } else if (value == "Sporadic Detections") {
      return("bg-custom-flu_sporadic_detections")
    }
  }
  
  return(NULL)  # Return NULL if no match is found
}

# Determine the state color based on the target and category
getStateColor <- function(target, category) {
  # target: pathogen target (e.g., "n" for COVID-19 or "infa", "infb", "rsv" for flu/RSV).
  # category: state level (e.g., "High", "Medium", etc.) or trend.
  # Returns a hex color code corresponding to the category.
  if (target == "n") {
    if (category == "High") {
      return("#C8534A")  # Red color
    } else if (category == "Medium") {
      return("#FEC309")  # Yellow color
    } else if (category == "Low") {
      return("#118987")  # Teal color
    } else if (category == "Not enough data") {
      return("#969696")  # Grey color
    } else {
      return(NULL)
    }
  } else if (target %in% c("infa", "infb", "rsv")) {
    if (category == "Decrease") {
      return("#67a867")  # Light green
    } else if (category == "Strong Increase") {
      return("#FF7F7F")  # Light coral
    } else if (category == "Increase") {
      return("#FFA07A")  # Light salmon
    } else if (category == "Very Strong Increase") {
      return("#CD5C5C")  # Indian red
    } else if (category == "Plateau") {
      return("#d4ac77")  # Light yellow
    } else if (category == "Not enough data") {
      return("#969696")  # Grey
    } else if (category == "Sporadic Detections") {
      return("#D3C7A0")  # Light blue
    } else if (category == "All Below Lod") {
      return("#D3D3D3")  # Light grey
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

# Define custom theme fonts for ggplot2 plots
theme_fonts <- function(base_size = 14) {
  # base_size: base font size for the plot.
  # Returns a ggplot2 theme object with customized font sizes and styles.
  theme_bw(base_size = base_size) %+replace%
    theme(
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text = element_text(size = rel(0.70), face = "bold"),
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}

# Function to add trend symbols (arrows/dashes) for displaying trend categories
trend_symbol <- function(trend_category) {
  # trend_category: a string representing the trend (e.g., "Increase", "Decrease")
  # Returns a symbol: up arrow for increases, down arrow for decreases, etc.
  if (trend_category == "Decrease") {
    return("↓")  # Down arrow
  } else if (trend_category %in% c("Increase", "Strong Increase", "Very Strong Increase")) {
    return("↑")  # Up arrow
  } else if (trend_category == "Plateau") {
    return("–")  # Dash for plateau
  } else if (trend_category == "Not enough data") {
    return("x")  # Cross for not enough data
  } else {
    return("")
  }
}
