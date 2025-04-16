# Load necessary libraries for UI, plots, mapping, data wrangling, and other Shiny functionalities
library(bslib)
library(cowplot)
library(DBI)
library(data.table)
library(dbplyr)
library(dplyr)
library(DT)
library(dygraphs)
library(ggmap)
library(ggpattern)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(kableExtra)
library(knitr)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(magrittr)
library(maps)
library(mapproj)
library(plotly)
library(purrr)
library(RColorBrewer)
library(RCurl)
library(readr)
library(readxl)
library(rlang)
library(rintrojs)
library(scales)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(sf)
library(stringr)
library(tidyr)
library(xts)
library(zoo)

# Source custom functions used throughout the app
source("R/functions.R")

# Define variables --------------------------------------------------------

# Define map colors for Flu and RSV status categories
flu_rsv_map_colors <- c(
  "Decrease" = "#67a867",            # LightGreen
  "Plateau" = "#d4ac77",             # Yellowish tone
  "Increase" = "#FFA07A",            # LightSalmon
  "Strong Increase" = "#FF7F7F",     # LightCoral
  "Very Strong Increase" = "#CD5C5C",# IndianRed
  "Not enough data" = "#969696",     # Gray
  "Sporadic Detections" = "#D3C7A0", # Light beige
  "All Samples Below LOD" = "#D3D3D3" # Light gray
)

# Same color scheme but with transparency (used for overlapping/hoverable layers)
flu_rsv_map_colors_transparent <- c(
  "Decrease" = "#67a86780",           
  "Plateau" = "#d4ac7780",            
  "Increase" = "#FFA07A80",           
  "Strong Increase" = "#FF7F7F80",    
  "Very Strong Increase" = "#CD5C5C80",
  "Not enough data" = "#96969680",    
  "Sporadic Detections" = "#D3C7A080",
  "All Samples Below LOD" = "#D3D3D380"
)

# Define COVID color categories for the map
covid_map_colors <- c(
  "Low" = "#118987",
  "Medium" = "#FEC309",
  "High" = "#C8534A",
  "Not enough data" = "#969696"
)

# Transparent version of COVID map colors
covid_map_colors_transparent <- c(
  "Low" = "#11898780",
  "Medium" = "#FEC30980",
  "High" = "#C8534A80",
  "Not enough data" = "#96969680"
)

# Store threshold color schemes in lists by pathogen type
state_threshold_colors = list(
  n = covid_map_colors,
  infa = flu_rsv_map_colors,
  infb = flu_rsv_map_colors,
  rsv = flu_rsv_map_colors
)

state_threshold_colors_transparent = list(
  n = covid_map_colors_transparent,
  infa = flu_rsv_map_colors_transparent,
  infb = flu_rsv_map_colors_transparent,
  rsv = flu_rsv_map_colors_transparent
)

# Define default map view zoom/center settings for each region
zoomlist = list(
  "ABAHO" = c(-122.2384, 37.70023, 8.4),
  "RANCHO" = c(-123.1403, 40.80435, 7),
  "SOCAL" = c(-118.249, 34.97365, 7),
  "SACRAMENTO" = c(-121.33, 39.6, 8),
  "SJVC" = c(-120.51, 36.88, 7.8)
)

# Currently only including SARS-CoV-2 for visualization
target_choice = c("SARS-CoV-2")
# target_choice = c("SARS-CoV-2", "Flu A", "Flu B", "RSV") # Uncomment to include other pathogens

# Display text used in level interpretation tables
levels_data <- data.frame(
  `Wastewater Level` = c("Below the 33rd percentile", "Between the 33rd and the 66th percentile", "Above the 66th percentile",
                         "Not enough data to produce levels*"),
  `Current Concentration` = c("Low", "Medium", "High", "Not enough data")
)

# Trend interpretation table
trends_data <- data.frame(
  `21-day Percent Change Estimate` = c("-21% to -99%", "-20% to 20%", "21% to 99%", "100% to 249%", "Greater than 250%", "Not enough data in the past 21 days*"),
  `Trend Category` = c("Decrease", "Plateau", "Increase", "Strong Increase", "Very Strong Increase","Not enough data")
)

# Add a symbolic representation for each trend category
trends_data$`Trend Symbol` <- sapply(trends_data$Trend.Category, trend_symbol)

# Set upper limit for y-axis on certain plots
upper_y_plot_limit = 1.20

# Baseline value used in trend calculations
low_base_value = 50

# Read Files --------------------------------------------------------------

# Read dashboard update status from CSV
dash_update_data <-
  read.csv("/path/to/your/dashboard/data_folder/dashboard_update_table.csv") %>%
  arrange(desc(date))

# Read main site info table and remove trailing spaces
td2 = readRDS(strwrap("/path/to/your/dashboard/data_folder/td2.RDS")) %>% 
  mutate(wwtp_name =  gsub(" $", "", wwtp_name),
         Label_Name = gsub(" $", "", Label_Name))

# Disable use of s2 geometry (to prevent topo/geospatial issues)
sf_use_s2(F)

# Read in coordinates for each sewershed (centroid-based)
shape_df = read.csv("/path/to/your/dashboard/data_folder/sewershed_location.csv")

# Code below reads shapefile instead of CSV — disabled for now
# shape_df <-
#   st_read("/path/to/your/dashboard/data_folder/sewershed_location.csv") %>% 
#   st_transform(crs = 4326) %>%
#   mutate(center = st_centroid(geometry)) %>% 
#   mutate(lng = st_coordinates(center)[,1],
#          lat = st_coordinates(center)[,2]
#   )  %>% 
#   st_zm()

# Read in CA region shapefile and extract centroid coordinates
ca_regions <- st_read("/path/to/your/dashboard/data_folder/saveCARegions.shp")  %>% 
  mutate(center = st_centroid(geometry)) %>% 
  mutate(lng = st_coordinates(center)[,1],
         lat = st_coordinates(center)[,2]) %>%
  st_transform(crs = 4326) %>%
  st_zm() 

# Read in CA counties shapefile
ca_counties <- st_read("/path/to/your/dashboard/data_folder/saveCACounties.shp")  %>% 
  st_transform(crs = 4326) %>%
  mutate(center = st_centroid(geometry)) %>% 
  mutate(lng = st_coordinates(center)[,1],
         lat = st_coordinates(center)[,2]) %>%
  st_zm() 

# Create region reference table
region_table = td2 %>% select(region, Label_Name) %>% distinct() %>% rename_region()

# Load regional aggregate COVID data
d1 <- get_latest_csv("/path/to/your/dashboard/data_folder/covid/saveRegionalAggregates/") %>%
  mutate(sample_date = as.Date(sample_date)) %>%
  select(-c("X"))

# Load individual WWTP-level COVID metrics
d2 <- get_latest_csv("/path/to/your/dashboard/data_folder/covid/saveReportMetrics/") %>%
  mutate(across(c(Label_Name, wwtp_name), ~ gsub(" $", "", .)))

# Placeholder for loading Flu/RSV aggregate data (commented out for now)
# f1 <- get_latest_csv("/path/to/your/dashboard/data_folder/Flu_RSV/saveRegionalAggregates/") %>%
#   mutate(sample_date = as.Date(sample_date)) %>%
#   select(-c("X")) %>% mutate(region = gsub(pattern = "state", replacement = "State", x = region))
# 
# f2 <- get_latest_csv("/path/to/your/dashboard/data_folder/Flu_RSV/saveReportMetrics/") %>%
#   mutate(across(c(Label_Name, wwtp_name), ~ gsub(" $", "", .))) %>% mutate(region = gsub(pattern = "state", replacement = "State", x = region))

# Combine COVID + (optionally) Flu/RSV aggregate data
c1 <- d1 #%>% bind_rows(f1)

# Combine site-level data and normalize trend labels
c2 <- d2 %>% #bind_rows(f2) %>%
  mutate(trend2 = gsub("Potential ", "", trend),
         trend2 = ifelse(trend2 == "Concentrations too low to call trend",
                         "Sporadic Detections",
                         trend2),
         trend2 = factor(trend2, levels = c("Very Strong Increase", 
                                            "Strong Increase",
                                            "Increase",
                                            "Increase from low levels",
                                            "Plateau",
                                            "Decrease", 
                                            "Not enough data",
                                            "Sporadic Detections",
                                            "All Samples Below LOD")),
         wwtp_name =  gsub(" $", "", wwtp_name),
         Label_Name = gsub(" $", "", Label_Name)
  ) %>%
  mutate_at(c("model_pc", "model_pc_lwr", "model_pc_upr"), ~round(., digits = 0)) %>%
  group_by(Label_Name, pcr_gene_target) %>%
  mutate(report_include = ifelse(n() == 1 & report_include == FALSE, TRUE, report_include)) %>%
  ungroup()

# ------------------------- Level Heatmap Data (commented out) ------------------------
# Code below is for dynamically building level heatmap data from many CSVs in a folder

# # Define the folder path
# sewershed_folder_path <- "/path/to/your/dashboard/data_folder/covid/saveReportMetrics/"
# 
# # List all CSV files in the folder
# csv_files <- list.files(sewershed_folder_path, pattern = "\\.csv$", full.names = TRUE)
# 
# # Define the expected column names
# expected_columns <- names(read.csv(csv_files[[165]]))  # Adjust as needed
# 
# # Function to read and validate a CSV file
# read_and_validate_csv <- function(file_path, expected_columns) {
#   df <- tryCatch(read.csv(file_path, stringsAsFactors = FALSE), 
#                  error = function(e) return(NULL))
#   
#   if (!is.null(df) && identical(names(df), expected_columns)) {
#     return(df)
#   } else {
#     return(NULL)
#   }
# }
# 
# # Read CSV files and store only valid data frames in a list
# valid_data_frames <- lapply(csv_files, read_and_validate_csv, expected_columns = expected_columns)
# 
# # Remove NULL values (invalid files)
# valid_data_frames <- Filter(Negate(is.null), valid_data_frames)
# 
# # Combine all valid CSVs into one dataframe
# level_heatmap_df = do.call(rbind, valid_data_frames) %>% 
#   mutate(date = metrics_as_of)

# Alternate: Load pre-built heatmap data from a single CSV
# level_heatmap_df = read.csv("level_heatmap.csv") %>% 
#   mutate(Label_Name = ifelse(is.na(Label_Name),
#                              region,
#                              Label_Name)) %>% 
#   mutate(across(c(Label_Name, wwtp_name), ~ gsub(" $", "", .))) %>% 
#   mutate(across(c(Label_Name, wwtp_name, region), ~ ifelse(. == "State", "Statewide", .))) %>% 
#   mutate(date = as.Date(date)) #%>% 
# # filter(weekdays(date) == "Thursday")

# # Define color palette for level categories
# level_colors <- c("High" = "#C8534A", "Medium" = "#FEC309", "Low" = "#118987", "Not enough data" = "#969696")
# 
# # Convert level categories to numeric for plotting
# level_heatmap_df <- level_heatmap_df %>% 
#   mutate(level_numeric = ifelse(level == "High",
#                                 4,
#                                 ifelse(level == "Medium",
#                                        3,
#                                        ifelse(level == "Low",
#                                               2,
#                                               ifelse(level == "Not enough data",
#                                                      1,
#                                                      NA)))))

# Data Wrangling  ---------------------------------------------------------

# Split c2 into subsets for site-level, state-level, and regional-level data
c2_wwtp = c2 %>% filter(!is.na(wwtp_name))
c2_state = c2 %>% filter(is.na(wwtp_name) & region == "State")
c2_region = c2 %>% filter(is.na(wwtp_name) & region != "State")

# Extract and format unique publish dates per target group and level
published_date_wwtp = list(
  n = strftime(unique(datafilter(c2_wwtp, value = "n")$metrics_as_of), "%m/%d/%Y"),
  infa = strftime(unique(datafilter(c2_wwtp, value = "infa")$metrics_as_of), "%m/%d/%Y"),
  infb = strftime(unique(datafilter(c2_wwtp, value = "infb")$metrics_as_of), "%m/%d/%Y"),
  rsv = strftime(unique(datafilter(c2_wwtp, value = "rsv")$metrics_as_of), "%m/%d/%Y")
)

published_date_state = list(
  n = strftime(unique(datafilter(c2_state, value = "n")$metrics_as_of), "%m/%d/%Y"),
  infa = strftime(unique(datafilter(c2_state, value = "infa")$metrics_as_of), "%m/%d/%Y"),
  infb = strftime(unique(datafilter(c2_state, value = "infb")$metrics_as_of), "%m/%d/%Y"),
  rsv = strftime(unique(datafilter(c2_state, value = "rsv")$metrics_as_of), "%m/%d/%Y")
)

published_date_region = list(
  n = strftime(unique(datafilter(c2_region, value = "n")$metrics_as_of), "%m/%d/%Y"),
  infa = strftime(unique(datafilter(c2_region, value = "infa")$metrics_as_of), "%m/%d/%Y"),
  infb = strftime(unique(datafilter(c2_region, value = "infb")$metrics_as_of), "%m/%d/%Y"),
  rsv = strftime(unique(datafilter(c2_region, value = "rsv")$metrics_as_of), "%m/%d/%Y")
)

# Combine all published date lists
published_date = list("Sewershed" = published_date_wwtp,
                      "State" = published_date_state,
                      "Region" = published_date_region)

# Filter and clean gene targets for reporting, joining with metadata
wdf = td2 %>%
  filter(pcr_gene_target %in% c("n1", "N", "InfA1", "infa1",
                                "infb", "RSV-A and RSV-B combined", "rsv-a and rsv-b combined")) %>%
  filter(!is.na(region)) %>%
  mutate(pcr_gene_target = ifelse(pcr_gene_target %in% c("n1", "N"), "n",
                                  ifelse(pcr_gene_target %in% c("InfA1", "infa1"), "infa",
                                         ifelse(pcr_gene_target %in% c("RSV-A and RSV-B combined", "rsv-a and rsv-b combined"), "rsv", pcr_gene_target)))) %>%
  left_join(
    c2 %>%
      filter(report_include == T) %>%
      select(wwtp_name, data_source, data_source_short, report_include, pcr_gene_target)
  ) %>% 
  filter(report_include == T)

# Create short- and long-term time windows, calculate adjusted values and classifications
w1 = wdf %>%
  filter(sample_date > Sys.Date()-60) %>%
  mutate(term = "short") %>%
  bind_rows(
    wdf %>%
      filter(sample_date > Sys.Date()-730) %>%
      mutate(term = "long")
  ) %>%
  mutate(wwtp_name = Label_Name) %>%
  left_join(c2 %>%
              select(Label_Name, q33, q66, report_include, pcr_gene_target) %>%
              filter(!is.na(Label_Name), report_include == T) %>%
              rename(wwtp_name = Label_Name)
  ) %>%
  group_by(data_source, wwtp_name, pcr_gene_target) %>%
  mutate(norm_pmmov = ifelse(below_LOD == TRUE & pcr_gene_target %in% "n", 6,
                             ifelse(below_LOD == TRUE & !pcr_gene_target %in% "n", 0, norm_pmmov))) %>%
  mutate(
    max_norm_pmmov_ten_rollapply = max(norm_pmmov_ten_rollapply, na.rm = TRUE)
  ) %>%
  mutate(
    norm_pmmov_limit = if_else(norm_pmmov > upper_y_plot_limit * max_norm_pmmov_ten_rollapply,
                               upper_y_plot_limit * 0.99 * max_norm_pmmov_ten_rollapply,
                               norm_pmmov),
    data_type = if_else(norm_pmmov > upper_y_plot_limit * max_norm_pmmov_ten_rollapply, "limited",
                        ifelse(below_LOD == TRUE, "below LOD", "regular"))
  ) %>%
  ungroup() %>%
  select(-max_norm_pmmov_ten_rollapply)

# Create spatial state-level data frame with geometry
state_df <- c2 %>%
  filter(is.na(wwtp_name)) %>%
  left_join(ca_regions, by = "region") %>%
  select(-X) %>%
  st_as_sf() %>%
  mutate(level = factor(level, levels = c("Low", "Medium", "High", "Not enough data")),
         wwtp_name = region) %>%
  mutate(lng = st_coordinates(center)[,1],
         lat = st_coordinates(center)[,2])

# Color palette for maps based on pathogen and trend level
pal <- setNames(
  lapply(names(state_threshold_colors), function(x) {
    colorFactor(
      palette = state_threshold_colors[[x]], 
      levels = names(state_threshold_colors[[x]])
    )
  }), 
  names(state_threshold_colors)
)

# Combine spatial point data for sites and counties
c3 = c2 %>% filter(!is.na(wwtp_name)) %>%
  left_join(shape_df, by = c("Label_Name" = "sewershed")) %>%
  bind_rows(
    d2 %>% filter(is.na(wwtp_name)) %>%
      left_join(ca_counties %>% select(region, lng, lat), by = c("region"))
  ) %>%
  mutate_at(c("model_pc", "model_pc_lwr", "model_pc_upr"), ~round(., digits = 0)) %>%
  mutate(wwtp_name = Label_Name)

# Flatten and filter mapped data for display
c4 = c3 %>% as.data.frame() %>%
  ungroup() %>%
  filter(!is.na(wwtp_name), report_include == T) %>%
  select(wwtp_name, data_source, region, trend, trend2, model_pc,
         model_pc_lwr, model_pc_upr, pcr_gene_target)

# Filter out small-scale/special sewersheds from mapping
c5 = c4 %>% filter(!wwtp_name %in% c(
  "Marin (West Railroad)",                     
  "Santa Clara (Stanford Campus)",             
  "Los Angeles (LAX Airport)",              
  "Alabama (Eastern Mission District)",        
  "Ingalls (Smaller section of the S.Bayview)",
  "Jackson (Chinatown & parts of N. Beach)",   
  "San Francisco (Newhall Fairfax)",           
  "Paris &Persia (Excelsior)",                 
  "Rayland & Rutland (Visitation Valley)",     
  "San Diego (South Bay)")
)

# Build a list of sites below LOD, by pathogen
targets <- c("n", "infa", "infb", "rsv")

below_LOD_list <- lapply(c("n", "infa", "infb", "rsv"), function(target) {
  w1 %>%
    filter(pcr_gene_target == target, data_type == "below LOD") %>%
    pull(Label_Name) %>%
    unique()
})
names(below_LOD_list) <- targets

# Data summary data and download table ---------------------------------------------

# Create a cleaned dataset for download
download_df1 = td2 %>% ungroup() %>%
  filter(!pcr_gene_target %in% c("hmpv", "vp1", "evd68", "caur", "hav", "hadv_f", "hpiv", "h5"),
         !pcr_target %in% c("fluav_h5"),
         pcr_target %in% c("FLUAV", "flubv", "fluav", "parainfluenza", "rsv", "RSV", "sars-cov-2" )) %>%
  mutate(sample_type = ifelse(units == "SARS-CoV-2/L", "liquid",
                              ifelse(units == "SARS-CoV-2/PMMoV", "solids", NA))) %>% 
  select(region, County_address, Label_Name, wwtp_name, sample_date, sample_type, pcr_gene_target, pcr_target,
         below_LOD, raw_concentration, raw_concentration_ten_rollapply,
         norm_pmmov, norm_pmmov_ten_rollapply, data_source) %>%
  rename(County = County_address,
         `County (City/Utility)` = Label_Name,
         `abbreviated_name` = wwtp_name,
         raw_conc_roll_average = raw_concentration_ten_rollapply,
         norm_pmmov_roll_average = norm_pmmov_ten_rollapply) %>%
  filter(!is.na(`County (City/Utility)`), !is.na(pcr_gene_target)) %>% 
  filter(sample_date > "2020-01-01") %>% 
  mutate(pcr_gene_target = ifelse(pcr_gene_target %in% c("n1", "N"), "n",
                                  ifelse(pcr_gene_target %in% c("InfA1", "infa1"), "infa",
                                         ifelse(pcr_gene_target %in% c("RSV-A and RSV-B combined", "rsv-a and rsv-b combined"), "rsv", pcr_gene_target)))) %>% 
  filter(pcr_gene_target %in% sapply(target_choice, rename_pathogen)) %>%
  rename_with(~ .x %>%
                gsub("_", " ", .) %>% str_to_title() %>% gsub("Pcr", "PCR", .))

# Define numerical columns for download formatting
download1_num_col = str_to_title(gsub("_", " ", c(
  "raw_concentration", "raw_conc_roll_average",
  "norm_pmmov", "norm_pmmov_roll_average")))

# Create summary table for the dashboard map
summary_table <- c2 %>%
  filter(!is.na(wwtp_name)) %>%
  as.data.frame() %>% 
  select(region, wwtp_name, level, trend2, report_include, data_source, pcr_gene_target,
         "model_pc", "model_pc_lwr", "model_pc_upr") %>%
  mutate(`Percent Change [95% CI]` = paste0(model_pc, "% [", model_pc_lwr, "%, ", model_pc_upr, "%]")) %>%
  mutate(`Percent Change [95% CI]` = ifelse(model_pc > 250, "> 250%",
                                            ifelse(is.na(model_pc), " ", `Percent Change [95% CI]`))) %>%
  select(-c("model_pc", "model_pc_lwr", "model_pc_upr")) %>%
  arrange(region, wwtp_name, `Percent Change [95% CI]`, report_include) %>%
  mutate(report_include = ifelse(report_include == T, "yes", 
                                 ifelse(report_include == F, "no", report_include))) %>%
  left_join(td2 %>% select(Label_Name, wwtp_name, County_address) %>% distinct(),
            by = c("wwtp_name" = "wwtp_name")) %>%
  rename(Region = region, `County (City/Utility)` = Label_Name,
         Level = level, `21 day Trend` = trend2, `Data Source` = data_source,
         `Data displayed on map` = report_include, County = County_address,
         `PCR Gene Target` = pcr_gene_target) %>%
  select(-wwtp_name)

# Filter and format data for download (summary table version)
download_df2 = summary_table %>% as.data.frame() %>% 
  filter(`PCR Gene Target` %in% sapply(target_choice, rename_pathogen)) %>% 
  mutate(`PCR Target` = sapply(`PCR Gene Target`, rename_pathogen)) %>% 
  select("Region", "County" ,"County (City/Utility)", "PCR Gene Target", "PCR Target",
         "Level", "21 day Trend", "Percent Change [95% CI]", "Data displayed on map",
         "Data Source")

# Generate list of counties per region
county_df = download_df1 %>% select(County, Region) %>% distinct()
county_list <- tapply(county_df$County, county_df$Region, function(x) sort(x)) %>%
  setNames(map_chr(names(.), regname, reverse = T))

# state map data ----------------------------------------------------------

# Subset last 365 days of regional-level data for plotting
state_region_plot_df = c1 %>% filter(sample_date > max(sample_date)-365)

# State level and trend ----------------------------------------------------------

# Pull statewide COVID and flu trends for display
covid_state_level = c2 %>% filter(region == "State", pcr_gene_target == "n") %>% pull(level)
covid_state_trend = c2 %>% filter(region == "State", pcr_gene_target == "n") %>% pull(trend)
infA_state_trend = c2 %>% filter(region == "State", pcr_gene_target == "infa") %>% pull(trend)

# Generate vector for data source ------------------------------------------------------------------

# Define custom region vector for UI dropdown
region_vec = c("ABAHO", "RANCHO", "San Joaquin Valley", "Sacramento", "Southern California")
region_choice = unname(sapply(setdiff(sort(unique(c1$region)), "State"), regname, reverse = TRUE))
