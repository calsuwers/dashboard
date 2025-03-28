# California Wastewater Surveillance Dashboard (Reproduction)

This repository is a **reproduction of the official California Wastewater Surveillance Dashboard** developed by the California Department of Public Health (CDPH):  
🔗 https://skylab.cdph.ca.gov/calwws/s

It is built using **R Shiny**, **Leaflet**, and **Plotly**, and includes interactive views of wastewater data by statewide, region or individual sewershed.

---

## 🧪 Pathogen Data Coverage

The dashboard is designed to support multiple pathogens:
- **SARS-CoV-2 (COVID-19)** (data provided in this repo)
- **Influenza A (Flu A)** (not included)
- **Influenza B (Flu B)** (not included)
- **Respiratory Syncytial Virus (RSV)** (not included)

Currently, only **SARS-CoV-2** data is included in this GitHub version.

---
## 📂 file structure
```
├── Data/             # Contains all input CSV files and data
├── www/              # Static files (CSS, icons, images)
├── global.R          # Loads packages and data, defines global functions
├── server.R          # Server logic (reactives, plots, maps, tables)
├── ui.R              # User interface layout (dashboardPage)
├── README.md         # This file
```



## :gear: Setup Instructions

### 1. Clone the Repository

```bash
[git clone https://github.com/calsuwers/public_dashboard.git]
cd public_dashboard
```
### 2. Install Required R Packages

### 3. Update File Paths

 After cloning this repo, update file paths in `global.R` so that they point to the `Data/` folder inside your project.  
   Example:
   ```r
   # Before:
   read_csv("path/to/your/data.csv")

   # After:
   read_csv("Data/data.csv")
   ```
### :pushpin: Notes

The shapefile used in this dashboard **does not contain sewershed polygons** — only latitude and longitude coordinates for each sewershed. Therefore, polygons are not shown on the sewershed heatmap — only point markers are used.
