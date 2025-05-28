# California Wastewater Surveillance Dashboard (Reproduction)

This repository is a **reproduction of the official California Wastewater Surveillance Dashboard** developed by the California Department of Public Health (CDPH):  
https://skylab.cdph.ca.gov/calwws/

It is built using **R Shiny**, **Leaflet**, and **Plotly**, and includes interactive views of wastewater data by statewide, region or individual sewershed. This project was developed using R version 4.4.2. It should also work with newer versions of R, but if you encounter issues during `renv::restore()`, please try using the same R version or check package compatibility.

---

## Pathogen Data Coverage

The dashboard is designed to support multiple pathogens:
- **SARS-CoV-2 (COVID-19)** (data provided in this repo)
- **Influenza A (Flu A)** (not included)
- **Influenza B (Flu B)** (not included)
- **Respiratory Syncytial Virus (RSV)** (not included)

Currently, only **SARS-CoV-2** data is included in this GitHub version.

---
## File structure
```
├── Data/             # Contains all input CSV files and data
├── www/              # Static files (CSS, icons, images)
├── global.R          # Loads packages and data, defines global functions
├── server.R          # Server logic (reactives, plots, maps, tables)
├── ui.R              # User interface layout (dashboardPage)
├── README.md         # This file
```



## Setup Instructions

### 1. Clone the Repository
In the terminal, navigate to the directory where you'd like to clone this repository and then run the following comment line. 
```bash
git clone https://github.com/calsuwers/dashboard.git
```
### 2. Install Required R Packages
This Shiny app uses `renv` to manage package dependencies. This ensures that the exact versions of R packages used in development are also used when you run the app — no version mismatches, no missing packages.

✔️ One-time setup:
1. Open R or RStudio in the project directory (the folder where app.R and renv.lock are located).
2. Run the following commands in the R console:
```R
setwd("/path to folder where app.R is located/") # Change the working directory in R console to the folder where app.R is located
install.packages("renv")  # Only needed if you haven't installed renv yet
renv::init()              # Initializes renv and install packages and select option 1 - Restore the project from the lockfile.
source("renv/activate.R")           # Activates the renv environment
```
This will download and install all necessary packages into a project-specific library managed by `renv` and you only need to run `source("renv/activate.R")` once unless the `renv.lock` file changes or you delete the local renv library.

:lock: Notes:
- The `renv.lock` file is committed to this repo — it ensures reproducibility.
- The `renv/library/` folder (where packages are installed) is local to your machine and should not be committed to Git. It’s listed in `.gitignore`.


### 3. Update File Paths

 After cloning this repo, update file paths in `global.R` so that they point to the `data_folder` folder inside your project.  
   Example:
   ```r
   # Before:
   read_csv("/path/to/your/dashboard/data_folder/data.csv")

   # After:
   read_csv("/your specific path/dashboard/data_folder/data.csv")
   ```

### 4. Run the app

Once the environment is set up, packages are downloaded and file paths are updated:
- Simply open app.R in RStudio and click **"Run App"**
```

### :pushpin: Notes

The shapefile used in this dashboard **does not contain sewershed polygons** — only latitude and longitude coordinates for each sewershed. Therefore, polygons are not shown on the sewershed heatmap — only point markers are used.
