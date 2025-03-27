# California Wastewater Surveillance Dashboard (Reproduction)

This repository contains the source code for reproducing the California Wastewater Surveillance Dashboard:  
🔗 https://skylab.cdph.ca.gov/calwws/

The dashboard is built with **R Shiny** and displays wastewater metrics by region and sewershed, using trend levels and customizable time series plots.

⚠️ **Important Notes Before Running:**

1. 📁 **Update File Paths**  
   After cloning this repo, update file paths in `global.R` so that they point to the `Data/` folder inside your project.  
   Example:
   ```r
   # Before:
   read_csv("path/to/your/data.csv")

   # After:
   read_csv("Data/data.csv")

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

## 📂 Getting Started

### 1. Clone the Repository

```bash
git clone https://github.com/yourusername/california-wastewater-dashboard.git
cd california-wastewater-dashboard
