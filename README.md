# PUBH6199 - Summer 2025
## Bart team project

[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/2V1dzZDL)


# Final Project: Clinic Visits with Complete Tobacco History

Authors: Ashlan Jackson & Sora Ely  
Course: PUBH 6199 – Visualizing Data with R  
Date: June 26, 2025


## 🔍 Project Overview

This pilot project aimed to increase tobacco history completeness at GW clinic visits. Therefore, our graphs are broken down by pilot versus non-pilot specialties/staff, or by individual specialties/staff, and can be viewed on a weekly or monthly interval. This is data that will be used to review lung cancer screening and to hopefully increase screening rates. 


## 📊 Final Write-up

The final write-up, including code and interpretation of the visualizations, is available here:

👉 [**https://github.com/pubh6199-data-viz-with-r/hw6-bart**]


## 📂 Repository Structure

```plaintext
.
├── .Rproj               # RStudio project file
├── index.qmd            # Main Quarto file for final write-up
├── prototype-app/       # Shiny app prototype V1 folder (incl app data files)
│   ├── app.R
├── shiny-app/           # Shiny app folder
│   ├── app.R
│   └── app-data/        # Data files for Shiny app
├── _quarto.yml          # Quarto configuration file
├── .gitignore           # Files to ignore in git
├── data/                # Cleaned data files used in project
├── AJ Edits june22      # Data wrangling code
├── docs/                # Rendered site (auto-generated)
├── renv/                # renv files
├── scratch/             # Scratch files for exploratory analysis         
└── README.md            # This file
```

## 🛠 How to Run the Code

### To render the write-up:

1. Open the `.Rproj` file in RStudio.
2. Open `index.qmd`.
3. Click **Render**. The updated html will be saved in the `docs/` folder.

### To run the Shiny app (if applicable):

1. Open the `shiny-app` folder.
2. Open the `app.R` file in RStudio.
3. Click "Run App" in the top right corner of the script editor.

```r
setwd("shiny-app") # Set working directory to shiny app folder
shiny::runApp("shiny-app")
```

> ⚠️ Make sure any necessary data files are in `shiny-app/app-data/`.


## 🔗 Shiny App Link

If your project includes a Shiny app, you can access it here:

👉 [GW Tobacco History Initiative Dashboard](https://srce-code.shinyapps.io/tobacco-hx-dashboard-PUBH6199/)


## 📦 Packages Used

- tidyverse
- janitor
- ggplot2
- ggrepel
- viridisLite
- viridis
- shiny 
- rsconnect
- bslib
- scales
- DT


## ✅ To-Do or Known Issues

We hope to add a working app to the weekly and monthly static graphs.

We also plan to later add additional interactive visualizations for individual specialties & departments to the dashboard.
Minor additional visual refinements also planned.