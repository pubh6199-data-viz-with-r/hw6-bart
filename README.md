# PUBH6199-Bart
## Bart team project

[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/2V1dzZDL)
# Final Project: Smoking Status and Clinic Visit

Authors: Ashlan Jackson & Sora Elly  
Course: PUBH 6199 – Visualizing Data with R  
Date: June 26, 2025

## 🔍 Project Overview

This project aimed to display tobacco use and clinic visits. Therefore, our graphs are broken down based on weekly and monthly 
to certain doctors offices. This is data that will be used to review lung cancer screening and to hopefully increase screening rates. 
## 📊 Final Write-up


👉 [**https://github.com/pubh6199-data-viz-with-r/hw6-bart/blob/main/_quarto.yml**]

## 📂 Repository Structure

```plaintext
.
├── _quarto.yml          # Quarto configuration file
├── .gitignore           # Files to ignore in git
├── data/                # Cleaned data files used in project
├── .Rproj               # RStudio project file
├── index.qmd            # Main Quarto file for final write-up
├── scratch/             # Scratch files for exploratory analysis         
├── shiny-app/           # Shiny app folder (if used)
│   ├── app.R
|   ├── www/             # Static files for Shiny app (CSS, JS, images)
│   └── app-data/        # Data files for Shiny app
├── docs/                # Rendered site (auto-generated)
└── README.md            # This file
```

## 🛠 How to Run the Code

### To render the write-up:

@SORA

1. Open the `.Rproj` file in RStudio.
2. Open `index.qmd`.
3. Click **Render**. The updated html will be saved in the `docs/` folder.

### To run the Shiny app (if applicable):

@SORA

```r
shiny::runApp("shiny-app")
```

> ⚠️ Make sure any necessary data files are in `shiny-app/app-data/`.

## 🔗 Shiny App Link
@SORA

If your project includes a Shiny app, you can access it here:

👉 [https://yourusername.shinyapps.io/your-app-name](https://yourusername.shinyapps.io/your-app-name)

## 📦 Packages Used
@SORA
#I added mine, just need yours

-'tidyverse
-'ggplot'
-'quarto'
-'shiny' 
-'lubridate'
-'dplyr'
-'forcats'
-'ggrepel'
- 'dplyr'
-'viridisLite'
 -'viridis'



## ✅ To-Do or Known Issues
@SORA
#added my challenges, just need yours
We hope to add a working app to the weekly and monthly static graphs.
