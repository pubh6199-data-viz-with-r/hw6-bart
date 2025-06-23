tobacco_clean_AJ_FINAL <- read.csv ("https://raw.githubusercontent.com/pubh6199-data-viz-with-r/hw6-bart/refs/heads/AJEDITS/tobacco_clean_AJ_FINAL.csv")


library(dplyr) 

library(lubridate)

tobacco_clean_AJ_FINAL <- tobacco_data_init %>%
  filter(location %in% c("Foggy Bottom", "West End", "M Street"))

rm(filtered_data)

tobacco_clean_AJ_FINAL <- tobacco_clean_AJ_FINAL %>%
  mutate(pilot = ifelse(location %in% c("FB PULMONOLOGY", "FB CARDIOLOGY", "FB ID", "FB NEPHROLOGY", "FB GER PALLIATIVE CARE"), 
                        "Yes", 
                        "No"))

unique(tobacco_clean_AJ_FINAL$department)

tobacco_clean_AJ_FINAL$date <- as.Date(tobacco_clean_AJ_FINAL$date)



tobacco_clean_AJ_FINAL<- floor_date(tobacco_clean_AJ_FINAL$date, unit = "week", week_start = 1)
tobacco_clean_AJ_FINAL$week <- floor_date(tobacco_clean_AJ_FINAL$date, unit = "week", week_start = 1)
head(tobacco_clean_AJ_FINAL[, c("date", "week")])

tobacco_clean_AJ_FINAL$date <- as.Date(tobacco_clean_AJ_FINAL$date)
tobacco_clean_AJ_FINAL$week <- floor_date(tobacco_clean_AJ_FINAL$date, unit = "week", week_start = 1) + 4

tobacco_clean_AJ_FINAL$date <- as.Date(tobacco_clean_AJ_FINAL$date)
tobacco_clean_AJ_FINAL <- tobacco_clean_AJ_FINAL %>%
  filter(!wday(date) %in% c(1, 7)) 

tobacco_clean_AJ_FINAL <- tobacco_clean_AJ_FINAL %>%
  arrange(date)


tobacco_clean_AJ_FINAL <- tobacco_clean_AJ_FINAL %>%
  mutate(weekday_index = cumsum(wday(date, week_start = 1) == 1),  # count Mondays
         week = paste0("Week ", weekday_index))


tobacco_clean_AJ_FINAL$month <- format(as.Date(tobacco_clean_AJ_FINAL$date), "%m")

format(as.Date(tobacco_clean_AJ_FINAL$date), "%B")
tobacco_clean_AJ_FINAL$month <- format(as.Date(tobacco_clean_AJ_FINAL$date), "%B")

tobacco_clean_AJ_FINAL <- tobacco_clean_AJ_FINAL %>%
  mutate(subspecialty = case_when(
    department %in% c("FB Cardiology") ~ "Cardiology",
    
    department %in% c("FB CARDIOTHORACIC SURGERY", "FB GENERAL SURGERY", "FB NEUROSURGERY",
                      "FB THORACIC SURGERY", "FB VASCULAR SURGERY", "M ST BREAST SURGERY",
                      "M ST CARDIAC SURGERY", "M ST COS/PLAST SUR", "M ST ORTHOPAEDIC SURG",
                      "FB ORTHOPAEDIC SURG") ~ "Surgery",
    
    department %in% c("FB DERMATOLOGY", "M ST DERMATOLOGY") ~ "Dermatology",
    
    department %in% c("FB GER PALLIATIVE CARE") ~ "Palliative Care",
    
    department %in% c("FB GI & LIVER DISEASES") ~ "Gastroenterology",
    
    department %in% c("FB HEM ONC", "FB INFUSION CENTER") ~ "Hematology/Oncology",
    
    department %in% c("FB RADIATION ONCOLOGY") ~ "Radiation Oncology",
    
    department %in% c("FB ID") ~ "Infectious Disease",
    
    department %in% c("FB IR") ~ "Interventional Radiology",
    
    department %in% c("FB MIDWIFERY", "M ST OB/GYN", "FB OB/GYN", "FB UROGYN", "FB MFM") ~ "Obstetrics/Gynecology",
    
    department %in% c("FB NEPHROLOGY", "M ST NEPHROLOGY") ~ "Nephrology",
    
    department %in% c("FB NEUROLOGY") ~ "Neurology",
    
    department %in% c("FB OPHTHALMOLOGY") ~ "Ophthalmology",
    
    department %in% c("FB PAIN") ~ "Pain Management",
    
    department %in% c("FB PMR") ~ "Physical Medicine and Rehabilitation",
    
    department %in% c("M ST PRIMARY CARE", "FB PRIMARY CARE", "FB PREVENTORIUM CLINIC") ~ "Primary Care",
    
    department %in% c("FB PULMONOLOGY") ~ "Pulmonology",
    
    department %in% c("FB UROLOGY") ~ "Urology",
    
    department %in% c("INGLESIDE GERIATRICS") ~ "Geriatrics",
    
    department %in% c("M ST ENT") ~ "ENT",
    
    department %in% c("M ST RHEUMATOLOGY") ~ "Rheumatology",
    
    department %in% c("M ST ENDOCRINOLOGY") ~ "Endocrinology",
    
    TRUE ~ "Other"
  ))

write.csv(tobacco_clean_AJ_FINAL, "/Users/ashlanjackson/Library/Mobile Documents/com~apple~CloudDocs/GWU/visualizing with R/tobacco_clean_AJ_FINAL.csv", row.names = FALSE)
write.csv(tobacco_clean_AJ_FINAL, "tobacco_clean_AJ_FINAL.csv", row.names = FALSE)

tobacco_clean_AJ_FINAL <- tobacco_clean_AJ_FINAL %>%
  mutate(date = as.Date(date),
         week = 1 + ((day(date) + wday(date, week_start = 1) - 2) %/% 7),
         month = month(date, label = TRUE, abbr = FALSE),
         week_label = paste0(month, " Week ", week)
  ) %>%
  select(-month)


write.csv(tobacco_clean_AJ_FINAL, "tobacco_clean_AJ_FINAL.csv", row.names = FALSE)

