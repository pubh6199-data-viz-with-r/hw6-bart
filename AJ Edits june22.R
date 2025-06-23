
library(dplyr) 

library(lubridate)
tobacco_data_init <- read_csv("data/tobacco_data_init.csv")

# The following 4 data wrangling steps are below

#Filtering by locaiton 

tobacco_data_by_location <- tobacco_data_init %>%
  filter(location %in% c("Foggy Bottom", "West End", "M Street"))

#create new column that is pilot column

tobacco_data_pilot <- tobacco_data_by_location %>%
  mutate(pilot = ifelse(department %in% c("FB PULMONOLOGY", "FB CARDIOLOGY", "FB ID", "FB NEPHROLOGY", "FB GER PALLIATIVE CARE"), 
                        "Yes", 
                        "No"))

table(tobacco_data_pilot$pilot)

#Modifying Dates

tobacco_data_clean_date <- tobacco_data_pilot %>%
  mutate(month_label = month(date, label = TRUE, abbr = FALSE),
         week_number=week(date),
         week_label=paste(month_label, "Week", week_number))
         
table(tobacco_data_clean_date$week_label)
         
glimpse(tobacco_data_clean_date)


#created subspecialites

tobacco_data_clean_subspecialities <- tobacco_data_clean_date %>%
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

#write clean data set out
write_csv(tobacco_data_clean_subspecialities, "data/tobacco_clean_AJ_FINAL.csv")

