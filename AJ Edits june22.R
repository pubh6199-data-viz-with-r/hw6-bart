library(tidyverse)
library(lubridate)

tobacco_data_init <- read_csv("data/tobacco_data_init.csv")

# The following 4 data wrangling steps are below

#Filtering by location 

tobacco_data_by_location <- tobacco_data_init %>%
  filter(location %in% c("Foggy Bottom", "West End", "M Street"))

#create pilot_staff column

pilot_depts <- c("FB PULMONOLOGY", "FB CARDIOLOGY", "FB ID", "FB NEPHROLOGY", "FB GER PALLIATIVE CARE")

# calculate the percentage of visits in pilot departments per staff 
tobacco_staff <- tobacco_data_by_location %>%
  select(roomed_by_ID, roomed_by_prov_type, department) %>%
  distinct()
tobacco_staff_perc <- tobacco_staff %>%
  group_by(roomed_by_ID) %>%
  count(department)
tobacco_staff_perc <- tobacco_staff_perc %>%
  group_by(roomed_by_ID) %>%
  mutate(pilot_staff_perc = (sum(department %in% pilot_depts) / n() * 100))

# create a variable pilot_staff_perc that will be "Pilot" if staff has > 50% visits in pilot depts
tobacco_staff_perc <- tobacco_staff_perc %>%
  mutate(pilot_staff = ifelse(pilot_staff_perc > 50, "Pilot", "Non-Pilot"))
pilot_staff <- tobacco_staff_perc |> 
  distinct(roomed_by_ID, pilot_staff)

# add pilot_staff to main data (& create new dataset name after the location step)
tobacco_data_pilot <- tobacco_data_by_location %>%
  left_join(pilot_staff, by = "roomed_by_ID") |> 
  mutate(pilot_staff = ifelse(is.na(pilot_staff), "Non-Pilot", pilot_staff)) 

tobacco_data_pilot <- tobacco_data_pilot |>
  relocate(pilot_staff, .after = roomed_by_ID)

#Create pilot_dept column

tobacco_data_pilot <- tobacco_data_pilot %>%
  mutate(pilot_dept = ifelse(department %in% pilot_depts, 
                        "Yes", 
                        "No"))

tobacco_data_pilot <- tobacco_data_pilot %>%
  relocate(pilot_dept, .after = department)

table(tobacco_data_pilot$pilot_dept)

#Modifying Dates

tobacco_data_clean_date <- tobacco_data_pilot %>%
  mutate(month_label = month(date, label = TRUE, abbr = FALSE),
         week_number=week(date),
         week_label=paste(month_label, "Week", week_number))
         
table(tobacco_data_clean_date$week_label)
         
glimpse(tobacco_data_clean_date)


#Create specialties

tobacco_data_clean_specialties <- tobacco_data_clean_date %>%
  mutate(spec = case_when(
    department %in% c("FB Cardiology") ~ "Cardiology",
    
    department %in% c("FB CARDIOTHORACIC SURGERY", "FB GENERAL SURGERY", "FB NEUROSURGERY",
                      "FB THORACIC SURGERY", "FB VASCULAR SURGERY", "M ST BREAST SURGERY",
                      "M ST CARDIAC SURGERY", "M ST COS/PLAST SUR", "M ST ORTHOPAEDIC SURG",
                      "FB ORTHOPAEDIC SURG") ~ "Surgery",
    
    department %in% c("FB DERMATOLOGY", "M ST DERMATOLOGY") ~ "Dermatology",
    
    department %in% c("FB GER PALLIATIVE CARE") ~ "Geri/Palliative Care",
    
    department %in% c("FB GI & LIVER DISEASES") ~ "Gastroenterology",
    
    department %in% c("FB HEM ONC", "FB INFUSION CENTER") ~ "Hematology/Oncology",
    
    department %in% c("FB RADIATION ONCOLOGY") ~ "Radiation Oncology",
    
    department %in% c("FB ID") ~ "Infectious Disease",
    
    department %in% c("FB IR") ~ "Interventional Radiology",
    
    department %in% c("FB MIDWIFERY", "M ST OB/GYN", "FB OB/GYN", "FB UROGYN", "FB MFM") ~ "Obstetrics/Gynecology",
    
    department %in% c("FB NEPHROLOGY") ~ "Nephrology (FB)",
    
    department %in% c("M ST NEPHROLOGY") ~ "Nephrology (M St)",
    
    department %in% c("FB NEUROLOGY") ~ "Neurology",
    
    department %in% c("FB OPHTHALMOLOGY") ~ "Ophthalmology",
    
    department %in% c("FB PAIN") ~ "Pain Management",
    
    department %in% c("FB PMR") ~ "Physical Medicine and Rehabilitation",
    
    department %in% c("M ST PRIMARY CARE", "FB PRIMARY CARE", "FB PREVENTORIUM CLINIC") ~ "Primary Care",
    
    department %in% c("FB PULMONOLOGY") ~ "Pulmonology",
    
    department %in% c("FB UROLOGY") ~ "Urology",
    
    department %in% c("INGLESIDE GERIATRICS") ~ "Geriatrics (Ingleside)",
    
    department %in% c("M ST ENT") ~ "ENT",
    
    department %in% c("M ST RHEUMATOLOGY") ~ "Rheumatology",
    
    department %in% c("M ST ENDOCRINOLOGY") ~ "Endocrinology",
    
    TRUE ~ "Other"
  ))

#shorten column names
tobacco_data_clean_specialties <- tobacco_data_clean_specialties %>%
  rename(
    loc = location,
    dept = department,
    roomed_by_type = roomed_by_prov_type
  )

#Write clean data set out
write_csv(tobacco_data_clean_specialties, "data/tobacco_clean_AJ_FINAL.csv")

