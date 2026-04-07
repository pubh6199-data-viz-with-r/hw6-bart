
library(tidyverse)

library(forcats)
library(dplyr) 

library(lubridate)

tobacco_data_init <- read_csv("data/tobacco_data_init.csv")

# The following 4 data wrangling steps are below

#Filtering by location 

tobacco_data_by_location <- tobacco_data_init %>%
  filter(location %in% c("Foggy Bottom", "West End", "M Street"))

#create pilot_staff column

pilot_depts <- c("PULMONOLOGY", "CARDIOLOGY", "ID", "NEPHROLOGY", "GER PALLIATIVE CARE")

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
                        "Pilot", 
                        "Non-Pilot"))

tobacco_data_pilot <- tobacco_data_pilot %>%
  relocate(pilot_dept, .after = department)

table(tobacco_data_pilot$pilot_dept)

#Modifying Dates

tobacco_data_clean_date <- tobacco_data_pilot %>%
  mutate(week_date = floor_date(date, unit = "week", week_start = 1)) %>%
  mutate(month_date = floor_date(date, unit = "month"))

# create week/month_num with the number of each week/month
tobacco_data_clean_date <- tobacco_data_clean_date %>% 
  mutate(week_num = week(week_date),
         month_num = month(month_date))

# create a new column with month # as month name (e.g. 3 = "Mar")
tobacco_data_clean_date <- tobacco_data_clean_date %>% 
  mutate(month_name = month(month_date, label = TRUE, abbr = TRUE))
         
glimpse(tobacco_data_clean_date)

#Create specialties

tobacco_data_clean_specialties <- tobacco_data_clean_date %>%
  mutate(spec = case_when(
    department %in% c("CARDIOLOGY") ~ "Cardiology",
    
    department %in% c("CARDIOTHORACIC SURGERY", "GENERAL SURGERY", "NEUROSURGERY",
                      "THORACIC SURGERY", "VASCULAR SURGERY", "BREAST SURGERY",
                      "CARDIAC SURGERY", "COS/PLAST SUR", "ORTHOPAEDIC SURG",
                      "ORTHOPAEDIC SURG", "COLON RECTAL SURG", "TRAUMA SURGERY") ~ "Surgery",
    
    department %in% c("DERMATOLOGY", "DERMATOLOGY") ~ "Dermatology",
    
    department %in% c("GER PALLIATIVE CARE") ~ "Geri/Palliative Care",
    
    department %in% c("GI & LIVER DISEASES") ~ "Gastroenterology",
    
    department %in% c("HEM ONC", "INFUSION CENTER") ~ "Hematology/Oncology",
    
    department %in% c("RADIATION ONCOLOGY") ~ "Radiation Oncology",
    
    department %in% c("ID") ~ "Infectious Disease",
    
    department %in% c("IR") ~ "Interventional Radiology",
    
    department %in% c("MIDWIFERY", "OB/GYN", "OB/GYN", "UROGYN", "MFM", "GYN ONC") ~ "Obstetrics/Gynecology",
    
    department %in% c("NEPHROLOGY") ~ "Nephrology",
    
    department %in% c("NEPHROLOGY") ~ "Nephrology",
    
    department %in% c("NEUROLOGY") ~ "Neurology",
    
    department %in% c("OPHTHALMOLOGY") ~ "Ophthalmology",
    
    department %in% c("PAIN") ~ "Pain Management",
    
    department %in% c("PMR") ~ "Physical Medicine and Rehabilitation",
    
    department %in% c("PRIMARY CARE", "PRIMARY CARE", "PREVENTORIUM CLINIC") ~ "Primary Care",
    
    department %in% c("PULMONOLOGY") ~ "Pulmonology",
    
    department %in% c("UROLOGY") ~ "Urology",
    
    department %in% c("GERIATRICS") ~ "Geriatrics",
    
    department %in% c("ENT") ~ "ENT",
    
    department %in% c("RHEUMATOLOGY") ~ "Rheumatology",
    
    department %in% c("ENDOCRINOLOGY") ~ "Endocrinology",
    
    department %in% c("PODIATRY") ~ "Podiatry",
    
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



#Created weekly dataset
#NOTE TO ASHLAN FOllow this structure for months
tobacco_data_weekly1 <-tobacco_data_clean_specialties %>%
  group_by(week_num, spec) %>%
  summarize(percent_complete = mean(hx_complete == "Complete")) %>%
  rename(group=spec)

tobacco_data_weekly2<-tobacco_data_clean_specialties %>%
  group_by(week_num, pilot_dept) %>%
  summarize(percent_complete = mean(hx_complete == "Complete")) %>%
rename(group=pilot_dept)

tobacco_clean_weeklymodified <-bind_rows(tobacco_data_weekly2,tobacco_data_weekly1) %>%
  ungroup() 

#created new dataset called tobacco_clean_weekly_modified to encompass all the weekly data. 
#has been added to qmd
view(tobacco_clean_weeklymodified)

write_csv(tobacco_clean_weeklymodified , "data/tobacco_data_weeklymodified.csv")

#New datasets made for monthly data
view(tobacco_data_clean_specialties)

tobacco_data_monthly1 <-tobacco_data_clean_specialties %>%
  group_by(month_num, spec) %>%
  summarize (percent_complete =mean(hx_complete == "Complete")) %>%
  rename(group=spec)

tobacco_data_monthly2 <-tobacco_data_clean_specialties %>%
  group_by(month_num, pilot_dept) %>%
  summarize(perecnt_complete = mean(hx_complete == "Complete")) %>%
  rename (group = pilot_dept)

  tobacco_clean_monthlymodified <-bind_rows(tobacco_data_monthly2, tobacco_data_monthly1) %>%
    ungroup()
  
  #saved new file
  write_csv(tobacco_clean_monthlymodified, "data/tobacco_data_monthly_modified.csv")

 view(tobacco_clean_weeklymodified)
 view(tobacco_data_weekly_modified)
 view (tobacco_data_monthly_moified)
 
 
 