
# BTC1877 TEAM PROJECT
# GROUP 1 

library(readxl)
library(dplyr)

data1 <- read_excel("transfusion data.xlsx")

# Q2: In conjunction with the above, what is the impact of transfusion 
# on patient outcomes, including mortality?

# Subset the relevant columns
data2_var <- c(
  # Demographics
  "STUDY ID #", "Gender (male)", "Age", "Height", "Weight", "BMI",
  
  # Transfusion variables
  "Total 24hr RBC", "Massive Transfusion",
  "RBC 0-24hrs", "RBC 24-48hrs", "RBC 48-72hrs", "RBC 72hr Total",
  "FFP 0-24hrs", "FFP 24-48hrs", "FFP 48-72hrs", "FFP 72hr Total",
  "Plt 0-24hrs", "Plt 24-48hrs", "Plt 48-72hrs", "Plt 72hr Total",
  "Cryo 0-24hrs", "Cryo 24-48hrs", "Cryo 48-72hrs", "Cryo 72hr Total",
  "Intra_Fresh Frozen Plasma", "Intra_Packed Cells", "Intra_PCC/Octaplex", 
  "Intra_Platelets", "Intra_Cryoprecipitate",
  
  # Mortality/Outcome variables
  "Duration of ICU Stay (days)", "Date of Extubation", "Duration of Ventilation",
  "DEATH_DATE", "ALIVE_30DAYS_YN", "ALIVE_90DAYS_YN", "ALIVE_12MTHS_YN",
  "ICU_LOS", "HOSPITAL_LOS",
  "Need for reoperation for bleeding within 24h"
)

data2 <- data[, data2_var]

# Rename columns
data2 <- data2 %>%
  rename(
    # Demographics
    study_id = `STUDY ID #`,
    gender = `Gender (male)`,
    age = Age,
    height_cm = Height,
    weight_kg = Weight,
    bmi = BMI,
    
    # Transfusion variables
    total_rbc_24hr = `Total 24hr RBC`,
    massive_transfusion = `Massive Transfusion`,
    rbc_0_24 = `RBC 0-24hrs`,
    rbc_24_48 = `RBC 24-48hrs`,
    rbc_48_72 = `RBC 48-72hrs`,
    rbc_72hr_total = `RBC 72hr Total`,
    ffp_0_24 = `FFP 0-24hrs`,
    ffp_24_48 = `FFP 24-48hrs`,
    ffp_48_72 = `FFP 48-72hrs`,
    ffp_72hr_total = `FFP 72hr Total`,
    plt_0_24 = `Plt 0-24hrs`,
    plt_24_48 = `Plt 24-48hrs`,
    plt_48_72 = `Plt 48-72hrs`,
    plt_72hr_total = `Plt 72hr Total`,
    cryo_0_24 = `Cryo 0-24hrs`,
    cryo_24_48 = `Cryo 24-48hrs`,
    cryo_48_72 = `Cryo 48-72hrs`,
    cryo_72hr_total = `Cryo 72hr Total`,
    intra_ffp = `Intra_Fresh Frozen Plasma`,
    intra_rbc = `Intra_Packed Cells`,
    intra_pcc = `Intra_PCC/Octaplex`,
    intra_platelets = `Intra_Platelets`,
    intra_cryo = `Intra_Cryoprecipitate`,
    
    # Mortality/Outcome variables
    icu_los_days = `Duration of ICU Stay (days)`,
    extubation_date = `Date of Extubation`,
    ventilation_duration = `Duration of Ventilation`,
    death_date = DEATH_DATE,
    alive_30d = ALIVE_30DAYS_YN,
    alive_90d = ALIVE_90DAYS_YN,
    alive_12m = ALIVE_12MTHS_YN,
    icu_los = ICU_LOS,
    hospital_los = HOSPITAL_LOS,
    reop_bleed_24hr = `Need for reoperation for bleeding within 24h`
  )


str(data2)

summary(data2)

# Convert variables to factors
data2 <- data2 %>%
  mutate(
    gender = factor(gender, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    reop_24hr = factor(reop_bleed_24hr, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    alive_30d = factor(alive_30d, levels = c("N", "Y"), labels = c("No", "Yes")),
    alive_90d = factor(alive_90d, levels = c("N", "Y"), labels = c("No", "Yes")),
    alive_12m = factor(alive_12m, levels = c("N", "Y"), labels = c("No", "Yes"))
  )


