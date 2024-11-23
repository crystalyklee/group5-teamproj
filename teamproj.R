
# BTC1877 TEAM PROJECT
# GROUP 1 

library(readxl)

data1 <- read_excel("transfusion data.xlsx")

# Q2: In conjunction with the above, what is the impact of transfusion 
# on patient outcomes, including mortality?

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
  "ICU_LOS", "HOSPITAL_LOS", "DEATH_DATE",
  "Need for reoperation for bleeding within 24h"
)

data2 <- data[, data2_var]
