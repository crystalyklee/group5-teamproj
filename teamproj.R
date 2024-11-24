
# BTC1877 TEAM PROJECT
# GROUP 1 

library(readxl)
library(dplyr)
library(MASS)

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
  "Duration of ICU Stay (days)", "Duration of Ventilation",
  "ALIVE_30DAYS_YN", "ALIVE_90DAYS_YN", "ALIVE_12MTHS_YN",
  "ICU_LOS", "HOSPITAL_LOS",
  "Need for reoperation for bleeding within 24h",
  
  # Dates
  "OR Date", "ICU Admission Date/Time", "Date of Extubation", "DEATH_DATE"
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
    massive_transfusion = `Massive Transfusion`, # More than 10 RBC
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
    ventilation_duration = `Duration of Ventilation`,
    alive_30d = ALIVE_30DAYS_YN,
    alive_90d = ALIVE_90DAYS_YN,
    alive_12m = ALIVE_12MTHS_YN,
    icu_los = ICU_LOS,
    hospital_los = HOSPITAL_LOS,
    reop_bleed_24hr = `Need for reoperation for bleeding within 24h`,
    
    # Dates
    or_date = `OR Date`,
    death_date = DEATH_DATE,
    icu_adm_date = `ICU Admission Date/Time`,
    extub_date = `Date of Extubation`
  )


str(data2)

summary(data2)

# Convert variables to factors
data2 <- data2 %>%
  mutate(
    gender = factor(gender, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    reop_bleed_24hr = factor(reop_bleed_24hr, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    alive_30d = factor(alive_30d, levels = c("N", "Y"), labels = c("No", "Yes")),
    alive_90d = factor(alive_90d, levels = c("N", "Y"), labels = c("No", "Yes")),
    alive_12m = factor(alive_12m, levels = c("N", "Y"), labels = c("No", "Yes"))
  )

# Convert extub date into proper format due to improper loading from excel
data2 <- data2 %>%
  mutate(
    extub_date = ifelse(extub_date == "?", NA, extub_date), # Replace ? with NA
    extub_date = as.numeric(extub_date),
    extub_date = as.POSIXct(as.Date(extub_date, origin = "1899-12-30"))
  )

# Convert death date into proper format
data2 <- data2 %>%
  mutate(
    death_date = dmy(death_date, tz = "UTC")  
  )


# Full logistic regression model including only transfusion predictors
full_transfusion_model <- glm(
  alive_30d ~ massive_transfusion + total_rbc_24hr +
    rbc_0_24 + rbc_24_48 + rbc_48_72 + rbc_72hr_total +
    ffp_0_24 + ffp_24_48 + ffp_48_72 + ffp_72hr_total +
    plt_0_24 + plt_24_48 + plt_48_72 + plt_72hr_total +
    cryo_0_24 + cryo_24_48 + cryo_48_72 + cryo_72hr_total +
    intra_ffp + intra_rbc + intra_pcc + intra_platelets + intra_cryo,
  data = data2,
  family = binomial
)

# View model summary
summary(full_transfusion_model)

# Calculate percentage of missing values for each column
missing_percentage <- sapply(data2, function(x) sum(is.na(x)) / nrow(data2) * 100)

# Variables to exclude based on =<30% missing data
vars_to_exclude <- c(
  "rbc_0_24", "rbc_24_48", "rbc_48_72",
  "ffp_0_24", "ffp_24_48", "ffp_48_72",
  "plt_0_24", "plt_24_48", "plt_48_72",
  "cryo_0_24", "cryo_24_48", "cryo_48_72",
  "death_date"
)

# Subset the dataset to exclude these variables
data2_filtered <- data2[, !(names(data2) %in% vars_to_exclude)]

# Trying logistic regression model on alive_30d

# Full log model
full_model <- glm(
  alive_30d ~ massive_transfusion + total_rbc_24hr + rbc_72hr_total +
    ffp_72hr_total + plt_72hr_total + cryo_72hr_total +
    intra_ffp + intra_rbc + intra_pcc + intra_platelets + intra_cryo,
  data = data2_filtered,
  family = binomial
)

# Stepwise selection based on AIC
# See which predictors best influence 30 day mortality
best_model <- stepAIC(full_model, direction = "both")

summary(best_model) # total_rbc_24hr, intra_rbc
# predictors aren't significant 
#> levels(ghdata1$gvhd)
# [1] "No"  "Yes"
# for every 1 unit increase in total_rbc_24hr, the log of odds for survival increases
# for every 1 unit increase in intra_rbc, the log of odds for survival deceases

# can do the same thing for alive_90 and alive_12m 

# Survival analysis
library(survival)
library(lubridate)

# Calculate time diff between OR date and death date
data3 <- data2 %>%
  mutate(
    or_death_diff = as.numeric(interval(or_date, death_date) / ddays(1)),
    icu_death_diff = as.numeric(interval(icu_adm_date, death_date) / ddays(1)),
    extub_death_diff = as.numeric(interval(extub_date, death_date) / ddays(1)),
    death = if_else(is.na(death_date), 0, 1) # death indicator
  )


# Kaplan-Meier curve for death in all patients from OR date, observe median 
sf <- survfit(Surv(or_death_diff, death == "1") ~ 1, data = data3)

# Plot KM curve
plot(sf,xlab = "Time from Operation Date", ylab="Death")
title("Time to Death Since Operation") # 50% of pop'n die at ~200 days from OR date

# Kaplan-Meier curve for death in all patients from OR date, observe median 
sf <- survfit(Surv(icu_death_diff, death == "1") ~ 1, data = data3)

# Plot KM curve
plot(sf,xlab = "Time from ICU admission Date", ylab="Death")
title("Time to Death Since ICU admission") # 50% of pop'n die at ~120 days from OR date

# Stratified Kaplan-Meier curve by transfusion for death in all patients from OR date

data3 <- data3 %>%
  mutate(
    transfusion_status = ifelse(total_rbc_24hr > 0, "Transfusion", "No Transfusion")  
  ) # tbh i dont know what the yellow column numbers mean, thought rbc 72hr might be better but it's not consistent with total rbc 24h??

as.factor(data3$transfusion_status)

sf2 <- survfit(Surv(or_death_diff, death == "1") ~ transfusion_status, data = data3)

# Plot KM curve
plot(sf2, xlab = "Time from Operation Date", ylab= "Survival", col=1:2)
title("Time to Death Since Operation by Transfusion Status") # 50% of pop'n die at ~120 days from OR date
legend("topright",legend = c("No Transfusion", "Transfusion"), lty = 1, col = 1:2) 

# Different looking plot
library(survminer)
ggsurvplot(sf2,
           data = data3,
           title = "Time to Death Since Operation by Transfusion",
           legend.labs = c("No Transfusion", "Transfusion"),  
           legend.title = "Transfusion Status",
           palette = c("lightpink", "skyblue"))


# Compare survival curves using Log-rank test 
# Check PH assumption using cloglog

plot(
  survfit(Surv(or_death_diff, death == "1") ~ transfusion_status, data = data3),
  fun = "cloglog",
  main = "Complementary Log-Log Survival Plot by Transfusion Status", 
  xlab = "Time from Operation Date", 
  ylab = "Complementary Log-Log Survival Probability",
  col=1:3)

legend("bottomright",legend = c("No Transfusion", "Transfusion"), lty = 1, col = 1:2) 

# generally parallel, can proceed

# Log rank test for death as outcome by transfusion status
logrank <- survdiff(Surv(or_death_diff, death == "1") ~ transfusion_status, data = data3)
logrank
# no transfusion group had fewer deaths than expected
# transfusion group had more deaths than expected
# p > 0.05, no significant difference in survival between groups

# Cox PH model
coxmod <- coxph(Surv(or_death_diff, death == "1") ~ transfusion_status, data = data3)
coxmodsummary <- summary(coxmod) 
coxmodsummary
# Patients who received transfusions had a 1.6x higher hazard of death
# compared to those who did not receive transfusions
# p > 0.05, not significantly different

