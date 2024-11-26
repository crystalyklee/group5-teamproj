
# BTC1877 TEAM PROJECT
# GROUP 1 

library(readxl)
library(dplyr)
library(MASS)
library(ggplot2)

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

data2 <- data1[, data2_var]

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

data2 <- data2 %>%
  mutate(
    death = if_else(is.na(death_date), 0, 1) # death indicator
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

# Variables to exclude based on =>30% missing data
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

# Full log model with alive_30d as outcome
full_log_alive30d <- glm(
  alive_30d ~ massive_transfusion + total_rbc_24hr + rbc_72hr_total +
    ffp_72hr_total + plt_72hr_total + cryo_72hr_total +
    intra_ffp + intra_rbc + intra_pcc + intra_platelets + intra_cryo,
  data = data2_filtered,
  family = binomial
)

summary(full_log_alive30d ) # no significant predictors in the full model 

# Stepwise selection based on AIC
# See which predictors best influence 30 day mortality
best_model <- stepAIC(full_log_alive30d, direction = "both")

summary(best_model) # total_rbc_24hr, intra_rbc
# predictors aren't significant 
#> levels(ghdata1$gvhd)
# [1] "No"  "Yes"
# for every 1 unit increase in total_rbc_24hr, the log of odds for survival increases
# for every 1 unit increase in intra_rbc, the log of odds for survival deceases

# can do the same thing for alive_90 and alive_12m 

full_log_death <- glm(
  death ~ massive_transfusion + total_rbc_24hr + rbc_72hr_total +
    ffp_72hr_total + plt_72hr_total + cryo_72hr_total +
    intra_ffp + intra_rbc + intra_pcc + intra_platelets + intra_cryo,
  data = data2_filtered,
  family = binomial
)

summary(full_log_death) # intra_ffp significant predictor

best_model_death <- stepAIC(full_log_death, direction = "both")

summary(best_model_death) # ffp_72hr_total, intra_ffp, intra_rbc significant predcitors

# Survival analysis
library(survival)
library(lubridate)

# Calculate time diff between OR date and death date
data3 <- data2 %>%
  mutate(
    or_death_diff = if_else(is.na(death_date), 365, as.numeric(death_date - or_date)),
    icu_death_diff = as.numeric(interval(icu_adm_date, death_date) / ddays(1)),
    extub_death_diff = as.numeric(interval(extub_date, death_date) / ddays(1)),
    death = if_else(is.na(death_date), 0, 1), # death indicator
  )


# Kaplan-Meier curve for death in all patients from OR date, observe median 
sf <- survfit(Surv(or_death_diff, death == 1) ~ 1, data = data3)

# Plot KM curve
plot(sf,xlab = "Time from Operation Date", ylab="Survival")
title("Time to Death Since Operation") # 50% of pop'n die at ~400 days from OR date

# Kaplan-Meier curve for death in all patients from ICU date, observe median 
sf <- survfit(Surv(icu_death_diff, death == 1) ~ 1, data = data3)

# Plot KM curve
plot(sf,xlab = "Time from ICU admission Date", ylab="Survival")
title("Time to Death Since ICU admission") # 50% of pop'n die at ~120 days from OR date

# Stratified Kaplan-Meier curve by transfusion for death in all patients from OR date

data3 <- data3 %>%
  mutate(
    transfusion_status = ifelse(total_rbc_24hr > 0, "Transfusion", "No Transfusion")  
  ) # tbh i dont know what the yellow column numbers mean, thought rbc 72hr might be better but it's not consistent with total rbc 24h??

as.factor(data3$transfusion_status)

sf2 <- survfit(Surv(or_death_diff, death == 1) ~ transfusion_status, data = data3)

# Plot KM curve
plot(sf2, xlab = "Time from Operation Date", ylab= "Survival", col=1:2)
title("Time to Death Since Operation by Transfusion Status") # 50% of pop'n die at ~400 days from OR date
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
logrank <- survdiff(Surv(or_death_diff, death == 1) ~ transfusion_status, data = data3)
logrank
# no transfusion group had fewer deaths than expected
# transfusion group had more deaths than expected
# p > 0.05, no significant difference in survival between groups

# Cox PH model
coxmod <- coxph(Surv(or_death_diff, death == 1) ~ transfusion_status, data = data3)
coxmodsummary <- summary(coxmod) 
coxmodsummary
# Patients who received transfusions had a 1.3x higher or 30% higher hazard of death
# compared to those who did not receive transfusions
# p > 0.05, not significantly different

######## TRYING SURVIVAL ANALYSIS AND COX MODEL ON ALIVE_30D AS OUTCOME ##########

data4 <- data3 %>%
  mutate(
    or_death_diff = if_else(is.na(death_date), 30, as.numeric(death_date - or_date)), # treat missing death dates as censored at 30 day follow up
    alive_30d = if_else(alive_30d == "No", 1, 0),  # 1 = Death at 30 days, 0 = Alive
  )

# Kaplan-Meier curve for death at 30d in all patients from OR date, observe median 
sf3 <- survfit(Surv(or_death_diff, alive_30d == 1) ~ 1, data = data4)

# Plot KM curve
plot(sf3,xlab = "Time from Operation Date", ylab="Survival at 30 days")
title("Time to Death Since Operation") # most pt's did not die at 30 days

# Stratified Kaplan-Meier curve by transfusion for death in all patients from OR date

sf4 <- survfit(Surv(or_death_diff, alive_30d == 1) ~ transfusion_status, data = data4)

# Plot KM curve
plot(sf4, xlab = "Time from Operation Date", ylab= "Survival at 30 days", col=1:2)
title("Time to Death at 30 Days Since Operation by Transfusion Status") 
legend("bottomright",legend = c("No Transfusion", "Transfusion"), lty = 1, col = 1:2) 

# Compare survival curves using Log-rank test 
# Check PH assumption using cloglog

plot(
  survfit(Surv(or_death_diff, alive_30d) ~ transfusion_status, data = data4),
  fun = "cloglog",
  main = "Complementary Log-Log Survival Plot by Transfusion Status", 
  xlab = "Time from Operation Date", 
  ylab = "Complementary Log-Log Survival Probability",
  col=1:3)

legend("bottomright",legend = c("No Transfusion", "Transfusion"), lty = 1, col = 1:2) 

# NOT PARALLEL

# Log rank test for death as outcome by transfusion status
logrank <- survdiff(Surv(or_death_diff, alive_30d == 1) ~ transfusion_status, data = data3)
logrank
# no transfusion group had fewer deaths than expected
# transfusion group had more deaths than expected
# p > 0.05, no significant difference in survival between groups

# Cox PH model
coxmod <- coxph(Surv(or_death_diff, alive_30d) ~ transfusion_status, data = data4)
coxmodsummary <- summary(coxmod) 
coxmodsummary
# Patients who received transfusions had a 4.47e08x higher hazard of death at 30 days
# compared to those who did not receive transfusions
# p > 0.05, not significantly different

######## TRYING SURVIVAL ANALYSIS AND COX MODEL ON ALIVE_90D AS OUTCOME ##########

data5 <- data3 %>%
  mutate(
    or_death_diff = if_else(is.na(death_date), 90, as.numeric(death_date - or_date)), # treat missing death dates as censored at 30 day follow up
    alive_90d = if_else(alive_90d == "No", 1, 0),  # 1 = Death at 90 days, 0 = Alive
  )

# Kaplan-Meier curve for death at 30d in all patients from OR date, observe median 
sf5 <- survfit(Surv(or_death_diff, alive_90d == 1) ~ 1, data = data5)

# Plot KM curve
plot(sf5,xlab = "Time from Operation Date", ylab="Survival at 90 days")
title("Time to Death Since Operation") # does not reach median survival

# Stratified Kaplan-Meier curve by transfusion for death in all patients from OR date

sf6 <- survfit(Surv(or_death_diff, alive_90d == 1) ~ transfusion_status, data = data5)

# Plot KM curve
plot(sf6, xlab = "Time from Operation Date", ylab= "Survival at 90 days", col=1:2)
title("Time to Death at 90 Days Since Operation by Transfusion Status") 
legend("bottomright",legend = c("No Transfusion", "Transfusion"), lty = 1, col = 1:2) 
# most of the pop'n has not experienced death at 90 days regardless of transfusion

# Compare survival curves using Log-rank test 
# Check PH assumption using cloglog

plot(
  survfit(Surv(or_death_diff, alive_90d == 1) ~ transfusion_status, data = data5),
  fun = "cloglog",
  main = "Complementary Log-Log Survival Plot by Transfusion Status", 
  xlab = "Time from Operation Date", 
  ylab = "Complementary Log-Log Survival Probability",
  col=1:3)

legend("bottomright",legend = c("No Transfusion", "Transfusion"), lty = 1, col = 1:2) 

# NOT PARALLEL

# Log rank test for death as outcome by transfusion status
logrank <- survdiff(Surv(or_death_diff, alive_90d == 1) ~ transfusion_status, data = data5)
logrank
# no transfusion group had fewer deaths than expected
# transfusion group had more deaths than expected
# p > 0.05, no significant difference in survival between groups

# Cox PH model
coxmod <- coxph(Surv(or_death_diff, alive_90d == 1) ~ transfusion_status, data = data5)
coxmodsummary <- summary(coxmod) 
coxmodsummary
# Patients who received transfusions had a 2.4x higher hazard of death at 90 days
# compared to those who did not receive transfusions
# p > 0.05, not significantly different

######## TRYING SURVIVAL ANALYSIS AND COX MODEL ON ALIVE_12M AS OUTCOME ##########

data6 <- data3 %>%
  mutate(
    or_death_diff = if_else(is.na(death_date), 365, as.numeric(death_date - or_date)), # treat missing death dates as censored at 30 day follow up
    alive_12m = if_else(alive_12m == "No", 1, 0),  # 1 = Death at 90 days, 0 = Alive
  )

# Kaplan-Meier curve for survival at 1 yr in all patients from OR date, observe median 
sf7 <- survfit(Surv(or_death_diff, alive_12m == 1) ~ 1, data = data6)

# Plot KM curve
plot(sf7,xlab = "Time from Operation Date", ylab="Survival at 1 yr")
title("Time to Death Since Operation") # 50% of pop'n die at 90 days ~500 days from OR date

# Stratified Kaplan-Meier curve by transfusion for death in all patients from OR date

sf8 <- survfit(Surv(or_death_diff, alive_12m == 1) ~ transfusion_status, data = data6)

# Plot KM curve
plot(sf8, xlab = "Time from Operation Date", ylab= "Survival at 1 yr", col=1:2)
title("Time to Death at 90 Days Since Operation by Transfusion Status") 
legend("bottomright",legend = c("No Transfusion", "Transfusion"), lty = 1, col = 1:2) 

# Compare survival curves using Log-rank test 
# Check PH assumption using cloglog

plot(
  survfit(Surv(or_death_diff, alive_12m == 1) ~ transfusion_status, data = data6),
  fun = "cloglog",
  main = "Complementary Log-Log Survival Plot by Transfusion Status", 
  xlab = "Time from Operation Date", 
  ylab = "Complementary Log-Log Survival Probability",
  col=1:3)

legend("bottomright",legend = c("No Transfusion", "Transfusion"), lty = 1, col = 1:2) 

# i think parallel???

# Log rank test for death as outcome by transfusion status
logrank <- survdiff(Surv(or_death_diff, alive_12m == 1) ~ transfusion_status, data = data6)
logrank
# no transfusion group had fewer deaths than expected
# transfusion group had more deaths than expected
# p > 0.05, no significant difference in survival between groups

# Cox PH model
coxmod <- coxph(Surv(or_death_diff, alive_12m == 1) ~ transfusion_status, data = data6)
coxmodsummary <- summary(coxmod) 
coxmodsummary
# Patients who received transfusions had a 1.3x higher or 80% higher hazard of death at 1 yr
# compared to those who did not receive transfusions
# p > 0.05, not significantly different

# ASSUMPTIONS:
# 1 year is the last follow up for patients so this is where we can impute time to event data

# CONCLUSIONS:
# probably use death as outcome, possibly alive_90d, the other alive variables aren't that insightful
# because most patients are alive at that timepoint and it's hard to compare plots but it does
# consistently show that people with transfusions have lower survival rates 
# limitations are that there is a lot of time to event data missing so will have to impute with censoring info
# can't do regular imputation bc it defeats the purpose of censored data and would distort results

#########T-TESTS COMPARING ICU AND HOSPITAL LENGTH OF STAY#############

# Assess normality

# No Transfusion 
icu_no_transfusion <- data3 %>%
  filter(transfusion_status == "No Transfusion") %>%
  pull(icu_los)

icu_transfusion <- data3 %>%
  filter(transfusion_status == "Transfusion") %>%
  pull(icu_los)

hist(icu_no_transfusion, main = "Histogram: ICU LOS (No Transfusion)", xlab = "ICU LOS (days)", col = "lightblue", breaks = 10) # very right skewed
qqnorm(icu_no_transfusion, main = "Q-Q Plot: ICU LOS (No Transfusion)")
qqline(icu_no_transfusion, col = "red")

# Transfusion 
hist(icu_transfusion, main = "Histogram: ICU LOS (Transfusion)", xlab = "ICU LOS (days)", col = "lightblue", breaks = 10) # very right skewed
qqnorm(icu_transfusion, main = "Q-Q Plot: ICU LOS (Transfusion)")
qqline(icu_transfusion, col = "red")

hospital_no_transfusion <- data3 %>%
  filter(transfusion_status == "No Transfusion") %>%
  pull(hospital_los)

hospital_transfusion <- data3 %>%
  filter(transfusion_status == "Transfusion") %>%
  pull(hospital_los)

# No Transfusion 
hist(hospital_no_transfusion, main = "Histogram: Hospital LOS (No Transfusion)", xlab = "Hospital LOS (days)", col = "lightblue", breaks = 10) # very right skewed
qqnorm(hospital_no_transfusion, main = "Q-Q Plot: Hospital LOS (No Transfusion)")
qqline(hospital_no_transfusion, col = "red")

# Transfusion 
hist(hospital_transfusion, main = "Histogram: Hospital LOS (Transfusion)", xlab = "Hospital LOS (days)", col = "lightblue", breaks = 10) # very right skewed
qqnorm(hospital_transfusion, main = "Q-Q Plot: Hospital LOS (Transfusion)")
qqline(hospital_transfusion, col = "red")

# two sample t-test for icu los
t_test_icu <- t.test(
  icu_los ~ transfusion_status,
  data = data3,
  var.equal = FALSE # assume unequal variance
)

t_test_icu # no significant difference between icu los

# two sample t-test for hospital los
t_test_hospital <- t.test(
  hospital_los ~ transfusion_status,
  data = data3,
  var.equal = FALSE # assuming unequal variances
)

t_test_hospital # no significant difference between hospital los

# Non-parametric tests - wilcox rank sum for independent samples
wilcox_icu <- wilcox.test(
  icu_los ~ transfusion_status,
  data = data3
)

wilcox_icu # p < 0.05 significant difference

wilcox_hospital <- wilcox.test(
  hospital_los ~ transfusion_status,
  data = data3
)

wilcox_hospital # p < 0.05 significant difference

data3 %>%
  group_by(transfusion_status) %>%
  summarize(
    median_hospital_los = median(hospital_los, na.rm = TRUE),
    mean_hospital_los = mean(hospital_los, na.rm = TRUE),
    count = n()
  )

# patients who received blood transfusions had a significantly longer hospital 
# length of stay compared to those who did not

# ICU LOS boxplot
ggplot(data3, aes(x = transfusion_status, y = icu_los, fill = transfusion_status)) +
  geom_boxplot() +
  labs(title = "ICU Length of Stay by Transfusion Status", x = "Transfusion Status", y = "ICU Length of Stay (days)") +
  theme_minimal()

# Hospital LOS boxplot
ggplot(data3, aes(x = transfusion_status, y = hospital_los, fill = transfusion_status)) +
  geom_boxplot() +
  labs(title = "Hospital Length of Stay by Transfusion Status", x = "Transfusion Status", y = "Hospital Length of Stay (days)") +
  theme_minimal

# Try without outliers

iqr <- IQR(data3$icu_los, na.rm = TRUE)
upper_bound <- quantile(data3$icu_los, 0.75, na.rm = TRUE) + (3 * iqr)

iqr <- IQR(data3$hospital_los, na.rm = TRUE)
upper_bound <- quantile(data3$hospital_los, 0.75, na.rm = TRUE) + (3 * iqr)

data_no_outl <- data3 %>%
  filter(hospital_los <= upper_bound)

wilcox_icu_no_outl <- wilcox.test(
  icu_los ~ transfusion_status,
  data = data_no_outl
)

wilcox_icu_no_outl # still significant

wilcox_hospital_no_outl <- wilcox.test(
  hospital_los ~ transfusion_status,
  data = data_no_outl
)

wilcox_hospital_no_outl # still significant 

# Try log transformation and re-run t-test

data_los_log <- data3 %>%
  mutate(
    icu_los_log = log(icu_los + 1), # Add 1 to avoid log(0)
    hospital_los_log = log(hospital_los + 1)
  )

icu_no_transfusion_log <- data_los_log %>%
  filter(transfusion_status == "No Transfusion") %>%
  pull(icu_los_log)

icu_transfusion_log <- data_los_log %>%
  filter(transfusion_status == "Transfusion") %>%
  pull(icu_los_log)

# No Transfusion group for ICU LOS
hist(icu_no_transfusion_log, main = "Histogram: Log ICU LOS (No Transfusion)", 
     xlab = "Log ICU LOS", col = "lightblue", breaks = 10)
qqnorm(icu_no_transfusion_log, main = "Q-Q Plot: Log ICU LOS (No Transfusion)")
qqline(icu_no_transfusion_log, col = "red")

# Transfusion group for ICU LOS
hist(icu_transfusion_log, main = "Histogram: Log ICU LOS (Transfusion)", 
     xlab = "Log ICU LOS", col = "lightblue", breaks = 10)
qqnorm(icu_transfusion_log, main = "Q-Q Plot: Log ICU LOS (Transfusion)")
qqline(icu_transfusion_log, col = "red")

hospital_no_transfusion_log <- data_los_log %>%
  filter(transfusion_status == "No Transfusion") %>%
  pull(hospital_los_log)

hospital_transfusion_log <- data_los_log %>%
  filter(transfusion_status == "Transfusion") %>%
  pull(hospital_los_log)

# No Transfusion group for Hospital LOS
hist(hospital_no_transfusion_log, main = "Histogram: Log Hospital LOS (No Transfusion)", 
     xlab = "Log Hospital LOS", col = "lightblue", breaks = 10)
qqnorm(hospital_no_transfusion_log, main = "Q-Q Plot: Log Hospital LOS (No Transfusion)")
qqline(hospital_no_transfusion_log, col = "red")

# Transfusion group for Hospital LOS
hist(hospital_transfusion_log, main = "Histogram: Log Hospital LOS (Transfusion)", 
     xlab = "Log Hospital LOS", col = "lightblue", breaks = 10)
qqnorm(hospital_transfusion_log, main = "Q-Q Plot: Log Hospital LOS (Transfusion)")
qqline(hospital_transfusion_log, col = "red")

print(shapiro.test(icu_no_transfusion_log))
print(shapiro.test(icu_transfusion_log))

print(shapiro.test(hospital_no_transfusion_log))
print(shapiro.test(hospital_transfusion_log))

# a bit better but still not normal, will stick to non-parametric

