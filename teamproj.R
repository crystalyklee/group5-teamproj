library(readxl)
library(tidyverse)
library(mice)
library(glmnet)
library(pROC)
library(tree)
library(visdat)
library(gtsummary)
library(gridExtra)
library(grid)

###############
## Data Prep ##
###############

setwd("C:/Users/ibrah/Desktop/Data Science in Health II/group5-teamproj") # ibrahim's wd
#setwd("~/Desktop/UTM/BTC1877/group5-teamproj") # crystal's wd

set.seed(4)

d_raw <- read_excel("transfusion data.xlsx")

summary(d_raw)

# remove irrelevant columns
d <- d_raw[,c(4:45, 51:54, 116)]

summary(d)

# format variables appropriately
factor_vars <- c("Type", "DCD vs DBD")

for(var in factor_vars) {
  d[[var]] <- as.factor(d[[var]])
}

char_vars <- names(d)[sapply(d, is.character)]

for(var in char_vars) {
  d[[var]] <- as.logical(d[[var]])
}

summary(d)

# DCD vs DBD column has some weird values e.g. false
# I will replace false with NA
d$`DCD vs DBD`[d$`DCD vs DBD` == "FALSE"] <- NA

#DCD vs DBD column has NDD as a value which is likely synonymous with DBD
d$`DCD vs DBD`[d$`DCD vs DBD` == "NDD"] <- "DBD"

# reset the levels
d$`DCD vs DBD` <- droplevels(d$`DCD vs DBD`)
summary(d)

# Create a binary column indicting if the patient received blood transfusion
d <- d %>% 
  mutate(transfusion = ifelse(`Total 24hr RBC` == 0, FALSE, TRUE))

# Check for % missingness in remaining variables
sapply(d, function(x) mean(is.na(x)) * 100)

# Pre_Fibrinogen and Protamine have high % missingness (>30%) so we will exclude
d <- d[,c(-34,-39)]

sapply(d, function(x) mean(is.na(x)) * 100)

# Remaining missingness is very low

# Impute missing data
names(d) <- gsub("[^A-Za-z0-9]+", "_", names(d))
imp <- mice(d, m = 1, seed = 7, print = F)
d_imp <- complete(imp, 1)
summary(d_imp)

#########
## EDA ##
#########

# Will use the unimputed dataset here

# Create a summary table for baseline/demographic characteristics
tbl_summary(
  d,
  missing = "ifany",
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"),
  label = list(
    Age ~ "Age (years)",
    Gender_male_ ~ "Gender (male)",
    BMI ~ "Body Mass Index",
    Type ~ "Transplant Type",
    DCD_vs_DBD ~ "Donor Death Type",
    First_Lung_Transplant ~ "First Lung Transplant",
    LAS_score ~ "LAS Score",
    transfusion ~ "Recieved Transfusion"),
  include = c(Gender_male_, Age, BMI, Type, DCD_vs_DBD, First_Lung_Transplant, LAS_score, transfusion)) %>%
  italicize_levels() %>% 
  bold_labels()

# Create a table summarizing commodities
tbl_summary(
  d,
  missing = "ifany",
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"),
  label = list(
    COPD ~ "Chronic Obstructive Pulmonary Disease",
    alpha1_Antitrypsin_Deficiency ~ "Alpha-1 Antitrypsin Deficiency",
    Cystic_Fibrosis ~ "Cystic Fibrosis",
    Idiopathic_Pulmonary_Hypertension ~ "Idiopathic Pulmonary Hypertension",
    Interstitial_Lung_Disease ~ "Interstitial Lung Disease",
    Pulm_Other ~ "Other Pulmonary Conditions",
    Coronary_Artery_Disease ~ "Coronary Artery Disease",
    Hypertension ~ "Hypertension",
    Diabetes_insulin_ ~ "Diabetes (Insulin-Dependent)",
    Diabetes_diet_OHGs_ ~ "Diabetes (Diet/Oral Hypoglycemics)",
    GERD_PUD ~ "Gastroesophageal Reflux Disease/Peptic Ulcer Disease",
    Renal_Failure ~ "Renal Failure",
    Stroke_CVA ~ "Stroke (CVA)",
    Liver_Disease ~ "Liver Disease",
    Thyroid_Disease ~ "Thyroid Disease"
  ),
  include = c(COPD, alpha1_Antitrypsin_Deficiency, Cystic_Fibrosis, 
              Idiopathic_Pulmonary_Hypertension, Interstitial_Lung_Disease, 
              Pulm_Other, Coronary_Artery_Disease, Hypertension, 
              Diabetes_insulin_, Diabetes_diet_OHGs_, GERD_PUD, Renal_Failure, 
              Stroke_CVA, Liver_Disease, Thyroid_Disease)
) %>%
  italicize_levels() %>% 
  bold_labels()

# Create summary table of Pre- and Intra-operative and Blood characteristics
tbl_summary(
  d,
  missing = "ifany",
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"),
  label = list(
    Pre_Hb ~ "Preoperative Hemoglobin (g/L)",
    Pre_Hct ~ "Preoperative Hematocrit (%)",
    Pre_Platelets ~ "Preoperative Platelets (×10⁹/L)",
    Pre_PT ~ "Preoperative Prothrombin Time (s)",
    Pre_INR ~ "Preoperative INR",
    Pre_PTT ~ "Preoperative PTT (s)",
    Pre_Creatinine ~ "Preoperative Creatinine (µmol/L)",
    Intraoperative_ECLS ~ "Intraoperative ECLS",
    ECLS_ECMO ~ "ECLS ECMO",
    ECLS_CPB ~ "ECLS Cardiopulmonary Bypass",
    Intra_Albumin_5_mL_ ~ "Intraoperative Albumin 5% (mL)",
    Intra_Crystalloid_mL_ ~ "Intraoperative Crystalloid (mL)",
    Intra_Cell_Saver_returned_mL_ ~ "Intraoperative Cell Saver Returned (mL)",
    Blood_Loss ~ "Blood Loss (mL)",
    Urine_Output ~ "Urine Output (mL)",
    Fluid_Balance ~ "Fluid Balance (mL)",
    Tranexamic_Acid_Used ~ "Tranexamic Acid Used"
  ),
  include = c(Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, Pre_PTT, Pre_Creatinine, 
              Intraoperative_ECLS, ECLS_ECMO, ECLS_CPB, Intra_Albumin_5_mL_, 
              Intra_Crystalloid_mL_, Intra_Cell_Saver_returned_mL_, Blood_Loss,
              Urine_Output, Fluid_Balance, Tranexamic_Acid_Used)
) %>%
  italicize_levels() %>%
  bold_labels()

## EDA Plots ##

# Only select variables will be plotted. Visualizing binary variables is not that helpful.

# Bar plot - Type, combined for commodities
ggplot(data = d, aes(x = Type)) +
  geom_bar(col = "black", fill = "lightblue", stat = "count") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) + 
  scale_y_continuous(limits = c(0, 200)) +
  labs(
    x = "Transplant Type",
    y = "Frequency",
    title = "Distribution of Lung Transplant Types"
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

# Create dataframe of comorbidities

# Reshape data to long format
d_long <- d %>%
  pivot_longer(cols = starts_with("COPD"):starts_with("Thyroid_Disease"), 
               names_to = "Comorbidity", 
               values_to = "Has_Comorbidity")

# Count number of patients with each comorbidity
comorbidity_df <- d_long %>%
  group_by(Comorbidity) %>%
  summarise(Count = sum(Has_Comorbidity))

# Rename comorbidities for clarity
comorbidity_df <- comorbidity_df %>% 
  mutate(
    Comorbidity = dplyr::recode(
      Comorbidity,
      COPD = "Chronic Obstructive Pulmonary Disease",
      alpha1_Antitrypsin_Deficiency = "Alpha-1 Antitrypsin Deficiency",
      Cystic_Fibrosis = "Cystic Fibrosis",
      Idiopathic_Pulmonary_Hypertension = "Idiopathic Pulmonary Hypertension",
      Interstitial_Lung_Disease = "Interstitial Lung Disease",
      Pulm_Other = "Other Pulmonary Conditions",
      Coronary_Artery_Disease = "Coronary Artery Disease",
      Hypertension = "Hypertension",
      Diabetes_insulin_ = "Diabetes (Insulin-Dependent)",
      Diabetes_diet_OHGs_ = "Diabetes (Diet/Oral Hypoglycemics)",
      GERD_PUD = "Gastroesophageal Reflux Disease/Peptic Ulcer Disease",
      Renal_Failure = "Renal Failure",
      Stroke_CVA = "Stroke (CVA)",
      Liver_Disease = "Liver Disease",
      Thyroid_Disease = "Thyroid Disease"
    )
  )

# Plot the frequency of commodities
ggplot(comorbidity_df, aes(x = reorder(Comorbidity, Count), y = Count)) +
  geom_col(col = "black", fill = "lightblue") +
  coord_flip() +                                      
  geom_text(aes(label = Count), vjust = 0.5, hjust = -0.3, size = 5) +
  labs(
    title = "Patient Comorbidities",
    x = "Comorbidity",
    y = "Frequency") +      
  theme_classic() +                                   
  theme(plot.title = element_text(hjust = 0.5))

# Hist - (Age, BMI, LAS), pre-operative, intra-operative

# Start with the figure for Age, BMI, and LAS
plot_list <- list()

dem_vars <- c("Age", "BMI", "LAS_score")

title_hist_dem <- c("Age" = "Age Distribution",
                    "BMI" = "BMI Distribution",
                    "LAS_score" = "Lung Allocation Score (LAS) Distribution")

x_lab_hist_dem <- c("Age" = "Age (Years)",
                    "BMI" = "BMI",
                    "LAS_score" = "Lung Allocation Score (LAS)")

for(var in dem_vars) {
  
  # Create historgram for current variable
  p <- ggplot(d, aes_string(x=var)) +
    geom_histogram(col="black", fill = "lightblue")+
    labs(
      title = title_hist_dem[var],
      x = x_lab_hist_dem[var],
      y = "Frequency") +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5))
  
  # Add the plot to the list
  plot_list[[var]] <- p
}

grid.arrange(grobs = plot_list, ncol = 3)

# Create histograms for important pre-operative vars

plot_list_2 <- list()

pre_vars <- c("Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_PT", "Pre_INR", "Pre_PTT", "Pre_Creatinine")

title_hist_pre<- c("Pre_Hb" = "Preoperative Hemoglobin \nDistribution",
                    "Pre_Hct" = "Preoperative Hematocrit \nDistribution",
                    "Pre_Platelets" = "Preoperative Platelets \nDistribution",
                    "Pre_PT" = "Preoperative Prothrombin Time \nDistribution",
                    "Pre_INR" = "Preoperative International Normalized Ratio \n(INR) Distribution",
                    "Pre_PTT" = "Partial Thromboplastin Time \nDistribution",
                    "Pre_Creatinine" = "Preoperative Creatinine \nDistribution")

x_lab_hist_pre <- c("Pre_Hb" = "Preoperative Hemoglobin Level (g/L)",
                    "Pre_Hct" = "Preoperative Hematocrit (%)",
                    "Pre_Platelets" = "Preoperative Platelet Count (×10⁹/L)",
                    "Pre_PT" = "Preoperative Prothrombin Time (s)",
                    "Pre_INR" = "Preoperative INR Index",
                    "Pre_PTT" = "Partial Thromboplastin Time (s)",
                    "Pre_Creatinine" = "Preoperative Creatinine Level (µmol/L)")

for(var in pre_vars) {
  
  # Create historgram for current variable
  p <- ggplot(d, aes_string(x=var)) +
    geom_histogram(col="black", fill = "lightblue")+
    labs(
      title = title_hist_pre[var],
      x = x_lab_hist_pre[var],
      y = "Frequency") +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5))
  
  # Add the plot to the list
  plot_list_2[[var]] <- p
}

grid.arrange(grobs = plot_list_2, ncol = 3)

# Create histograms for important intra-operative vars

plot_list_3 <- list()

intra_vars <- c("Intra_Albumin_5_mL_", "Intra_Crystalloid_mL_", "Intra_Cell_Saver_returned_mL_", "Blood_Loss", "Fluid_Balance")

title_hist_intra<- c("Intra_Albumin_5_mL_" = "Distribution of Intraoperative Albumin \nAdministered",
                   "Intra_Crystalloid_mL_" = "Distribution of Intraoperative Crystalloid \nAdministered",
                   "Intra_Cell_Saver_returned_mL_" = "Distribution of Intraoperative Cell Saver Volume \nReturned",
                   "Blood_Loss" = "Distribution of Intraoperative Blood Loss",
                   "Fluid_Balance" = "Distribution of Intraoperative Fluid Balance")

x_lab_hist_intra <- c("Intra_Albumin_5_mL_" = "Intraoperative Albumin Administered 5% (mL)",
                    "Intra_Crystalloid_mL_" = "Intraoperative Crystalloid Administered (mL)",
                    "Intra_Cell_Saver_returned_mL_" = "Intraoperative Cell Saver Volume Returned (mL)",
                    "Blood_Loss" = "Intraoperative Blood Loss (mL)",
                    "Fluid_Balance" = "Intraoperative Fluid Balance (mL)")


for(var in intra_vars) {
  
  # Create historgram for current variable
  p <- ggplot(d, aes_string(x=var)) +
    geom_histogram(col="black", fill = "lightblue")+
    labs(
      title = title_hist_intra[var],
      x = x_lab_hist_intra[var],
      y = "Frequency") +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5))
  
  # Add the plot to the list
  plot_list_3[[var]] <- p
}

grid.arrange(grobs = plot_list_3, ncol = 3)

###########################################
## Comparing Lasso Model and Tree Models ##
###########################################

# Create a version of the data without the RBC count because that is not a predictor

# Since BMI, weight, and height will include co-linearity, we can keep just BMI
# Also, pre-INR is a standardized version of pre-PT, so we can keep only the standardized one
# Similarly, we can keep only intraoperative_ECLS since it accounts for both ECMO and CPB.

# Also, can exclude binary predictors with fewer than 5% in one of the levels:
# a1t, idiopathic pulmonary, renal failure, stroke, liver disease, and tranexamic acid

d1 <- d_imp[,c(-3,-4,-8,-10,-18,-19,-20,-31,-36,-37,-44,-45)]

# Create an empty dataframe to store the AUC for each model and respective replicates
model.eval <- data.frame(model = NA, trial = NA, auc = NA)[0,]

# Create a list to keep track of which predictors are included in each model
selected_predictors <- list()

# Pick 5 seeds to randomize the testing/training split
seeds <- sample(1:1000, 5)

d1$transfusion <- as.factor(as.numeric(d1$transfusion))

# For each seed, create and evaluate the models (via AUC)
for (i in 1:length(seeds)) {
  
  # Split data into training and test
  set.seed(seeds[i])
  train.I <- sample(nrow(d1),round(nrow(d1)/(10/7)))
  
  # Create model matrix, exclude intercept column
  x <- model.matrix(transfusion ~.,d1)[train.I,-1]
  
  # Create a vector with the response values
  y <- d1$transfusion[train.I]
  
  # Fit the model
  lasso.mod <- glmnet(x,y,family="binomial")
  
  # Plot coefficient weights for different values of lambda
  plot(lasso.mod,label = T, xvar = "lambda")
  
  # Use cross validation (k=5) to determine the optimal value of lambda
  cv.lasso <- cv.glmnet(x,y,alpha=1,family = "binomial", type.measure = "auc", nfolds = 5)
  plot(cv.lasso)
  
  # Extract the value of lambda that maximizes auc
  # also extract the 1SE value of lambda that yields a more parsimonious model
  cv.lasso$lambda.min
  cv.lasso$lambda.1se
  
  # Extract coefficients at lambda.1se
  coefs <- coef(lasso.mod, s = cv.lasso$lambda.1se)
  non_zero_predictors <- rownames(coefs)[which(coefs != 0)]
  non_zero_predictors <- non_zero_predictors[non_zero_predictors != "(Intercept)"]
  
  # Store the selected predictors for this seed
  selected_predictors[[i]] <- non_zero_predictors
  
  # Create predictions for the test set
  pred.lasso <- as.numeric(predict(lasso.mod, newx = model.matrix(transfusion ~.,d1)[-train.I,-1], s=cv.lasso$lambda.1se, type = "response"))
  
  # Plot the ROC curve
  myroc <- roc(transfusion ~ pred.lasso, data=d1[-train.I,])
  ggroc(myroc) + geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "red") + theme_classic() + ggtitle("ROC Curve")
  
  # Extracting the Area Under the Curve, a measure of discrimination
  auc.lasso <- myroc$auc
  auc.lasso
  
  # Add the auc to the dataframe
  new_row <- data.frame(model = "Lasso", trial = i, auc = auc.lasso)
  model.eval <- rbind(model.eval, new_row)
  
  # Create a classification tree
  tree.mod <- tree(transfusion ~ . ,data = d1, subset=train.I)
  plot(tree.mod)
  text(tree.mod,pretty=0)
  
  
  # Cross-validation for pruning
  cv.res <- cv.tree(tree.mod, FUN=prune.tree)
  cv.res
  
  best.size <- cv.res$size[which.min(cv.res$dev)]
  best.size
  
  # Override size of 1 to avoid stump
  if(best.size == 1) {
    best.size <- 2
  }
  
  pruned <- prune.misclass(tree.mod,best=best.size)
  
  plot(pruned)
  text(pruned,pretty=0)
  
  
  # Calculating the accuracy on the test set for the pruned tree
  preds.tmp <- predict(pruned,newdata=d1[-train.I,],type="vector")
  pred.probs.tree <- as.numeric(preds.tmp[,2])
  myroc <- roc(transfusion ~ pred.probs.tree, data=d1[-train.I,])
  plot(myroc)
  myroc$auc
  
  # Add the auc to the dataframe
  new_row <- data.frame(model = "Pruned Tree", trial = i, auc = myroc$auc)
  model.eval <- rbind(model.eval, new_row)
  
  # Calculate the accuracy on the test set for the unpruned tree
  preds.tmp <- predict(tree.mod,newdata=d1[-train.I,],type="vector")
  pred.probs.tree <- as.numeric(preds.tmp[,2])
  myroc <- roc(transfusion ~ pred.probs.tree, data=d1[-train.I,])
  plot(myroc)
  myroc$auc
  
  # Add the auc to the dataframe
  new_row <- data.frame(model = "Unpruned Tree", trial = i, auc = myroc$auc)
  model.eval <- rbind(model.eval, new_row)
}

# Create a column graph to show the area under the curve for each model for each split
ggplot(model.eval, aes(x = trial, y = auc, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  theme_classic()

# It seems the lasso classification model is consistently better than the tree models

# Lets see which predictors were consistently included in the model

# Summarize predictors across all seeds
all_selected <- unlist(selected_predictors)
predictor_frequency <- table(all_selected)

# View predictors with their selection frequency
print(predictor_frequency)

# Identify predictors consistently selected in at least 4 of 5 seeds
consistent_predictors <- names(predictor_frequency[predictor_frequency > 3])
print(consistent_predictors)

########################
## Create Lasso Model ##
########################

# Now that we know the lasso model is the best choice, we can fit a lasso model
# using 100% of the data

# Create model matrix, exclude intercept column
x <- model.matrix(transfusion ~.,d1)[,-1]

# Create a vector with the response values
y <- d1$transfusion

# Fit the model
lasso.mod <- glmnet(x,y,family="binomial")

# Use cross validation (k=5) to determine the optimal value of lambda
cv.lasso <- cv.glmnet(x,y,alpha=1,family = "binomial", type.measure = "auc", nfolds = 5)
plot(cv.lasso)

# Extract the value of lambda that maximizes auc
# also extract the 1SE value of lambda that yields a more parsimonious model
cv.lasso$lambda.min
cv.lasso$lambda.1se

# Find the predictors in the model
coef(lasso.mod, s = cv.lasso$lambda.min)
coef_min_class <- coef(lasso.mod, s = cv.lasso$lambda.min)

# Plot the coefficients

# Convert the sparse matrix to a dataframe
coef_df_class <- as.data.frame(as.matrix(coef_min_class))

# Name the columns appropriately
colnames(coef_df_class) <- "Coefficient"

# Add the predictor names as a column
coef_df_class$Predictor <- rownames(coef_df_class)

# Filter for non-zero coefficients
coef_df_class <- coef_df_class[coef_df_class$Coefficient != 0, ]

# Only include features that were included in the model consistently
coef_df_class <- coef_df_class[coef_df_class$Predictor %in% consistent_predictors,]

# Sort predictors by magnitude of coefficients
coef_df_class <- coef_df_class[order(abs(coef_df_class$Coefficient), decreasing = TRUE), ]

# Reset row names
rownames(coef_df_class) <- NULL

# Plot the coefficients for the classification model
ggplot(coef_df_class, aes(x = reorder(Predictor, Coefficient), y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Predictor", y = "Coefficient", title = "LASSO Classification Model Coefficients")

############################
## Regression Lasso Model ##
############################

# Remove the binary column showing if the patient received transfusion
# Remove variables that may introduce co-linearity (same vars as model above)
# Remove binary variables with fewer than 5% in a level (same vars as model above)
# Also, only include patients who received transfusion
d2 <- d_imp[d_imp$Total_24hr_RBC != 0,c(-3,-4,-8,-10,-18,-19,-20,-31,-36,-37,-44,-46)]

# We want to test how sensitive the lasso model is for different data splits
# Create an empty dataframe to store the MSE for each replicate
model.eval.reg <- data.frame(trial = NA, test_mse = NA)[0,]

# Create a list to keep track of which predictors are included in each model
selected_predictors_2 <- list()

for(i in 1:length(seeds)) {
  
  # Split data into training and test
  set.seed(seeds[i])
  train.I <- sample(nrow(d2), round(nrow(d2)/(10/7)))
  
  # Obtain a matrix with the feature values
  x <- model.matrix(Total_24hr_RBC ~ . , d2)[train.I,-1]
  x_test <- model.matrix(Total_24hr_RBC ~ . , d2)[-train.I,-1]
  
  # Create a vector with the response values
  y <- d2$Total_24hr_RBC[train.I]
  y_test <- d2$Total_24hr_RBC[-train.I]
  
  # Train the models
  lasso.mod <- glmnet(x,y,family="gaussian")
  
  # Plot the results against different values of log(lambda)
  plot(lasso.mod,xvar = "lambda")
  
  # for different values of lambda we are getting different coefficient estimates (weights)
  # We need to decide on an optimal value for lambda
  # We will do it by performing cross-validation
  
  cv.lasso.reg <- cv.glmnet(x,y,nfolds = 5)
  plot(cv.lasso.reg)
  
  # We can extract the value that gives the lowest Cross-validated Mean Squared Error
  cv.lasso.reg$lambda.min
  
  # We can also extract the value of lambda within the 1SE of the best value to improve parsimony
  cv.lasso.reg$lambda.1se
  
  # The MSE for those value of lambda
  print(cv.lasso.reg)
  
  # We can see the value of the features that stay in the model when using the 1se lambda
  coef_min_reg <- coef(lasso.mod, s = cv.lasso.reg$lambda.1se)
  
  # Extract coefficients at lambda.1se
  non_zero_predictors <- rownames(coef_min_reg)[which(coefs != 0)]
  non_zero_predictors <- non_zero_predictors[non_zero_predictors != "(Intercept)"]
  
  # Store the selected predictors for this seed
  selected_predictors_2[[i]] <- non_zero_predictors
  
  # Make predictions on the testing set
  y_pred <- predict(lasso.mod, newx = x_test, s = cv.lasso.reg$lambda.1se)
  
  # Calculate the Mean Squared Error (MSE) on the testing set
  mse_test <- mean((y_test - y_pred)^2)
  
  # Update the dataframe with the test_mse
  new_row <- data.frame(trial = i, test_mse = mse_test)
  model.eval.reg <- rbind(model.eval.reg, new_row)
  
}

# Create a column graph to show the test MSE for each split
ggplot(model.eval.reg, aes(x = trial, y = test_mse)) +
  geom_col(fill = "lightblue", color = "black") +
  labs(
    x = "Trial",
    y = "Test MSE") +
  theme_classic()

# Lets see which predictors were consistently included in the model

# Summarize predictors across all seeds
all_selected <- unlist(selected_predictors_2)
predictor_frequency_2 <- table(all_selected)

# View predictors with their selection frequency
print(predictor_frequency_2)

# Identify predictors consistently selected in at least 4 of 5 seeds
consistent_predictors_2 <- names(predictor_frequency_2[predictor_frequency_2 > 3])
print(consistent_predictors_2)

# Nice - the same three predictors were included in all 5 models

# Now that we have an idea of how consistent the model is, we can train a new
# version using 100% of the data
x <- model.matrix(Total_24hr_RBC ~ . , d2)[,-1]
y <- d2$Total_24hr_RBC

# Train the models
lasso.mod <- glmnet(x,y,family="gaussian")

# Plot the results against different values of log(lambda)
plot(lasso.mod,xvar = "lambda")

# for different values of lambda we are getting different coefficient estimates (weights)
# We need to decide on an optimal value for lambda
# We will do it by performing cross-validation

cv.lasso.reg <- cv.glmnet(x,y,nfolds = 5)
plot(cv.lasso.reg)

# We can extract the value that gives the lowest Cross-validated Mean Squared Error
cv.lasso.reg$lambda.min

# We can also extract the value of lambda within the 1SE of the best value to improve parsimony
cv.lasso.reg$lambda.1se

# The MSE for those value of lambda
print(cv.lasso.reg)

# We can see the value of the features that stay in the model when using the 1se lambda
coef_min_reg <- coef(lasso.mod, s = cv.lasso.reg$lambda.1se)

# Plot the coefficients

# Convert the sparse matrix to a dataframe
coef_df_reg <- as.data.frame(as.matrix(coef_min_reg))

# Name the columns appropriately
colnames(coef_df_reg) <- "Coefficient"

# Add the predictor names as a column
coef_df_reg$Predictor <- rownames(coef_df_reg)

# Filter for non-zero coefficients
coef_df_reg <- coef_df_reg[coef_df_reg$Coefficient != 0, ]

# Sort predictors by magnitude of coefficients (optional)
coef_df_reg <- coef_df_reg[order(abs(coef_df_reg$Coefficient), decreasing = TRUE), ]

# Reset row names
rownames(coef_df_reg) <- NULL

# Plot the coefficients for the regression model
ggplot(coef_df_reg, aes(x = reorder(Predictor, Coefficient), y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Predictor", y = "Coefficient", title = "LASSO Regression Model Coefficients")

##############################
####### QUESTION TWO #########
##############################

library(dplyr)
library(survival)
library(lubridate)
library(survminer)
library(MASS)
library(ggplot2)
library(car)
library(cardx)

# Calculate percentage of missing values for each column
missing_percentage <- sapply(d_raw, function(x) sum(is.na(x)) / nrow(d_raw) * 100)

missing_percentage

# Variables to exclude based on =>30% missing data

# RBC 0-24hrs
# RBC 24-48hrs
# RBC 48-72hrs
# FFP 0-24hrs
# FFP 24-48hrs
# FFP 48-72hrs
# Plt 0-24hrs
# Plt 24-48hrs
# Plt 48-72hrs
# Cryo 0-24hrs
# Cryo 24-48hrs
# Cryo 48-72hrs

missing_columns <- c(
  "Massive Transfusion",
  "RBC 72hr Total", "FFP 72hr Total", "Plt 72hr Total", "Cryo 72hr Total",
  "Intra_Fresh Frozen Plasma", "Intra_Packed Cells", "Intra_PCC/Octaplex", 
  "Intra_Platelets", "Intra_Cryoprecipitate",
  
  "Duration of ICU Stay (days)", "Duration of Ventilation",
  "ALIVE_12MTHS_YN", "OR Date", "DEATH_DATE",
  "ICU_LOS", "HOSPITAL_LOS"
)

# Extract missing data from the original dataset
missing_data <- d_raw %>%
  dplyr::select(all_of(missing_columns))

# Combine `d_imp` with the missing columns
d_imp_bind <- cbind(d_imp, missing_data)

# Standardize column names
names(d_imp_bind) <- gsub("[^A-Za-z0-9]+", "_", names(d_imp_bind))  

# Convert death date and calculate time-to-event variables
data3 <- d_imp_bind %>%
  mutate(
    gender = factor(`Gender_male_`, levels = c(FALSE, TRUE), labels = c("Female", "Male")),
    death_date = dmy(DEATH_DATE, tz = "UTC"),  
    or_death_diff = if_else(is.na(death_date), 365, as.numeric(death_date - OR_Date)),
    death = as.factor(if_else(is.na(death_date), 0, 1)), # death indicator
    transfusion_status = ifelse(Total_24hr_RBC > 0, "Transfusion", "No Transfusion"),
    alive_12m = factor(ALIVE_12MTHS_YN, levels = c("N", "Y"), labels = c("No", "Yes")),
    
  )

###############
##### EDA #####
###############

q2_summary <- tbl_summary(
  data3,
  by = transfusion_status,  # Stratify by transfusion_status
  missing = "ifany",       
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",  
    all_categorical() ~ "{n} ({p}%)"    
  ),
  label = list(  # Custom labels for variables
    death ~ "Death Indicator (1 = Yes, 0 = No)",
    ICU_LOS ~ "ICU Length of Stay (days)",
    HOSPITAL_LOS ~ "Hospital Length of Stay (days)"
  ),
  include = c(death, ICU_LOS, HOSPITAL_LOS)  
) %>%
  add_p() %>%            
  italicize_levels() %>%
  bold_labels()

q2_summary

# Mortality rate by transfusion status 
ggplot(data3, aes(x = transfusion_status, fill = factor(death))) +
  geom_bar(position = "stack") + 
  scale_fill_manual(
    values = c("0" = "skyblue", "1" = "maroon"),  
    labels = c("Alive", "Dead")) +
  labs(
    title = "Mortality Rate by Transfusion Status",
    x = "Transfusion Status",
    y = "Proportion",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "right")

######################################
######### Survival Analysis ##########
######################################

# Survival Analysis - Kaplan-Meier curve
sf <- survfit(Surv(or_death_diff, death) ~ 1, data = data3)

# Plot Kaplan-Meier curve
plot(sf, xlab = "Time from Operation Date (days)", ylab = "Survival Probability")
title("Kaplan-Meier Curve: Survival from Operation Date")

# Stratified KM Curve by Transfusion Status
sf2 <- survfit(Surv(or_death_diff, death == "1") ~ transfusion_status, data = data3)

# Plot stratified KM curve
ggsurvplot(
  sf2,
  data = data3,
  title = "Survival by Transfusion Status",
  xlab = "Time from Operation Date (days)",
  ylab = "Survival Probability",
  legend.title = "Transfusion Status",
  legend.labs = c("No Transfusion", "Transfusion"),
  palette = c("skyblue", "lightpink")
)

# Log-rank test for survival differences by transfusion status
logrank <- survdiff(Surv(or_death_diff, death) ~ transfusion_status, data = data3)
print(logrank)

# Check PH assumption using cloglog
plot(
  survfit(Surv(or_death_diff, death == "1") ~ transfusion_status, data = data3),
  fun = "cloglog",
  main = "Complementary Log-Log Survival Plot by Transfusion Status", 
  xlab = "Time from Operation Date", 
  ylab = "Complementary Log-Log Survival Probability",
  col=1:3)

legend("bottomright",legend = c("No Transfusion", "Transfusion"), lty = 1, col = 1:2, cex = 0.8) 

# Cox PH model
coxmod <- coxph(Surv(or_death_diff, death == 1) ~ transfusion_status, data = data3)
coxmodsummary <- summary(coxmod) 
print(coxmodsummary)
# Patients who received transfusions had a 1.3x higher or 30% higher hazard of death
# compared to those who did not receive transfusions

################################
## Logistic Regression Models ##
################################

# Logistic Regression - Death as Outcome
log_model_death <- glm(
  death ~ transfusion_status + Massive_Transfusion + Total_24hr_RBC + RBC_72hr_Total +
    FFP_72hr_Total + Plt_72hr_Total + Cryo_72hr_Total +
    Intra_Fresh_Frozen_Plasma + Intra_Packed_Cells + Intra_PCC_Octaplex + 
    Intra_Platelets + Intra_Cryoprecipitate + gender + Height + Weight + Age + BMI,
  data = data3,
  family = binomial
)

summary(log_model_death)

# Stepwise AIC for best predictors
best_log_model_death <- stepAIC(log_model_death, direction = "both")
summary(best_log_model_death)

# Check for multicolinearity 
vif(log_model_death) # red flag

# Logistic Regression - Alive at 12m as Outcome
full_log_alive12m <- glm(
  alive_12m ~ transfusion_status + Massive_Transfusion + Total_24hr_RBC + RBC_72hr_Total +
    FFP_72hr_Total + Plt_72hr_Total + Cryo_72hr_Total +
    Intra_Fresh_Frozen_Plasma + Intra_Packed_Cells + Intra_PCC_Octaplex + 
    Intra_Platelets + Intra_Cryoprecipitate + gender + Height + Weight + Age + BMI,
  data = data3,
  family = binomial
)

summary(full_log_alive12m) 

# Stepwise AIC for best predictors
best_model_alive12m <- stepAIC(full_log_alive12m, direction = "both")
summary(best_model_alive12m)

####################################
# ICU and Hospital LOS Comparisons #
###################################

# # Assess Normality for ICU and Hospital Length of Stay by Transfusion Status
icu_los_normality <- shapiro.test(data3$ICU_LOS)
hospital_los_normality <- shapiro.test(data3$HOSPITAL_LOS)

# Print Shapiro test results
print(icu_los_normality)
print(hospital_los_normality)

# Extract ICU LOS for both groups
icu_no_transfusion <- data3 %>%
  filter(transfusion_status == "No Transfusion") %>%
  pull(ICU_LOS)

icu_transfusion <- data3 %>%
  filter(transfusion_status == "Transfusion") %>%
  pull(ICU_LOS)

# Extract Hospital LOS for both groups
hospital_no_transfusion <- data3 %>%
  filter(transfusion_status == "No Transfusion") %>%
  pull(HOSPITAL_LOS)

hospital_transfusion <- data3 %>%
  filter(transfusion_status == "Transfusion") %>%
  pull(HOSPITAL_LOS)

# Function to create histograms and QQ plots
plot_normality <- function(data, title_hist, title_qq, xlab_hist) {
  # Histogram
  hist(
    data, 
    main = title_hist, 
    xlab = xlab_hist, 
    col = "lightblue", 
    breaks = 10
  )
  
  # QQ Plot
  qqnorm(data, main = title_qq)
  qqline(data, col = "red")
}

# Normality plots for ICU LOS
plot_normality(
  icu_no_transfusion,
  title_hist = "Histogram: ICU LOS (No Transfusion)",
  title_qq = "QQ Plot: ICU LOS (No Transfusion)",
  xlab_hist = "ICU LOS (days)"
)

plot_normality(
  icu_transfusion,
  title_hist = "Histogram: ICU LOS (Transfusion)",
  title_qq = "QQ Plot: ICU LOS (Transfusion)",
  xlab_hist = "ICU LOS (days)"
)

# Normality plots for Hospital LOS
plot_normality(
  hospital_no_transfusion,
  title_hist = "Histogram: Hospital LOS (No Transfusion)",
  title_qq = "QQ Plot: Hospital LOS (No Transfusion)",
  xlab_hist = "Hospital LOS (days)"
)

plot_normality(
  hospital_transfusion,
  title_hist = "Histogram: Hospital LOS (Transfusion)",
  title_qq = "QQ Plot: Hospital LOS (Transfusion)",
  xlab_hist = "Hospital LOS (days)"
)

# Perform Wilcoxon test for LOS variables
wilcox_icu <- wilcox.test(ICU_LOS ~ transfusion_status, data = data3)
wilcox_hospital <- wilcox.test(HOSPITAL_LOS ~ transfusion_status, data = data3)

# Print Wilcoxon test results
print(wilcox_icu)
print(wilcox_hospital)

# Boxplots for LOS by Transfusion Status
ggplot(data3, aes(x = transfusion_status, y = ICU_LOS, fill = transfusion_status)) +
  geom_boxplot() +
  labs(title = "ICU Length of Stay by Transfusion Status", x = "Transfusion Status", y = "ICU LOS (days)") +
  theme_minimal()

ggplot(data3, aes(x = transfusion_status, y = HOSPITAL_LOS, fill = transfusion_status)) +
  geom_boxplot() +
  labs(title = "Hospital Length of Stay by Transfusion Status", x = "Transfusion Status", y = "Hospital LOS (days)") +
  theme_minimal()