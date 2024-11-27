
# Brainstorm for Q1

library(readxl)
library(tidyverse)
library(mice)
library(glmnet)
library(pROC)
library(tree)
library(visdat)

###############
## Data Prep ##
###############

setwd("C:/Users/ibrah/Desktop/Data Science in Health II/group5-teamproj")

d <- read_excel("transfusion data.xlsx")

View(d)
summary(d)

# remove irrelevant columns
d <- d[,c(4:45, 51:54, 116)]

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

###########################################
## Comparing Lasso Model and Tree Models ##
###########################################

# Create a version of the data without the RBC count because that is not a predictor
d1 <- d_imp[,-47]

# Create an empty dataframe to store the AUC for each model and respective replicates
model.eval <- data.frame(model = NA, trial = NA, auc = NA)[0,]

# Pick 5 seeds to randomize the testing/training split
seeds <- sample(1:1000, 5)

roc_plots <- list()

d1$transfusion <- as.factor(as.numeric(d1$transfusion))

# For each seed, create and evaluate the models (via AUC)
for (i in 1:length(seeds)) {
  
  # Split data into training and test, equal size
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
  
  # Extract the value of lambda
  cv.lasso$lambda.min
  
  # Find the predictors in the model
  coef(cv.lasso, s = "lambda.min")
  
  # Create predictions for the test set
  pred.lasso <- as.numeric(predict(lasso.mod, newx = model.matrix(transfusion ~.,d1)[-train.I,-1], s=cv.lasso$lambda.min, type = "response"))
  
  # Plot the ROC curve
  myroc <- roc(transfusion ~ pred.lasso, data=d1[-train.I,])
  ggroc(myroc) + geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "red") + theme_classic() + ggtitle("ROC Curve with Diagonal Line")
  plot(myroc, main = "Test")
  
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

##################################################
## Create Lasso Model for each Imputed Data Set ##
##################################################

# Now, we can use the multiple imputations to see if the relevant predictors are consistent
# across each imputation

# Start by initializing a dataframe where we can keep the results
predictor_results <- data.frame(
  predictor = character(), 
  matrix(NA, nrow = 0, ncol = imp$m, dimnames = list(NULL, paste0("dataset_", 1:imp$m))),
  stringsAsFactors = FALSE
)

# We can loop through each of the 5 imputed data sets, create a lasso model for each,
# and see if the resulting models are consistent with each other

for(i in 1:imp$m) {
  
  # Extract the imputed data set
  d.imp <- complete(imp, i)
  
  # Drop the number of RBC units column since we do not want it as a potential predictor
  d.imp <- d.imp[, -47]
  
  # Randomly split the data in a 7:3 ratio for training and testing respectively
  train.I <- sample(nrow(d.imp),round(nrow(d.imp)/(10/7)))
  
  # Create model matrix, exclude intercept column
  x <- model.matrix(transfusion ~.,d.imp)[train.I,-1]
  
  # Create a vector with the response values
  y <- d.imp$transfusion[train.I]
  
  # Fit the model
  lasso.mod <- glmnet(x,y,family="binomial")
  
  # Plot coefficient weights for different values of lambda
  plot(lasso.mod,label = T, xvar = "lambda")
  
  # Use cross validation (k=5) to determine the optimal value of lambda
  cv.lasso <- cv.glmnet(x,y,alpha=1,family = "binomial", type.measure = "auc", nfolds = 5)
  plot(cv.lasso)
  
  # Extract the value of lambda
  cv.lasso$lambda.min
  
  # Find the predictors in the model that are non-zero
  coef_lasso <- coef(cv.lasso, s = "lambda.min")
  predictors <- rownames(coef_lasso)[coef_lasso[, 1] != 0]
  coefficients <- coef_lasso[coef_lasso[, 1] != 0]
  
  # Add predictors and coefficients to the results data frame
  for (j in seq_along(predictors)) {
    predictor <- predictors[j]
    coefficient <- coefficients[j]
    
    # Check if predictor is already in the data frame
    if (!(predictor %in% predictor_results$predictor)) {
      # Add a new row for the predictor
      predictor_results <- rbind(
        predictor_results, 
        data.frame(predictor = predictor, stringsAsFactors = FALSE)
      )
    }
    
    # Update the coefficient for the current dataset
    predictor_results[predictor_results$predictor == predictor, paste0("dataset_", i)] <- coefficient
  }
  
  # Create predictions for the test set
  pred.lasso <- as.numeric(predict(lasso.mod, newx = model.matrix(transfusion ~.,d.imp)[-train.I,-1], s=cv.lasso$lambda.min, type = "response"))
  
  # Plot the ROC curve
  myroc <- roc(transfusion ~ pred.lasso, data=d.imp[-train.I,])
  ggroc(myroc) + geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "red") + theme_classic() + ggtitle("ROC Curve with Diagonal Line")
  plot(myroc, main = "Test")
  
  # Extracting the Area Under the Curve, a measure of discrimination
  auc.lasso <- myroc$auc
  auc.lasso
  
}


############################
## Regression Lasso Model ##
############################

d2 <- d_imp[d_imp$Total_24hr_RBC != 0,-48]

# Obtain a matrix with the feature values
x <- model.matrix(Total_24hr_RBC ~ . , d2)[,-1]

# Create a vector with the response values
y <- d2$Total_24hr_RBC

# Train the models
lasso.mod <- glmnet(x,y,family="gaussian")

# Plot the results against different values of log(lambda)
plot(lasso.mod,xvar = "lambda")

# for different values of lambda we are getting different coefficient estimates (weights)
# We need to decide on an optimal value for lambda
# We will do it by performing cross-validation

set.seed(123)

cv.lasso <- cv.glmnet(x,y,nfolds = 5)
plot(cv.lasso)

# We can extract the value that gives the lowest Cross-validated Mean Squared Error
cv.lasso$lambda.min

# The MSE for that value of lambda
print(cv.lasso)

# We can see the value of the features that stay in the model when using the optimal lambda
coef.min <- coef(cv.lasso, s = "lambda.min")
coef.min

# List of selected predictors, those that stayed in the model
rownames(coef.min)[coef.min[,1] != 0][-1]

## Experimenting ##
deviance_explained <- cv.lasso$glmnet.fit$dev.ratio[cv.lasso$lambda == cv.lasso$lambda.min]
deviance_explained

lin.mod <- lm(Total_24hr_RBC ~ ., data=d2)
summary(lin.mod)

lin.mod2 <- lm(Total_24hr_RBC ~ Pre_Hb + Pre_Hct + Pre_Platelets + Pre_PT + Pre_INR + Pre_PTT + Pre_Fibrinogen + Pre_Creatinine + Blood_Loss + Fluid_Balance + Preoperative_ECLS + Age + BMI + COPD + Coronary_Artery_Disease + Hypertension + Renal_Failure + Preoperative_ECLS + Urine_Output , data=d2)
summary(lin.mod2)

lin.mod3 <- lm(log(Total_24hr_RBC) ~ Pre_Hb + Pre_Hct + Pre_Platelets + Pre_PT + Pre_INR + Pre_PTT + Pre_Fibrinogen + Pre_Creatinine + Blood_Loss + Fluid_Balance, data=d2)
summary(lin.mod3)
