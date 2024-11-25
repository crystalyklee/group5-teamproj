
# Brainstorm for Q1

library(readxl)
library(tidyverse)

###############
## Data Prep ##
###############

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

##########################
## Logistic Lasso Model ##
##########################

