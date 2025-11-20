setwd("~/Desktop/Econometrics/R-Data")

# Opening libraries and packages 
library(tidyverse)
library(ggplot2)
library(haven)

#Exercise 1

### Define the pooled-model coefficients (use any numbers you like)
y0 <- 1.2   # intercept
y1 <- 0.05  # slope on Age
y2 <- -0.8  # effect of D
y3 <- 0.02  # interaction effect Age*D

### Relationships implied by the model:
### White households (D = 0): Yw = β0 + β1*Age
beta0 <- y0
beta1 <- y1

### Non-White households (D = 1): Ynw = α0 + α1*Age
alpha0 <- y0 + y2
alpha1 <- y1 + y3

### Print all relationships
cat("White group coefficients:\n")
cat("β0 =", beta0, "\n")
cat("β1 =", beta1, "\n\n")

cat("Non-White group coefficients:\n")
cat("α0 =", alpha0, "\n")
cat("α1 =", alpha1, "\n\n")

### OPTIONAL: Recover y-coefficients from α and β
y0_recovered <- beta0
y1_recovered <- beta1
y2_recovered <- alpha0 - beta0
y3_recovered <- alpha1 - beta1

cat("Recovered pooled-model coefficients:\n")
cat("y0 =", y0_recovered, "\n")
cat("y1 =", y1_recovered, "\n")
cat("y2 =", y2_recovered, "\n")
cat("y3 =", y3_recovered, "\n")

# Exercise 2

load("~/Desktop/Econometrics/R-Data/d_HHP2020_24.Rdata")

# Identify coefficients
names(d_HHP2020_24)

# OLS regression
ols_model1 <- lm(K4SUM ~ Education + income_midpoint + Age,
                 data = d_HHP2020_24)

summary(ols_model1)

library(car)

data_clean <- d_HHP2020_24 %>%
  dplyr::select(K4SUM, Education, income_midpoint, Age) %>%
  na.omit()

ols_model1 <- lm(K4SUM ~ Education + income_midpoint + Age,
                 data = data_clean)

model_no_educ <- lm(K4SUM ~ income_midpoint + Age,
                    data = data_clean)

model_no_income <- lm(K4SUM ~ Education + Age,
                      data = data_clean)

anova(model_no_educ, ols_model1)

anova(model_no_income, ols_model1)

# Joint test: all Education coefficients = 0
anova(model_no_educ, ols_model1)

# Joint test: all income coefficients = 0
anova(model_no_income, ols_model1)

# Exercise 3

# SUBSET: keep only individuals with income > 75,000
high_inc <- d_HHP2020_24 %>%
  filter(income_midpoint > 75000) %>%
  na.omit()   # remove missing values for clean summaries

# Summary statistics
summary(high_inc$K4SUM)
summary(high_inc$Education)
summary(high_inc$income_midpoint)
summary(high_inc$Age)

# Or: full summary table
summary(high_inc)

# Optional: quick look at the distribution of K4SUM
table(high_inc$K4SUM)

# Optional: mean K4SUM among high-income individuals
mean(high_inc$K4SUM)

library(dplyr)
install.packages("skimr")
library(skimr)

# High-income subset (already created earlier)
high_inc <- d_HHP2020_24 %>%
  filter(income_midpoint > 75000) %>%
  na.omit()

# Clean summary table of key variables
summary_table <- high_inc %>%
  select(K4SUM, Education, income_midpoint, Age)

skim(summary_table)

summary_table <- summary(high_inc[, c("K4SUM", "Education", "income_midpoint", "Age")])
summary_table

library(dplyr)

data.frame(
  Variable = c("K4SUM", "Age", "Income Midpoint"),
  Mean     = c(mean(high_inc$K4SUM),
               mean(high_inc$Age),
               mean(high_inc$income_midpoint)),
  Median   = c(median(high_inc$K4SUM),
               median(high_inc$Age),
               median(high_inc$income_midpoint)),
  SD       = c(sd(high_inc$K4SUM),
               sd(high_inc$Age),
               sd(high_inc$income_midpoint)),
  Min      = c(min(high_inc$K4SUM),
               min(high_inc$Age),
               min(high_inc$income_midpoint)),
  Max      = c(max(high_inc$K4SUM),
               max(high_inc$Age),
               max(high_inc$income_midpoint))
)

# Exercise 4
# Part A

high_inc$MentalHealth_01 <- as.numeric(high_inc$K4SUM > 8)

# OLS model (Linear Probability Model)
lpm1 <- lm(MentalHealth_01 ~ Age + Education + income_midpoint + Mar_Stat +
             Age:Education,   # interaction
           data = high_inc)

summary(lpm1)

# Part B

# You already ran:
# lpm1 <- lm(MentalHealth_01 ~ Age + Education + income_midpoint + Mar_Stat +
#              Age:Education, data = high_inc)

# 1. Full regression output
summary(lpm1)

# 2. Extract coefficients and p-values
coefs <- summary(lpm1)$coefficients
coefs

# 3. See which variables are statistically significant at 5%
sig_5pct <- coefs[, "Pr(>|t|)"] < 0.05
sig_5pct

# Part C

linearHypothesis(lpm1,
                 c("Educationsome hs = 0",
                   "Educationhigh school = 0",
                   "Educationsome college = 0",
                   "Educationassoc deg = 0",
                   "Educationcollege grad = 0",
                   "Educationadv degree = 0"))

linearHypothesis(lpm1,
                 c("Educationsome hs = 0",
                   "Educationhigh school = 0",
                   "Educationsome college = 0",
                   "Educationassoc deg = 0",
                   "Educationcollege grad = 0",
                   "Educationadv degree = 0",
                   "Age:Educationsome hs = 0",
                   "Age:Educationhigh school = 0",
                   "Age:Educationsome college = 0",
                   "Age:Educationassoc deg = 0",
                   "Age:Educationcollege grad = 0",
                   "Age:Educationadv degree = 0"))

# Part D

# Example people
new_people <- data.frame(
  Age = c(25, 45, 60),
  Education = factor(c("some hs", "college grad", "adv degree"),
                     levels = levels(high_inc$Education)),
  income_midpoint = c(80000, 150000, 200000),
  Mar_Stat = factor(c("never", "Married", "divorced"),
                    levels = levels(high_inc$Mar_Stat))
)

# Predicted probabilities from your OLS model
predict(lpm1, newdata = new_people)

# Part E

# 1. Get predicted probabilities from the LPM
pred_probs <- predict(lpm1, newdata = high_inc)

# 2. Turn probabilities into predicted classes (0/1) using 0.5 cutoff
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

# 3. Actual classes
actual <- high_inc$MentalHealth_01

# 4. Confusion matrix (for reference)
conf_matrix <- table(predicted = pred_class, actual = actual)
print(conf_matrix)

# 5. Type I and Type II errors computed directly

# Type I Error (False Positive): predict 1, actual 0
type1_error_count <- sum(pred_class == 1 & actual == 0, na.rm = TRUE)

# Type II Error (False Negative): predict 0, actual 1
type2_error_count <- sum(pred_class == 0 & actual == 1, na.rm = TRUE)

cat("Type I Errors (False Positives):", type1_error_count, "\n")
cat("Type II Errors (False Negatives):", type2_error_count, "\n")

# Exercise 5

# Part A

logit1 <- glm(MentalHealth_01 ~ Age + Education + income_midpoint + Mar_Stat,
              data = high_inc,
              family = binomial)

summary(logit1)


library(car)

linearHypothesis(logit1,
                 c("Educationsome hs = 0",
                   "Educationhigh school = 0",
                   "Educationsome college = 0",
                   "Educationassoc deg = 0",
                   "Educationcollege grad = 0",
                   "Educationadv degree = 0"))

linearHypothesis(logit1,
                 c("Mar_Statwidowed = 0",
                   "Mar_Statdivorced = 0",
                   "Mar_Statseparated = 0",
                   "Mar_Statnever = 0"))

# PArt D

# Example profiles for prediction
new_people_logit <- data.frame(
  Age = c(25, 45, 60),
  Education = factor(c("some hs", "college grad", "adv degree"),
                     levels = levels(high_inc$Education)),
  income_midpoint = c(80000, 150000, 200000),
  Mar_Stat = factor(c("never", "Married", "divorced"),
                    levels = levels(high_inc$Mar_Stat))
)

# Predicted probabilities from the LOGIT model
predict(logit1, newdata = new_people_logit, type = "response")

# PArt E

# 1. Predicted probabilities from the logit model
logit_probs <- predict(logit1, newdata = high_inc, type = "response")

# 2. Turn probabilities into predicted classes (0/1) with cutoff 0.5
logit_pred_class <- ifelse(logit_probs > 0.5, 1, 0)

# 3. Actual values
actual <- high_inc$MentalHealth_01

# 4. Confusion matrix
conf_matrix_logit <- table(predicted = logit_pred_class,
                           actual = actual)
conf_matrix_logit

# 5. Count Type I and Type II errors

# Type I error (False Positive): predicted 1, actual 0
type1_logit <- sum(logit_pred_class == 1 & actual == 0, na.rm = TRUE)

# Type II error (False Negative): predicted 0, actual 1
type2_logit <- sum(logit_pred_class == 0 & actual == 1, na.rm = TRUE)

cat("Type I errors (false positives):", type1_logit, "\n")
cat("Type II errors (false negatives):", type2_logit, "\n")


# Exercise 6

library(randomForest)

set.seed(123)
rf_model <- randomForest(
  as.factor(MentalHealth_01) ~ Age + Education + income_midpoint + Mar_Stat,
  data = high_inc,
  ntree = 300,
  importance = TRUE
)

rf_model
importance(rf_model)

rf_pred <- predict(rf_model, high_inc)
rf_conf <- table(predicted = rf_pred, actual = high_inc$MentalHealth_01)
rf_conf

library(glmnet)

X <- model.matrix(MentalHealth_01 ~ Age + Education + income_midpoint + Mar_Stat,
                  data = high_inc)[, -1]
y <- high_inc$MentalHealth_01

cv_en <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
en_model <- glmnet(X, y, family = "binomial", alpha = 0.5, lambda = cv_en$lambda.min)

en_model

