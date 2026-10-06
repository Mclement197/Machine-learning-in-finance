################################################################################
# TASK 3A - LOGISTIC REGRESSION
# Simple model to compare with the Random Forest
################################################################################

rm(list = ls())

library(readr)

set.seed(72)

################################################################################
# 1. LOAD THE FINAL MODEL-READY DATA
################################################################################

train <- read_csv("../Data/processed/train.csv",
                  show_col_types = FALSE)

test <- read_csv("../Data/processed/test.csv",
                 show_col_types = FALSE)

# Check the data
dim(train)
dim(test)

names(train)
summary(train)

################################################################################
# 2. DEFINE THE PREDICTORS
################################################################################

features <- c(
  "log_access_pct_quota",
  "planned_months",
  "n_reviews_sched",
  "n_struct_cond",
  "n_prior_actions",
  "prev_n_20y",
  "years_since_prev",
  "growth",
  "inflation",
  "current_account_gdp",
  "concessional",
  "extended",
  "no_track_record"
)

################################################################################
# 3. FIT THE LOGISTIC REGRESSION
################################################################################

# The dependent variable is:
# interrupted = 1 if the IMF programme was interrupted
# interrupted = 0 if the IMF programme was completed

logit_formula <- as.formula(
  paste("interrupted ~", paste(features, collapse = " + "))
)

logit_model <- glm(
  logit_formula,
  data = train,
  family = binomial()
)

# View the regression results
summary(logit_model)

################################################################################
# 4. PREDICT PROBABILITIES ON THE TEST SET
################################################################################

# Logistic regression produces probabilities between 0 and 1.

test_prob <- predict(
  logit_model,
  newdata = test,
  type = "response"
)

# Look at the predicted probabilities
test_prob

################################################################################
# 5. CLASSIFY PROGRAMMES
################################################################################

# Standard classification threshold:
# probability >= 0.50 -> predicted interruption
# probability < 0.50  -> predicted no interruption

test_pred <- ifelse(test_prob >= 0.50, 1, 0)

################################################################################
# 6. CONFUSION MATRIX
################################################################################

confusion_matrix <- table(
  Actual = test$interrupted,
  Predicted = test_pred
)

confusion_matrix

################################################################################
# 7. CALCULATE TP, TN, FP AND FN
################################################################################

TP <- sum(test_pred == 1 & test$interrupted == 1)
TN <- sum(test_pred == 0 & test$interrupted == 0)
FP <- sum(test_pred == 1 & test$interrupted == 0)
FN <- sum(test_pred == 0 & test$interrupted == 1)

TP
TN
FP
FN

################################################################################
# 8. ACCURACY
################################################################################

accuracy <- mean(test_pred == test$interrupted)

accuracy

################################################################################
# 9. SENSITIVITY AND SPECIFICITY
################################################################################

sensitivity <- TP / (TP + FN)

specificity <- TN / (TN + FP)

sensitivity
specificity

################################################################################
# 10. BALANCED ACCURACY
################################################################################

balanced_accuracy <- (sensitivity + specificity) / 2

balanced_accuracy

################################################################################
# 11. AUC
################################################################################

# AUC measures how well the model ranks interrupted programmes
# above non-interrupted programmes across different thresholds.

positive_ranks <- rank(test_prob)[test$interrupted == 1]

n_positive <- sum(test$interrupted == 1)
n_negative <- sum(test$interrupted == 0)

auc <- (
  sum(positive_ranks) -
    n_positive * (n_positive + 1) / 2
) / (n_positive * n_negative)

auc

################################################################################
# 12. FINAL RESULTS
################################################################################

cat("\n==============================\n")
cat("LOGISTIC REGRESSION RESULTS\n")
cat("==============================\n")

cat("Test observations:", nrow(test), "\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Sensitivity:", round(sensitivity, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")
cat("Balanced Accuracy:", round(balanced_accuracy, 3), "\n")
cat("AUC:", round(auc, 3), "\n")

cat("\nConfusion Matrix:\n")
print(confusion_matrix)

cat("\nTP:", TP, "\n")
cat("TN:", TN, "\n")
cat("FP:", FP, "\n")
cat("FN:", FN, "\n")