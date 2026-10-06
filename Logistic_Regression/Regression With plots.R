################################################################################
# TASK 3A – LOGISTIC REGRESSION
# Simpler model alternative to Random Forest
################################################################################

library(readr)

################################################################################
# 1. LOAD DATA
################################################################################

train <- read_csv("../Data/processed/train.csv", show_col_types = FALSE)
test  <- read_csv("../Data/processed/test.csv", show_col_types = FALSE)


################################################################################
# 2. DEFINE FEATURES
################################################################################

features <- c(
  "log_access_pct_quota",
  "planned_months",
  "n_reviews_sched",
  "n_struct_cond",
  "n_prior_actions",
  "prev_n_20y",
  "years_since_prev",
  "no_track_record",
  "growth",
  "inflation",
  "current_account_gdp",
  "concessional",
  "extended"
)


################################################################################
# 3. TRAIN LOGISTIC REGRESSION
################################################################################

logit_formula <- as.formula(
  paste("interrupted ~", paste(features, collapse = " + "))
)

logit_model <- glm(
  logit_formula,
  data = train,
  family = binomial()
)

summary(logit_model)


################################################################################
# 4. PREDICTIONS ON TEST SET
################################################################################

# Predicted probability of interruption
test_prob <- predict(
  logit_model,
  newdata = test,
  type = "response"
)

# Convert probabilities into 0/1 predictions
# 0.50 threshold
test_pred <- ifelse(test_prob >= 0.50, 1, 0)


################################################################################
# 5. CONFUSION MATRIX
################################################################################

conf_matrix <- table(
  Actual = test$interrupted,
  Predicted = test_pred
)

print(conf_matrix)


################################################################################
# 6. CALCULATE PERFORMANCE METRICS
################################################################################

TN <- conf_matrix["0", "0"]
FP <- conf_matrix["0", "1"]
FN <- conf_matrix["1", "0"]
TP <- conf_matrix["1", "1"]

accuracy <- (TP + TN) / (TP + TN + FP + FN)

sensitivity <- TP / (TP + FN)

specificity <- TN / (TN + FP)

balanced_accuracy <- (sensitivity + specificity) / 2

cat("\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Sensitivity:", round(sensitivity, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")
cat("Balanced Accuracy:", round(balanced_accuracy, 3), "\n")


################################################################################
# 7. AUC
################################################################################

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}

library(pROC)

roc_logit <- roc(
  test$interrupted,
  test_prob
)

auc_value <- auc(roc_logit)

cat("AUC:", round(auc_value, 3), "\n")


################################################################################
# 8. GRAPH 1 – ROC CURVE
################################################################################

plot(
  roc_logit,
  main = "ROC Curve – Logistic Regression",
  lwd = 2
)

abline(
  a = 0,
  b = 1,
  lty = 2
)

legend(
  "bottomright",
  legend = paste("AUC =", round(auc_value, 3)),
  bty = "n"
)

# Save graph
dev.copy(
  png,
  "fig_logit_roc.png",
  width = 800,
  height = 600
)

dev.off()


################################################################################
# 9. GRAPH 2 – LOGISTIC REGRESSION COEFFICIENTS
################################################################################

coef_values <- coef(logit_model)

# Remove intercept
coef_values <- coef_values[
  names(coef_values) != "(Intercept)"
]

par(mar = c(5, 12, 4, 2))

barplot(
  coef_values,
  horiz = TRUE,
  las = 1,
  main = "Logistic Regression Coefficients",
  xlab = "Coefficient (log-odds)"
)

abline(
  v = 0,
  lty = 2
)

# Save graph
dev.copy(
  png,
  "fig_logit_coefficients.png",
  width = 1000,
  height = 700
)

dev.off()

# Reset plotting margins
par(
  mar = c(5.1, 4.1, 4.1, 2.1)
)


################################################################################
# 10. GRAPH 3 – PREDICTED PROBABILITIES
################################################################################

boxplot(
  test_prob ~ factor(
    test$interrupted,
    levels = c(0, 1),
    labels = c(
      "Not interrupted",
      "Interrupted"
    )
  ),
  main = "Predicted Probability of Interruption",
  xlab = "Actual outcome",
  ylab = "Predicted probability",
  ylim = c(0, 1)
)

# Show 50% classification threshold
abline(
  h = 0.5,
  lty = 2
)

# Save graph
dev.copy(
  png,
  "fig_logit_probabilities.png",
  width = 800,
  height = 600
)

dev.off()


################################################################################
# 11. OPTIONAL – ODDS RATIOS
################################################################################

odds_ratios <- exp(coef(logit_model))

print(odds_ratios)


################################################################################
# 12. SUMMARY
################################################################################

cat("\n")
cat("========================================\n")
cat("LOGISTIC REGRESSION RESULTS\n")
cat("========================================\n")
cat("Test observations:", nrow(test), "\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Sensitivity:", round(sensitivity, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")
cat("Balanced Accuracy:", round(balanced_accuracy, 3), "\n")
cat("AUC:", round(auc_value, 3), "\n")
cat("========================================\n")