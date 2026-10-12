################################################################################
# RANDOM FOREST: PREDICTING IMF PROGRAMME INTERRUPTION
################################################################################
# Run Data_Prep/01_data.R first, then run this script from Random_Forest/.
# In RStudio: Session > Set Working Directory > To Source File Location.
# Results are saved in rf_outputs/; the seven graphs are generated in Section 11.

################################################################################
# 1. PACKAGES AND DATA
################################################################################
options(install.packages.check.source = "no")
if (!requireNamespace("tree", quietly = TRUE)) install.packages("tree")
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")

library(tree)
library(randomForest)
library(caret)

getwd()
if (!all(file.exists(c("../Data/processed/train.csv", "../Data/processed/test.csv")))) {
  stop("Run Data_Prep/01_data.R first, then set the working directory to Random_Forest/.")
}

# Load development (2002-2013) and test (2014-2016) programmes.
# Missing values have already been filled using development-set medians.
dev_data  <- read.csv("../Data/processed/train.csv")
test_data <- read.csv("../Data/processed/test.csv")

# Use label_B for the interruption outcome and iso3c for country grouping.
dev_data$label_B  <- dev_data$interrupted
test_data$label_B <- test_data$interrupted
dev_data$iso3c    <- dev_data$Member.Code
test_data$iso3c   <- test_data$Member.Code

dim(dev_data)
dim(test_data)

# Select 13 predictors available at programme approval.
# Country identifiers are used for validation folds, not as predictors.
predictors <- c("log_access_pct_quota", "planned_months", "n_reviews_sched",
                "concessional", "extended",
                "n_struct_cond", "n_prior_actions",
                "years_since_prev", "prev_n_20y", "no_track_record",
                "growth", "inflation", "current_account_gdp")

# Check required columns, missing values and outcome coding.
required <- c("label_B", "iso3c", predictors)
if (!all(required %in% names(dev_data)) || !all(required %in% names(test_data))) {
  stop("Missing columns: regenerate train.csv and test.csv with the updated 01_data.R.")
}
dev_rf  <- dev_data[, c("label_B", predictors)]
test_rf <- test_data[, c("label_B", predictors)]
colSums(is.na(dev_rf))
if (anyNA(dev_rf) || anyNA(test_rf) || anyNA(dev_data$iso3c)) {
  stop("Missing model values or training country IDs; check the data-preparation output.")
}
if (!all(dev_rf$label_B %in% c(0, 1)) || !all(test_rf$label_B %in% c(0, 1))) {
  stop("The interrupted outcome must be coded 0 or 1.")
}

# Use a factor for classification: no = completed, yes = interrupted.
# Interruption means at least one approval-scheduled review was never completed.
# Evaluation treats "yes" as the positive class.
dev_rf$label_B <- factor(dev_rf$label_B, levels = c(0, 1), labels = c("no", "yes"))
test_rf$label_B <- factor(test_rf$label_B, levels = c(0, 1), labels = c("no", "yes"))
summary(dev_rf)
summary(test_rf)
table(dev_rf$label_B)
round(prop.table(table(dev_rf$label_B)), 3)
table(test_rf$label_B)
round(prop.table(table(test_rf$label_B)), 3)

################################################################################
# 2. PERFORMANCE METRICS
################################################################################
# Balanced accuracy averages sensitivity (interrupted programmes) and
# specificity (completed programmes), giving both classes equal weight.
# Both classes must be present for the metric to be defined.
balanced_accuracy <- function(y_true, y_pred) {
  sens <- mean(y_pred[y_true == "yes"] == "yes")
  spec <- mean(y_pred[y_true == "no"]  == "no")
  (sens + spec) / 2
}

# Predict "yes" when its vote share reaches the threshold.
# This helper defaults to 0.50; the final model uses a tuned threshold.
to_class <- function(p, threshold = 0.5) {
  factor(ifelse(p >= threshold, "yes", "no"), levels = c("no", "yes"))
}

# Extract four performance metrics from a caret confusion matrix.
cm_row <- function(cm, name, sample_name) {
  data.frame(model = name, sample = sample_name,
             accuracy    = round(cm$overall[["Accuracy"]], 3),
             sensitivity = round(cm$byClass[["Sensitivity"]], 3),
             specificity = round(cm$byClass[["Specificity"]], 3),
             bal_acc     = round(cm$byClass[["Balanced Accuracy"]], 3))
}

################################################################################
# 3. CLASSIFICATION TREE
################################################################################
# Fit a single classification tree on the development data.
tree.dev_rf <- tree(label_B ~ ., data = dev_rf)
summary(tree.dev_rf)

################################################################################
# 4. DEFAULT AND COMPARISON FORESTS
################################################################################
# Fit 500 trees with default classification settings: mtry = 3, nodesize = 1.
# mtry is the number of candidate predictors considered at each split.
set.seed(72)
rf_default <- randomForest(label_B ~ ., data = dev_rf, ntree = 500)
rf_default

# Calculate accuracy on the observations used to train the model.
p_in_sample <- predict(rf_default, newdata = dev_rf, type = "response")
mean(p_in_sample == dev_rf$label_B)

# Evaluate the default model on the test set at threshold 0.50.
# These scores are not used to choose model settings or the final threshold.
p_default_test <- predict(rf_default, newdata = test_rf, type = "prob")[, "yes"]
cm_default <- confusionMatrix(to_class(p_default_test), test_rf$label_B, positive = "yes")
cm_default

# Fit 1,000 trees to plot out-of-bag (OOB) error against forest size.
set.seed(72)
rf_long <- randomForest(label_B ~ ., data = dev_rf, ntree = 1000)

# Fit a separate default-settings forest to calculate permutation importance.
# This is not the forest used for the default model's test predictions.
set.seed(72)
rf_default_imp <- randomForest(label_B ~ ., data = dev_rf, ntree = 500, importance = TRUE)
importance(rf_default_imp, type = 1, scale = FALSE)

# Fit a comparison forest with mtry = 4, nodesize = 5 and 500 trees.
set.seed(72)
rf_slides <- randomForest(label_B ~ ., data = dev_rf, mtry = 4, ntree = 500,
                          nodesize = 5, importance = TRUE)
rf_slides
importance(rf_slides, type = 1, scale = FALSE)

################################################################################
# 5. HYPERPARAMETER GRID AND VALIDATION SETTINGS
################################################################################
# Four tree counts x six mtry values = 24 settings per validation scheme.
# caret tunes mtry; the surrounding for-loop supplies ntree.
ntree_grid <- c(100, 250, 500, 1000)
mtry_grid  <- expand.grid(mtry = c(2, 3, 4, 6, 8, 12))

# Keep all programmes of a country in the same validation fold.
# The final test set instead contains later years, not necessarily new countries.
set.seed(72)
folds_country <- groupKFold(dev_data$iso3c, k = 10)

# Save out-of-fold classes and probabilities for every tuning combination.
control_10cv <- trainControl(method = "cv", number = 10, classProbs = TRUE,
                             savePredictions = "all")
control_country <- trainControl(method = "cv", index = folds_country, classProbs = TRUE,
                                savePredictions = "all")
control_loocv <- trainControl(method = "LOOCV", classProbs = TRUE,
                              savePredictions = "all")

# Select settings using balanced accuracy calculated from saved predictions.
# caret's internal bestTune uses ordinary accuracy and is not used for selection.
cv_results <- NULL

################################################################################
# 6A. 10-FOLD CV
################################################################################
# Hold out individual programmes and average performance across folds.
models_10cv <- list()
for (n_tree in ntree_grid) {
  set.seed(72)
  model <- train(dev_rf[, predictors], dev_rf$label_B, method = "rf",
                 tuneGrid = mtry_grid, trControl = control_10cv, ntree = n_tree)
  models_10cv[[as.character(n_tree)]] <- model

  for (m in mtry_grid$mtry) {
    pred_m <- model$pred[model$pred$mtry == m, ]
    bal_folds <- c()
    acc_folds <- c()
    for (f in unique(pred_m$Resample)) {
      fold <- pred_m[pred_m$Resample == f, ]
      bal_folds <- c(bal_folds, balanced_accuracy(fold$obs, fold$pred))
      acc_folds <- c(acc_folds, mean(fold$obs == fold$pred))
    }
    bal <- mean(bal_folds, na.rm = TRUE)
    acc <- mean(acc_folds, na.rm = TRUE)
    cv_results <- rbind(cv_results,
                        data.frame(scheme = "10-fold CV", ntree = n_tree, mtry = m,
                                   bal_acc = round(bal, 3), accuracy = round(acc, 3)))
  }
}

################################################################################
# 6B. 10-FOLD CV BY COUNTRY
################################################################################
# Hold out entire countries and average performance across folds.
models_country <- list()
for (n_tree in ntree_grid) {
  set.seed(72)
  model <- train(dev_rf[, predictors], dev_rf$label_B, method = "rf",
                 tuneGrid = mtry_grid, trControl = control_country, ntree = n_tree)
  models_country[[as.character(n_tree)]] <- model

  for (m in mtry_grid$mtry) {
    pred_m <- model$pred[model$pred$mtry == m, ]
    bal_folds <- c()
    acc_folds <- c()
    for (f in unique(pred_m$Resample)) {
      fold <- pred_m[pred_m$Resample == f, ]
      bal_folds <- c(bal_folds, balanced_accuracy(fold$obs, fold$pred))
      acc_folds <- c(acc_folds, mean(fold$obs == fold$pred))
    }
    bal <- mean(bal_folds, na.rm = TRUE)
    acc <- mean(acc_folds, na.rm = TRUE)
    cv_results <- rbind(cv_results,
                        data.frame(scheme = "10-fold CV by country", ntree = n_tree, mtry = m,
                                   bal_acc = round(bal, 3), accuracy = round(acc, 3)))
  }
}

################################################################################
# 6C. LOOCV
################################################################################
# Leave-one-out CV: hold out one programme at a time, not one country.
models_loocv <- list()
for (n_tree in ntree_grid) {
  set.seed(72)
  model <- train(dev_rf[, predictors], dev_rf$label_B, method = "rf",
                 tuneGrid = mtry_grid, trControl = control_loocv, ntree = n_tree)
  models_loocv[[as.character(n_tree)]] <- model

  for (m in mtry_grid$mtry) {
    # Pool leave-one-out predictions: a single observation cannot provide
    # both sensitivity and specificity.
    pred_m <- model$pred[model$pred$mtry == m, ]
    bal <- balanced_accuracy(pred_m$obs, pred_m$pred)
    acc <- mean(pred_m$obs == pred_m$pred)
    cv_results <- rbind(cv_results,
                        data.frame(scheme = "LOOCV", ntree = n_tree, mtry = m,
                                   bal_acc = round(bal, 3), accuracy = round(acc, 3)))
  }
}

################################################################################
# 7. SELECT THE FINAL SETTINGS AND THRESHOLD
################################################################################
# Select the highest balanced accuracy, rounded to three decimals.
# Ties use the first grid entry: fewer trees, then smaller mtry.
cv_results
res_10cv    <- cv_results[cv_results$scheme == "10-fold CV", ]
res_country <- cv_results[cv_results$scheme == "10-fold CV by country", ]
res_loocv   <- cv_results[cv_results$scheme == "LOOCV", ]
best_10cv    <- res_10cv[which.max(res_10cv$bal_acc), ]
best_country <- res_country[which.max(res_country$bal_acc), ]
best_loocv   <- res_loocv[which.max(res_loocv$bal_acc), ]
rbind(best_10cv, best_country, best_loocv)

# Use the settings selected by country-grouped cross-validation.
final_ntree <- best_country$ntree
final_mtry  <- best_country$mtry
final_ntree
final_mtry

# Retrieve predictions made while each programme's country was held out.
model_country <- models_country[[as.character(final_ntree)]]
oof <- model_country$pred[model_country$pred$mtry == final_mtry, ]
oof <- oof[order(oof$rowIndex), ]

# Maximise pooled out-of-fold balanced accuracy using development data only.
# Search thresholds from 0.05 to 0.95; ties select the lowest threshold.
thresholds <- seq(0.05, 0.95, by = 0.01)
best_threshold <- 0.5
best_bal <- -1
for (t in thresholds) {
  bal <- balanced_accuracy(oof$obs, to_class(oof$yes, t))
  if (bal > best_bal) {
    best_bal <- bal
    best_threshold <- t
  }
}
cat(sprintf("Best balanced accuracy (dev out-of-fold): %.3f\n", best_bal))
cat(sprintf("Best threshold: %.2f\n", best_threshold))

################################################################################
# 8. FINAL FOREST, TEST PREDICTIONS AND ACCURACY
################################################################################
# Fit the selected forest on all development observations and calculate
# permutation importance. Evaluate test predictions at the tuned threshold.
set.seed(72)
rf_final <- randomForest(label_B ~ ., data = dev_rf, ntree = final_ntree,
                         mtry = final_mtry, importance = TRUE)
rf_final

p_test <- predict(rf_final, newdata = test_rf, type = "prob")[, "yes"]
pred_test <- to_class(p_test, best_threshold)
cm_final <- confusionMatrix(pred_test, test_rf$label_B, positive = "yes")
cm_final

# Benchmark: predict interruption for every test programme.
pred_baseline <- factor(rep("yes", nrow(test_rf)), levels = c("no", "yes"))
cm_baseline <- confusionMatrix(pred_baseline, test_rf$label_B, positive = "yes")

# Evaluate pooled out-of-fold predictions at the tuned threshold.
# This differs from averaging fold scores using the saved class predictions.
cm_oof <- confusionMatrix(to_class(oof$yes, best_threshold), oof$obs, positive = "yes")
results_tbl <- rbind(cm_row(cm_baseline, "Baseline: all interrupted", "test"),
                     cm_row(cm_default,  "RF default", "test"),
                     cm_row(cm_final,    "RF final (tuned)", "test"),
                     cm_row(cm_oof,      "RF final (tuned)", "dev out-of-fold"))
results_tbl

################################################################################
# 9. PERMUTATION IMPORTANCE AND OOB COMPARISON
################################################################################
# Average the drop in each tree's OOB accuracy after shuffling a predictor.
# type = 1 selects permutation importance; scale = FALSE keeps accuracy units.
# scale = TRUE divides by the standard error. This uses ordinary accuracy,
# not balanced accuracy, and does not establish direction or causality.
final_importance <- importance(rf_final, type = 1, scale = FALSE)
round(final_importance[order(final_importance[, 1], decreasing = TRUE), , drop = FALSE], 4)
importance(rf_final, type = 1, scale = TRUE)

# Compare the default and tuned forests using their final OOB error rates.
default_oob <- tail(rf_default$err.rate[, "OOB"], 1)
final_oob   <- tail(rf_final$err.rate[, "OOB"], 1)
default_oob
final_oob

################################################################################
# 10. AUC AND SAVED RESULTS
################################################################################
# AUC evaluates the ranking of vote shares without a classification threshold.
# Higher scores represent a greater predicted probability of interruption.
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
library(pROC)
auc_score <- function(y_true, p) {
  as.numeric(auc(roc(y_true, p, levels = c("no", "yes"), direction = "<", quiet = TRUE)))
}
auc_results <- data.frame(
  model = c("RF default", "RF final (tuned)", "RF final (tuned)"),
  sample = c("test", "test", "dev out-of-fold"),
  AUC = round(c(auc_score(test_rf$label_B, p_default_test),
                auc_score(test_rf$label_B, p_test),
                auc_score(oof$obs, oof$yes)), 3)
)
auc_results

# Save tuning scores, model metrics, settings, test predictions and importance.
dir.create("rf_outputs", showWarnings = FALSE)
write.csv(cv_results, "rf_outputs/cv_results.csv", row.names = FALSE)
write.csv(results_tbl, "rf_outputs/model_scores.csv", row.names = FALSE)
write.csv(auc_results, "rf_outputs/auc_results.csv", row.names = FALSE)
write.csv(data.frame(ntree = final_ntree, mtry = final_mtry,
                     threshold = best_threshold),
          "rf_outputs/selected_settings.csv", row.names = FALSE)
write.csv(data.frame(country = test_data$Member.Code,
                     actual = test_rf$label_B, probability = p_test,
                     predicted = pred_test),
          "rf_outputs/test_predictions.csv", row.names = FALSE)
write.csv(data.frame(variable = rownames(final_importance),
                     MeanDecreaseAccuracy = final_importance[, 1]),
          "rf_outputs/permutation_importance.csv", row.names = FALSE)

################################################################################
# 11. GRAPHS
################################################################################
# Plot the tree, OOB errors, tuning results and importance; save seven PNGs.
# Enable plot recording so dev.copy() can export the current plot.
par(mfrow = c(1, 1))
dev.control(displaylist = "enable")

# 1. Single classification tree.
par(mar = c(1, 1, 1, 1))
plot(tree.dev_rf, lwd = 1.5)
text(tree.dev_rf, pretty = 0, cex = 0.8)
dev.copy(png, "fig_tree.png", width = 1400, height = 900)
dev.off()
par(mar = c(5.1, 4.1, 4.1, 2.1))

# Plot overall and class-specific OOB error as the number of trees increases.
plot_oob <- function(rf, title) {
  par(mar = c(5, 5, 4, 2))
  plot(rf$err.rate[, "OOB"], type = "l", lwd = 2, col = "steelblue",
       ylim = c(0.2, 0.75), cex.lab = 1.3, cex.axis = 1.2, cex.main = 1.4,
       xlab = "number of trees", ylab = "OOB error", main = title)
  lines(rf$err.rate[, "no"],  lwd = 1, col = "grey60", lty = 2)
  lines(rf$err.rate[, "yes"], lwd = 1, col = "tomato", lty = 2)
  legend("topright", legend = c("OOB (all)", "completed programmes", "interrupted programmes"),
         col = c("steelblue", "grey60", "tomato"), lty = c(1, 2, 2), lwd = c(2, 1, 1), bty = "n")
}


# 2. OOB error for the 1,000-tree default-settings fit.
plot_oob(rf_long, "OOB error, default forest")
abline(v = 500, lty = 3, col = "grey40")
dev.copy(png, "fig_rf_oob_default.png", width = 800, height = 500)
dev.off()

# 3. Default-settings permutation importance.
par(mar = c(5, 5, 4, 2))
varImpPlot(rf_default_imp, sort = TRUE, type = 1, scale = FALSE, pch = 19, col = "steelblue",
           cex = 1.1, main = "Permutation importance, default forest")
dev.copy(png, "fig_rf_importance_default.png", width = 800, height = 600)
dev.off()


# 4. Comparison-forest OOB error.
plot_oob(rf_slides, "OOB error, forest with the starting values of the slides")
dev.copy(png, "fig_rf_oob_slides.png", width = 800, height = 500)
dev.off()


# 5. Comparison-forest permutation importance.
par(mar = c(5, 5, 4, 2))
varImpPlot(rf_slides, sort = TRUE, type = 1, scale = FALSE, pch = 19, col = "steelblue",
           cex = 1.1, main = "Permutation importance, forest with the slides' values")
dev.copy(png, "fig_rf_importance_slides.png", width = 800, height = 600)
dev.off()


# 6. All three tuning schemes, one panel each.
par(mfrow = c(1, 3), mar = c(5, 5, 4, 1))
line_cols <- c("steelblue", "darkgreen", "orange", "tomato")
for (scheme in c("10-fold CV", "10-fold CV by country", "LOOCV")) {
  results <- cv_results[cv_results$scheme == scheme, ]
  plot(NA, xlim = range(mtry_grid$mtry), ylim = c(0.45, 0.70),
       xlab = "mtry (variables tried at each split)", ylab = "balanced accuracy",
       main = results$scheme[1], cex.lab = 1.3, cex.axis = 1.2, cex.main = 1.4)
  for (k in seq_along(ntree_grid)) {
    temp <- results[results$ntree == ntree_grid[k], ]
    lines(temp$mtry, temp$bal_acc, type = "b", pch = 19, lwd = 2, col = line_cols[k])
  }
  abline(h = 0.5, lty = 2, col = "grey40")
}
legend("bottomright", legend = paste("ntree", ntree_grid), col = line_cols,
       lty = 1, pch = 19, lwd = 2, bty = "n")
dev.copy(png, "fig_rf_tuning.png", width = 1100, height = 450)
dev.off()
par(mfrow = c(1, 1))


# 7. Final-model permutation importance.
par(mar = c(5, 5, 4, 2))
varImpPlot(rf_final, sort = TRUE, type = 1, scale = FALSE, pch = 19, col = "steelblue",
           cex = 1.1, main = "Permutation importance, final random forest")
dev.copy(png, "fig_rf_importance_final.png", width = 800, height = 600)
dev.off()

# Optional: run the separate seed-sensitivity and bootstrap diagnostics.
# source("Random_Forest_Diagnostics.R")
