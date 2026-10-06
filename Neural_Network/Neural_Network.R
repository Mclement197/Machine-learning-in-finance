################################################################################
# NEURAL NETWORK: WILL AN IMF PROGRAMME GO OFF TRACK? (Task 3)
################################################################################
# NEW DATA:
#   train.csv = development/training data
#   test.csv  = final test data
#
# This script is adapted to the CLEANER train.csv/test.csv files.
#
# Target:
#   interrupted
#
# Predictors:
#   log_access_pct_quota
#   planned_months
#   n_reviews_sched
#   n_struct_cond
#   n_prior_actions
#   prev_n_20y
#   years_since_prev
#   growth
#   inflation
#   current_account_gdp
#   concessional
#   extended
#   no_track_record
#
# Country identifiers and other descriptive variables are NOT used as predictors.
#
# Method:
#   1. Load train.csv and test.csv
#   2. Scale predictors using the training set only
#   3. Tune a one-hidden-layer neural network using 10-fold CV by country
#   4. Select size and decay using balanced accuracy
#   5. Select the classification threshold using out-of-fold predictions
#   6. Fit the final network on the full training set
#   7. Evaluate once on the test set
#   8. Check sensitivity to random starting weights

################################################################################
# PACKAGES
################################################################################

options(install.packages.check.source = "no")

if (!requireNamespace("nnet", quietly = TRUE)) {
  install.packages("nnet")
}

if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

if (!requireNamespace("NeuralNetTools", quietly = TRUE)) {
  install.packages("NeuralNetTools")
}

library(nnet)
library(caret)
library(NeuralNetTools)

par(mfrow = c(1, 1))

################################################################################
# LOAD THE DATA
################################################################################

# IMPORTANT:
# In RStudio, use:
#   Session > Set Working Directory > To Source File Location
#
# Then train.csv and test.csv should be in the same folder as this script.

getwd()

# If the files are in the same folder as the script:
train_data <- read.csv("../Data/processed/train.csv", stringsAsFactors = FALSE)
test_data  <- read.csv("../Data/processed/test.csv", stringsAsFactors = FALSE)

cat("Training observations:", nrow(train_data), "\n")
cat("Test observations:", nrow(test_data), "\n")

################################################################################
# DEFINE TARGET AND PREDICTORS
################################################################################

# Outcome:
# interrupted = 1 if at least one scheduled review was never completed
# interrupted = 0 otherwise

target <- "interrupted"

# These are the 13 predictors available in the NEW clean datasets.
predictors <- c(
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

# Country identifier used only to create grouped cross-validation folds.
# It is NOT included as a predictor.
country_id <- "Member.Code"

################################################################################
# CHECK THAT THE REQUIRED COLUMNS EXIST
################################################################################

required_train <- c(target, country_id, predictors)
required_test  <- c(target, country_id, predictors)

missing_train <- setdiff(required_train, names(train_data))
missing_test  <- setdiff(required_test, names(test_data))

if (length(missing_train) > 0) {
  stop(
    "These required columns are missing from train.csv: ",
    paste(missing_train, collapse = ", ")
  )
}

if (length(missing_test) > 0) {
  stop(
    "These required columns are missing from test.csv: ",
    paste(missing_test, collapse = ", ")
  )
}

################################################################################
# CHECK DATA TYPES / MISSING VALUES
################################################################################

# The neural network requires numerical predictors.
for (x in predictors) {
  
  if (!is.numeric(train_data[[x]])) {
    stop("Training predictor is not numeric: ", x)
  }
  
  if (!is.numeric(test_data[[x]])) {
    stop("Test predictor is not numeric: ", x)
  }
}

if (anyNA(train_data[, c(target, country_id, predictors)])) {
  stop("Missing values found in the required training columns.")
}

if (anyNA(test_data[, c(target, country_id, predictors)])) {
  stop("Missing values found in the required test columns.")
}

# Check that target is binary.
if (!all(train_data[[target]] %in% c(0, 1))) {
  stop("train.csv: interrupted must contain only 0 and 1.")
}

if (!all(test_data[[target]] %in% c(0, 1))) {
  stop("test.csv: interrupted must contain only 0 and 1.")
}

################################################################################
# PREPARE DATA FOR THE NEURAL NETWORK
################################################################################

dev_nn <- train_data[, c(target, predictors)]
test_nn <- test_data[, c(target, predictors)]

# Convert the outcome to a factor.
# "yes" = interrupted
# "no"  = not interrupted

dev_nn[[target]] <- factor(
  dev_nn[[target]],
  levels = c(0, 1),
  labels = c("no", "yes")
)

test_nn[[target]] <- factor(
  test_nn[[target]],
  levels = c(0, 1),
  labels = c("no", "yes")
)

cat("\nTraining outcome distribution:\n")
print(table(dev_nn[[target]]))

cat("\nTest outcome distribution:\n")
print(table(test_nn[[target]]))

################################################################################
# SCALING
################################################################################

# Neural networks are sensitive to the scale of the inputs.
#
# We use min-max scaling:
#
#       x_scaled = (x - min) / (max - min)
#
# IMPORTANT:
# The minimum and maximum are calculated ONLY from the training set.
# The exact same values are then applied to the test set.
#
# This prevents information from the test set leaking into model development.

mins <- sapply(
  dev_nn[, predictors, drop = FALSE],
  min
)

maxs <- sapply(
  dev_nn[, predictors, drop = FALSE],
  max
)

ranges <- maxs - mins

# Protect against a predictor with no variation.
ranges[ranges == 0] <- 1

dev_nn[, predictors] <- sweep(
  dev_nn[, predictors, drop = FALSE],
  2,
  mins,
  "-"
)

dev_nn[, predictors] <- sweep(
  dev_nn[, predictors, drop = FALSE],
  2,
  ranges,
  "/"
)

test_nn[, predictors] <- sweep(
  test_nn[, predictors, drop = FALSE],
  2,
  mins,
  "-"
)

test_nn[, predictors] <- sweep(
  test_nn[, predictors, drop = FALSE],
  2,
  ranges,
  "/"
)

cat("\nScaled predictor summary:\n")
print(summary(dev_nn[, predictors]))

################################################################################
# ACCURACY MEASURES
################################################################################

# Balanced accuracy:
#
#   (sensitivity + specificity) / 2
#
# Sensitivity = share of interrupted programmes correctly identified
# Specificity = share of non-interrupted programmes correctly identified
#
# Balanced accuracy is useful because the two outcome classes are not
# necessarily equally frequent.

balanced_accuracy <- function(y_true, y_pred) {
  
  y_true <- factor(
    y_true,
    levels = c("no", "yes")
  )
  
  y_pred <- factor(
    y_pred,
    levels = c("no", "yes")
  )
  
  sensitivity <- if (sum(y_true == "yes") > 0) {
    mean(y_pred[y_true == "yes"] == "yes")
  } else {
    NA_real_
  }
  
  specificity <- if (sum(y_true == "no") > 0) {
    mean(y_pred[y_true == "no"] == "no")
  } else {
    NA_real_
  }
  
  (sensitivity + specificity) / 2
}


# Turns a predicted probability into a class.
#
# threshold = 0.50 gives the conventional classification rule.
to_class <- function(p, threshold = 0.5) {
  
  factor(
    ifelse(p >= threshold, "yes", "no"),
    levels = c("no", "yes")
  )
}


# Extract useful values from a caret confusion matrix.
cm_row <- function(cm, name, sample_name) {
  
  data.frame(
    model = name,
    sample = sample_name,
    accuracy = round(
      unname(cm$overall["Accuracy"]),
      3
    ),
    sensitivity = round(
      unname(cm$byClass["Sensitivity"]),
      3
    ),
    specificity = round(
      unname(cm$byClass["Specificity"]),
      3
    ),
    bal_acc = round(
      unname(cm$byClass["Balanced Accuracy"]),
      3
    )
  )
}

################################################################################
# TUNING THE NETWORK WITH CARET
################################################################################

# Two settings are tuned:
#
# size  = number of neurons in the hidden layer
# decay = weight decay / regularisation
#
# Because the training dataset is relatively small, we keep the network
# deliberately simple to reduce overfitting.

nn_grid <- expand.grid(
  size = c(1, 2, 3, 5),
  decay = c(0.01, 0.1, 0.5, 1)
)

################################################################################
# 10-FOLD CROSS-VALIDATION BY COUNTRY
################################################################################

# Programmes belonging to the same country stay in the same fold.
#
# This is important because otherwise observations from the same country
# could appear in both the training and validation portions of a fold.

set.seed(72)

folds_country <- groupKFold(
  train_data[[country_id]],
  k = 10
)

# Custom scoring function so that hyperparameters are selected according
# to balanced accuracy rather than ordinary accuracy.

balanced_summary <- function(
    data,
    lev = NULL,
    model = NULL
) {
  
  c(
    Balanced_Accuracy = balanced_accuracy(
      data$obs,
      data$pred
    )
  )
}

control_country <- trainControl(
  method = "cv",
  index = folds_country,
  classProbs = TRUE,
  savePredictions = "all",
  summaryFunction = balanced_summary
)

################################################################################
# TRAIN / TUNE THE NEURAL NETWORK
################################################################################

# Neural networks start with random weights, so fix the seed.

set.seed(72)

nn_cv <- train(
  x = dev_nn[, predictors, drop = FALSE],
  y = dev_nn[[target]],
  method = "nnet",
  tuneGrid = nn_grid,
  metric = "Balanced_Accuracy",
  trControl = control_country,
  maxit = 500,
  trace = FALSE,
  MaxNWts = 10000
)

cat("\nNeural network cross-validation results:\n")
print(nn_cv)

cat("\nBest hyperparameters:\n")
print(nn_cv$bestTune)

################################################################################
# TUNING RESULTS
################################################################################

nn_results <- nn_cv$results[
  ,
  c("size", "decay", "Balanced_Accuracy")
]

names(nn_results)[3] <- "bal_acc"

nn_results$bal_acc <- round(
  nn_results$bal_acc,
  3
)

cat("\nBalanced accuracy for every size/decay combination:\n")
print(nn_results)

best_nn <- nn_cv$bestTune

################################################################################
# PLOT: NETWORK TUNING
################################################################################

par(
  mar = c(5, 5, 4, 2)
)

decay_values <- sort(
  unique(nn_grid$decay)
)

line_cols <- c(
  "steelblue",
  "darkgreen",
  "orange",
  "tomato"
)

plot(
  NA,
  xlim = range(nn_grid$size),
  ylim = c(
    max(
      0,
      min(nn_results$bal_acc, na.rm = TRUE) - 0.05
    ),
    min(
      1,
      max(nn_results$bal_acc, na.rm = TRUE) + 0.05
    )
  ),
  xlab = "size (neurons in the hidden layer)",
  ylab = "balanced accuracy",
  main = "Neural network tuning (10-fold CV by country)",
  cex.lab = 1.3,
  cex.axis = 1.2,
  cex.main = 1.4
)

for (k in seq_along(decay_values)) {
  
  temp <- nn_results[
    nn_results$decay == decay_values[k],
  ]
  
  lines(
    temp$size,
    temp$bal_acc,
    type = "b",
    pch = 19,
    lwd = 2,
    col = line_cols[k]
  )
}

abline(
  h = 0.5,
  lty = 2,
  col = "grey40"
)

legend(
  "topright",
  legend = paste(
    "decay",
    decay_values
  ),
  col = line_cols,
  lty = 1,
  pch = 19,
  lwd = 2,
  bty = "n"
)

dev.copy(
  png,
  "fig_nn_tuning.png",
  width = 900,
  height = 550
)

dev.off()

################################################################################
# THRESHOLD SELECTION
################################################################################

# Get the out-of-fold predictions for the best neural network.
#
# Each observation is predicted by a model that did not train on that
# observation (or its country).

oof <- nn_cv$pred[
  nn_cv$pred$size == best_nn$size &
    nn_cv$pred$decay == best_nn$decay,
]

oof <- oof[
  order(oof$rowIndex),
]

# Search for the threshold with the best balanced accuracy.
#
# The test set is NOT used here.

thresholds <- seq(
  0.05,
  0.95,
  by = 0.01
)

bal_values <- sapply(
  thresholds,
  function(t) {
    balanced_accuracy(
      oof$obs,
      to_class(
        oof$yes,
        t
      )
    )
  }
)

best_threshold <- thresholds[
  which.max(bal_values)
]

cat("\nBest threshold:", best_threshold, "\n")

cat(
  "Best out-of-fold balanced accuracy:",
  round(
    max(bal_values, na.rm = TRUE),
    3
  ),
  "\n"
)

################################################################################
# PLOT: THRESHOLD
################################################################################

par(
  mar = c(5, 5, 4, 2)
)

plot(
  thresholds,
  bal_values,
  type = "l",
  lwd = 2,
  col = "steelblue",
  ylim = c(
    max(
      0,
      min(bal_values, na.rm = TRUE) - 0.05
    ),
    min(
      1,
      max(bal_values, na.rm = TRUE) + 0.05
    )
  ),
  xlab = "threshold",
  ylab = "balanced accuracy (out of fold)",
  main = "Neural network: balanced accuracy vs threshold",
  cex.lab = 1.3,
  cex.axis = 1.2,
  cex.main = 1.4
)

abline(
  v = best_threshold,
  col = "tomato",
  lty = 2,
  lwd = 2
)

abline(
  h = 0.5,
  lty = 3,
  col = "grey40"
)

dev.copy(
  png,
  "fig_nn_threshold.png",
  width = 900,
  height = 550
)

dev.off()

################################################################################
# FINAL NETWORK
################################################################################

# Fit the selected architecture on the COMPLETE training dataset.

set.seed(72)

nn_final <- nnet(
  interrupted ~ .,
  data = dev_nn,
  size = best_nn$size,
  decay = best_nn$decay,
  maxit = 500,
  trace = FALSE,
  MaxNWts = 10000
)

cat("\nFinal neural network:\n")
print(nn_final)

################################################################################
# DRAW THE NETWORK
################################################################################

par(
  mar = c(1, 1, 1, 1)
)

plotnet(
  nn_final,
  cex_val = 0.7,
  circle_cex = 3
)

dev.copy(
  png,
  "fig_nn_network.png",
  width = 1100,
  height = 800
)

dev.off()

par(
  mar = c(5.1, 4.1, 4.1, 2.1)
)

################################################################################
# TEST-SET PREDICTIONS
################################################################################

# Predict probability of "interrupted".

p_test <- as.numeric(
  predict(
    nn_final,
    newdata = test_nn[, predictors, drop = FALSE],
    type = "raw"
  )
)

# Main classification uses the threshold selected from training/CV only.

pred_test <- to_class(
  p_test,
  best_threshold
)

cm_final <- confusionMatrix(
  pred_test,
  test_nn[[target]],
  positive = "yes"
)

# Also calculate performance using the conventional 0.50 threshold.

pred_test_05 <- to_class(
  p_test,
  0.50
)

cm_final_05 <- confusionMatrix(
  pred_test_05,
  test_nn[[target]],
  positive = "yes"
)

cat("\nConfusion matrix - threshold selected from development data:\n")
print(cm_final)

cat("\nConfusion matrix - threshold 0.50:\n")
print(cm_final_05)

################################################################################
# BASELINE
################################################################################

# Baseline:
# predict "interrupted" for every programme.
#
# This is the same baseline used in the original script.

pred_baseline <- factor(
  rep(
    "yes",
    nrow(test_nn)
  ),
  levels = c("no", "yes")
)

cm_baseline <- confusionMatrix(
  pred_baseline,
  test_nn[[target]],
  positive = "yes"
)

################################################################################
# OUT-OF-FOLD DEVELOPMENT PERFORMANCE
################################################################################

pred_oof <- to_class(
  oof$yes,
  best_threshold
)

cm_oof <- confusionMatrix(
  pred_oof,
  oof$obs,
  positive = "yes"
)

################################################################################
# FINAL RESULTS TABLE
################################################################################

results_nn <- rbind(
  
  cm_row(
    cm_baseline,
    "Baseline: all interrupted",
    "test"
  ),
  
  cm_row(
    cm_final_05,
    "NN (threshold 0.50)",
    "test"
  ),
  
  cm_row(
    cm_final,
    "NN (threshold from dev)",
    "test"
  ),
  
  cm_row(
    cm_oof,
    "NN (threshold from dev)",
    "dev out-of-fold"
  )
)

cat("\nFINAL RESULTS:\n")
print(results_nn)

################################################################################
# RANDOM-SEED STABILITY CHECK
################################################################################

# The starting weights of a neural network are random.
#
# We therefore refit the same architecture using 20 different seeds and
# report the range/median of test-set balanced accuracy.

seed_bal <- c()

for (s in 1:20) {
  
  set.seed(s)
  
  nn_s <- nnet(
    interrupted ~ .,
    data = dev_nn,
    size = best_nn$size,
    decay = best_nn$decay,
    maxit = 500,
    trace = FALSE,
    MaxNWts = 10000
  )
  
  p_s <- as.numeric(
    predict(
      nn_s,
      newdata = test_nn[, predictors, drop = FALSE],
      type = "raw"
    )
  )
  
  seed_bal <- c(
    seed_bal,
    balanced_accuracy(
      test_nn[[target]],
      to_class(
        p_s,
        best_threshold
      )
    )
  )
}

cat("\n20-seed test balanced-accuracy stability:\n")

print(
  round(
    c(
      min = min(seed_bal, na.rm = TRUE),
      median = median(seed_bal, na.rm = TRUE),
      max = max(seed_bal, na.rm = TRUE)
    ),
    3
  )
)

################################################################################
# END
################################################################################

