################################################################################
# NEURAL NETWORK: WILL AN IMF PROGRAMME GO OFF TRACK? (Task 3)
################################################################################
# In this script we train a small neural network on the same train/test split
# as the random forest, so that we can compare the two models at the end.
#
# Data (made by Data_Prep/01_data.R, so run that script first!):
#   Data/processed/train.csv = the data we train and tune on
#   Data/processed/test.csv  = the data we only touch once, at the very end
#
# What we want to predict:
#   interrupted (1 = the programme went off track, 0 = it didn't)
#
# The 13 inputs (predictors) we use:
#   log_access_pct_quota, planned_months, n_reviews_sched, n_struct_cond,
#   n_prior_actions, prev_n_20y, years_since_prev, growth, inflation,
#   current_account_gdp, concessional, extended, no_track_record
#
# The country code and the other descriptive columns are NOT used as inputs.
#
# Steps:
#   1. load train.csv and test.csv
#   2. rescale the inputs (using only the training data)
#   3. try different network sizes with 10-fold CV, where the folds are by country
#   4. pick the size and decay with the best balanced accuracy
#   5. pick the cut-off (threshold) for "yes" using the CV predictions
#   6. fit the final network on all the training data
#   7. test it once on the test set
#   8. check how much the result changes with different random seeds
#
# The plots are shown in the plot window and also saved as .png files in the
# Neural_Network folder (we use some in the slides and report).

################################################################################
# PACKAGES
################################################################################

# On Windows this stops R from asking about Rtools when installing packages
options(install.packages.check.source = "no")

# Install the packages only if they are missing (needs internet the first time)
if (!requireNamespace("nnet", quietly = TRUE)) {
  install.packages("nnet")
}

if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

if (!requireNamespace("NeuralNetTools", quietly = TRUE)) {
  install.packages("NeuralNetTools")
}

library(nnet)            # the neural network itself
library(caret)           # cross-validation, tuning and confusion matrices
library(NeuralNetTools)  # only used to draw the network

# One plot at a time in the plot window
par(mfrow = c(1, 1))

################################################################################
# FIND THE DATA FOLDER
################################################################################

# R looks for files starting from the "working directory" (getwd() shows it).
# Depending on how you open the script this is either the Neural_Network folder
# or the main repo folder, so we just check both and use whichever one works.
# Easiest way in RStudio: Session > Set Working Directory > To Source File Location
getwd()

if (file.exists("../Data/processed/train.csv")) {
  # working directory = Neural_Network folder
  repo_dir <- ".."
} else if (file.exists("Data/processed/train.csv")) {
  # working directory = main repo folder
  repo_dir <- "."
} else if (requireNamespace("rstudioapi", quietly = TRUE) &&
           rstudioapi::isAvailable() &&
           nzchar(rstudioapi::getSourceEditorContext()$path)) {
  # last try: in RStudio we can ask where this script is saved and go one folder up
  repo_dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  repo_dir <- NA
}

if (is.na(repo_dir) || !file.exists(file.path(repo_dir, "Data/processed/train.csv"))) {
  stop(
    "Can't find Data/processed/train.csv.\n",
    "  1) Did you run Data_Prep/01_data.R first? (train.csv and test.csv are not on GitHub)\n",
    "  2) In RStudio: Session > Set Working Directory > To Source File Location"
  )
}

# All the paths we need, built from the repo folder
train_file <- file.path(repo_dir, "Data", "processed", "train.csv")
test_file  <- file.path(repo_dir, "Data", "processed", "test.csv")
fig_dir    <- file.path(repo_dir, "Neural_Network")   # where the .png files go

################################################################################
# LOAD THE DATA
################################################################################

train_data <- read.csv(train_file, stringsAsFactors = FALSE)
test_data  <- read.csv(test_file, stringsAsFactors = FALSE)

cat("Training observations:", nrow(train_data), "\n")
cat("Test observations:", nrow(test_data), "\n")

################################################################################
# TARGET AND PREDICTORS
################################################################################

# interrupted = 1 if at least one planned review was never completed, 0 if not
target <- "interrupted"

# The 13 inputs of the network
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

# Country code: we only need it to build the CV folds, it is NOT an input
country_id <- "Member.Code"

################################################################################
# QUICK CHECKS ON THE DATA
################################################################################

# First make sure all the columns we need are actually in the files
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

# nnet only works with numbers, so every input has to be numeric
for (x in predictors) {

  if (!is.numeric(train_data[[x]])) {
    stop("Training predictor is not numeric: ", x)
  }

  if (!is.numeric(test_data[[x]])) {
    stop("Test predictor is not numeric: ", x)
  }
}

# No missing values allowed (nnet would drop or break on them)
if (anyNA(train_data[, c(target, country_id, predictors)])) {
  stop("Missing values found in the required training columns.")
}

if (anyNA(test_data[, c(target, country_id, predictors)])) {
  stop("Missing values found in the required test columns.")
}

# The target should only be 0 or 1
if (!all(train_data[[target]] %in% c(0, 1))) {
  stop("train.csv: interrupted must contain only 0 and 1.")
}

if (!all(test_data[[target]] %in% c(0, 1))) {
  stop("test.csv: interrupted must contain only 0 and 1.")
}

################################################################################
# PREPARE THE DATA FOR THE NETWORK
################################################################################

# Keep only the target and the 13 inputs
dev_nn <- train_data[, c(target, predictors)]
test_nn <- test_data[, c(target, predictors)]

# caret wants the target as a factor with proper names, so 0/1 becomes "no"/"yes"
# ("yes" = interrupted)
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

# How many "yes" and "no" do we have in each set?
cat("\nTraining outcome distribution:\n")
print(table(dev_nn[[target]]))

cat("\nTest outcome distribution:\n")
print(table(test_nn[[target]]))

################################################################################
# SCALING
################################################################################

# Neural networks don't like inputs on very different scales (e.g. inflation
# can be in the hundreds while concessional is just 0 or 1), so we squeeze
# everything between 0 and 1 with min-max scaling:
#
#       x_scaled = (x - min) / (max - min)
#
# Important: min and max come ONLY from the training data, and we use the same
# numbers for the test data. Otherwise the test set would "leak" into training.

mins <- sapply(
  dev_nn[, predictors, drop = FALSE],
  min
)

maxs <- sapply(
  dev_nn[, predictors, drop = FALSE],
  max
)

ranges <- maxs - mins

# If a column has the same value everywhere the range is 0 and we would divide
# by 0, so we set it to 1 just in case
ranges[ranges == 0] <- 1

# sweep() applies the operation column by column: first subtract the min...
dev_nn[, predictors] <- sweep(
  dev_nn[, predictors, drop = FALSE],
  2,
  mins,
  "-"
)

# ...then divide by the range
dev_nn[, predictors] <- sweep(
  dev_nn[, predictors, drop = FALSE],
  2,
  ranges,
  "/"
)

# Same thing for the test data (with the training min and range!)
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

# Quick check: the training columns should now go from 0 to 1
cat("\nScaled predictor summary:\n")
print(summary(dev_nn[, predictors]))

################################################################################
# HELPER FUNCTIONS
################################################################################

# Balanced accuracy = (sensitivity + specificity) / 2
#
# sensitivity = share of interrupted programmes we catch
# specificity = share of non-interrupted programmes we get right
#
# We use this instead of normal accuracy because "yes" and "no" are not equally
# common, and normal accuracy can look good even if the model always says the
# same thing.

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


# Turns a probability into "yes"/"no".
# With threshold = 0.5 it's the usual rule (above 50% -> "yes").
to_class <- function(p, threshold = 0.5) {

  factor(
    ifelse(p >= threshold, "yes", "no"),
    levels = c("no", "yes")
  )
}


# Takes a caret confusion matrix and pulls out the numbers we want in our
# results table (rounded to 3 decimals)
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
# TUNING GRID
################################################################################

# We tune two things:
#   size  = how many neurons in the hidden layer
#   decay = weight decay, a penalty that keeps the weights small (less overfitting)
#
# We only have a small dataset, so we keep the network small on purpose.
# A big network would just memorise the training data.

nn_grid <- expand.grid(
  size = c(1, 2, 3, 5),
  decay = c(0.01, 0.1, 0.5, 1)
)

################################################################################
# 10-FOLD CROSS-VALIDATION BY COUNTRY
################################################################################

# All the programmes of one country go into the same fold. If we didn't do
# this, the model could train on e.g. Argentina 2005 and be validated on
# Argentina 2008, which would make the CV results look better than they are.

set.seed(72)

folds_country <- groupKFold(
  train_data[[country_id]],
  k = 10
)

# caret picks the best model with accuracy by default. We want it to use
# balanced accuracy, so we give it our own summary function.

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
  index = folds_country,           # our country folds
  classProbs = TRUE,               # we also want probabilities, not only classes
  savePredictions = "all",         # keep the CV predictions (needed for the threshold)
  summaryFunction = balanced_summary
)

################################################################################
# TRAIN / TUNE THE NETWORK
################################################################################

# The network starts from random weights, so we fix the seed to get the same
# result every time we run it.

set.seed(72)

nn_cv <- train(
  x = dev_nn[, predictors, drop = FALSE],
  y = dev_nn[[target]],
  method = "nnet",
  tuneGrid = nn_grid,
  metric = "Balanced_Accuracy",
  trControl = control_country,
  maxit = 500,        # max number of training iterations
  trace = FALSE,      # don't print every iteration
  MaxNWts = 10000     # allowed number of weights (the default is too low sometimes)
)

cat("\nNeural network cross-validation results:\n")
print(nn_cv)

cat("\nBest hyperparameters:\n")
print(nn_cv$bestTune)

################################################################################
# TUNING RESULTS
################################################################################

# Small table with the balanced accuracy for every size/decay combination
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

# One line per decay value, size on the x-axis

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

# Empty plot first (the ylim just adds a bit of space above and below the lines)
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

# Then add one line for each decay
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

# 0.5 = what you would get by guessing
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

# Save what is in the plot window as a .png
dev.copy(
  png,
  file.path(fig_dir, "fig_nn_tuning.png"),
  width = 900,
  height = 550
)

dev.off()

################################################################################
# CHOOSING THE THRESHOLD
################################################################################

# Take the cross-validation predictions of the best network. Each programme was
# predicted by a model that never saw it (or any programme of its country)
# during training, so these are "honest" predictions.

oof <- nn_cv$pred[
  nn_cv$pred$size == best_nn$size &
    nn_cv$pred$decay == best_nn$decay,
]

# put them back in the original order of the rows
oof <- oof[
  order(oof$rowIndex),
]

# Try every threshold from 0.05 to 0.95 and keep the one with the best
# balanced accuracy. We don't use the test set here!

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

# Balanced accuracy for every threshold, red dashed line = the one we picked

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
  file.path(fig_dir, "fig_nn_threshold.png"),
  width = 900,
  height = 550
)

dev.off()

################################################################################
# FINAL NETWORK
################################################################################

# Now we fit the network again with the best size and decay, but this time on
# ALL the training data (not just 9 of the 10 folds)

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

# Picture of the final network: inputs on the left, hidden layer in the middle,
# output on the right. Thicker lines = bigger weights.

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
  file.path(fig_dir, "fig_nn_network.png"),
  width = 1100,
  height = 800
)

dev.off()

# set the margins back to the R default
par(
  mar = c(5.1, 4.1, 4.1, 2.1)
)

################################################################################
# PREDICTIONS ON THE TEST SET
################################################################################

# Probability that each test programme gets interrupted
p_test <- as.numeric(
  predict(
    nn_final,
    newdata = test_nn[, predictors, drop = FALSE],
    type = "raw"
  )
)

# Main result: use the threshold we chose with cross-validation
pred_test <- to_class(
  p_test,
  best_threshold
)

cm_final <- confusionMatrix(
  pred_test,
  test_nn[[target]],
  positive = "yes"
)

# Just to compare: the normal 0.50 threshold
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

# Very simple baseline: just say "interrupted" for every programme.
# The network should at least beat this (same baseline as in the random forest).

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
# PERFORMANCE ON THE CV PREDICTIONS
################################################################################

# Same thing but on the out-of-fold predictions from the training data,
# so we can see if the test result is similar to what CV told us

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

# Everything in one table so it's easy to compare

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
# DOES THE RESULT DEPEND ON THE SEED?
################################################################################

# Since the starting weights are random, a different seed can give a different
# network. To check how much this matters, we fit the same network 20 times
# with seeds 1 to 20 and look at the min / median / max test balanced accuracy.

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
# RANDOM FOREST vs NEURAL NETWORK (test set): table + graphs
################################################################################

# pROC is the package for ROC curves and AUC (the RF script uses it too)
if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}

library(pROC)

# --- Neural network numbers ---
# Accuracy and balanced accuracy come from the confusion matrix above (cm_final).
# The AUC we still have to compute: it's the area under the ROC curve, so
# 0.5 = guessing and 1 = perfect.
# levels = c("no", "yes") tells pROC that "yes" is the positive class, and
# direction = "<" means a higher probability should mean "yes".
roc_nn <- roc(
  test_nn$interrupted,
  p_test,
  levels = c("no", "yes"),
  direction = "<",
  quiet = TRUE
)

nn_auc <- as.numeric(auc(roc_nn))

nn_metrics <- c(
  Accuracy        = unname(cm_final$overall["Accuracy"]),
  `Bal. Accuracy` = unname(cm_final$byClass["Balanced Accuracy"]),
  AUC             = nn_auc
)

# --- Random forest numbers ---
# Hardcoded from the RF slide (= RF final (tuned) in Random_Forest/rf_outputs/).
# If someone re-runs the RF and the numbers change, update them here too!
rf_metrics <- c(
  Accuracy        = 0.500,
  `Bal. Accuracy` = 0.508,
  AUC             = 0.586
)

# Put both in one small table (one row per model)
comp_tbl <- round(
  rbind(
    `Random Forest`  = rf_metrics,
    `Neural Network` = nn_metrics
  ),
  3
)

cat("\nRandom forest vs neural network (test set):\n")
print(comp_tbl)

# --- Bar chart: the three scores side by side for both models ---
par(
  mar = c(5, 5, 4, 2)
)

bp <- barplot(
  comp_tbl,
  beside = TRUE,                   # bars next to each other, not stacked
  ylim = c(0, 1),
  col = c("steelblue", "tomato"),
  legend.text = rownames(comp_tbl),
  args.legend = list(x = "topright", bty = "n"),
  ylab = "score",
  main = "Random Forest vs Neural Network - test set",
  cex.lab = 1.3,
  cex.axis = 1.2,
  cex.main = 1.4
)

# write the value on top of each bar
text(
  bp,
  comp_tbl,
  labels = round(comp_tbl, 3),
  pos = 3,
  cex = 1.0
)

# again, 0.5 = guessing
abline(
  h = 0.5,
  lty = 2,
  col = "grey40"
)

dev.copy(
  png,
  file.path(fig_dir, "fig_compare_metrics.png"),
  width = 900,
  height = 550
)

dev.off()

# --- ROC curve: NN and RF in the same plot ---
# The RF script saves its test-set probabilities in rf_outputs/test_predictions.csv,
# so we can rebuild the RF ROC curve from that file. If the file isn't there
# (e.g. the RF script was never run) we just plot the NN on its own.
rf_pred_file <- file.path(repo_dir, "Random_Forest", "rf_outputs", "test_predictions.csv")

par(
  mar = c(5, 5, 4, 2)
)

# Read the file only if it's there, and check it really has the predictions in it
rf_pred <- NULL

if (file.exists(rf_pred_file)) {
  rf_pred <- read.csv(rf_pred_file, stringsAsFactors = FALSE)

  if (!all(c("actual", "probability") %in% names(rf_pred))) {
    rf_pred <- NULL
  }
}

if (!is.null(rf_pred)) {

  roc_rf <- roc(
    rf_pred$actual,
    rf_pred$probability,
    levels = c("no", "yes"),
    direction = "<",
    quiet = TRUE
  )

  plot(
    roc_rf,
    col = "steelblue",
    lwd = 2,
    main = "ROC Curve - RF vs NN (test set)"
  )

  # add = TRUE draws the NN curve on top of the RF one
  plot(
    roc_nn,
    col = "tomato",
    lwd = 2,
    add = TRUE
  )

  legend(
    "bottomright",
    legend = c(
      paste0("RF  (AUC = ", round(auc(roc_rf), 3), ")"),
      paste0("NN  (AUC = ", round(nn_auc, 3), ")")
    ),
    col = c("steelblue", "tomato"),
    lwd = 2,
    bty = "n"
  )

} else {

  plot(
    roc_nn,
    col = "tomato",
    lwd = 2,
    main = paste0("ROC Curve - Neural Network (AUC = ", round(nn_auc, 3), ")")
  )
}

dev.copy(
  png,
  file.path(fig_dir, "fig_compare_roc.png"),
  width = 800,
  height = 600
)

dev.off()

################################################################################
# END
################################################################################
