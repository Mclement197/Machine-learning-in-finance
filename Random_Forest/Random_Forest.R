################################################################################
# RANDOM FOREST: WILL AN IMF PROGRAMME GO OFF TRACK? (Task 2)
################################################################################
#This script follows the decision tree and random forest scripts of the course
#(R_code-trees.R and R_code-forests.R) and uses the same packages: tree,
#randomForest and caret.
#
#What the script does, in the order of the brief:
#  b. a random forest with default hyperparameters
#  c. tuning of ntree and mtry
#  d. tuning with caret under 10-fold CV and under leave-one-out CV (LOOCV)
#  e. the final model, its predictions and its prediction accuracy
#  f. permutation importance with the randomForest package
#
#Section d also contains a third scheme, 10-fold CV by country, because the
#professor asked that all programmes of a country stay in the same fold.
#Every graph is shown in the plot window and saved as a .png file in the
#working directory (for the slides).

#Install the packages if needed (this needs an internet connection, once).
#On Windows, this line avoids the "Rtools is required" warning when packages are installed.
options(install.packages.check.source = "no")
if (!requireNamespace("tree", quietly = TRUE)) install.packages("tree")
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")

library(tree)
library(randomForest)
library(caret)

#Make sure the plot window shows one plot at a time (a previous script may have
#split it into several panels).
par(mfrow = c(1, 1))

################################################################################
#LOAD THE DATA
################################################################################
#Please go session > set working directory > to source file location
getwd()
#check -> ......./Machine-learning-in-finance/Random_Forest if you cloned the repo correctly

#dev = development set: programmes approved from 2002 to 2013. We fit and tune
#      the forest on it.
#test = test set: programmes approved from 2014 to 2016. We use it only once,
#      at the end, to measure the prediction accuracy.
dev_data  <- read.csv("../Data/processed/train.csv")
test_data <- read.csv("../Data/processed/test.csv")
dim(dev_data)
dim(test_data)
summary(dev_data)
summary(test_data)

#Keep only the predictors: we remove the identifiers and every column that is
#only known after the approval day (otherwise the model would see the future).
#The 17 predictors below are all known on the day the Board approves the programme.
#Choices compared with the first version of this script:
#- extended (0/1) and concessional (0/1) replace facility_type. facility_type is
#  text (Blend, ECF, EFF, SBA, SCF) and carries the same information: Blend, ECF
#  and EFF are the extended facilities.
#- prev_label_A replaces prev_label_B, which is not a column of our clean data.
#- n_tranches is dropped: it is the number of reviews + 1 in 131 of the 141
#  programmes (correlation 0.90 with n_reviews_sched), so it adds nothing.
predictors <- c("log_access_pct_quota", "planned_months", "n_reviews_sched",
                "concessional", "extended",
                "n_struct_cond", "n_prior_actions", "frontload_share",
                "prev_label_A", "years_since_prev", "prev_n_20y", "no_track_record",
                "growth", "inflation", "current_account_gdp",
                "gov_debt_gdp", "net_lending_gdp")

dev_rf  <- dev_data[, c("label_B", predictors)]
test_rf <- test_data[, c("label_B", predictors)]

# Check no missing values 
colSums(is.na(dev_rf))

#The outcome label_B is 1 if at least one scheduled review was never completed.
#To get a classification forest (and not a regression forest), randomForest()
#and caret need the outcome as a factor. We recode 0/1 into "no"/"yes".
#"yes" = interrupted. It is listed second, so it is the second level of the factor.
dev_rf$label_B <- factor(dev_rf$label_B, levels = c(0, 1), labels = c("no", "yes"))
test_rf$label_B <- factor(test_rf$label_B, levels = c(0, 1), labels = c("no", "yes"))

#Share of interrupted programmes in the two sets
table(dev_rf$label_B)
round(prop.table(table(dev_rf$label_B)), 3)
table(test_rf$label_B)
round(prop.table(table(test_rf$label_B)), 3)
#The test set has more interrupted programmes (63%) than the development set
#(52%). We keep this in mind when we choose the accuracy measure below.

################################################################################
#ACCURACY MEASURES
################################################################################
#The accuracy is the share of programmes classified correctly. With classes of
#different sizes it can be misleading: on the test set, a rule that predicts
#"interrupted" for every programme is right 63% of the time and knows nothing.
#Sensitivity = share of interrupted programmes that we catch.
#Specificity = share of completed programmes that we catch.
#Balanced accuracy = (sensitivity + specificity) / 2. The rule above gets 0.50.
#We therefore tune and compare the models on the balanced accuracy and report
#the accuracy next to it. confusionMatrix() of caret gives all of them, but we
#also need the balanced accuracy inside caret and inside loops, so we write it out.
balanced_accuracy <- function(y_true, y_pred) {
  sens <- mean(y_pred[y_true == "yes"] == "yes")
  spec <- mean(y_pred[y_true == "no"]  == "no")
  (sens + spec) / 2
}

#Turns the forest's probability (share of trees voting "yes") into a class.
#The threshold is 0.50 by default: "yes" if at least half of the trees vote "yes".
to_class <- function(p, threshold = 0.5) {
  factor(ifelse(p >= threshold, "yes", "no"), levels = c("no", "yes"))
}

#One line of scores from a confusion matrix of caret
cm_row <- function(cm, name, sample_name) {
  data.frame(model = name, sample = sample_name,
             accuracy    = round(cm$overall[["Accuracy"]], 3),
             sensitivity = round(cm$byClass[["Sensitivity"]], 3),
             specificity = round(cm$byClass[["Specificity"]], 3),
             bal_acc     = round(cm$byClass[["Balanced Accuracy"]], 3))
}

################################################################################
#A SINGLE CLASSIFICATION TREE (starting point)
################################################################################
#Before the forest, one classification tree on the development set, as in the
#tree script of the course. A forest averages many such trees.
tree.dev_rf <- tree(label_B ~ ., data = dev_rf)

#summary() lists the variables used as internal nodes in the tree, the number
#of terminal nodes and the training error rate.
summary(tree.dev_rf)

#Show the tree graphically. text() adds the node labels; pretty = 0 tells R to
#write the category names of qualitative predictors instead of a letter per
#category. cex sets the size of the labels.
#The label size, line width and figure size were chosen with the help of AI.
par(mar = c(1, 1, 1, 1))
plot(tree.dev_rf, lwd = 1.5)
text(tree.dev_rf, pretty = 0, cex = 0.8)
dev.copy(png, "fig_tree.png", width = 1400, height = 900)
dev.off()
par(mar = c(5.1, 4.1, 4.1, 2.1))
#A single tree is unstable: drop a few programmes and its shape can change
#completely (high variance). The forest reduces this variance by averaging.

################################################################################
#b. RANDOM FOREST WITH DEFAULT HYPERPARAMETERS
################################################################################
#ntree = number of trees. mtry (not set here) = number of predictors tried at
#each split; the package default for classification is the square root of p,
#i.e. floor(sqrt(17)) = 4 here. Node size default = 1 (trees fully grown).
set.seed(72)
rf_default <- randomForest(label_B ~ ., data = dev_rf, ntree = 500)
rf_default
#The "OOB estimate of error rate" and the confusion matrix in this print-out are
#computed from the out-of-bag (OOB) observations, i.e. for each tree, the
#programmes that were NOT used to build that tree.

#In-sample accuracy: the forest predicts the programmes it was trained on.
p_in_sample <- predict(rf_default, newdata = dev_rf, type = "response")
mean(p_in_sample == dev_rf$label_B)
#This is close to 1. The forest almost reproduces the training data.

#Predictions on the test set. type = "prob" gives, for each programme, the
#share of trees that vote "yes".
p_default_test <- predict(rf_default, newdata = test_rf, type = "prob")[, "yes"]
cm_default <- confusionMatrix(to_class(p_default_test), test_rf$label_B, positive = "yes")
cm_default
#positive = "yes" is needed: without it caret takes the first level ("no") as
#the positive class and swaps sensitivity and specificity.

#Plot the OOB error as a function of the number of trees. We use a forest with
#the default settings and 1000 trees to see what happens after 500.
set.seed(72)
rf_long <- randomForest(label_B ~ ., data = dev_rf, ntree = 1000)

#Function for the OOB error plots of this script.
#The layout, colours and line widths of this graph were made with the help of AI.
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

plot_oob(rf_long, "OOB error, default forest")
abline(v = 500, lty = 3, col = "grey40")
dev.copy(png, "fig_rf_oob_default.png", width = 800, height = 500)
dev.off()
#The error settles after a few hundred trees: more trees do not hurt, they only
#cost time. Random forests do not overfit with more trees.

#Permutation importance of the default forest (explained in section f). We use
#scale = FALSE in all importance plots, so they are all in accuracy points.
#importance = TRUE makes randomForest() draw extra random numbers, so with the
#same seed the trees come out slightly different. We keep the scores of
#rf_default above (they are the ones in the report) and fit a twin forest with
#importance = TRUE only for this plot.
set.seed(72)
rf_default_imp <- randomForest(label_B ~ ., data = dev_rf, ntree = 500, importance = TRUE)
importance(rf_default_imp, type = 1, scale = FALSE)
#The size, colours and layout of this graph were made with the help of AI.
par(mar = c(5, 5, 4, 2))
varImpPlot(rf_default_imp, sort = TRUE, type = 1, scale = FALSE, pch = 19, col = "steelblue",
           cex = 1.1, main = "Permutation importance, default forest")
dev.copy(png, "fig_rf_importance_default.png", width = 800, height = 600)
dev.off()

#Default with the starting values from the course slides: mtry = p/3, node
#size = 5 and B = 500 trees. With p = 17, p/3 is 5 (rounded down).
set.seed(72)
rf_slides <- randomForest(label_B ~ ., data = dev_rf, mtry = 5, ntree = 500,
                          nodesize = 5, importance = TRUE)
rf_slides
importance(rf_slides, type = 1, scale = FALSE)

plot_oob(rf_slides, "OOB error, forest with the starting values of the slides")
dev.copy(png, "fig_rf_oob_slides.png", width = 800, height = 500)
dev.off()

#The size, colours and layout of this graph were made with the help of AI.
par(mar = c(5, 5, 4, 2))
varImpPlot(rf_slides, sort = TRUE, type = 1, scale = FALSE, pch = 19, col = "steelblue",
           cex = 1.1, main = "Permutation importance, forest with the slides' values")
dev.copy(png, "fig_rf_importance_slides.png", width = 800, height = 600)
dev.off()

################################################################################
#c. and d. TUNING ntree AND mtry WITH CARET
################################################################################
#mtry = number of predictors considered at each split. ntree = number of trees.
#We tune both with a grid search combined with cross validation, as in the
#course. The grid has 4 values of ntree and 6 values of mtry, 24 pairs in total.
#We stop at mtry = 12 (p = 17): with a high mtry the forest is close to bagging
#(all predictors are tried at every split), and with mtry = 1 the tree cannot
#choose its split variable at all.
#The OOB plot above shows that the error is flat after a few hundred trees, so
#we do not go beyond 1000 trees.
ntree_grid <- c(100, 250, 500, 1000)
mtry_grid  <- expand.grid(mtry = c(2, 3, 4, 6, 8, 12))

#caret tunes only mtry. ntree is passed on to randomForest(), so we run the
#grid search once for each value of ntree, in a loop.

#caret scores each held-out fold with the accuracy. We want the balanced
#accuracy, so we ask caret to keep all its out-of-fold predictions and we
#compute the balanced accuracy ourselves in a loop (see the grid search below).

#Three ways to split the development set (all with caret's trainControl):
#1. 10-fold CV: the 141 programmes are split at random into 10 groups. We train
#   on 9 groups and score on the 10th, and repeat 10 times.
#2. 10-fold CV by country: same, but all programmes of a country fall in the
#   same fold, so the forest is never scored on a country it has seen in
#   training. groupKFold() of caret builds the 10 folds from the country codes.
#3. LOOCV: we train on 140 programmes and predict the one left out, 141 times.
#   Other programmes of the same country stay in training.

set.seed(72)
folds_country <- groupKFold(dev_data$iso3c, k = 10)

control_10cv <- trainControl(method = "cv", number = 10, classProbs = TRUE,
                             savePredictions = "all")
control_country <- trainControl(method = "cv", index = folds_country, classProbs = TRUE,
                                savePredictions = "all")
control_loocv <- trainControl(method = "LOOCV", classProbs = TRUE,
                              savePredictions = "all")
#classProbs = TRUE keeps the share of "yes" votes of each held-out programme and
#savePredictions = "all" stores these out-of-fold predictions for every value
#of mtry (we use them for the balanced accuracy and in section e).

#Grid search: for each scheme and each number of trees, caret fits the 6 values
#of mtry. That is 3 x 4 x 6 = 72 combinations, so the LOOCV part takes a few minutes.
controls <- list("10-fold CV" = control_10cv,
                 "10-fold CV by country" = control_country,
                 "LOOCV" = control_loocv)

cv_results <- NULL
models <- list()

for (scheme in names(controls)) {
  for (n_tree in ntree_grid) {
    set.seed(72)
    model <- train(dev_rf[, predictors], dev_rf$label_B, method = "rf",
                   tuneGrid = mtry_grid, trControl = controls[[scheme]], ntree = n_tree)
    models[[paste(scheme, n_tree)]] <- model

    #Balanced accuracy of each mtry, from the out-of-fold predictions.
    #With 10-fold CV we score each fold and take the average over the folds.
    #With LOOCV each fold holds one programme, so we score the 141 predictions together.
    for (m in mtry_grid$mtry) {
      pred_m <- model$pred[model$pred$mtry == m, ]
      if (scheme == "LOOCV") {
        bal <- balanced_accuracy(pred_m$obs, pred_m$pred)
        acc <- mean(pred_m$obs == pred_m$pred)
      } else {
        bal_folds <- c()
        acc_folds <- c()
        for (f in unique(pred_m$Resample)) {
          fold <- pred_m[pred_m$Resample == f, ]
          bal_folds <- c(bal_folds, balanced_accuracy(fold$obs, fold$pred))
          acc_folds <- c(acc_folds, mean(fold$obs == fold$pred))
        }
        bal <- mean(bal_folds, na.rm = TRUE)
        acc <- mean(acc_folds, na.rm = TRUE)
      }
      cv_results <- rbind(cv_results,
                          data.frame(scheme = scheme, ntree = n_tree, mtry = m,
                                     bal_acc = round(bal, 3), accuracy = round(acc, 3)))
    }
  }
}

#All 72 results (3 schemes x 24 pairs)
cv_results

#Best pair of (ntree, mtry) under each scheme: highest balanced accuracy.
#If there is a tie, which.max() takes the first row, i.e. the smaller forest
#and then the smaller mtry (the order in which the loop filled the table).
res_10cv    <- cv_results[cv_results$scheme == "10-fold CV", ]
res_country <- cv_results[cv_results$scheme == "10-fold CV by country", ]
res_loocv   <- cv_results[cv_results$scheme == "LOOCV", ]

best_10cv    <- res_10cv[which.max(res_10cv$bal_acc), ]
best_country <- res_country[which.max(res_country$bal_acc), ]
best_loocv   <- res_loocv[which.max(res_loocv$bal_acc), ]
rbind(best_10cv, best_country, best_loocv)
rbind(best_10cv, best_country, best_loocv)
#All three schemes pick mtry = 2, with different numbers of trees. The balanced
#accuracy is flat across the grid (about 0.55 to 0.64), so the choice of the
#pair matters little: the differences are of the size of the noise.

#Plot of the grid search: one panel per scheme, one line per ntree.
#The size, colours and layout of this graph were made with the help of AI.
par(mfrow = c(1, 3), mar = c(5, 5, 4, 1))
line_cols <- c("steelblue", "darkgreen", "orange", "tomato")
for (scheme in names(controls)) {
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

################################################################################
#e. FINAL RANDOM FOREST, PREDICTIONS AND PREDICTION ACCURACY
################################################################################
#We take the pair chosen by the 10-fold CV by country: it is the only scheme in
#which the programmes held out in each fold come from countries that the
#forest has not seen, which is the situation we face on the test set.
final_ntree <- best_country$ntree
final_mtry  <- best_country$mtry
final_ntree
final_mtry

#Out-of-fold predictions of this forest on the development set: each
#programme is predicted by a forest that did NOT see it (or its country).
model_country <- models[[paste("10-fold CV by country", final_ntree)]]
oof <- model_country$pred[model_country$pred$mtry == final_mtry, ]
oof <- oof[order(oof$rowIndex), ]

#we look for the threshold that gives the best score on these
#out-of-fold predictions (never on the test set). 0.50 is the default.
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
#The best threshold is 0.50, so the default threshold is kept.

#Final forest, fitted on the whole development set.
#importance = TRUE asks R to compute the variable importance as well (section f).
set.seed(72)
rf_final <- randomForest(label_B ~ ., data = dev_rf, ntree = final_ntree,
                         mtry = final_mtry, importance = TRUE)
rf_final

#now we test (the test set is used once)
p_test <- predict(rf_final, newdata = test_rf, type = "prob")[, "yes"]
pred_test <- to_class(p_test, best_threshold)

cm_final <- confusionMatrix(pred_test, test_rf$label_B, positive = "yes")
cm_final

#Baseline: predict "interrupted" for every programme
pred_baseline <- factor(rep("yes", nrow(test_rf)), levels = c("no", "yes"))
cm_baseline <- confusionMatrix(pred_baseline, test_rf$label_B, positive = "yes")

#Same scores on the development set, out of fold
cm_oof <- confusionMatrix(to_class(oof$yes, best_threshold), oof$obs, positive = "yes")

results_tbl <- rbind(cm_row(cm_baseline, "Baseline: all interrupted", "test"),
                     cm_row(cm_default,  "RF default", "test"),
                     cm_row(cm_final,    "RF final (tuned)", "test"),
                     cm_row(cm_oof,      "RF final (tuned)", "dev out-of-fold"))
results_tbl
#How to read it: on the test set the final forest catches most interrupted
#programmes (sensitivity) but almost none of the completed ones (specificity),
#so the balanced accuracy is below 0.50 and the accuracy is below that of the
#baseline. Out of fold on the development set the forest does better (0.62).

#The test set has only 27 programmes (10 completed), so one extra correct call
#moves the scores a lot. Two checks of how much of the result is luck:
#1. Random forests are random: we refit the default and the final forest with
#   20 different seeds and score the test set each time.
seed_scores <- NULL
for (s in 1:20) {
  set.seed(s)
  rf_d <- randomForest(label_B ~ ., data = dev_rf, ntree = 500)
  set.seed(s)
  rf_f <- randomForest(label_B ~ ., data = dev_rf, ntree = final_ntree, mtry = final_mtry)
  p_d <- predict(rf_d, newdata = test_rf, type = "prob")[, "yes"]
  p_f <- predict(rf_f, newdata = test_rf, type = "prob")[, "yes"]
  seed_scores <- rbind(seed_scores,
                       data.frame(seed = s, model = "RF default",
                                  bal_acc = balanced_accuracy(test_rf$label_B, to_class(p_d))),
                       data.frame(seed = s, model = "RF final",
                                  bal_acc = balanced_accuracy(test_rf$label_B, to_class(p_f))))
}
aggregate(bal_acc ~ model, data = seed_scores,
          FUN = function(v) round(c(min = min(v), median = median(v), max = max(v)), 3))

#2. Bootstrap (as in the Resampling lecture): we draw 27 test programmes with
#   replacement 2000 times, score each draw, and look at the 2.5% and 97.5%
#   quantiles of the scores. The forests are not refitted.
boot_ci <- function(p, B = 2000) {
  scores <- numeric(B)
  for (b in 1:B) {
    i <- sample(1:nrow(test_rf), replace = TRUE)
    scores[b] <- balanced_accuracy(test_rf$label_B[i], to_class(p[i]))
  }
  round(quantile(scores, c(0.025, 0.975), na.rm = TRUE), 3)
}
set.seed(72)
boot_ci(p_default_test)
boot_ci(p_test)
#Both intervals are wide and contain 0.50.

################################################################################
#f. PERMUTATION IMPORTANCE OF THE FINAL FOREST
################################################################################
#PERMUTATION IMPORTANCE (mean decrease in accuracy)
#For every tree: take the out-of-bag programmes, score them, shuffle the values
#of one predictor, score them again, and record how much the accuracy drops.
#The drop is averaged over the trees. A predictor whose shuffling hurts a lot
#is important. type = 1 selects this measure. With scale = FALSE the result
#is in accuracy points; with scale = TRUE (the default) it is divided by its
#standard error.
final_importance <- importance(rf_final, type = 1, scale = FALSE)
round(final_importance[order(final_importance[, 1], decreasing = TRUE), , drop = FALSE], 4)
importance(rf_final, type = 1, scale = TRUE)

#The size, colours and layout of this graph were made with the help of AI.
par(mar = c(5, 5, 4, 2))
varImpPlot(rf_final, sort = TRUE, type = 1, scale = FALSE, pch = 19, col = "steelblue",
           cex = 1.1, main = "Permutation importance, final random forest")
dev.copy(png, "fig_rf_importance_final.png", width = 800, height = 600)
dev.off()
#Shuffling a predictor whose dot is at or below zero changes nothing: the
#forest does not use it in a way that helps prediction. Importance describes
#what the forest uses to predict.

################################################################################
#COMPARE THE DEFAULT AND THE FINAL FOREST (OOB error on the development set)
################################################################################
rf_default
rf_final

default_oob <- tail(rf_default$err.rate[, "OOB"], 1)
final_oob   <- tail(rf_final$err.rate[, "OOB"], 1)
default_oob
final_oob
#The two OOB errors are close (about 0.42 and 0.43): tuning did not improve
#the forest in a way we can measure with 141 programmes.

################################################################################
#EXTRA (not covered in the course): AUC
################################################################################
#The AUC is the area under the ROC curve. It does not depend on the threshold:
#it is the chance that a random interrupted programme gets a higher vote share
#than a random completed one (0.50 = coin flip). We report it in the text next
#to the balanced accuracy. It needs the package pROC. If you drop the AUC from
#the report, delete this block.
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
library(pROC)
auc_score <- function(y_true, p) {
  as.numeric(auc(roc(y_true, p, levels = c("no", "yes"), direction = "<", quiet = TRUE)))
}
round(auc_score(test_rf$label_B, p_default_test), 3)
round(auc_score(test_rf$label_B, p_test), 3)
round(auc_score(oof$obs, oof$yes), 3)
