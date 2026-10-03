################################################################################
# DECISION TREES
################################################################################
#Install the packages if needed (this needs an internet connection, once).
#Note: we use the package ISLR (not ISLR2).
if (!requireNamespace("tree", quietly = TRUE)) install.packages("tree")
if (!requireNamespace("ISLR", quietly = TRUE)) install.packages("ISLR")
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")

library(tree)
library(ISLR)

#Please go session > set working directory > to source file location
getwd() 
#check -> ......./Machine-learning-in-finance/Random_Forest if you cloned the repo correctly

dev_data <- read.csv("../data/ml_dev.csv")

test_data <- read.csv("../data/ml_test.csv")

################################################################################
#CLASSIFICATION TREE
################################################################################
#We analyse the Carseats data set: sales of child car seats at 400 stores.
summary(dev_data)
summary(test_data)

#Remove useless columns (identifiers...) and columns that cannot be used to predict lable_A

predictors <- c(
  "log_access_pct_quota",
  "planned_months",
  "concessional",
  "facility_type",
  "n_reviews_sched",
  "prev_label_B",
  "years_since_prev",
  "prev_n_20y",
  "no_track_record",
  "net_lending_gdp",
  "gov_debt_gdp",
  "growth",
  "inflation",
  "current_account_gdp",
  "frontload_share",
  "n_tranches",
  "n_struct_cond",
  "n_prior_actions"
)

dev_rf <- dev_data[, c("label_B", predictors)]
test_rf <- test_data[, c("label_B", predictors)]

# Check no missing values
colSums(is.na(dev_rf))

#We create a variable High, which takes the value "Yes" if Sales exceeds 8
#(thousand units) and "No" otherwise. This turns the problem into classification.
dev_rf$label_B <- factor(
  dev_rf$label_B,
  levels = c(0, 1),
  labels = c("no", "yes")
)

test_rf$label_B <- factor(
  test_rf$label_B,
  levels = c(0, 1),
  labels = c("no", "yes")
)

#We use the tree() function to fit a classification tree in order to predict
#High using all variables but Sales (Sales must be excluded: High was built from it).
tree.dev_rf <- tree(label_B ~ ., data = dev_rf)

#summary() lists the variables used as internal nodes in the tree, the number
#of terminal nodes and the training error rate.
#(See Section 8.3.1 of the ISLR book for the definition of the residual mean
#deviance reported below.)
summary(tree.dev_rf)

#Show the tree graphically. text() adds the node labels; pretty = 0 tells R to
#write the category names of qualitative predictors instead of a letter per
#category. cex makes the labels smaller so that they fit.
plot(tree.dev_rf)
text(tree.dev_rf, pretty = 0, cex = 0.5)

#Use the random forest library as requested

if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}
library(randomForest)

#Train default random forest on values from the library

set.seed(600)

rf_default_lib <- randomForest(
  label_B ~ .,
  data = dev_rf,
  importance = TRUE
)

rf_default_lib
plot(rf_default_lib)
importance(rf_default_lib)
varImpPlot(
  rf_default_lib,
  type = 1,
  main = "Permutation Importance"
)


#Default with the values from the slides mtry = p/3 , node size = 5 and B = 500

set.seed(600)

rf_default <- randomForest(
  label_B ~ .,
  data = dev_rf,
  mtry = 6,
  ntree = 500,
  nodesize = 5,
  importance = TRUE
)

rf_default
plot(rf_default)
importance(rf_default)
varImpPlot(
  rf_default,
  type = 1,
  main = "Permutation Importance"
)

#Now tweak the hyperparameters

if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

library(caret)

#Candidate values

mtry_grid <- expand.grid(
  mtry = c(1, 2, 4, 6, 8, 12, 18)
)

ntree_grid <- c(50, 100, 250, 500, 750, 1000, 1500)

####
#10-fold CV
####

# Control CV with fixed country folds

country <- as.character(dev_data$iso3c)
countries <- unique(country)

stopifnot(
  length(country) == nrow(dev_rf),
  !anyNA(country),
  all(nzchar(country)),
  length(countries) >= 10L
)

set.seed(600)

country_fold <- sample(
  rep(seq_len(10), length.out = length(countries))
)

row_fold <- country_fold[match(country, countries)]

folds_10cv <- lapply(
  seq_len(10),
  function(k) which(row_fold != k)
)

names(folds_10cv) <- paste0("Fold", seq_len(10))

control_10cv <- trainControl(
  method = "cv",
  number = 10,
  index = folds_10cv
)





results_10cv <- list()

for (n_tree in ntree_grid) {
  set.seed(600)
  
  model <- train(
    label_B ~ .,
    data = dev_rf,
    method = "rf",
    metric = "Accuracy",
    tuneGrid = mtry_grid,
    trControl = control_10cv,
    ntree = n_tree
  )
  
  results_10cv[[as.character(n_tree)]] <- model
}

#save results of CV

cv_results <- do.call(
  rbind,
  lapply(names(results_10cv), function(n_tree) {
    
    x <- results_10cv[[n_tree]]$results
    
    data.frame(
      ntree = as.numeric(n_tree),
      mtry = x$mtry,
      Accuracy = x$Accuracy,
      AccuracySD = x$AccuracySD
    )
  })
)

cv_results

#find the best

best_10cv <- cv_results[
  which.max(cv_results$Accuracy),
]

cv_results %>%
  arrange(desc(Accuracy))

best_10cv

# 13   100   12 0.6671429  0.1635837

#plot

plot(
  x = range(cv_results$mtry),
  y = range(cv_results$Accuracy),
  type = "n",
  xlab = "mtry",
  ylab = "CV Accuracy",
  main = "10-fold CV Accuracy for Random Forest"
)

ntree_values <- sort(unique(cv_results$ntree))
cols <- 1:length(ntree_values)

for (i in seq_along(ntree_values)) {
  this_ntree <- ntree_values[i]
  temp <- cv_results[cv_results$ntree == this_ntree, ]
  
  lines(temp$mtry, temp$Accuracy, type = "b", col = cols[i], pch = 16)
}

legend(
  "bottomright",
  legend = paste("ntree =", ntree_values),
  col = cols,
  lty = 1,
  pch = 16,
  cex = 0.8
)

####
#LOOCV
####

# Hold out every programme from one country at a time.
folds_loco <- lapply(
  countries,
  function(held_out_country) which(country != held_out_country)
)

names(folds_loco) <- paste0("Country_", countries)

control_loocv <- trainControl(
  method = "cv",
  number = length(folds_loco),
  index = folds_loco
)

set.seed(600)

results_loocv <- list()

for (n_tree in ntree_grid) {
  
  model <- train(
    label_B ~ .,
    data = dev_rf,
    method = "rf",
    metric = "Accuracy",
    tuneGrid = mtry_grid,
    trControl = control_loocv,
    ntree = n_tree
  )
  
  results_loocv[[as.character(n_tree)]] <- model
}

# Save LOOCV results
loocv_results <- do.call(
  rbind,
  lapply(names(results_loocv), function(n_tree) {
    
    x <- results_loocv[[n_tree]]$results
    
    data.frame(
      ntree = as.numeric(n_tree),
      mtry = x$mtry,
      Accuracy = x$Accuracy
    )
  })
)

loocv_results %>%
  arrange(desc(Accuracy))

# Find best LOOCV combination
best_loocv <- loocv_results[
  which.max(loocv_results$Accuracy),
]

best_loocv

# 15   250    1 0.6666667

#We are getting different best hyperparameters using LOOCV and 10-fold so we are going to use 10-fold's result because as said in the lecture, 10-fold has higher variance than LOOCV
#Especially considering 23   100    1 0.6242857 0.06600007 si rankied 23rd best with 10-fold
#but 3    100   12 0.6595745 is ranked 3rd according to LOOCV so 100 12 is prob one of the best


################################################################################
# FINAL RANDOM FOREST
################################################################################

set.seed(600)

rf_final <- randomForest(
  label_B ~ .,
  data = dev_rf,
  ntree = best_10cv$ntree,
  mtry = best_10cv$mtry,
  importance = TRUE
)

rf_final

#now we test

pred_test <- predict(
  rf_final,
  newdata = test_rf
)

confusionMatrix(
  pred_test,
  test_rf$label_B
)

cm_final <- confusionMatrix(
  data = pred_test,
  reference = test_rf$label_B,
  positive = "yes"
)

cm_final

final_accuracy <- cm_final$overall["Accuracy"]

final_accuracy

#permutation on final model
final_importance <- importance(
  rf_final,
  type = 1
)

final_importance

varImpPlot(
  rf_final,
  type = 1,
  main = "Permutation Importance - Final Random Forest"
)

#comapre
rf_default_lib
rf_final

default_oob <- tail(rf_default_lib$err.rate[, "OOB"], 1)
final_oob   <- tail(rf_final$err.rate[, "OOB"], 1)

default_oob
final_oob
