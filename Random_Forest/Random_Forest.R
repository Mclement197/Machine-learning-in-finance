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

################################################################################
#CLASSIFICATION TREE
################################################################################
#We analyse the Carseats data set: sales of child car seats at 400 stores.
summary(Carseats)

#We create a variable High, which takes the value "Yes" if Sales exceeds 8
#(thousand units) and "No" otherwise. This turns the problem into classification.
High <- ifelse(Carseats$Sales <= 8, "No", "Yes")
High <- as.factor(High)
table(High)

#We use data.frame() to merge High with the rest of the Carseats data
Carseats1 <- data.frame(Carseats, High)

#We use the tree() function to fit a classification tree in order to predict
#High using all variables but Sales (Sales must be excluded: High was built from it).
tree.carseats <- tree(High ~ . - Sales, data = Carseats1)

#summary() lists the variables used as internal nodes in the tree, the number
#of terminal nodes and the training error rate.
#(See Section 8.3.1 of the ISLR book for the definition of the residual mean
#deviance reported below.)
summary(tree.carseats)

#Show the tree graphically. text() adds the node labels; pretty = 0 tells R to
#write the category names of qualitative predictors instead of a letter per
#category. cex makes the labels smaller so that they fit.
plot(tree.carseats)
text(tree.carseats, pretty = 0, cex = 0.5)

#Print the branches of the tree: split rule, number of observations, deviance,
#predicted class, and the share of Yes/No in the node. A * marks a terminal node.
tree.carseats

################################################################################
#Estimate the test error: 200 of the 400 stores as training set, the rest as test set
################################################################################
set.seed(2)
train <- sample(1:nrow(Carseats1), 200)
Carseats.test <- Carseats1[-train, ]
High.test <- Carseats1$High[-train]

#Fit the classification tree on the training data only
tree.carseats <- tree(High ~ . - Sales, data = Carseats1, subset = train)

#Predict the class on the test data
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")

#Confusion matrix: rows = predicted, columns = true
table(tree.pred, High.test)

#Misclassification error on the test data
mean(tree.pred != High.test)

################################################################################
#Pruning the tree
################################################################################
#We next consider whether pruning the tree might lead to improved results.
#cv.tree() performs cross validation in order to determine the optimal level of
#tree complexity. Cost-complexity pruning is used to select a sequence of trees
#for consideration. FUN = prune.misclass tells R to use the classification
#error rate to guide the cross validation and the pruning, rather than the
#default of cv.tree(), which is the deviance.
set.seed(10)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
#size: the number of terminal nodes of each tree considered
#dev: the cross validation error (number of misclassified observations) for each tree
#k: the value of the cost-complexity parameter (alpha in the slides)
cv.carseats

par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b",
     xlab = "number of terminal nodes", ylab = "CV error")
plot(cv.carseats$k, cv.carseats$dev, type = "b",
     xlab = "alpha (cost-complexity parameter)", ylab = "CV error")
par(mfrow = c(1, 1))

#The best size is the one with the lowest CV error. We read it off the results
#instead of typing it by hand, so that the code also works with another seed.
#(If several sizes tie, which.min takes the first one listed, i.e. the largest.)
best.size <- cv.carseats$size[which.min(cv.carseats$dev)]
best.size

#Prune the tree to the best size and display it
prune.carseats <- prune.misclass(tree.carseats, best = best.size)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

#Compute the test error rate using the pruned tree and compare it with the
#unpruned tree above. A smaller tree that predicts as well is easier to read.
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred != High.test)

################################################################################
#REGRESSION TREE
################################################################################
#We use the Boston Housing data set again (package MASS). Target: medv.
library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)
#Only a few of the 13 variables are used. lstat and rm dominate.

plot(tree.boston)
text(tree.boston, pretty = 0)

#Cross validation to see whether pruning helps (default: deviance, i.e. squared error)
set.seed(1)
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b",
     xlab = "number of terminal nodes", ylab = "CV deviance")
#Here the most complex tree is (usually) selected by cross validation.
#If we wish to prune the tree anyway, we can do so with prune.tree():
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

#In keeping with the CV results, we use the unpruned tree to make predictions
#on the test data.
yhat <- predict(tree.boston, newdata = Boston[-train, ])

#True values on the test data
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test, xlab = "predicted medv", ylab = "true medv")
abline(0, 1)
#The points form horizontal bands: a tree can only predict a handful of
#different values, one per terminal node.

#Test MSE of the regression tree
mean((yhat - boston.test)^2)
#Keep this number in mind: the random forest script gets a much lower one.
