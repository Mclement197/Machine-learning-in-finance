# Machine Learning in Finance – Group Assignment

## Goal

Build a financial predictive machine learning project using a financial dataset of our choice.

The assignment counts for 30% of the final course grade. All group members receive the same grade.

## Dataset Requirements

The dataset should:

- Be related to finance.
- Have at least 10 predictors.
- Ideally contain at least a few hundred observations.
- NOT be a time-series dataset.
- Have a clearly defined target variable Y.
- Have clearly defined predictor variables X.
- Address either a regression or classification problem.

Possible data sources include Bloomberg, Eikon/LSEG, Yahoo Finance, Google Finance, World Bank, FRED, ECB, IMF, ESG databases, etc.

---

## Task 1 – Research Question, Data & EDA | 20%

1. Introduce the financial topic.
2. Conduct an academic literature review, ideally using top finance journals and related academic sources.
3. Identify whether there is a gap in the existing literature that the project addresses.
4. Define the research goal using predictive modelling terminology.
5. Explain why the prediction problem is financially relevant.
6. Clearly state whether it is a regression or classification problem.
7. Describe the study design and data collection process.
8. Present summary statistics using R.
9. Preprocess the data if necessary.
10. Perform exploratory data analysis (EDA) using R.
11. Define the input variables X and output variable Y.
12. Explain why these variables are appropriate from a predictive modelling perspective.

---

## Task 2 – Random Forest | 40%

The Random Forest is the main model of the project.

1. Explain the Random Forest methodology.
2. Train a Random Forest using the `randomForest` package in R with default hyperparameters.
3. Tune the Random Forest hyperparameters:
   - Number of trees (`ntree`)
   - Number of variables considered at each split (`mtry`)
4. Perform the tuning using the `caret` package with BOTH:
   - 10-fold cross-validation
   - Leave-One-Out Cross-Validation (LOOCV)
5. Choose the final Random Forest model.
6. Make predictions using the final model.
7. Report prediction accuracy.
8. Discuss the prediction results using appropriate predictive modelling terminology.
9. Calculate predictor importance using permutation importance with the `randomForest` package.
10. Interpret and discuss the variable importance results.
11. Finish with remarks covering:
   - Main findings
   - Limitations
   - Potential future improvements

---

## Task 3 – Alternative Models | 10%

### Simple Model

Train and test a simpler model, for example:

- Linear regression
- Decision tree

Compare it with the Random Forest in terms of:

- Prediction accuracy
- Interpretability

Discuss the trade-off between interpretability and predictive performance.

### Neural Network

Train and test a Neural Network on the same dataset.

Compare its predictive accuracy with the Random Forest and discuss the results.

---

## Task 4 – Presentation | 20%

Prepare a 15-minute presentation INCLUDING Q&A.

- Presentation date: 07.10.2026
- One or several group members can present.
- All group members should attend and be able to answer questions.

---

## Code Quality | 10%

The R code must be:

- Clear
- Well structured
- Reproducible

Someone with access to the submitted dataset and R code must be able to reproduce the results shown in the report and presentation.

Use `set.seed()` where necessary to ensure reproducibility.

---

## Deliverables

Submit:

project/
├── data.xlsx
├── analysis.R
├── report.pdf
└── presentation.pdf

### Report

Approximately:

- 10 pages
- 1.5 line spacing
- 11 pt font

The report must include all references used, including coding or other aids such as ChatGPT where applicable.

### Deadline

Final submission: 14.10.2026 at 17:00

Submission is via StudyNet/Canvas email to the lecturer.

The email should contain:

- Canvas group number/name
- Names of all group members
- All required files

---

## Recommended Workflow

Research question
→ Find dataset
→ Define X and Y
→ Literature review
→ Summary statistics
→ EDA
→ Preprocessing
→ Train/test setup
→ Default Random Forest
→ Tune ntree + mtry with 10-fold CV
→ Tune with LOOCV
→ Select final Random Forest
→ Generate predictions
→ Evaluate prediction accuracy
→ Permutation importance
→ Train simple model
→ Train Neural Network
→ Compare all models
→ Discuss limitations
→ Conclusion
→ Write report
→ Prepare presentation

---

## Main Principle

This is primarily a PREDICTIVE modelling project, not a causal inference project.

The central question is:

"How accurately can we predict Y for new observations using the available X variables?"

Therefore, focus on OUT-OF-SAMPLE predictive performance rather than simply obtaining a good fit on the training data.