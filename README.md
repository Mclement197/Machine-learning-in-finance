# Machine Learning in Finance

This project predicts whether an IMF lending programme will be interrupted. The
workflow is split into data preparation and model-specific analysis. Data
preparation must always be completed before a model is trained or tested.

## Project Structure

```text
Machine-learning-in-finance/
|-- Data/
|   |-- raw/                 # Source data used by the preparation script
|   |-- processed/           # Generated train and test data
|   `-- summary/             # Generated summary tables
|-- Data_Prep/
|   |-- 01_data.R            # Cleans data and creates train.csv and test.csv
|   `-- figures/             # Generated exploratory-analysis figures
|-- Random_Forest/
|   |-- Random_Forest.R      # Trains, tunes, and tests the random forest
|   `-- fig_*.png            # Generated model figures
|-- Presentation/            # Presentation source files
`-- README.md
```

The files in `Data/processed/`, `Data/summary/`, and `Data_Prep/figures/` are
outputs of the data-preparation script. Do not run a model against stale or
manually edited versions of these files.

## Requirements

- R and, preferably, RStudio
- Git LFS for downloading the raw data files
- An internet connection the first time the R packages are installed

After cloning the repository, install Git LFS and download the LFS-managed
files:

```bash
git lfs install
git lfs pull
```

The R scripts install missing packages automatically. Data preparation uses
`readxl` and `writexl`. The random forest analysis uses `tree`, `randomForest`,
`caret`, and `pROC`.

## Run the Project

### 1. Prepare the data

Run `Data_Prep/01_data.R` first. The script reads the source files from
`Data/raw/`, cleans and splits the observations, and creates:

- `Data/processed/train.csv`
- `Data/processed/test.csv`
- `Data/processed/imf_programmes.xlsx`
- Summary tables in `Data/summary/`
- Exploratory-analysis figures in `Data_Prep/figures/`

The scripts use paths relative to their own directories. In RStudio, open
`Data_Prep/01_data.R`, select **Session > Set Working Directory > To Source File
Location**, and run the script.

Alternatively, run it from R after setting `Data_Prep` as the working directory:

```r
setwd("path/to/Machine-learning-in-finance/Data_Prep")
source("01_data.R", echo = TRUE)
```

Do not continue until `Data/processed/train.csv` and
`Data/processed/test.csv` have been generated successfully.

### 2. Train and test a model

Each model belongs in its own folder and reads the train and test files created
in the previous step. Currently, the random forest is the only implemented
model.

In RStudio, open `Random_Forest/Random_Forest.R`, select **Session > Set Working
Directory > To Source File Location**, and run the script.

Alternatively, run it from R after setting `Random_Forest` as the working
directory:

```r
setwd("path/to/Machine-learning-in-finance/Random_Forest")
source("Random_Forest.R", echo = TRUE)
```

The script:

- Loads `Data/processed/train.csv` and `Data/processed/test.csv`
- Trains a default random forest
- Tunes `ntree` and `mtry` using cross-validation
- Selects and evaluates the final model on the test data
- Calculates permutation variable importance
- Saves model plots in `Random_Forest/`

Model tuning, particularly leave-one-out cross-validation, can take several
minutes.

## Required Execution Order

```text
Data/raw/
    |
    v
Data_Prep/01_data.R
    |
    v
Data/processed/train.csv + Data/processed/test.csv
    |
    v
Random_Forest/Random_Forest.R
    |
    v
Trained model, test results, and figures
```

If the raw data or preparation logic changes, rerun `Data_Prep/01_data.R`
before rerunning any model.

## Adding Models

Future models should follow the same structure as `Random_Forest/`: keep each
model in a separate folder and load the generated files from
`Data/processed/`. All models should use the same train and test split so their
out-of-sample results are comparable.
