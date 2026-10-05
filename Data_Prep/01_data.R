################################################################################
# 01_data.R
# IMF lending programmes: data preparation (1e), summary statistics (1d),
# exploratory data analysis (1f), inputs and output (1g)
# Machine Learning in Finance - group exam paper (Autumn 2026)
#
# TASK: predict, on the day the IMF Executive Board approves a lending
# programme, whether the programme will be INTERRUPTED (binary classification).
#
# HOW TO RUN: open the RStudio project (the repository root, the folder that
# contains Data/ and Data_Prep/) and run
#     source("Data_Prep/01_data.R", echo = TRUE)
# INPUT:  Data/raw/        OUTPUT: Data/processed/ and Data_Prep/figures/
# Nothing in this script is random (the split is by approval year), so no
# set.seed() is needed.
################################################################################

################################################################################
# 0. Packages and file paths
################################################################################
#Install the package only if needed (requireNamespace gets flipped)
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)

# Avoid issues with Windows in German (or any language other than English)
Sys.setlocale("LC_TIME", "C")

#Folders in repo
dir_raw  <- "../Data/raw/"
dir_proc <- "../Data/processed/"
dir_sum <- "../Data/summary/"
dir_fig  <- "../Data_Prep/figures/"
if (!dir.exists(dir_proc)) dir.create(dir_proc, recursive = TRUE)
if (!dir.exists(dir_fig))  dir.create(dir_fig,  recursive = TRUE)

#Raw files (datasets)
f_commitments <- paste0(dir_raw, "COMMITMENTS.xls")
f_description <- paste0(dir_raw, "Description.xlsx")
f_reviews     <- paste0(dir_raw, "Reviews.xlsx")
f_combined    <- paste0(dir_raw, "Combined.xlsx")
f_mecon       <- paste0(dir_raw, "Mecon.xlsx")

#Check WD and that R is looking where it should
getwd()
file.exists(c(f_commitments, f_description, f_reviews, f_combined, f_mecon))

################################################################################
# 1e. PREPARE THE DATA
# Step 1: read every raw file and look at it before using it
################################################################################

#--- COMMITMENTS ----------------------------------------------------------------
#Reads first 11 lines but keeps first 60 characters of each (some are very long)
substr(readLines(f_commitments, n = 11, encoding = "UTF-8"), 1, 60)

#Skips the first 9 lines (metadata and group headers), defines NAs;
#Quote handling; white spaces get trimmed; UTF-8 to avoid errors and keep special chars
commit <- read.delim(f_commitments, skip = 9, header = TRUE, quote = "",
                     na.strings = c("n.a.", ""), strip.white = TRUE,
                     stringsAsFactors = FALSE, fileEncoding = "UTF-8")
dim(commit)
names(commit)   #duplicate names: R appends .1 to the "As Of" columns (not to be used, unknown on approval day)
str(commit) #structure
head(commit, 3)
tail(commit, 3) #the last row is a "Total" row, not an arrangement
table(commit$Type)
table(commit$Precautionary, useNA = "ifany")   #N, Y and many missing (old rows)

#--- MONA exports ---------------------------------------------------------------
#Each file has one sheet called "Report". The two small files are read in full.
descr   <- read_excel(f_description, sheet = "Report")
reviews <- read_excel(f_reviews,     sheet = "Report")

#Combined (about 58,000 rows) and Mecon (about 152,000 rows) are big: for a
#first look the first 2,000 rows are enough. They are read completely later,
#with only the columns we need.
comb  <- read_excel(f_combined, sheet = "Report", n_max = 2000)
mecon <- read_excel(f_mecon,    sheet = "Report", n_max = 2000)

dim(descr); dim(reviews); dim(comb); dim(mecon)
str(descr)
head(descr, 3)
str(reviews)
str(comb)
str(mecon)

#MONA's column names are quite bad, so we check what the columns really hold.
#"Review Type" holds the snapshot (R0 = approval, R1 = after the 1st review,
#...). trimws() removes blanks that MONA pads its codes with.
table(trimws(descr[["Review Type"]]))
length(unique(descr[["Arrangement Number"]]))      #385 arrangements
length(unique(reviews[["Arrangement Number"]]))    #375

################################################################################
# Step 2: clean COMMITMENTS and apply the sample filters
################################################################################

#We drop rows with no member code (the last one - 'Total')
commit <- commit[!is.na(commit$Member.Code), ]
nrow(commit)        #1576 arrangements

#Text -> dates and numbers.
#Amounts contain thousands commas ("6,750,000"): remove them, then as.numeric().
#Dates change from "June 17, 1965": %B = month name, %d = day, %Y = 4-digit year.
#Only the "Upon Approval" columns are used as inputs (known on approval day).
#The "As Of" columns (.1) and Amount.Drawn must be excluded (would be looking ahead)
commit$approval_date    <- as.Date(commit$Date.Of.Commitment, format = "%B %d, %Y")
commit$planned_end      <- as.Date(commit$Expiration.Date,    format = "%B %d, %Y")
commit$approval_year    <- as.numeric(format(commit$approval_date, "%Y"))
commit$amount_agreed    <- as.numeric(gsub(",", "", commit$Amount.Agreed))
commit$access_pct_quota <- commit$X..Of.Quota
commit$amount_drawn     <- as.numeric(gsub(",", "", commit$Amount.Drawn))

#Parsing check: only planned_end has missing values (151). These are the rapid
#loans (RCF / RFI), which have no expiry date; they are removed anyways in Step 3.
sum(is.na(commit$approval_date)); sum(is.na(commit$planned_end))
sum(is.na(commit$amount_agreed)); sum(is.na(commit$amount_drawn))
summary(commit$approval_date) # dates in the whole file

#The sample filters, with the number of programmes left after each.
#%in% "N" is FALSE for missing flags, so rows without a flag drop out safely.
in_window       <- commit$approval_year >= 2002 & commit$approval_year <= 2016
not_precaut     <- commit$Precautionary %in% "N"
something_drawn <- commit$amount_drawn > 0
c(all                   = nrow(commit),
  approved_2002_2016    = sum(in_window),
  and_not_precautionary = sum(in_window & not_precaut),
  and_drawn_something   = sum(in_window & not_precaut & something_drawn))

#Which flags do the removed rows in the window have?
table(commit$Precautionary[in_window], useNA = "ifany")

commit_s <- commit[in_window & not_precaut & something_drawn, ]

#What is in the sample?
table(commit_s$Facility)
table(commit_s$approval_year)

################################################################################
# Step 3: keep programmes with a review schedule, merge blended programmes
################################################################################

#The rapid facilities (RCF, RFI) are one-off loans with no expiry date and no
#reviews, so "interrupted" is undefined for them. The PLL is a precautionary
#line. The ESF (Exogenous Shocks Facility, 2008-09) STAYS in: it was superseded
#by the Standby Credit Facility (SCF) in January 2010, so it is the SCF's
#predecessor, just as the PRGF is the ECF's. It is treated like an SCF below.
#The sample is SBA, EFF, ECF, SCF (incl. ESF) and blends of them.
drop_fac <- c("Rapid Credit Facility (No Tier)", "Rapid Financing Instrument",
              "Precautionary and Liquidity Line")
commit_s <- commit_s[!(commit_s$Facility %in% drop_fac), ]
nrow(commit_s)                      #190
sum(is.na(commit_s$planned_end))    #0

#Blended programmes (e.g. EFF + ECF, or SBA + ESF) appear as TWO rows with the
#same country and approval date.
key <- paste(commit_s$Member.Code, commit_s$approval_date)
same_day <- commit_s[key %in% key[duplicated(key)],
                     c("Member.Code", "approval_date", "Facility",
                       "amount_agreed", "access_pct_quota")]
same_day[order(same_day$Member.Code, same_day$approval_date), ]

#We treat the rows of a blend as ONE programme: one Board decision, one review
#schedule. Keeping both rows would give two near-identical observations with
#the same label, and in cross-validation the twin of a validation row would sit
#in the training folds. Amounts and % of quota are added up; a blend is
#concessional if any component is a PRGT facility; it ends when the longer
#component ends.
merge_blends <- function(rows) {
  rows$key <- paste(rows$Member.Code, rows$approval_date)
  out <- rows[!duplicated(rows$key), ]      #first row of every programme
  out$n_components <- 1
  out$is_prgt      <- 0
  for (i in 1:nrow(out)) {
    g <- rows[rows$key == out$key[i], ]     #all rows of this programme
    out$n_components[i]     <- nrow(g)
    out$Facility[i]         <- paste(g$Facility, collapse = " + ")
    out$planned_end[i]      <- max(g$planned_end)
    out$is_prgt[i]          <- as.numeric(any(g$Type == "PRGT"))
    out$amount_agreed[i]    <- sum(g$amount_agreed)
    out$access_pct_quota[i] <- sum(g$access_pct_quota)
    out$amount_drawn[i]     <- sum(g$amount_drawn)  #after approval: NOT an input
  }
  out <- out[order(out$approval_date, out$Member.Code), ]
  rownames(out) <- NULL
  out[, c("Member.Code", "Member", "approval_date", "approval_year",
          "planned_end", "Facility", "n_components", "is_prgt",
          "amount_agreed", "access_pct_quota", "amount_drawn")]
}

prog <- merge_blends(commit_s)
nrow(prog)                  #182 programmes
table(prog$n_components)    #1: 174, 2: 8
prog[prog$n_components == 2,
     c("Member.Code", "approval_date", "Facility", "access_pct_quota", "amount_agreed")]

################################################################################
# Step 4: match each programme to its MONA arrangement
################################################################################

#MONA has one row per arrangement AND snapshot. The approval snapshot (R0)
#gives exactly one row per arrangement.
descr$snapshot <- trimws(descr[["Review Type"]]) #column name contains space
mona_r0 <- descr[descr$snapshot == "R0", ]
mona <- data.frame(arr_id       = mona_r0[["Arrangement Number"]],
                   mona_name    = mona_r0[["Country Name"]],
                   mona_type    = mona_r0[["Arrangement Type"]],
                   mona_date    = as.Date(mona_r0[["Approval Date"]], format = "%d-%b-%y"),
                   mona_precaut = mona_r0[["Precautionary"]])
nrow(mona)                      #385
sum(duplicated(mona$arr_id))    #0: one row per arrangement
sum(is.na(mona$mona_date))      #0
range(mona$mona_date)

#Country keys: capital letters only, so spaces and punctuation do not matter to match MONA and Commitments
country_key <- function(x) gsub("[^A-Z]", "", toupper(x))
mona$key <- country_key(mona$mona_name)
prog$key <- country_key(prog$Member)

#Countries of our sample that MONA does not find (names are spelled differently in the two sets)
unique(prog$Member[!(prog$key %in% mona$key)])

#We map these by hand to MONA's spelling, using the 3-letter COMMITMENTS code
alias <- c(ARM = "ARMENIA", BLR = "BELARUS", CPV = "CAPE VERDE",
           KOS = "KOSOVO, REPUBLIC OF", LVA = "LATVIA", MDA = "MOLDOVA",
           STP = "SAO TOME AND PRINCIPE", TJK = "TAJIKISTAN", TUR = "TURKEY")
has_alias <- prog$Member.Code %in% names(alias)
prog$key[has_alias] <- country_key(alias[prog$Member.Code[has_alias]])

#Left over: countries that MONA does not know at all
unique(prog$Member[!(prog$key %in% mona$key)])      #Guyana

#Match on country + approval date within 60 days (the two sources differ a bit)
tol <- 60 #60 day tolerance
prog$arr_id   <- NA_real_
prog$gap_days <- NA_real_     #MONA date minus COMMITMENTS date
prog$n_cand   <- 0
for (i in 1:nrow(prog)) {
  gap  <- as.numeric(mona$mona_date - prog$approval_date[i])
  cand <- which(mona$key == prog$key[i] & abs(gap) <= tol)
  prog$n_cand[i] <- length(cand)
  if (length(cand) >= 1) {                    #if several: take the closest
    best <- cand[which.min(abs(gap[cand]))]
    prog$arr_id[i]   <- mona$arr_id[best]
    prog$gap_days[i] <- gap[best]
  }
}

table(prog$n_cand)                                 #0 = no match, 1 = unique
sum(duplicated(prog$arr_id[!is.na(prog$arr_id)]))  #a MONA arrangement used twice?
table(abs(prog$gap_days))                          #how far apart are the dates?

#The programmes without a match are not in MONA's export; they are dropped later
prog[is.na(prog$arr_id), c("Member.Code", "approval_date", "Facility")]

#Sanity check: does MONA's own arrangement type agree with our facility?
prog$mona_type    <- mona$mona_type[match(prog$arr_id, mona$arr_id)]
prog$mona_precaut <- mona$mona_precaut[match(prog$arr_id, mona$arr_id)]
table(prog$mona_type, useNA = "ifany")
table(prog$mona_precaut, useNA = "ifany")
prog[prog$mona_precaut %in% "Y", c("Member.Code", "approval_date", "Facility", "mona_type")]

################################################################################
# Step 5: the outcome "interrupted"
################################################################################

#"Review Type" is the snapshot, as in descr. R0 = reviews scheduled at approval
reviews$snapshot <- trimws(reviews[["Review Type"]])

#For every programme: how many program reviews were scheduled at approval (R0),
#and how many of them were completed? A completion date in ANY snapshot counts.
#Rows without a review name belong to the other review type (financing
#assurances) and are ignored.
prog$n_reviews_sched <- NA_real_
prog$n_reviews_done  <- NA_real_
for (i in 1:nrow(prog)) {
  if (!is.na(prog$arr_id[i])) {
    r <- reviews[reviews[["Arrangement Number"]] == prog$arr_id[i] &
                   !is.na(reviews[["Program Review Sequence"]]), ]
    sched <- unique(r[["Program Review Sequence"]][r$snapshot == "R0"])
    done  <- unique(r[["Program Review Sequence"]][!is.na(r[["Program Review Completed Date"]])])
    prog$n_reviews_sched[i] <- length(sched)
    prog$n_reviews_done[i]  <- sum(sched %in% done)
  }
}

#interrupted = 1 if fewer reviews were completed than were scheduled at approval
prog$interrupted <- as.numeric(prog$n_reviews_done < prog$n_reviews_sched)

table(prog$n_reviews_sched, useNA = "ifany")
table(prog$interrupted, useNA = "ifany")     #NA = the 9 programmes without a match
prop.table(table(prog$interrupted))          #about 47% interrupted

#Cross-check with MONA's own "Cancelled" flag (Y in any snapshot)
cancelled_ids <- unique(descr[["Arrangement Number"]][descr[["Cancelled"]] %in% "Y"])
prog$mona_cancelled <- as.numeric(prog$arr_id %in% cancelled_ids)
table(interrupted = prog$interrupted, cancelled = prog$mona_cancelled)

################################################################################
# Step 6: features known on approval day (deal structure and conditions)
################################################################################

#6a. From COMMITMENTS (upon-approval columns only)
prog$log_access_pct_quota <- log(prog$access_pct_quota)   #skewed, so use the log
prog$planned_months <- as.numeric(prog$planned_end - prog$approval_date) / 30.4375
prog$concessional   <- prog$is_prgt                       #1 if a PRGT facility is part of it
prog$extended       <- as.numeric(grepl("Extended", prog$Facility))   #EFF or ECF

#facility_type is only for describing the sample (tables and plots), NOT an
#input: concessional + extended already say the same thing (ECF = 1/1,
#SCF = 1/0, EFF = 0/1, SBA = 0/0). ESF programmes get the default "SCF".
prog$facility_type <- "SCF"                               #default (SCF and ESF)
prog$facility_type[grepl("Stand-By",       prog$Facility)] <- "SBA"
prog$facility_type[grepl("Extended Fund",  prog$Facility)] <- "EFF"
prog$facility_type[grepl("Extended Credit", prog$Facility)] <- "ECF"
prog$facility_type[prog$n_components == 2] <- "Blend"     #last, so it overrides
table(prog$facility_type)                                 #Blend 8, ECF 99, EFF 17, SBA 50, SCF 8

#6b. From MONA Combined (conditions), approval snapshot R0.
#Only 3 columns are read: "skip" drops the others.
hdr <- names(comb)
ct  <- ifelse(hdr == "Arrangement Number", "numeric",
              ifelse(hdr %in% c("Review Type", "Key Code"), "text", "skip"))
comb_full <- read_excel(f_combined, sheet = "Report", col_types = ct)
comb_full$snapshot <- trimws(comb_full[["Review Type"]])
c0 <- comb_full[comb_full$snapshot == "R0", ]
c0$kind <- trimws(c0[["Key Code"]])
table(c0$kind, useNA = "ifany")     #PA prior action; SB, SPC, SAC structural conditions

prog$n_prior_actions <- NA_real_
prog$n_struct_cond   <- NA_real_
for (i in 1:nrow(prog)) {
  if (!is.na(prog$arr_id[i])) {
    k <- c0$kind[c0[["Arrangement Number"]] == prog$arr_id[i]]
    if (length(k) > 0) {
      prog$n_prior_actions[i] <- sum(k %in% "PA")
      prog$n_struct_cond[i]   <- sum(k %in% c("SB", "SPC", "SAC"))
    }
  }
}

#6c. Check on the matched programmes
m <- !is.na(prog$arr_id)
vars6 <- c("log_access_pct_quota", "planned_months", "concessional", "extended",
           "n_prior_actions", "n_struct_cond")
summary(prog[m, vars6])
colSums(is.na(prog[m, vars6]))

################################################################################
# Step 7: track record (the country's earlier programmes, 1952-2026)
################################################################################
#7a. History table: every earlier programme of the type we study, in ALL years
hist_rows <- commit[!(commit$Precautionary %in% "Y") &
                      commit$amount_drawn > 0 &
                      !(commit$Facility %in% drop_fac), ]
hist_prog <- merge_blends(hist_rows)                 #blends merged, as in Step 3
nrow(hist_prog)                                      #1099

#7b. For every programme: look at the same country's EARLIER programmes
prog$prev_n_20y       <- 0
prog$no_track_record  <- 1
prog$years_since_prev <- NA_real_
for (i in 1:nrow(prog)) {
  hh <- hist_prog[hist_prog$Member.Code == prog$Member.Code[i] &
                    hist_prog$approval_date < prog$approval_date[i], ]
  prog$prev_n_20y[i] <- sum(hh$approval_date >= prog$approval_date[i] - 20 * 365.25)
  if (nrow(hh) > 0) {
    last <- hh[which.max(hh$approval_date), ]     #the most recent earlier programme
    prog$no_track_record[i]  <- 0
    prog$years_since_prev[i] <- max(0, as.numeric(prog$approval_date[i] - last$planned_end) / 365.25)
  }
}

#7c. Check (all 182 programmes)
table(prog$no_track_record)
summary(prog$years_since_prev)
table(prog$prev_n_20y)

################################################################################
# Step 8: macro features from Mecon (staff projections in the approval snapshot)
################################################################################

#8a. Mecon has about 152,000 rows (1-3 minutes). Read only the columns we need,
#as in Step 6: "skip" drops the others. T and T-1 are the staff estimates for
#the approval year and the year before it.
hdr   <- names(mecon)
ctype <- ifelse(hdr %in% c("Arrangement Number", "T-1", "T"), "numeric",
                ifelse(hdr %in% c("Review Type", "Mneumonic"), "text", "skip"))
mecon_full <- read_excel(f_mecon, sheet = "Report", col_types = ctype)
dim(mecon_full)

#Approval snapshot (R0) and the five indicators we need:
#NGDP_R real GDP, PCPI consumer prices, BCA current account (US$ bn),
#NGDP nominal GDP (national currency), ENDA exchange rate (national currency per US$)
mecon_full$snapshot <- trimws(mecon_full[["Review Type"]])
mecon_full$ind      <- trimws(mecon_full[["Mneumonic"]])
m0 <- mecon_full[mecon_full$snapshot == "R0" &
                   mecon_full$ind %in% c("NGDP_R", "PCPI", "BCA", "NGDP", "ENDA"), ]
nrow(m0)                                                      #1731
sum(duplicated(paste(m0[["Arrangement Number"]], m0$ind)))    #0

#8b. One value of one indicator for one arrangement (NA if it is not there)
get_val <- function(e, ind, col) {
  v <- e[[col]][e$ind == ind]
  if (length(v) == 1) v else NA_real_
}

#8c. Growth, inflation and current account, programme by programme
prog$growth              <- NA_real_
prog$inflation           <- NA_real_
prog$current_account_gdp <- NA_real_
for (i in 1:nrow(prog)) {
  if (!is.na(prog$arr_id[i])) {
    e <- m0[m0[["Arrangement Number"]] == prog$arr_id[i], ]
    gdp_now <- get_val(e, "NGDP_R", "T");  gdp_before <- get_val(e, "NGDP_R", "T-1")
    cpi_now <- get_val(e, "PCPI",   "T");  cpi_before <- get_val(e, "PCPI",   "T-1")
    prog$growth[i]    <- (gdp_now / gdp_before - 1) * 100            #real GDP growth, %
    prog$inflation[i] <- (cpi_now / cpi_before - 1) * 100            #CPI inflation, %
    prog$current_account_gdp[i] <- get_val(e, "BCA", "T") /          #US$ bn / (GDP in US$)
      (get_val(e, "NGDP", "T") / get_val(e, "ENDA", "T")) * 100      #in % of GDP
  }
}

#Division by zero or missing inputs give Inf or NaN: make them proper NA
for (v in c("growth", "inflation", "current_account_gdp")) {
  prog[[v]][!is.finite(prog[[v]])] <- NA
}
#Data errors, not economics (Liberia 2008 current account -4,286% of GDP,
#Macedonia 2005 inflation -433%): set to NA
prog$current_account_gdp[abs(prog$current_account_gdp) > 100] <- NA
prog$inflation[prog$inflation < -50] <- NA

#8d. Check on the matched programmes
m <- !is.na(prog$arr_id)
summary(prog[m, c("growth", "inflation", "current_account_gdp")])
colSums(is.na(prog[m, c("growth", "inflation", "current_account_gdp")]))   #5, 8, 10

#No fiscal variables: the WEO only exists as the revised April 2026 vintage
#(= look-ahead), and in Mecon the government balance / debt are missing for
#24% / 45% of our programmes. See the "excluded" table in Step 12.

################################################################################
# Step 9: final sample, time split, imputation with TRAIN medians, export
################################################################################

#9a. Final sample = the programmes matched to MONA, in chronological order
final <- prog[!is.na(prog$arr_id), ]
final <- final[order(final$approval_date), ]
rownames(final) <- NULL
nrow(final)

#Column groups (used again in the next steps)
id_cols    <- c("Member.Code", "Member", "approval_date", "approval_year",
                "Facility", "facility_type", "arr_id")         #facility_type: info only
out_col    <- "interrupted"
#numeric inputs (can be missing -> imputed below)
num_cols   <- c("log_access_pct_quota", "planned_months", "n_reviews_sched",
                "n_struct_cond", "n_prior_actions", "prev_n_20y",
                "years_since_prev", "growth", "inflation", "current_account_gdp")
#0/1 inputs, never missing
bin_cols   <- c("concessional", "extended", "no_track_record")
inputs     <- c(num_cols, bin_cols)                               #the 13 inputs
#information from AFTER approval: kept for documentation only, never an input
audit_cols <- c("amount_agreed", "amount_drawn", "n_reviews_done", "mona_cancelled")

setdiff(c(id_cols, out_col, inputs, audit_cols), names(final))    #character(0)

#Missing values BEFORE imputation (this is the table for section 1d)
na_table <- colSums(is.na(final[, c(out_col, inputs)]))
na_table[na_table > 0]
mean(final$interrupted)                                           #share interrupted

#9b. Time-based split: train = approved 2002-2013, test = approved 2014-2016
#(no shuffling: the model is tested on programmes LATER than the ones it learned from)
final$split <- ifelse(final$approval_year <= 2013, "train", "test")
train <- final[final$split == "train", ]
test  <- final[final$split == "test", ]
c(train = nrow(train), test = nrow(test))
c(train = mean(train$interrupted), test = mean(test$interrupted))

#9c. Imputation: fill each missing value with the TRAIN median. Using the test
#rows to compute it would leak information from the test period.
#(no_track_record already tells which rows have no years_since_prev)
miss_cols <- num_cols[na_table[num_cols] > 0]
med <- sapply(train[, miss_cols], median, na.rm = TRUE)
round(med, 3)
for (v in miss_cols) {
  train[[v]][is.na(train[[v]])] <- med[v]
  test[[v]][is.na(test[[v]])]   <- med[v]
}
model_cols <- c(id_cols, out_col, inputs)
sum(is.na(train[, model_cols])); sum(is.na(test[, model_cols]))  #0 and 0

#9d. Export for the modelling scripts (the Excel file comes in the last step)
#Note for the neural network: inputs are NOT scaled here. Standardise them in
#the NN script with the mean and sd of the TRAIN set (same logic as above).
write.csv(train[, model_cols], paste0(dir_proc, "train.csv"), row.names = FALSE)
write.csv(test[,  model_cols], paste0(dir_proc, "test.csv"),  row.names = FALSE)
dim(read.csv(paste0(dir_proc, "train.csv")))                     #21 columns
dim(read.csv(paste0(dir_proc, "test.csv")))

################################################################################
# 1d. Step 10: summary statistics (all on `final`, i.e. BEFORE imputation)
################################################################################

#10a. Quick overview as in the lecture: summary() of every input
summary(final[, inputs])

#One table for the paper (binary variables: mean = share of 1s)
desc <- data.frame(
  variable  = inputs,
  n_missing = sapply(final[inputs], function(x) sum(is.na(x))),
  mean      = sapply(final[inputs], mean,   na.rm = TRUE),
  sd        = sapply(final[inputs], sd,     na.rm = TRUE),
  min       = sapply(final[inputs], min,    na.rm = TRUE),
  median    = sapply(final[inputs], median, na.rm = TRUE),
  max       = sapply(final[inputs], max,    na.rm = TRUE)
)
rownames(desc) <- NULL
desc[, -1] <- round(desc[, -1], 3)
desc

#10b. The key table for a classification task: does each input differ between
#completed (0) and interrupted (1) programmes? t.test() gives the p-value of the
#difference in means (only a rough guide: we predict, we do not test hypotheses)
by_outcome <- data.frame(
  variable    = inputs,
  completed   = sapply(inputs, function(v) mean(final[[v]][final$interrupted == 0], na.rm = TRUE)),
  interrupted = sapply(inputs, function(v) mean(final[[v]][final$interrupted == 1], na.rm = TRUE)),
  p_value     = sapply(inputs, function(v) t.test(final[[v]] ~ final$interrupted)$p.value)
)
by_outcome$difference <- by_outcome$interrupted - by_outcome$completed
rownames(by_outcome) <- NULL
by_outcome[, -1] <- round(by_outcome[, -1], 3)
by_outcome[order(by_outcome$p_value), ]          #sorted: clearest differences first

#10c. Class balance and the train/test comparison. If the INPUTS also differ
#between the two periods, the test set is not "more of the same" (covariate shift)
balance <- data.frame(
  sample      = c("all", "train (2002-2013)", "test (2014-2016)"),
  n           = c(nrow(final), nrow(train), nrow(test)),
  interrupted = c(sum(final$interrupted), sum(train$interrupted), sum(test$interrupted))
)
balance$share <- round(balance$interrupted / balance$n, 3)
balance

by_split <- data.frame(
  variable = inputs,
  train    = sapply(inputs, function(v) mean(final[[v]][final$split == "train"], na.rm = TRUE)),
  test     = sapply(inputs, function(v) mean(final[[v]][final$split == "test"],  na.rm = TRUE))
)
rownames(by_split) <- NULL
by_split[, -1] <- round(by_split[, -1], 3)
by_split

#10d. What is in the sample?
table(final$facility_type)
round(tapply(final$interrupted, final$facility_type, mean), 3)   #share interrupted by type
table(final$approval_year)
length(unique(final$Member.Code))                                #number of countries
table(table(final$Member.Code))                                  #countries with 1, 2, 3... programmes

#Save the tables for the paper
write.csv(desc,       paste0(dir_sum, "summary_statistics.csv"), row.names = FALSE)
write.csv(by_outcome, paste0(dir_sum, "summary_by_outcome.csv"), row.names = FALSE)
write.csv(by_split,   paste0(dir_sum, "summary_by_split.csv"),   row.names = FALSE)
write.csv(balance,    paste0(dir_sum, "class_balance.csv"),      row.names = FALSE)

################################################################################
# 1f. Step 11: exploratory data analysis
################################################################################
#Each figure goes straight into a png file: png() opens the file, dev.off()
#closes it. (Plotting in the small RStudio pane gave "figure margins too large".)

#11a. Share of interrupted programmes by facility type
rate_fac <- tapply(final$interrupted, final$facility_type, mean)
rate_fac
png(paste0(dir_fig, "eda1_facility.png"), width = 900, height = 600)
bp <- barplot(rate_fac, ylim = c(0, 1), col = "steelblue", ylab = "Share interrupted",
              main = "Interrupted programmes by facility type")
text(bp, rate_fac, labels = paste0("n=", table(final$facility_type)), pos = 3)
abline(h = mean(final$interrupted), lty = 2)    #dashed = overall share
dev.off()

#11b. The outcome over time (periods of 3 years, single years are too small)
period <- cut(final$approval_year, breaks = c(2001, 2004, 2007, 2010, 2013, 2016),
              labels = c("2002-04", "2005-07", "2008-10", "2011-13", "2014-16"))
table(period)
rate_period <- tapply(final$interrupted, period, mean)
round(rate_period, 2)
png(paste0(dir_fig, "eda2_period.png"), width = 900, height = 600)
barplot(rate_period, ylim = c(0, 1), col = "steelblue",
        ylab = "Share interrupted", main = "Interrupted programmes by approval period")
abline(h = mean(final$interrupted), lty = 2)
dev.off()

#11c. Key inputs: interrupted vs completed programmes
box_vars <- c("log_access_pct_quota", "planned_months", "n_reviews_sched",
              "n_struct_cond", "n_prior_actions", "growth")
png(paste0(dir_fig, "eda3_boxplots.png"), width = 1200, height = 800)
par(mfrow = c(2, 3))
for (v in box_vars) {
  boxplot(final[[v]] ~ final$interrupted, names = c("completed", "interrupted"),
          col = c("grey80", "steelblue"), main = v, xlab = "", ylab = "")
}
dev.off()

#11d. Skewed inputs (why access is used in logs)
png(paste0(dir_fig, "eda4_histograms.png"), width = 1000, height = 800)
par(mfrow = c(2, 2))
hist(final$access_pct_quota,     breaks = 30, col = "grey80",
     main = "Access, % of quota", xlab = "")
hist(final$log_access_pct_quota, breaks = 30, col = "steelblue",
     main = "Access, % of quota (log)", xlab = "")
hist(final$inflation,            breaks = 30, col = "grey80",
     main = "Inflation, %", xlab = "")
hist(final$years_since_prev,     breaks = 30, col = "grey80",
     main = "Years since previous programme", xlab = "")
dev.off()

#11e. Correlation of every input with the outcome, and among the inputs
cors <- sapply(inputs, function(v) cor(final[[v]], final$interrupted, use = "complete.obs"))
cors <- sort(cors)
round(cors, 2)
png(paste0(dir_fig, "eda5_correlations.png"), width = 900, height = 700)
par(mar = c(4, 11, 3, 1))                       #wide left margin for the names
barplot(cors, horiz = TRUE, las = 1, col = ifelse(cors > 0, "firebrick", "steelblue"),
        xlab = "Correlation with interrupted", main = "Inputs and the outcome")
abline(v = 0)
dev.off()

#Which inputs are strongly related to EACH OTHER? (|r| > 0.6)
#(warning "standard deviation is zero" is fine: no_track_record is always 0
#where years_since_prev exists)
cm   <- cor(final[, inputs], use = "pairwise.complete.obs")
high <- which(abs(cm) > 0.6 & upper.tri(cm), arr.ind = TRUE)
data.frame(var1 = rownames(cm)[high[, 1]], var2 = colnames(cm)[high[, 2]],
           r = round(cm[high], 2))

#11f. Is the relation linear? Share interrupted by quarter (quartile group) of
#each continuous input. A correlation only measures a straight-line relation;
#if the share goes up and down across the groups, the pattern is non-linear.
#unique() is needed because some inputs have many equal values (e.g. 6 reviews)
quart_vars <- c("log_access_pct_quota", "n_reviews_sched", "n_struct_cond",
                "years_since_prev", "growth", "inflation")
png(paste0(dir_fig, "eda6_quartiles.png"), width = 1200, height = 800)
par(mfrow = c(2, 3))
for (v in quart_vars) {
  groups <- cut(final[[v]], breaks = unique(quantile(final[[v]], na.rm = TRUE)),
                include.lowest = TRUE)
  rate <- tapply(final$interrupted, groups, mean)
  print(v); print(round(rate, 2))
  barplot(rate, ylim = c(0, 1), col = "steelblue", main = v, ylab = "Share interrupted")
  abline(h = mean(final$interrupted), lty = 2)
}
dev.off()

#What the EDA means for the modelling (argument for the paper):
#- every input on its own is only weakly related to the outcome (|r| <= 0.31),
#  so no single variable predicts interruptions; a model must combine many;
#- several relations are not straight lines (11f) and some inputs matter only
#  for some programme types (11a) -> this is what a random forest can capture
#  (non-linear effects and interactions) and a linear model cannot without help;
#- strongly related inputs (11e) are no problem for prediction with a forest,
#  but they make the coefficients of the simple model hard to interpret;
#- the interrupted share rises over time (11b): test accuracy will partly
#  reflect this shift, not only the quality of the model.

################################################################################
# 1g. Step 12: inputs, output and what is left out
################################################################################
#The guiding rule (Shmueli 2010, "To Explain or to Predict?"): in predictive
#modelling an input must be available at the time of prediction ("ex-ante
#availability"). Our prediction is made on the day the Board approves the
#programme, so every input is taken from approval-day information only.

#12a. Variable table: one row per variable, with the reason to expect it to
#help the prediction
var_info <- data.frame(rbind(
  c("interrupted", "output", "Outcome", "1 = fewer reviews completed than scheduled at approval", "MONA Reviews",
    "the event we want to predict; defined against the approval-day schedule, so later changes cannot move the goalposts"),
  c("log_access_pct_quota", "input", "Deal structure", "log of agreed access as % of the IMF quota", "COMMITMENTS, upon approval",
    "larger programmes signal larger problems and carry more at stake at each review"),
  c("planned_months", "input", "Deal structure", "planned length, approval to scheduled expiry, months", "COMMITMENTS, upon approval",
    "longer programmes are exposed longer to shocks and changes of government"),
  c("concessional", "input", "Deal structure", "1 = includes a concessional (PRGT) facility", "COMMITMENTS",
    "low-income borrowers on cheap terms: different lenders' incentives and country circumstances"),
  c("extended", "input", "Deal structure", "1 = EFF or ECF, 0 = SBA or SCF/ESF", "COMMITMENTS",
    "medium-term structural programmes vs short-term crisis programmes"),
  c("n_reviews_sched", "input", "Deal structure", "number of reviews scheduled at approval", "MONA Reviews, R0",
    "each review is a hurdle: more hurdles, more chances to stop (partly mechanical)"),
  c("n_struct_cond", "input", "Conditionality", "number of structural conditions at approval", "MONA Combined, R0",
    "more reform conditions are harder to meet on time"),
  c("n_prior_actions", "input", "Conditionality", "number of prior actions (before Board approval)", "MONA Combined, R0",
    "upfront demands: a sign of low trust, or of commitment already shown"),
  c("prev_n_20y", "input", "Track record", "number of programmes in the 20 years before approval", "COMMITMENTS history",
    "repeated users may have persistent problems in implementing programmes"),
  c("years_since_prev", "input", "Track record", "years from the previous programme's planned end to approval (0 if overlapping)", "COMMITMENTS history",
    "a quick return suggests the previous programme did not solve the problem"),
  c("no_track_record", "input", "Track record", "1 = no earlier programme", "COMMITMENTS history",
    "first-time borrowers have no history with the IMF; also marks the imputed years_since_prev"),
  c("growth", "input", "Macro", "real GDP growth, %, staff estimate for the approval year", "MONA Mecon, R0",
    "a weak economy makes fiscal and other targets harder to reach"),
  c("inflation", "input", "Macro", "CPI inflation, %, staff estimate for the approval year", "MONA Mecon, R0",
    "high inflation signals macro instability and harder monetary targets"),
  c("current_account_gdp", "input", "Macro", "current account balance, % of GDP", "MONA Mecon, R0",
    "the size of the external gap the programme has to close")
), stringsAsFactors = FALSE)
names(var_info) <- c("variable", "role", "group", "description", "source", "why_useful")

length(inputs)                                  #13
setdiff(c(inputs, out_col), var_info$variable)  #character(0): every variable is documented
setdiff(var_info$variable, c(inputs, out_col))  #character(0): nothing extra
var_info[, c("variable", "role", "group", "source")]

#12b. What is NOT used as an input, and why. Four reasons: known only after
#approval (look-ahead), the same information twice, too many missing values,
#or not usable for prediction (identifiers)
excluded <- data.frame(rbind(
  c("amount_drawn", "look-ahead", "what was actually paid out: known only afterwards"),
  c("n_reviews_done, mona_cancelled", "look-ahead", "known only afterwards: they describe the outcome"),
  c("Revised / Actual columns, snapshots R1...Rn", "look-ahead", "contain what happened after approval"),
  c("actual end date, extensions, review delays", "look-ahead", "known only during or after the programme"),
  c("previous programme drew < 90%", "look-ahead", "uses the 2026 drawn amount: partly unknown on approval day"),
  c("government debt and balance (WEO)", "look-ahead", "only the revised 2026 vintage exists, not what was known at approval"),
  c("government debt and balance (Mecon)", "missing values", "missing for 45% / 24% of the programmes"),
  c("facility_type", "duplicate", "same information as concessional + extended (kept as information only)"),
  c("n_tranches", "duplicate", "number of instalments = about reviews + 1 (correlation about 0.9 with n_reviews_sched)"),
  c("frontload_share", "duplicate / fragile", "needs text matching on a free-text column; mostly set by the number of instalments"),
  c("previous programme still running", "duplicate", "rare, and almost the same as years_since_prev = 0"),
  c("country, approval_date, approval_year", "identifier", "not inputs: the model should generalise to new programmes")
), stringsAsFactors = FALSE)
names(excluded) <- c("variable", "reason_type", "reason")
excluded
table(excluded$reason_type)

#12c. Save: CSV plus the Excel file for the submission (close it in Excel first!).
#writexl writes .xlsx (readxl can only read).
write.csv(var_info, paste0(dir_sum, "variable_description.csv"), row.names = FALSE)
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
all_data <- rbind(cbind(split = "train", train[, model_cols]),
                  cbind(split = "test",  test[,  model_cols]))
outcome_details <- final[, c("Member.Code", "approval_date", "n_reviews_sched", audit_cols)]
writexl::write_xlsx(list(programmes = all_data, variables = var_info,
                         excluded_variables = excluded, outcome_details = outcome_details),
                    paste0(dir_proc, "imf_programmes.xlsx"))
excel_sheets(paste0(dir_proc, "imf_programmes.xlsx"))
dim(read_excel(paste0(dir_proc, "imf_programmes.xlsx"), sheet = "programmes"))
list.files(dir_proc)

################################################################################
# Done. Model-ready data: Data/processed/train.csv and test.csv
# Output: interrupted. Inputs: the 13 variables in `inputs`
# (facility_type is in the files for information only, it is NOT an input)
################################################################################

