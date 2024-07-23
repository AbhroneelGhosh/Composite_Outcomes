# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 11_FINAL CODE FOR SURVIVAL ANALYSIS PAPER        #######
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Importing libraries
# For data manipulations
library(tidyverse)
library(Hmisc)
library(reshape2)
library(mice)
library(caret)

# For stats/logit
library(MASS)
library(tidymodels)

# For survival analysis
library(survival)
library(cmprsk)
library(riskRegression)
library(survminer)

# For data viz
library(ggplot2)
library(ggfortify)
library(gridExtra)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA               ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("Cleaned Data/Cleaned_Data (2024-04-26).RData")

# Analysis date
analysis_date <- "2024-05-23"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ELIGIBILITY CRITERIA      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# NOTE: Some of the exclusion criteria has missing values


# For mom outcomes, use dat_full because we want to keep all "mums" even if they do not give 
#  birth to a living child or are discharged with them
#  We also want some of the neonatal columns as potential predictors

# For baby outcomes, we can only keep the babies that are alive
#  and subsequently only the mothers who gave birth to at least one living 
#  child and discharged together
# We also want the maternal columns as potential predictors
dat_mom <- dat_full_raw %>% 
  filter(studyid_adm %in% dat_mom_raw$studyid_adm)
dat_baby <- dat_full_raw %>% 
  filter(studyid_adm %in% dat_baby_raw$studyid_adm)

# All data: 9497 moms, 7436 babies
nrow(dat_mom_raw)
nrow(dat_baby_raw)
n_distinct(dat_mom$studyid_adm)

## ~~~~~~~~~~~~~~~
## Mum Exclusion  ####
## ~~~~~~~~~~~~~~~

# Admitted to hospital for delivery: 9373 moms, 7430 babies
admit_exclude <- subset(dat_mom, reason_adm_v2 == "No")
dat_mom <- subset(dat_mom, reason_adm_v2 == "Yes" | is.na(reason_adm_v2))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
n_distinct(admit_exclude$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)

# Eligible age: 9366 moms, 7425 babies
age_exclude <- subset(dat_mom, eligibleage_adm_v2 == "No")
dat_mom <- subset(dat_mom, eligibleage_adm_v2 == "Yes" | is.na(eligibleage_adm_v2))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
n_distinct(age_exclude$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)

# Check other exclusion: 7199 moms, 7410 babies
other_exclusion <- table(dat_mom %>% 
                           group_by(studyid_adm) %>% 
                           slice(1) %>% 
                           pull(exclusion_adm_v2))
dat_mom <- subset(dat_mom, exclusion_adm_v2 == "No exclusion criteria apply" | is.na(exclusion_adm_v2))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
other_exclusion
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)

# Consent form filled: 7197 moms, 7408 babies
#  Note: Combine refused consent and did not fill consent form for the flow diagram
consent_exclude <- subset(dat_mom, consentform_adm_v2 == "No")
dat_mom <- subset(dat_mom, consentform_adm_v2 == "Yes" | is.na(consentform_adm_v2))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
n_distinct(consent_exclude$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)

# Remove miscarriages (stillbirth before 20 weeks): 7154 moms, 7363 babies
miscarriage_exclude <- subset(dat_mom, sb20wk_del == "No")
dat_mom <- subset(dat_mom, !(studyid_adm %in% miscarriage_exclude$studyid_adm))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
n_distinct(miscarriage_exclude$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)

# Check if any moms still have unknown exclusion criteria: 7132 moms, 7363 babies
unknown_exclude <- dat_mom %>% 
  filter(is.na(reason_adm_v2),
         is.na(eligibleage_adm_v2),
         is.na(exclusion_adm_v2),
         is.na(consentform_adm_v2))
dat_mom <- subset(dat_mom, !(studyid_adm %in% unknown_exclude$studyid_adm))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
n_distinct(unknown_exclude$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)

# Mother died giving birth: 7130 moms, 7361 babies
# Note: missing discharge status irrelevant since they are alive at follow-up
died_exclude <- subset(dat_mom, dischstat_mat == "Died")
dat_mom <- subset(dat_mom, dischstat_mat != "Died" | is.na(dischstat_mat))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
n_distinct(died_exclude$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)


# Complete follow-up for mothers: N = 7061 moms, 7286 babies
ltfu_exclude <- subset(dat_mom, six_week_follow_up_maternal_complete == "Incomplete" | 
                         is.na(six_week_follow_up_maternal_complete))
unknown_outcome <- subset(dat_mom, six_week_follow_up_maternal_complete == "Complete" & 
                            (is.na(momalive_swf) | 
                               (momalive_swf == "Yes" & is.na(momadmit_swf))))
dat_mom <- dat_mom %>% 
  filter(!(studyid_adm %in% c(ltfu_exclude$studyid_adm, unknown_outcome$studyid_adm)))
n_distinct(ltfu_exclude$studyid_adm)
n_distinct(unknown_outcome$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_mom)

# Total survived, died, and re-admitted
mom_survived <- subset(dat_mom, momalive_swf == "Yes" & momadmit_swf == "No")
mom_died <- subset(dat_mom, momalive_swf == "No")
mom_admitted <- subset(dat_mom, momadmit_swf == "Yes")

n_distinct(mom_survived$studyid_adm)
n_distinct(mom_died$studyid_adm)
n_distinct(mom_admitted$studyid_adm)


## ~~~~~~~~~~~~~~~
## Baby Exclusion  ####
## ~~~~~~~~~~~~~~~

# Only apply these criteria for the babies

# Total births: 7361 births   #7439
nrow(dat_baby)

# Remove stillbirths: 7082 babies   #7152
stillbirths_exclude <- subset(dat_baby, sb20wk_del_new == "Yes")
dat_baby <- subset(dat_baby, sb20wk_del_new == "No")
nrow(stillbirths_exclude) #287
nrow(dat_baby)

# Exclude if baby died at birth or during admission: 6955 #7015
died_baby_exclude <- subset(dat_baby, babedeath_dis == "Baby Died")
dat_baby <- subset(dat_baby, babedeath_dis == "Baby Survived")
nrow(died_baby_exclude) #137
nrow(dat_baby)

# Discharged alive: 6135 babies  #6176
admitted_exclude <- table(dat_baby$dispbb_neo)
dat_baby <- subset(dat_baby, dispbb_neo == "Discharged" | is.na(dispbb_neo))
admitted_exclude
nrow(dat_baby)

# Check if still unknown reason: 6112 babies  #6151
unknown_baby_exclude <- subset(dat_baby, (is.na(dispbb_neo)))
dat_baby <- subset(dat_baby, !is.na(dispbb_neo))
nrow(unknown_baby_exclude)
nrow(dat_baby)

# Follow-up: N = 6053   #6092
ltfu_baby_exclude <- subset(dat_baby, six_week_follow_up_neonatal_complete == "Incomplete" | 
                              is.na(six_week_follow_up_neonatal_complete))
unknown_baby_outcome <- subset(dat_baby, six_week_follow_up_neonatal_complete == "Complete" & 
                                 (is.na(babealive_swf) | 
                                    (babealive_swf == "Yes" & is.na(babeadmit_swf))))
nrow(ltfu_baby_exclude) #57
nrow(unknown_baby_outcome) #2
dat_baby <- dat_baby %>% 
  filter(six_week_follow_up_neonatal_complete == "Complete",
         babealive_swf == "No" | 
           (babealive_swf == "Yes" & !is.na(babeadmit_swf)))
nrow(dat_baby)


# Total survived, died, and re-admitted
baby_survived <- subset(dat_baby, babealive_swf == "Yes" & babeadmit_swf == "No")
baby_died <- subset(dat_baby, babedeath_swf == "Yes")
baby_admitted <- subset(dat_baby, babeadmit_swf == "Yes")

nrow(baby_survived) #5814
nrow(baby_died) #36
nrow(baby_admitted) #242

## ~~~~~~~~~~~~~~~~~~~
## Final Datasets ####
## ~~~~~~~~~~~~~~~~~~~

# Combined dataset for predicting mother's outcomes
n_distinct(dat_mom$studyid_adm); nrow(dat_mom) 

# Only babies for predicting baby outcomes
nrow(dat_baby)

## ~~~~~~~~~~~~~~~~~~~
## Check twins and triplets ####
## ~~~~~~~~~~~~~~~~~~~

# Check for twins and triplets
num_babies <- dat_baby %>% 
  count(studyid_adm, name = "num_babies") %>% 
  count(num_babies, name = "count")

cat(num_babies$count[1], "Individuals\n"); cat(num_babies$count[2], "Twins\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# POTENTIAL PREDICTORS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Admission Subject Details ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

admission_subject_vars <- c(
  # "fetaldemise_adm",
  "admdeldiff_new",
  "admdisdiff_new",
  "admlabdiff_new",
  "labdisdiff_new",
  "agemomyears_calc",
  "agemomyears_cat_new"
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Admission Variables       ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

admission_vars <- c(
  "time_tohosp_adm_new",
  "transport_adm",
  grep("^delay", colnames(dat_mom), value = TRUE),
  "isreferral_adm",
  "csection_adm",
  "labdeldiff_new",
  "takevitals_adm",
  "sys_bp_adm",
  "dia_bp_adm",
  "temp_adm",
  "hr_adm",
  "distress_adm"
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pregnancy History         ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pregnancy_history_vars <- c(
  grep("^medhx", colnames(dat_mom), value = TRUE),
  "duedate_adm",
  "gestation_period_cat_new",
  "gravid_adm",
  "parity_adm",
  "pregnancy_loss_new",
  "csect_adm_new",
  grep("^preghx", colnames(dat_mom), value = TRUE),
  grep("^placenta_adm", colnames(dat_mom), value = TRUE),
  "medhbp_adm",
  "hiv_status_new",
  "prevadm_adm_new",
  # "uti_abx_adm",
  "numberanc_adm_new",
  grep("^ancprovider_adm", colnames(dat_mom), value = TRUE)
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## SES and Demographics      ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ses_vars <- c(
  "housenum_ses",
  "numchild_ses",
  "marry_ses",
  "livfather_ses",
  "schoolyrs_ses_new",
  "nutri_adm",
  # grep("^sesindex_flooring", colnames(dat_mom), value = TRUE),
  # grep("^sesindex_toilet", colnames(dat_mom), value = TRUE),
  # grep("^sesindex_cooking", colnames(dat_mom), value = TRUE),
  # "sesindex_safewater",
  # "sesindex_safewaterdistance",
  # grep("^sesindex_assets_", colnames(dat_mom), value = TRUE),
  # grep("^sesindex_assets2_", colnames(dat_mom), value = TRUE),
  # "sesindex_room",
  "child_death_ses",
  "sesindex_sum",
  "sesindex_cat"
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Delivery Maternal         ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

delivery_maternal_vars <- c(
  "numbabe_del",
  "prom_del_new",
  "deldisdiff_new",
  "delmode_del_new",
  "episiotomy_del",
  "degreetear_del_new",
  "induce_del",
  grep("^inducetype_del", colnames(dat_mom), value = TRUE),
  "pph_del",
  "transfx_del_new",
  "obstruct_del",
  "meconium_del",
  "vagexam_del_new",
  "placenta_del",
  "man_placenta_del",
  "csecturgency_del_new"
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Delivery Neonatal         ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

delivery_neonatal_vars <- c(
  # "infantstatus_del",
  # "sb20wk_del_new",
  # "sbcongenital_del_new",
  "sexbb_del_new",
  "apgar1_del",
  "apgar5_del",
  "cord_delay_del",
  "weightbb_del",
  "weightbb_del_cat_new",
  "height_neo",
  "rescus_del",
  # grep("^resustype_del", colnames(dat_mom), value = TRUE),
  "rescusoxy_del_new"
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Discharge Maternal        ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

discharge_maternal_vars <- c(
  "adm_mat",
  "sbp_mat",
  "dbp_mat",
  "rr_mat",
  "temp_mat",
  "best_spo2_mat",
  "best_hr_mat",
  "hem_mat",
  "destination_mat",
  grep("^support_mat", colnames(dat_mom), value = TRUE),
  "bf_mat",
  # grep("^symp_mat", colnames(dat_mom), value = TRUE),
  grep("^abx_mat", colnames(dat_mom), value = TRUE)
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Discharge Neonatal        ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

discharge_neonatal_vars <- c(
  # "dispbb_neo",
  # "dispbb_neo_new",
  # "admitdisposition_neo",
  grep("^admitdiagnosis_", colnames(dat_mom), value = TRUE),
  "poop_neo",
  "pee_neo",
  "bf_neo",
  "jaundice_neo_new",
  "eyedischarge_neo",
  "mean_temp_neo_new",
  "rr_neo",
  "mean_spo2_neo_new",
  "mean_hr_neo_new",
  "abx_neo"
)



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Predictors                ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

predictors <- c(admission_subject_vars,
                admission_vars,
                pregnancy_history_vars,
                ses_vars,
                delivery_maternal_vars,
                delivery_neonatal_vars,
                discharge_maternal_vars,
                discharge_neonatal_vars
)

exclude_predictors <- c("infantstatus_del", 
                        "sb20wk_del", 
                        "sbcongenital_del",
                        "dispbb_neo_new",
                        "admitdisposition_neo",
                        grep("^admitdiagnosis_", colnames(dat_mom), value = TRUE))

predictors <- predictors[!(predictors %in% exclude_predictors)]

# Labels
predictors_labels <- unlist(sapply(dat_mom[, predictors], 
                                   label))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA PREPROCESSING              ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Selecting only predictors ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat_process <- dat_baby %>% 
  dplyr::select(studyid_adm,
                all_of(predictors), 
                babeadmit_swf,
                babedeath_swf,
                babeoutcome_swf)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Removing vars with low incidence ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

low_incidence_cols <- c("delay_adm___4",
                        "placenta_adm___1",
                        "cord_delay_del",
                        "abx_mat___99",
                        "support_mat___99",
                        "bf_neo_new",
                        "eyedischarge_neo_new",
                        "abx_neo")

for (i in c(3:9, 98)) {
  low_incidence_cols <- c(low_incidence_cols, paste("medhx_adm___", i, sep = ""))
}

for (i in c(1:6, 8, 13:18)) {
  low_incidence_cols <- c(low_incidence_cols, paste("preghx_adm___", i, sep = ""))
}

for (i in c(1:2, 97)) {
  low_incidence_cols <- c(low_incidence_cols, paste("placenta_adm___", i, sep = ""))
}

for (i in c(1,3,98)) {
  low_incidence_cols <- c(low_incidence_cols, paste("delay_adm___", i, sep = ""))
}

for (i in c(1, 3:5, 7)) {
  low_incidence_cols <- c(low_incidence_cols, paste("ancprovider_adm___", i, sep = ""))
}

dat_process <- dat_process[, !(colnames(dat_process) %in% low_incidence_cols)]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA IMPUTATION     #############
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~
## Remove variables ####
## ~~~~~~~~~~~~~~~~~~~~~

# Identify columns which do not contribute as they have only one unique value other than NA
# Iterate through predictors to find the list of variables to exclude 

exclude = c()
for (i in predictors) {
  if (length(unique(dat_baby[[i]])) == 1) {
    exclude = c(exclude, i)
  }
  if (sum(is.na(unique(dat_baby[[i]]))) > 0 && 
      length(unique(dat_baby[[i]])) == 2) {
    exclude = c(exclude, i)
  }
}

exclude
# Removed 2 columns, "sb20wk_del_new" and "sbcongenital_del_new"
predictors <- predictors[!(predictors %in% exclude)]
length(exclude)

# Identify variables with high amounts (>25%) of missing data for removal
# Use the combined derivation and validation sets for this

# Remove variables
remove_var <- 
  dat_baby %>% 
  dplyr::select(all_of(predictors)) %>% 
  #ungroup() %>% 
  summarise_all(~sum(is.na(.))) %>% 
  reshape2::melt() %>% 
  mutate(PCMissing = value / nrow(dat_baby))

sum(remove_var$PCMissing > 0.25)
# No variables with more than 25% missing

## ~~~~~~~~~~~~~~~~~~~~~
## Imputation of data ####
## ~~~~~~~~~~~~~~~~~~~~~

max(remove_var$PCMissing)*100

dat_impute <- mice(dat_process, m = 5, method = 'pmm')

max_missing <- remove_var %>% 
  arrange(desc(PCMissing)) %>%
  slice_head(n = 4)

max_missing$variable

stripplot(dat_impute, admlabdiff_new+labdeldiff_new+labdisdiff_new+csection_adm ~ .imp)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PROCESSING FOR SURVIVAL ANALYSIS    #############
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Converting to dataframe ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat_impute_df <- complete(dat_impute, action = "long", include = TRUE)
# dat_impute_ref <- dat_impute_df

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## One-Hot Encoding, Adding the days_to_outcome vars ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# One hot encoding

# Removing the outcome variables and studyid_adm column
dat_surv <- dat_impute_df[, !(colnames(dat_impute_df) %in% c("studyid_adm", "babedeath_swf", "babeadmit_swf", "babeoutcome_swf"))]

# Encoding
dummy <- dummyVars("~.", data = dat_surv)
dat_surv <- data.frame(predict(dummy, newdata=dat_surv))
dat_surv[1:10, 1:15]

# Add the outcome columns back

dat_surv["babedeath_swf"] = dat_process$babedeath_swf
dat_surv["babeadmit_swf"] = dat_process$babeadmit_swf
dat_surv["babeoutcome_swf"] = dat_process$babeoutcome_swf

# Including the Time to Death/Time to admit/Time to outcome columns from dat_baby

dat_surv["babedeath_swf_days"] = rep(dat_baby$babedeath_swf_days, 6)
dat_surv["babeadmit_swf_days"] = rep(dat_baby$babeadmit_swf_days, 6)
dat_surv["babeoutcome_swf_days"] = rep(dat_baby$babeoutcome_swf_days, 6)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Discrepancy between babeoutcome, babeoutcome_days ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 7 extra rows where babeoutcome_swf_days is NULL, though babeoutcome_swf is Yes
sum(is.na(dat_surv$babeoutcome_swf_days)) #5821
sum(dat_surv$babeoutcome_swf == 'No') #5814

# Checking for discrepancy in admit

6092 - sum(is.na(dat_baby$babeadmitage_swf))   #242
6092 - sum(is.na(dat_baby$babeadmit_swf_days)) #242
# 242 readmits agrees with both babeadmitage_swf and babeadmit_swf_days also

# Checking for discrepancy in death

sum(dat_baby$babedeath_swf == 'Yes') #36
6092-sum(is.na(dat_baby$babedeath_swf_days)) #29
# Discrepancy of 7 extra NA values where babedeath_swf = Yes

dat_baby  %>%
  filter(babedeath_swf == 'Yes' & is.na(babedeath_swf_days)) %>% 
  dplyr::select(studyid_adm, dispdate_neo, babedeathdate_swf)
# Out of 7 cases, 6 of them have dispdate_neo = NA, 
#      and one of them babedeathdeath_swf = NA

# Remove the 7 rows for survival analysis.
# NOTE this removes 7/36 incidences of infant death and so might be better to handle missing dates better
studyid_remove <- dat_baby  %>%
  filter(babedeath_swf == 'Yes' & is.na(babedeath_swf_days)) %>% 
  dplyr::select(studyid_adm)

studyid_remove <- deframe(studyid_remove)

dat_surv <- dat_surv[!(dat_process$studyid_adm %in% studyid_remove) , ]
dim(dat_surv)

sum(is.na(dat_surv$babeoutcome_swf_days)) #5814
sum(dat_surv$babeoutcome_swf == 'No') #5814
# Now both the values match

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Creating the final status and time columns ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat_surv <- dat_surv %>% mutate(
  
  # Creating status which is 0 if no outcome, 1 if death and 2 if readmit 
  status = case_when(babeoutcome_swf == 'No' ~ 0,
                     babeoutcome_swf == 'Yes' & babedeath_swf == 'Yes' ~ 1,
                     babeoutcome_swf == 'Yes' & babeadmit_swf == 'Yes' ~ 2),
  
  # time, days to outcome in case of outcome else 42 (6 weeks)
  time = case_when( babeoutcome_swf == 'Yes' ~ as.integer(babeoutcome_swf_days),
                    TRUE ~ 42)
)

table(dat_surv$status)/6
table(dat_surv$time)/6

# Removing outcomes other than time, status

dat_surv <- dat_surv[, !(colnames(dat_surv) %in% c("babedeath_swf", "babedeath_swf_days", 
                                                   "babeadmit_swf", "babeadmit_swf_days", 
                                                   "babeoutcome_swf", "babeoutcome_swf_days"))]

dim(dat_surv)
colnames(dat_surv)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Converting back to a mids object ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat_surv <- as.mids(dat_surv)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMPETING RISK ANALYSIS            ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting the estimated CIF ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Getting dataframe for plot

dat_cif <- complete(dat_surv, action = 0)

cif <- cuminc(ftime = dat_cif$time, fstatus = dat_cif$status)
plot(cif, col = c(2,4),
     lty = c(1,1),
     curvlab = c("Death", "Readmit"),
     xlab = "Time (days from birth)",
     ylab = "Marginal Incidence Probability",
     ylim = c(0, 0.15),
     main = "CIF for Death/Readmission")

# Note both probabilities are very small, start out same but readmit increases more

#ggsave(paste0('Death-Readmission CIF Plot', plt1, analysis_date, ".png"), path = 'Results', device = 'png', width=1280/72, height = 720/72, dpi = 144)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fine Gray models ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Running a loop to store the univariable coefficients/p-vals/CIs

# Initializing vector variables

coeffs_death_surv <- c()
death_pvals_surv <- c()
death_CIstart_coeff <- c()
death_CIend_coeff <- c()

coeffs_admit_surv <- c()
admit_pvals_surv <- c()
admit_CIstart_coeff <- c()
admit_CIend_coeff <- c()

# Identifying categorical columns with >1 category

cols_cat_start <- c(6,9,12,43,50,76,81,90,97,106,110,121,133,136,143,156,168,184)
cols_cat_end <- c(8,11,17,46,53,80,83,94,100,108,115,123,135,140,145,158,173,186)

cols_cat <- c()
for (i in 1:length(cols_cat_start)) {
  cols_cat <- c(cols_cat, cols_cat_start[i]:cols_cat_end[i])
}
cols_notcat <- setdiff((1:(length(colnames(dat_cif))-2)), cols_cat)

# Finding the column names

col_names <- colnames(complete(dat_surv, action = 0))
length(col_names)

# Iterating over categorical predictors and storing values

for (i in 1:length(cols_cat_start)) {
  
  # Univariable Fine-Gray Model for death
  model_death <- with(dat_surv, crr(time, status, 
                                    cov1 = sapply(mget(col_names[(cols_cat_start[i]+1):cols_cat_end[i]]), cbind), 
                                    failcode = 1))
  model_death_pooled <- pool(model_death)
  model_temp <- tidy(model_death_pooled, conf.int = TRUE)
  # Storing the required values for death outcome
  coeffs_death_surv <- c(coeffs_death_surv, model_temp$estimate)
  death_pvals_surv <- c(death_pvals_surv, model_temp$p.value)
  death_CIstart_coeff <- c(death_CIstart_coeff, model_temp$conf.low)
  death_CIend_coeff <- c(death_CIend_coeff, model_temp$conf.high)
  
  # Model for readmit 
  model_admit <- with(dat_surv, crr(time, status, 
                                    cov1 = sapply(mget(col_names[(cols_cat_start[i]+1):cols_cat_end[i]]), cbind), 
                      failcode = 2))
  model_admit_pooled <- pool(model_admit)
  model_temp <- tidy(model_admit_pooled, conf.int = TRUE)
  
  # Storing values
  coeffs_admit_surv <- c(coeffs_admit_surv, model_temp$estimate)
  admit_pvals_surv <- c(admit_pvals_surv, model_temp$p.value)
  admit_CIstart_coeff <- c(admit_CIstart_coeff, model_temp$conf.low)
  admit_CIend_coeff <- c(admit_CIend_coeff, model_temp$conf.high)
}

# Naming with categorical predictor names

# predictor_names <- colnames(dat_surv)[!(colnames(dat_surv) %in% c("status", "time"))]
# length(predictor_names)
# 
names_cat <- c()
for (i in 1:length(cols_cat_start)) {
 names_cat <- c(names_cat, (cols_cat_start[i]+1):cols_cat_end[i])
}

names(coeffs_death_surv) <- col_names[names_cat]
names(coeffs_admit_surv) <- col_names[names_cat]
names(death_pvals_surv) <- col_names[names_cat]
names(admit_pvals_surv) <- col_names[names_cat]

names(death_CIstart_coeff) <- col_names[names_cat]
names(death_CIend_coeff) <- col_names[names_cat]
names(admit_CIstart_coeff) <- col_names[names_cat]
names(admit_CIend_coeff) <- col_names[names_cat]

# Iterating over non-categorical predictors and storing values

for (i in cols_notcat) {
  
  # Univariable Fine-Gray Model for death
  model_death <- with(dat_surv, crr(time, status, 
                                    cov1 = sapply(mget(col_names[i]), cbind), 
                                    failcode = 1))
  model_death_pooled <- pool(model_death)
  model_temp <- tidy(model_death_pooled, conf.int = TRUE)
  # Storing the required values for death outcome
  coeffs_death_surv <- c(coeffs_death_surv, model_temp$estimate)
  death_pvals_surv <- c(death_pvals_surv, model_temp$p.value)
  death_CIstart_coeff <- c(death_CIstart_coeff, model_temp$conf.low)
  death_CIend_coeff <- c(death_CIend_coeff, model_temp$conf.high)
  
  # Model for readmit 
  model_admit <- with(dat_surv, crr(time, status, 
                                    cov1 = sapply(mget(col_names[i]), cbind), 
                                    failcode = 2))
  model_admit_pooled <- pool(model_admit)
  model_temp <- tidy(model_admit_pooled, conf.int = TRUE)
  # Storing values
  coeffs_admit_surv <- c(coeffs_admit_surv, model_temp$estimate)
  admit_pvals_surv <- c(admit_pvals_surv, model_temp$p.value)
  admit_CIstart_coeff <- c(admit_CIstart_coeff, model_temp$conf.low)
  admit_CIend_coeff <- c(admit_CIend_coeff, model_temp$conf.high)
}

# Naming with non-categorical predictor names

names(coeffs_death_surv) <- c(col_names[names_cat], col_names[cols_notcat])
names(coeffs_admit_surv) <- c(col_names[names_cat], col_names[cols_notcat])
names(death_pvals_surv) <- c(col_names[names_cat], col_names[cols_notcat])
names(admit_pvals_surv) <- c(col_names[names_cat], col_names[cols_notcat])

names(death_CIstart_coeff) <- c(col_names[names_cat], col_names[cols_notcat])
names(death_CIend_coeff) <- c(col_names[names_cat], col_names[cols_notcat])
names(admit_CIstart_coeff) <- c(col_names[names_cat], col_names[cols_notcat])
names(admit_CIend_coeff) <- c(col_names[names_cat], col_names[cols_notcat])

# Checking that lengths match
length(coeffs_death_surv)
length(coeffs_admit_surv)
length(death_pvals_surv)
length(admit_pvals_surv)

# Exponentiating to get sub-hazard ratios and their CIs

ratios_death <- exp(coeffs_death_surv)
ratios_death_start <- exp(death_CIstart_coeff)
ratios_death_end <- exp(death_CIend_coeff)

ratios_admit <- exp(coeffs_admit_surv)
ratios_admit_start <- exp(admit_CIstart_coeff)
ratios_admit_end <- exp(admit_CIend_coeff)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FITTING COX REGRESSION FOR A COMBINED OUTCOME          ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Recreating the babeoutcome column ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat_cox <- complete(dat_surv, action = "long", include = TRUE)

dat_cox <- dat_cox %>%
  mutate(babeoutcome_swf = case_when(status != 0 ~ 1,
                                     status == 0 ~ 0))
table(dat_cox$babeoutcome_swf)/6

dat_cox$status <- NULL

dat_cox <- as.mids(dat_cox)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## KM Survival Curve ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat_km <- complete(dat_cox, action = 0)

surv_Object <- Surv(dat_km$time, dat_km$babeoutcome_swf)
comb_fit <- survfit(formula = surv_Object ~ sexbb_del_new.Male, data = dat_km)
summary(comb_fit)

plot1 <- ggsurvplot(comb_fit, data = dat_km, conf.int = TRUE, pval = TRUE, fun = "event",
                    legend.labs = c("Female", "Male"), legend.title = "Sex of the baby",
                    legend = c(0.8, 0.8),
                    ylim = c(0, 0.1), xlab = "Time (days from birth)", title = "KM Mortality Curve - Combined Outcome")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Cox regression and odds ratios ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initializing vector variables

coeffs_comb_surv <- c()
comb_pvals_surv <- c()
comb_CIstart_coeff <- c()
comb_CIend_coeff <- c()

# Finding column names

col_names <- colnames(complete(dat_cox, action = 0))
length(col_names)

# Iterating over every categorical (>2 cats) predictor variable and storing the values

for (i in 1:length(cols_cat_start)) {
  
  # Univariable Fine-Gray Model for death
  model_comb <- with(dat_cox, 
                     coxph(Surv(time, babeoutcome_swf) ~ ., 
                      data = data.frame(sapply(mget(col_names[c((cols_cat_start[i]+1):cols_cat_end[i], length(col_names)-1, length(col_names))]), cbind))))
  model_comb_pooled <- pool(model_comb)
  model_temp <- tidy(model_comb_pooled, conf.int = TRUE)
  # Storing the required values for death outcome
  coeffs_comb_surv <- c(coeffs_comb_surv, model_temp$estimate)
  comb_pvals_surv <- c(comb_pvals_surv, model_temp$p.value)
  comb_CIstart_coeff <- c(comb_CIstart_coeff, model_temp$conf.low)
  comb_CIend_coeff <- c(comb_CIend_coeff, model_temp$conf.high)
}

# Iterating over every non-categorical predictor variable and storing the values

for (i in cols_notcat) {
  
  # Univariable Fine-Gray Model for death
  model_comb <- with(dat_cox, 
                     coxph(Surv(time, babeoutcome_swf) ~ ., 
                           data = data.frame(sapply(mget(col_names[c(i, length(col_names)-1, length(col_names))]), cbind))))
  model_comb_pooled <- pool(model_comb)
  model_temp <- tidy(model_comb_pooled, conf.int = TRUE)
  # Storing the required values for death outcome
  coeffs_comb_surv <- c(coeffs_comb_surv, model_temp$estimate)
  comb_pvals_surv <- c(comb_pvals_surv, model_temp$p.value)
  comb_CIstart_coeff <- c(comb_CIstart_coeff, model_temp$conf.low)
  comb_CIend_coeff <- c(comb_CIend_coeff, model_temp$conf.high)
}

# Checking that lengths match
length(coeffs_comb_surv)
length(comb_pvals_surv)
length(comb_CIstart_coeff)
length(comb_CIend_coeff)

# Naming according to the predictor names (without removed reference columns)

names(coeffs_comb_surv) <- c(col_names[names_cat], col_names[cols_notcat])
names(comb_pvals_surv) <- c(col_names[names_cat], col_names[cols_notcat])
names(comb_CIstart_coeff) <- c(col_names[names_cat], col_names[cols_notcat])
names(comb_CIend_coeff) <- c(col_names[names_cat], col_names[cols_notcat])

# Exponentiating to get sub-hazard ratios and their CIs

ratios_comb <- exp(coeffs_comb_surv)
ratios_comb_start <- exp(comb_CIstart_coeff)
ratios_comb_end <- exp(comb_CIend_coeff)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA VIZ WITH COMB AND NEW CATEGORIES        ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Classifying variables based on effect ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Identifying predictors
predictor_names <- c(col_names[names_cat], col_names[cols_notcat])
length(predictor_names)

# Extracting lists based on the sign of coefficients
pred_same_sign_pos <- (coeffs_death_surv>0) & (coeffs_admit_surv>0)
pred_same_sign_neg <- (coeffs_death_surv<0) & (coeffs_admit_surv<0)
pred_diff_sign_posneg <- (coeffs_death_surv>0) & (coeffs_admit_surv<0)
pred_diff_sign_negpos <- (coeffs_death_surv<0) & (coeffs_admit_surv>0)

# Extracting columns where death is positive/negative but admit negligible
pred__1 <- ((ratios_death/ratios_admit > 2) | (ratios_death/ratios_admit < 0.5)) & (ratios_admit<1.25 & ratios_admit>(1/1.25))
sum(pred__1)
predictor_names[pred__1 & pred_same_sign_neg]
predictor_names[pred__1 & pred_same_sign_pos]

pred_diff_sign_posother <- ((pred__1 & pred_same_sign_pos) | pred_diff_sign_posneg)
pred_diff_sign_negother <- ((pred__1 & pred_same_sign_neg) | pred_diff_sign_negpos)
pred_same_sign_pos <- (pred_same_sign_pos & (!pred__1))
pred_same_sign_neg <- (pred_same_sign_neg & (!pred__1))

# Extracting columns where admit is positive/negative but death negligible
pred__1 <- ((ratios_admit/ratios_death > 2) | (ratios_admit/ratios_death < 0.5)) & (ratios_death<1.25 & ratios_death>(1/1.25))
sum(pred__1)
predictor_names[pred__1 & pred_same_sign_neg]
predictor_names[pred__1 & pred_same_sign_pos]

pred_diff_sign_posother <- ((pred__1 & pred_same_sign_neg) | pred_diff_sign_posother)
pred_diff_sign_negother <- ((pred__1 & pred_same_sign_pos) | pred_diff_sign_negother)
pred_same_sign_pos <- (pred_same_sign_pos & (!pred__1))
pred_same_sign_neg <- (pred_same_sign_neg & (!pred__1))

# Checking the final lengths of each
sum(pred_diff_sign_posother)
sum(pred_diff_sign_negother)
sum(pred_same_sign_pos)
sum(pred_same_sign_neg)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting CIs ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Identifying columns that are continuous
cols_continuos <- c("admlabdiff_new", "best_hr_mat", 
                    "mean_hr_neo_new", "admdisdiff_new", 
                    "agemomyears_calc", "deldisdiff_new",
                    "labdisdiff_new", "height_neo",
                    "dia_bp_adm", "mean_spo2_neo_new",
                    "admdeldiff_new", "dbp_mat",
                    "hem_mat", "hr_adm", "labdeldiff_new",
                    "rr_mat", "rr_neo",
                    "sbp_mat", "sys_bp_adm")

# Modifying the continuous column ratios to x20 for better visualization

ratios_death[cols_continuos] <- ratios_death[cols_continuos]^20
ratios_death_start[cols_continuos] <- ratios_death_start[cols_continuos]^20
ratios_death_end[cols_continuos] <- ratios_death_end[cols_continuos]^20

ratios_admit[cols_continuos] <- ratios_admit[cols_continuos]^20
ratios_admit_start[cols_continuos] <- ratios_admit_start[cols_continuos]^20
ratios_admit_end[cols_continuos] <- ratios_admit_end[cols_continuos]^20

ratios_comb[cols_continuos] <- ratios_comb[cols_continuos]^20
ratios_comb_start[cols_continuos] <- ratios_comb_start[cols_continuos]^20
ratios_comb_end[cols_continuos] <- ratios_comb_end[cols_continuos]^20

#Same sign - positive
# Creating dataframe
df_ratios <- data.frame(ratios = c(ratios_death[pred_same_sign_pos], ratios_admit[pred_same_sign_pos], ratios_comb[pred_same_sign_pos]),
                        CI_start = c(ratios_death_start[pred_same_sign_pos], ratios_admit_start[pred_same_sign_pos], ratios_comb_start[pred_same_sign_pos]),
                        CI_end = c(ratios_death_end[pred_same_sign_pos], ratios_admit_end[pred_same_sign_pos], ratios_comb_end[pred_same_sign_pos]),
                        pred = rep(factor(predictor_names[pred_same_sign_pos], levels = sort(predictor_names[pred_same_sign_pos], decreasing = TRUE)), 3), # To order the variables alphabetically
                        outcome = c(rep('death', sum(pred_same_sign_pos)), rep('admit', sum(pred_same_sign_pos)), rep('comb', sum(pred_same_sign_pos))))

temp_ind = (dim(df_ratios)[1])/3

# Plotting CIs
plot1 <- df_ratios %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.75))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Same direction positive", subtitle = 'Confidence intervals for SubHazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark() +
  scale_y_continuous(limits = c(0,30))

# Saving to Survival folder
ggplot2::ggsave('samedir_pos.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival - MI', device = 'png', width=1280/72, height = 720/72, dpi = 144)

#Same sign - negative
# Creating dataframe
df_ratios <- data.frame(ratios = c(ratios_death[pred_same_sign_neg], ratios_admit[pred_same_sign_neg], ratios_comb[pred_same_sign_neg]),
                        CI_start = c(ratios_death_start[pred_same_sign_neg], ratios_admit_start[pred_same_sign_neg], ratios_comb_start[pred_same_sign_neg]),
                        CI_end = c(ratios_death_end[pred_same_sign_neg], ratios_admit_end[pred_same_sign_neg], ratios_comb_end[pred_same_sign_neg]),
                        pred = rep(factor(predictor_names[pred_same_sign_neg], levels = sort(predictor_names[pred_same_sign_neg], decreasing = TRUE)), 3),
                        outcome = c(rep('death', sum(pred_same_sign_neg)), rep('admit', sum(pred_same_sign_neg)), rep('comb', sum(pred_same_sign_neg))))

df_ratios <- df_ratios %>% 
  arrange(outcome, pred)
temp_ind = (dim(df_ratios)[1])/3

# Plotting CIs
plot1 <- df_ratios[c(1:20,(temp_ind+1):(temp_ind+20), (2*temp_ind+1):(2*temp_ind+20)), ] %>%
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.75))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Same direction negative", subtitle = 'Confidence intervals for SubHazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark() +
  scale_y_continuous(limits = c(0,20))

plot2 <- df_ratios[c(21:temp_ind, (temp_ind+21):(2*temp_ind), (2*temp_ind+21):(3*temp_ind)),] %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.75))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Same direction negative", subtitle = 'Confidence intervals for SubHazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark() +
  scale_y_continuous(limits = c(0,20))

ggplot2::ggsave('samedir_neg2.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival - MI', device = 'png', width=1280/72, height = 720/72, dpi = 144)
ggplot2::ggsave('samedir_neg1.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival - MI', device = 'png', width=1280/72, height = 720/72, dpi = 144)

#Different sign - death positive, readmit other
# Creating dataframe
df_ratios <- data.frame(ratios = c(ratios_death[pred_diff_sign_posother], ratios_admit[pred_diff_sign_posother], ratios_comb[pred_diff_sign_posother]),
                        CI_start = c(ratios_death_start[pred_diff_sign_posother], ratios_admit_start[pred_diff_sign_posother], ratios_comb_start[pred_diff_sign_posother]),
                        CI_end = c(ratios_death_end[pred_diff_sign_posother], ratios_admit_end[pred_diff_sign_posother], ratios_comb_end[pred_diff_sign_posother]),
                        pred = rep(factor(predictor_names[pred_diff_sign_posother], levels = sort(predictor_names[pred_diff_sign_posother], decreasing = TRUE)), 3),
                        outcome = c(rep('death', sum(pred_diff_sign_posother)), rep('admit', sum(pred_diff_sign_posother)), rep('comb', sum(pred_diff_sign_posother))))

df_ratios <- df_ratios %>% 
  arrange(outcome, pred)
temp_ind = (dim(df_ratios)[1])/3

# Plotting CIs
plot1 <- df_ratios[c(1:30,(temp_ind+1):(temp_ind+30), (2*temp_ind+1):(2*temp_ind+30)), ] %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.75))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death positive, Readmit other", subtitle = 'Confidence intervals for Subhazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark()+
  scale_y_continuous(limits = c(0,30))

plot2 <- df_ratios[c(31:temp_ind, (temp_ind+31):(2*temp_ind), (2*temp_ind+31):(3*temp_ind)),] %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.75))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death positive, Readmit other", subtitle = 'Confidence intervals for Subhazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark()+
  scale_y_continuous(limits = c(0,30))

ggplot2::ggsave('diffdir_posneg2.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival - MI', device = 'png', width=1280/72, height = 720/72, dpi = 144)
ggplot2::ggsave('diffdir_posneg1.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival - MI', device = 'png', width=1280/72, height = 720/72, dpi = 144)

#Different sign - death negative, readmit positive
# Creating dataframe
df_ratios <- data.frame(ratios = c(ratios_death[pred_diff_sign_negother], ratios_admit[pred_diff_sign_negother], ratios_comb[pred_diff_sign_negother]),
                        CI_start = c(ratios_death_start[pred_diff_sign_negother], ratios_admit_start[pred_diff_sign_negother], ratios_comb_start[pred_diff_sign_negother]),
                        CI_end = c(ratios_death_end[pred_diff_sign_negother], ratios_admit_end[pred_diff_sign_negother], ratios_comb_end[pred_diff_sign_negother]),
                        pred = rep(factor(predictor_names[pred_diff_sign_negother], sort(predictor_names[pred_diff_sign_negother], decreasing = TRUE)), 3),
                        outcome = c(rep('death', sum(pred_diff_sign_negother)), rep('admit', sum(pred_diff_sign_negother)), rep('comb', sum(pred_diff_sign_negother))))

df_ratios <- df_ratios %>% 
  arrange(outcome, pred)
temp_ind = (dim(df_ratios)[1])/3

# Plotting CIs
plot1 <- df_ratios[c(1:30,(temp_ind+1):(temp_ind+30), (2*temp_ind+1):(2*temp_ind+30)), ] %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.75))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death negative, Readmit other", subtitle = 'Confidence intervals for Subhazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark()+
  scale_y_continuous(limits = c(0,20))

plot2 <- df_ratios[c(31:temp_ind, (temp_ind+31):(2*temp_ind), (2*temp_ind+31):(3*temp_ind)),] %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.75))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death negative, Readmit other", subtitle = 'Confidence intervals for Subhazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark()+
  scale_y_continuous(limits = c(0,20))

ggplot2::ggsave('diffdir_negpos2.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival - MI', device = 'png', width=1280/72, height = 720/72, dpi = 144)
ggplot2::ggsave('diffdir_negpos1.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival - MI', device = 'png', width=1280/72, height = 720/72, dpi = 144)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURES FOR EXPORT             ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Figure 2 ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat_cif <- complete(dat_surv, action = 0)

cif <- cuminc(ftime = dat_cif$time, fstatus = dat_cif$status)

times <- timepoints(cif, 1:42)
# length(times$est)

temp_plot_data <- data.frame(Time = rep(0:42, 2),
                             Incidence = c(0, times$est[seq(1,84,2)], 0, times$est[seq(2,84,2)]),
                             Outcome = c(rep("Mortality", 43), rep("Readmission", 43)))

plot1 <- temp_plot_data %>% 
  ggplot(aes(Time, Incidence, colour = Outcome)) +
  geom_step() +
  #geom_step(aes(Time, Readmit, colour = 'blue')) +
  scale_x_continuous(breaks = seq(0,42,7)) +
  scale_y_continuous(breaks = seq(0,0.1,0.02), limits = c(0,0.1)) +
  labs(title = "Cumulative Incidence Function", subtitle = 'Mortality and Readmission as Competing Risks') +
  labs(x = 'Time (days from birth)') + 
  labs(y = 'Incidence Probability') +
  labs(tag = 'A)') +
  theme_bw() +
  theme(plot.title = element_text(size = 10), 
        text = element_text(size = 7),
        legend.position = c(0.8, 0.8))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Comb KM Survival ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

surv_Object <- Surv(dat_cox$time, dat_cox$babeoutcome_swf)
comb_fit <- survfit(formula = surv_Object ~ sexbb_del_new.Male, data = dat_cox)
# summary(comb_fit)

plot2 <- ggsurvplot(comb_fit, data = dat_cox, conf.int = TRUE, pval = TRUE, fun = "event",
                    legend.labs = c("Female", "Male"), legend.title = "Sex of the baby",
                    legend = c(0.8, 0.8),
                    ylim = c(0, 0.1), xlab = "Time (days from birth)", title = "KM Mortality Curve - Combined Outcome")

plot2$plot <- plot2$plot +
  labs(title = "KM Mortality Curve", subtitle = "Combined Outcome by sex") +
  labs(y = "Incidence Probability") +
  scale_x_continuous(breaks = seq(0,42,7)) +
  scale_y_continuous(breaks = seq(0,0.1,0.02), limits = c(0,0.1)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10), 
        text = element_text(size = 7),
        legend.position = c(0.8, 0.8)) +
  labs(tag = 'B)')    

# plot2$plot

tiff('Results/Figure 1.tiff', res = 300, compression = "lzw", width = 32, height = 10, units = 'cm')
grid.arrange(plot1, plot2$plot, ncol = 2)

dev.off()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Figure 3 ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 3a

cols_3a <-c("hiv_status_new.Checked", 
            "medhx_adm___2.Checked", 
            "weightbb_del_cat_new...2.5kg", 
            "temp_mat")

outcome = c(rep('Mortality', 4), rep('Readmission', 4), rep('Combined outcome', 4))
odds = c(ratios_death[cols_3a], ratios_admit[cols_3a], ratios_comb[cols_3a])
labs = rep(c("HIV history of the mother", "Medical history of High BP",
             "Low birth weight of baby (<= 2.5 kg)", "Temperature of mother at discharge"), 3)

df_plot = data.frame(outcome, odds, labs)

plot3a <- ggplot(df_plot, aes(x=labs, y=odds, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  geom_hline(yintercept = 1) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same Direction: +ve", subtitle = '>1 implies higher risk, <1 implies lower risk') +
  labs(y = 'SubHazards ratios', x = '') + 
  labs(tag = 'A)') +
  scale_y_continuous(limits = c(0,7)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10), 
        text = element_text(size = 8),
        legend.position = c(0.8, 0.8),
        axis.text.y = element_text(size = 8.5))

# 3b

cols_3b <-c("agemomyears_cat_new.18...35.years", 
            "adm_mat.Yes",
            "admdisdiff_new",
            "height_neo")

outcome = c(rep('Mortality', 4), rep('Readmission', 4), rep('Combined outcome', 4))
odds = c(ratios_death[cols_3b], ratios_admit[cols_3b], ratios_comb[cols_3b])
labs = rep(c("Age of mother 18-35 (ref: <18)", "Was mother admitted to higher level of care",
             "Difference between admission and discharge (20 days)", "Length of baby at delivery (cm)"), 3)

df_plot = data.frame(outcome, odds, labs)

plot3b <- ggplot(df_plot, aes(x=labs, y=odds, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  geom_hline(yintercept = 1) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same Direction: +ve", subtitle = '>1 implies higher risk, <1 implies lower risk') +
  labs(y = 'SubHazards ratios', x = '') + 
  labs(tag = 'A)') +
  scale_y_continuous(limits = c(0,2)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10), 
        text = element_text(size = 8),
        legend.position = c(0.8, 0.8),
        axis.text.y = element_text(size = 8.5))

# 3c

cols_3c <-c("agemomyears_cat_new.18...35.years", 
            "adm_mat.Yes",
            "admdisdiff_new",
            "height_neo")

outcome = c(rep('Mortality', 4), rep('Readmission', 4), rep('Combined outcome', 4))
odds = c(ratios_death[cols_3b], ratios_admit[cols_3b], ratios_comb[cols_3b])
labs = rep(c("Age of mother 18-35 (ref: <18)", "Was mother admitted to higher level of care",
             "Difference between admission and discharge (20 days)", "Length of baby at delivery (cm)"), 3)

df_plot = data.frame(outcome, odds, labs)

plot3b <- ggplot(df_plot, aes(x=labs, y=odds, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  geom_hline(yintercept = 1) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same Direction: +ve", subtitle = '>1 implies higher risk, <1 implies lower risk') +
  labs(y = 'SubHazards ratios', x = '') + 
  labs(tag = 'A)') +
  scale_y_continuous(limits = c(0,2)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10), 
        text = element_text(size = 8),
        legend.position = c(0.8, 0.8),
        axis.text.y = element_text(size = 8.5))



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plots for Pascal Document ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cols_1 <- c("agemomyears_cat_new.18...35.years", 
            "hiv_status_new.Checked", 
            "weightbb_del",
            "height_neo")

cols_2 <- c("time_tohosp_adm_new.more.than.1hr",
            "sexbb_del_new.Female",
            "child_death_ses.Yes",
            "rescus_del.Yes")

outcome = c(rep('death', 4), rep('admit', 4))
odds = c(ratios_death[cols_1], ratios_admit[cols_1])
labs = rep(c("Age of mother 18-35 (ref: <18)", "HIV history of the mother",
             "Weight of baby at delivery (kg)", "Length of baby at delivery (cm)"), 2)

df_plot = data.frame(outcome, odds, labs)

plot1 <- ggplot(df_plot, aes(x=labs, y=odds, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  geom_hline(yintercept = 1) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Readmission as Infection Risk", subtitle = '>1 implies higher risk, <1 implies lower risk') +
  labs(y = 'SubHazards ratios', x = '') + 
  labs(tag = 'A)') +
  scale_y_continuous(limits = c(0,3)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10), 
        text = element_text(size = 8),
        legend.position = c(0.8, 0.8),
        axis.text.y = element_text(size = 8.5))

plot1

outcome = c(rep('death', 4), rep('admit', 4))
odds = c(ratios_death[cols_2], ratios_admit[cols_2])
labs = rep(c("Travel time to hospital > 1 hr", "Sex of the baby - Female",
             "History of child death in the mother", "Resuscitation needed at birth"), 2)

df_plot = data.frame(outcome, odds, labs)

plot2 <- ggplot(df_plot, aes(x=labs, y=odds, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  geom_hline(yintercept = 1) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Readmission as Care-seeking Behavior", subtitle = '>1 implies higher risk, <1 implies lower risk') +
  labs(y = 'SubHazards ratios', x = '') +
  labs(tag = 'B)') +
  scale_y_continuous(limits = c(0,3)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10), 
        text = element_text(size = 8),
        legend.position = c(0.8, 0.8),
        axis.text.y = element_text(size = 8.5))

tiff('Results/Pascal Figure.tiff', res = 300, compression = "lzw", width = 17.5, height = 20, units = 'cm')
grid.arrange(plot1, plot2, nrow = 2)

dev.off()