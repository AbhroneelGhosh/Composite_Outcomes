# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 05_MOM OUTCOME MODEL        #######
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Predictive model for mother's outcomes

# For data manipulations
library(tidyverse)
library(Hmisc)
library(openxlsx)
library(summarytools)

# For missing data imputation
library(VIM)

# For survival analysis
library(survival)
library(survminer)

# For machine learning
library(caret)
library(pROC)
library(PRROC)
library(DescTools)
library(MLmetrics)
library(performanceEstimation)

# For parallel processing
library(parallel)
library(doParallel)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA               ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("Cleaned Data/Cleaned_Data (2023-11-21).RData")

# Analysis date
analysis_date <- "2023-12-11"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HELPER FUNCTIONS          #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Helper function to produce all summary stats for ML models
# ROC, sensitivity, specificity, accuracy, kappa, AUC, precision and recall
allStats <- function(...) { 
  c(twoClassSummary(...),
    defaultSummary(...),
    prSummary(...))
}

# Helper function to train models on F1 metric
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- MLmetrics::F1_Score(y_pred = data$pred,
                                y_true = data$obs,
                                positive = lev[1])
  c(F1 = f1_val)
}

# ggplot color hue
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

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

# Total births: 7361 births
nrow(dat_baby)

# Remove stillbirths: 7082 babies
stillbirths_exclude <- subset(dat_baby, sb20wk_del_new == "Yes")
dat_baby <- subset(dat_baby, sb20wk_del_new == "No")
nrow(stillbirths_exclude)
nrow(dat_baby)

# Exclude if baby died at birth or during admission: 6955
died_baby_exclude <- subset(dat_baby, babedeath_dis == "Baby Died")
dat_baby <- subset(dat_baby, babedeath_dis == "Baby Survived")
nrow(died_baby_exclude)
nrow(dat_baby)

# Discharged alive: 6135 babies
admitted_exclude <- table(dat_baby$dispbb_neo)
dat_baby <- subset(dat_baby, dispbb_neo == "Discharged" | is.na(dispbb_neo))
admitted_exclude
nrow(dat_baby)

# Check if still unknown reason: 6112 babies
unknown_baby_exclude <- subset(dat_baby, (is.na(dispbb_neo)))
dat_baby <- subset(dat_baby, !is.na(dispbb_neo))
nrow(unknown_baby_exclude)
nrow(dat_baby)

# Follow-up: N = 6053
ltfu_baby_exclude <- subset(dat_baby, six_week_follow_up_neonatal_complete == "Incomplete" | 
                              is.na(six_week_follow_up_neonatal_complete))
unknown_baby_outcome <- subset(dat_baby, six_week_follow_up_neonatal_complete == "Complete" & 
                                 (is.na(babealive_swf) | 
                                    (babealive_swf == "Yes" & is.na(babeadmit_swf))))
nrow(ltfu_baby_exclude)
nrow(unknown_baby_outcome)
dat_baby <- dat_baby %>% 
  filter(six_week_follow_up_neonatal_complete == "Complete",
         babealive_swf == "No" | 
           (babealive_swf == "Yes" & !is.na(babeadmit_swf)))
nrow(dat_baby)


# Total survived, died, and re-admitted
baby_survived <- subset(dat_baby, babealive_swf == "Yes" & babeadmit_swf == "No")
baby_died <- subset(dat_baby, babedeath_swf == "Yes")
baby_admitted <- subset(dat_baby, babeadmit_swf == "Yes")

nrow(baby_survived)
nrow(baby_died)
nrow(baby_admitted)

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
  "fetaldemise_adm",
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
  "numberanc_adm",
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
  "vagexam_del",
  "placenta_del",
  "man_placenta_del",
  "csecturgency_del_new"
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Delivery Neonatal         ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

delivery_neonatal_vars <- c(
  "infantstatus_del",
  "sb20wk_del_new",
  "sbcongenital_del_new",
  "sexbb_del_new",
  "apgar1_del",
  "apgar5_del",
  "cord_delay_del",
  "weightbb_del",
  "weightbb_del_cat_new",
  "height_neo",
  "rescus_del"
  # grep("^resustype_del", colnames(dat_mom), value = TRUE),
  # "rescusoxy_del_new"
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
  "dispbb_neo_new",
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

# Labels
predictors_labels <- unlist(sapply(dat_mom[, predictors], 
                                   label))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RE-ADJUST OUTCOME       #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create alternate re-admission outcome that excludes those 
#  potentially related to:
#  HIV (5), malaria (6), or PPD/psychosis (15)

dat_mom <- dat_mom %>% 
  mutate(
    excludeconditions = case_when(momadmitcond_swf___5 == "Checked" | 
                                    momadmitcond_swf___6 == "Checked" | 
                                    momadmitcond_swf___15 == "Checked" ~ 
                                    1,
                                  TRUE ~ 0),
    momoutcome_swf_exclude = factor(case_when(momoutcome_swf == "Yes" & 
                                                excludeconditions == 1 ~ "No",
                                              TRUE ~ unlabel(momoutcome_swf))))

# Create alternate re-admission outcome that only includes 
#  infectious outcomes:
#  surgical site infection (10), puerperal sepsis (11), or other infection (12)
# and excludes: 
#  only malaria (6)

admitcond_cols <- grep("momadmitcond_swf", colnames(dat_mom), value = TRUE)
admitcond_cols <- admitcond_cols[-length(admitcond_cols)]
dat_mom$totalconditions <- apply(dat_mom[, admitcond_cols], 1, function(X) sum(X == "Checked"))

dat_mom <- dat_mom %>% 
  mutate(
    infectiousconditions = case_when(momadmitcond_swf___10 == "Checked" | 
                                       momadmitcond_swf___11 == "Checked" | 
                                       momadmitcond_swf___12 == "Checked" ~ 1,
                                     TRUE ~ 0),
    malariaonly = case_when(momadmitcond_swf___6 == "Checked" & 
                              totalconditions == 1 ~ 1,
                            TRUE ~ 0),
    momoutcome_swf_infectious = factor(case_when(momoutcome_swf == "Yes" & 
                                                   infectiousconditions == 1 & 
                                                   malariaonly == 0 ~ "Yes" ,
                                                 TRUE ~ "No")))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA IMPUTATION     #############
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~
## Remove variables ####
## ~~~~~~~~~~~~~~~~~~~~~

# Identify variables with high amounts (>25%) of missing data for removal
# Use the combined derivation and validation sets for this

# Remove variables
remove_var <- 
  dat_mom %>% 
  select(all_of(predictors)) %>% 
  ungroup() %>% 
  summarise_all(~sum(is.na(.))) %>% 
  reshape2::melt() %>% 
  mutate(PCMissing = value / nrow(dat_mom)) %>% 
  filter(PCMissing > 0.25)
remove_var


## ~~~~~~~~~~~~~~~~~~~~~
## Imputation of data ####
## ~~~~~~~~~~~~~~~~~~~~~

dat_impute <- dat_mom %>% 
  select(studyid_adm,
         all_of(predictors), 
         momoutcome_swf,
         momoutcome_swf_exclude,
         momoutcome_swf_infectious) %>% 
  select(-remove_var$variable)

set.seed(10002)
dat_impute <- kNN(dat_impute, imp_var = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ALL OUTCOMES        #############
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 10 fold cross validation
set.seed(10012)
folds <- createFolds(dat_impute$momoutcome_swf, k = 10)
N <- length(folds)

# Machine learning parameters 
fitControl <- trainControl(method = "cv", # Cross-fold validation
                           number = 10,  # 10 folds
                           classProbs = TRUE, # Predicted class probabilities 
                           summaryFunction = allStats, # Summary stats
                           savePredictions = TRUE, # Save the predictions
                           allowParallel = TRUE) # Enable parallel processing

# Tuning grid for parameter search
glmnGrid <- expand.grid(alpha = seq(.1, 1, length = 10),
                        lambda = seq(.01, 1, length = 100))

# Empty vector to store results
perf_glmnet_cv <- data.frame(Threshold = rep(NA, N),
                             Spec = NA,
                             Sens = NA, 
                             AUC = NA,
                             PPV = NA,
                             NPV = NA,
                             PRAUC = NA,
                             F1 = NA,
                             BrierScore = NA)
## ~~~~~~~~~~~~~~~
## Nested CV  ####
## ~~~~~~~~~~~~~~~

# Number of cores for parallel processing
n_cores <- detectCores() - 1

# create the cluster for caret to use
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

# Begin loop for cross-validation
for (i in 1:N) {
  
  # Training and test set
  train_set <- dat_impute[-folds[[i]], ]
  test_set <- dat_impute[folds[[i]], ]
  
  # Fit model
  set.seed(10002)
  elastic_net_fold <- 
    train(momoutcome_swf ~ . * delmode_del_new,
          data = train_set %>% 
            select(predictors,
                   momoutcome_swf),
          method = "glmnet",
          tuneGrid = glmnGrid,
          preProc = c("center", "scale"), #center and scale variables 
          metric = "ROC",
          trControl = fitControl) 
  
  # Predictions
  predictions_fold <- predict(elastic_net_fold, test_set, "prob")
  
  # ROC
  roc_fold <- roc(test_set$momoutcome_swf, predictions_fold$Yes, ci = TRUE)
  
  # Get optimal cut-offs
  coords_fold <- pROC::coords(roc_fold, "all")
  coords_fold_youden <- pROC::coords(roc_fold, "best", best.method = "youden")
  
  perf_glmnet_cv$Threshold[i] <- coords_fold_youden$threshold
  
  # Classification at optimal cut-off and true classes
  predictions_fold <- predictions_fold %>% 
    mutate(
      CutoffClasses = 
        factor(ifelse(Yes > coords_fold_youden$threshold,
                      "Yes", "No")),
      TrueClasses = test_set$momoutcome_swf)
  
  # Confusion matrix using optimal cut-off
  confusion_matrix_fold <- confusionMatrix(predictions_fold$CutoffClasses, 
                                           predictions_fold$TrueClasses,
                                           mode = "everything",
                                           positive = "Yes")
  
  # Store performance metrics
  perf_glmnet_cv$Sens[i] <- confusion_matrix_fold$byClass[1]
  perf_glmnet_cv$Spec[i] <- confusion_matrix_fold$byClass[2]
  perf_glmnet_cv$AUC[i] <- roc_fold$auc
  
  perf_glmnet_cv$PPV[i] <- confusion_matrix_fold$byClass[3]
  perf_glmnet_cv$NPV[i] <- confusion_matrix_fold$byClass[4]
  perf_glmnet_cv$F1[i] <- confusion_matrix_fold$byClass[7]
  
  # Precision, recall, and PRAUC
  pr_curve_fold <- pr.curve(predictions_fold$Yes[predictions_fold$TrueClasses == "Yes"],
                            predictions_fold$Yes[predictions_fold$TrueClasses == "No"],
                            curve = TRUE)
  
  perf_glmnet_cv$PRAUC[i] <- pr_curve_fold$auc.integral
  
  # Brier score
  perf_glmnet_cv$BrierScore[i] <- BrierScore(as.numeric(test_set$momoutcome_swf) - 1, 
                                             predictions_fold$Yes)
  
  # Variable importance 
  if (i == 1) {
    var_imp_glmnet_cv <- data.frame(varImp(elastic_net_fold)$importance)
    var_imp_glmnet_cv <- cbind(var_imp_glmnet_cv,
                               replicate(9, var_imp_glmnet_cv))
    colnames(var_imp_glmnet_cv) <- paste0("Fold", 1:N)
    var_imp_glmnet_cv[, 2:N] <- NA
  } else {
    var_imp_glmnet_cv[,i] <- varImp(elastic_net_fold)$importance
  }
}

# End parallel processing
stopCluster(cl)


## ~~~~~~~~~~
## Average cv performance ####
## ~~~~~~~~~~

# Average performance across fold
perf_glmnet_cv <- round(perf_glmnet_cv, 3) %>% 
  rbind(round(apply(perf_glmnet_cv, 2, mean), 3))

# Add column for Fold
perf_glmnet_cv <- perf_glmnet_cv %>% 
  rownames_to_column(var = "Fold")
perf_glmnet_cv$Fold[11] <- "Average"



## ~~~~~~~~~~
## Average variable importance ####
## ~~~~~~~~~~

# Get average rank of variable importance
rank_glmnet_cv <- apply(var_imp_glmnet_cv, 2, function(X) rank(-X))
ave_rank_glmnet_cv <- apply(rank_glmnet_cv, 1, mean) %>% 
  sort() %>% 
  data.frame() %>% 
  rownames_to_column(var = "Variable") %>% 
  mutate(Variable = factor(Variable, levels = Variable))
colnames(ave_rank_glmnet_cv)[2] <- "Rank"

# How many times variable was in top 8
count_rank_glmnet_cv <- apply(rank_glmnet_cv, 1, function(X) sum(X %in% 1:8)) %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() %>% 
  rownames_to_column(var = "Variable")

# Combine
ave_rank_glmnet_cv$Selected <- count_rank_glmnet_cv$.[match(ave_rank_glmnet_cv$Variable,
                                                            count_rank_glmnet_cv$Variable)]

ave_rank_glmnet_cv %>% 
  arrange(desc(Selected)) %>% 
  slice(1:10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXCLUDE HIV ETC.            #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 10 fold cross validation
set.seed(10012)
folds <- createFolds(dat_impute$momoutcome_swf_exclude, k = 10)
N <- length(folds)

# Machine learning parameters 
fitControl <- trainControl(method = "cv", # Cross-fold validation
                           number = 10,  # 10 folds
                           classProbs = TRUE, # Predicted class probabilities 
                           summaryFunction = allStats, # Summary stats
                           savePredictions = TRUE, # Save the predictions
                           allowParallel = TRUE) # Enable parallel processing

# Tuning grid for parameter search
glmnGrid <- expand.grid(alpha = seq(.1, 1, length = 10),
                        lambda = seq(.01, 1, length = 100))

# Empty vector to store results
perf_glmnet_exclude_cv <- data.frame(Threshold = rep(NA, N),
                                     Spec = NA,
                                     Sens = NA, 
                                     AUC = NA,
                                     PPV = NA,
                                     NPV = NA,
                                     PRAUC = NA,
                                     F1 = NA,
                                     BrierScore = NA)

## ~~~~~~~~~~~~~~~
## Nested CV  ####
## ~~~~~~~~~~~~~~~

# Number of cores for parallel processing
n_cores <- detectCores() - 1

# create the cluster for caret to use
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

# Begin loop for cross-validation
for (i in 1:N) {
  
  # Training and test set
  train_set <- dat_impute[-folds[[i]], ]
  test_set <- dat_impute[folds[[i]], ]
  
  # Fit model
  set.seed(10002)
  elastic_net_fold <- 
    train(momoutcome_swf_exclude ~ . * delmode_del_new,
          data = train_set %>% 
            select(predictors,
                   momoutcome_swf_exclude),
          method = "glmnet",
          tuneGrid = glmnGrid,
          preProc = c("center", "scale"), #center and scale variables 
          metric = "ROC",
          trControl = fitControl) 
  
  # Predictions
  predictions_fold <- predict(elastic_net_fold, test_set, "prob")
  
  # ROC
  roc_fold <- roc(test_set$momoutcome_swf_exclude, predictions_fold$Yes, ci = TRUE)
  
  # Get optimal cut-offs
  coords_fold <- pROC::coords(roc_fold, "all")
  coords_fold_youden <- pROC::coords(roc_fold, "best", best.method = "youden")
  
  perf_glmnet_exclude_cv$Threshold[i] <- coords_fold_youden$threshold
  
  # Classification at optimal cut-off and true classes
  predictions_fold <- predictions_fold %>% 
    mutate(
      CutoffClasses = 
        factor(ifelse(Yes > coords_fold_youden$threshold,
                      "Yes", "No")),
      TrueClasses = test_set$momoutcome_swf_exclude)
  
  # Confusion matrix using optimal cut-off
  confusion_matrix_fold <- confusionMatrix(predictions_fold$CutoffClasses, 
                                           predictions_fold$TrueClasses,
                                           mode = "everything",
                                           positive = "Yes")
  
  # Store performance metrics
  perf_glmnet_exclude_cv$Sens[i] <- confusion_matrix_fold$byClass[1]
  perf_glmnet_exclude_cv$Spec[i] <- confusion_matrix_fold$byClass[2]
  perf_glmnet_exclude_cv$AUC[i] <- roc_fold$auc
  
  perf_glmnet_exclude_cv$PPV[i] <- confusion_matrix_fold$byClass[3]
  perf_glmnet_exclude_cv$NPV[i] <- confusion_matrix_fold$byClass[4]
  perf_glmnet_exclude_cv$F1[i] <- confusion_matrix_fold$byClass[7]
  
  # Precision, recall, and PRAUC
  pr_curve_fold <- pr.curve(predictions_fold$Yes[predictions_fold$TrueClasses == "Yes"],
                            predictions_fold$Yes[predictions_fold$TrueClasses == "No"],
                            curve = TRUE)
  
  perf_glmnet_exclude_cv$PRAUC[i] <- pr_curve_fold$auc.integral
  
  # Brier score
  perf_glmnet_exclude_cv$BrierScore[i] <- BrierScore(as.numeric(test_set$momoutcome_swf_exclude) - 1, 
                                                     predictions_fold$Yes)
  
  # Variable importance 
  if (i == 1) {
    var_imp_glmnet_exclude_cv <- data.frame(varImp(elastic_net_fold)$importance)
    var_imp_glmnet_exclude_cv <- cbind(var_imp_glmnet_exclude_cv,
                                       replicate(9, var_imp_glmnet_exclude_cv))
    colnames(var_imp_glmnet_exclude_cv) <- paste0("Fold", 1:N)
    var_imp_glmnet_exclude_cv[, 2:N] <- NA
  } else {
    var_imp_glmnet_exclude_cv[,i] <- varImp(elastic_net_fold)$importance
  }
}

# End parallel processing
stopCluster(cl)


## ~~~~~~~~~~
## Average cv performance ####
## ~~~~~~~~~~

# Average performance across fold
perf_glmnet_exclude_cv <- round(perf_glmnet_exclude_cv, 3) %>% 
  rbind(round(apply(perf_glmnet_exclude_cv, 2, mean), 3))

# Add column for Fold
perf_glmnet_exclude_cv <- perf_glmnet_exclude_cv %>% 
  rownames_to_column(var = "Fold")
perf_glmnet_exclude_cv$Fold[11] <- "Average"


## ~~~~~~~~~~
## Average variable importance ####
## ~~~~~~~~~~

# Get average rank of variable importance
rank_glmnet_exclude_cv <- apply(var_imp_glmnet_exclude_cv, 2, function(X) rank(-X))
ave_rank_glmnet_exclude_cv <- apply(rank_glmnet_exclude_cv, 1, mean) %>% 
  sort() %>% 
  data.frame() %>% 
  rownames_to_column(var = "Variable") %>% 
  mutate(Variable = factor(Variable, levels = Variable))
colnames(ave_rank_glmnet_exclude_cv)[2] <- "Rank"

# How many times variable was in top 8
count_rank_glmnet_exclude_cv <- apply(rank_glmnet_exclude_cv, 1, function(X) sum(X %in% 1:8)) %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() %>% 
  rownames_to_column(var = "Variable")

# Combine
ave_rank_glmnet_exclude_cv$Selected <- count_rank_glmnet_exclude_cv$.[match(ave_rank_glmnet_exclude_cv$Variable,
                                                                            count_rank_glmnet_exclude_cv$Variable)]
ave_rank_glmnet_exclude_cv %>% 
  arrange(desc(Selected)) %>% 
  slice(1:10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INFECTIOUS OUTCOMES         #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 10 fold cross validation
set.seed(10012)
folds <- createFolds(dat_impute$momoutcome_swf_infectious, k = 10)
N <- length(folds)

# Machine learning parameters 
fitControl <- trainControl(method = "cv", # Cross-fold validation
                           number = 10,  # 10 folds
                           classProbs = TRUE, # Predicted class probabilities 
                           summaryFunction = allStats, # Summary stats
                           savePredictions = TRUE, # Save the predictions
                           allowParallel = TRUE) # Enable parallel processing

# Tuning grid for parameter search
glmnGrid <- expand.grid(alpha = seq(.1, 1, length = 10),
                        lambda = seq(.01, 1, length = 100))

# Empty vector to store results
perf_glmnet_infectious_cv <- data.frame(Threshold = rep(NA, N),
                                        Spec = NA,
                                        Sens = NA, 
                                        AUC = NA,
                                        PPV = NA,
                                        NPV = NA,
                                        PRAUC = NA,
                                        F1 = NA,
                                        BrierScore = NA)

## ~~~~~~~~~~~~~~~
## Nested CV  ####
## ~~~~~~~~~~~~~~~

# Number of cores for parallel processing
n_cores <- detectCores() - 1

# create the cluster for caret to use
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

# Begin loop for cross-validation
for (i in 1:N) {
  
  # Training and test set
  train_set <- dat_impute[-folds[[i]], ]
  test_set <- dat_impute[folds[[i]], ]
  
  # Fit model
  set.seed(10002)
  elastic_net_fold <- 
    train(momoutcome_swf_infectious ~ . * delmode_del_new,
          data = train_set %>% 
            select(predictors,
                   momoutcome_swf_infectious),
          method = "glmnet",
          tuneGrid = glmnGrid,
          preProc = c("center", "scale"), #center and scale variables 
          metric = "ROC",
          trControl = fitControl) 
  
  # Predictions
  predictions_fold <- predict(elastic_net_fold, test_set, "prob")
  
  # ROC
  roc_fold <- roc(test_set$momoutcome_swf_infectious, predictions_fold$Yes, ci = TRUE)
  
  # Get optimal cut-offs
  coords_fold <- pROC::coords(roc_fold, "all")
  coords_fold_youden <- pROC::coords(roc_fold, "best", best.method = "youden")
  
  perf_glmnet_infectious_cv$Threshold[i] <- coords_fold_youden$threshold
  
  # Classification at optimal cut-off and true classes
  predictions_fold <- predictions_fold %>% 
    mutate(
      CutoffClasses = 
        factor(ifelse(Yes > coords_fold_youden$threshold,
                      "Yes", "No")),
      TrueClasses = test_set$momoutcome_swf_infectious)
  
  # Confusion matrix using optimal cut-off
  confusion_matrix_fold <- confusionMatrix(predictions_fold$CutoffClasses, 
                                           predictions_fold$TrueClasses,
                                           mode = "everything",
                                           positive = "Yes")
  
  # Store performance metrics
  perf_glmnet_infectious_cv$Sens[i] <- confusion_matrix_fold$byClass[1]
  perf_glmnet_infectious_cv$Spec[i] <- confusion_matrix_fold$byClass[2]
  perf_glmnet_infectious_cv$AUC[i] <- roc_fold$auc
  
  perf_glmnet_infectious_cv$PPV[i] <- confusion_matrix_fold$byClass[3]
  perf_glmnet_infectious_cv$NPV[i] <- confusion_matrix_fold$byClass[4]
  perf_glmnet_infectious_cv$F1[i] <- confusion_matrix_fold$byClass[7]
  
  # Precision, recall, and PRAUC
  pr_curve_fold <- pr.curve(predictions_fold$Yes[predictions_fold$TrueClasses == "Yes"],
                            predictions_fold$Yes[predictions_fold$TrueClasses == "No"],
                            curve = TRUE)
  
  perf_glmnet_infectious_cv$PRAUC[i] <- pr_curve_fold$auc.integral
  
  # Brier score
  perf_glmnet_infectious_cv$BrierScore[i] <- BrierScore(as.numeric(test_set$momoutcome_swf_infectious) - 1, 
                                                        predictions_fold$Yes)
  
  # Variable importance 
  if (i == 1) {
    var_imp_glmnet_infectious_cv <- data.frame(varImp(elastic_net_fold)$importance)
    var_imp_glmnet_infectious_cv <- cbind(var_imp_glmnet_infectious_cv,
                                          replicate(9, var_imp_glmnet_infectious_cv))
    colnames(var_imp_glmnet_infectious_cv) <- paste0("Fold", 1:N)
    var_imp_glmnet_infectious_cv[, 2:N] <- NA
  } else {
    var_imp_glmnet_infectious_cv[,i] <- varImp(elastic_net_fold)$importance
  }
}

# End parallel processing
stopCluster(cl)


## ~~~~~~~~~~
## Average cv performance ####
## ~~~~~~~~~~

# Average performance across fold
perf_glmnet_infectious_cv <- round(perf_glmnet_infectious_cv, 3) %>% 
  rbind(round(apply(perf_glmnet_infectious_cv, 2, mean), 3))

# Add column for Fold
perf_glmnet_infectious_cv <- perf_glmnet_infectious_cv %>% 
  rownames_to_column(var = "Fold")
perf_glmnet_infectious_cv$Fold[11] <- "Average"


## ~~~~~~~~~~
## Average variable importance ####
## ~~~~~~~~~~

# Get average rank of variable importance
rank_glmnet_infectious_cv <- apply(var_imp_glmnet_infectious_cv, 2, function(X) rank(-X))
ave_rank_glmnet_infectious_cv <- apply(rank_glmnet_infectious_cv, 1, mean) %>% 
  sort() %>% 
  data.frame() %>% 
  rownames_to_column(var = "Variable") %>% 
  mutate(Variable = factor(Variable, levels = Variable))
colnames(ave_rank_glmnet_infectious_cv)[2] <- "Rank"

# How many times variable was in top 8
count_rank_glmnet_infectious_cv <- apply(rank_glmnet_infectious_cv, 1, function(X) sum(X %in% 1:8)) %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() %>% 
  rownames_to_column(var = "Variable")

# Combine
ave_rank_glmnet_infectious_cv$Selected <- count_rank_glmnet_infectious_cv$.[match(ave_rank_glmnet_infectious_cv$Variable,
                                                                                  count_rank_glmnet_infectious_cv$Variable)]

ave_rank_glmnet_infectious_cv %>% 
  arrange(desc(Selected)) %>% 
  slice(1:10)


## ~~~~~~~~~~~~~~~~~~~~~~~~
## Look at interaction ####
## ~~~~~~~~~~~~~~~~~~~~~~~~

delmode_interaction <- glm(momoutcome_swf_infectious ~ rescusoxy_del_new * delmode_del_new, 
                           data = dat_mom, family = "binomial")

possibleCombo <- data.frame(rescusoxy_del_new = rep(c("Not resuscitated", 
                                                      "Resuscitation with oxygen",
                                                      "Resuscitation without oxygen"),
                                                    each = 2),
                            delmode_del_new = rep(c("Vaginal", "Caesarean"), times = 3))


delmode_interaction_predict <- predict(test, possibleCombo, type = "response") * 100
delmode_interaction_predict <- data.frame(possibleCombo, delmode_interaction_predict)

ggplot(delmode_interaction_predict, aes(factor(delmode_del_new), delmode_interaction_predict)) + 
  geom_bar(aes(fill=factor(rescusoxy_del_new)), position="dodge", stat="identity")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CESAREAN AND VAGINAL MODEL  #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~
## Caesarean    ####
## ~~~~~~~~~~~~~~~~~

# 10 fold cross validation
set.seed(10012)
folds <- createFolds(dat_impute$momoutcome_swf, k = 10)
N <- length(folds)

# Machine learning parameters 
fitControl <- trainControl(method = "cv", # Cross-fold validation
                           number = 10,  # 10 folds
                           classProbs = TRUE, # Predicted class probabilities 
                           summaryFunction = allStats, # Summary stats
                           savePredictions = TRUE, # Save the predictions
                           allowParallel = TRUE) # Enable parallel processing

# Tuning grid for parameter search
glmnGrid <- expand.grid(alpha = seq(.1, 1, length = 10),
                        lambda = seq(.01, 1, length = 100))

# Empty vector to store results
perf_glmnet_caes_cv <- data.frame(Threshold = rep(NA, N),
                                  Spec = NA,
                                  Sens = NA, 
                                  AUC = NA,
                                  PPV = NA,
                                  NPV = NA,
                                  PRAUC = NA,
                                  F1 = NA,
                                  BrierScore = NA)

### ~~~~~~~~~~~~~~~
### Nested CV  ####
### ~~~~~~~~~~~~~~~

# Number of cores for parallel processing
n_cores <- detectCores() - 1

# create the cluster for caret to use
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

# Begin loop for cross-validation
for (i in 1:N) {
  
  # Training and test set
  train_set <- dat_impute[-folds[[i]], ]
  test_set <- dat_impute[folds[[i]], ]
  
  # Fit model
  set.seed(10002)
  elastic_net_fold <- 
    train(momoutcome_swf ~ . ,
          data = train_set %>% 
            select(predictors,
                   momoutcome_swf) %>% 
            filter(delmode_del_new == "Caesarean"),
          method = "glmnet",
          tuneGrid = glmnGrid,
          preProc = c("center", "scale"), #center and scale variables 
          metric = "ROC",
          trControl = fitControl) 
  
  # Predictions
  predictions_fold <- predict(elastic_net_fold, test_set, "prob")
  
  # ROC
  roc_fold <- roc(test_set$momoutcome_swf, predictions_fold$Yes, ci = TRUE)
  
  # Get optimal cut-offs
  coords_fold <- pROC::coords(roc_fold, "all")
  coords_fold_youden <- pROC::coords(roc_fold, "best", best.method = "youden")
  
  perf_glmnet_caes_cv$Threshold[i] <- coords_fold_youden$threshold
  
  # Classification at optimal cut-off and true classes
  predictions_fold <- predictions_fold %>% 
    mutate(
      CutoffClasses = 
        factor(ifelse(Yes > coords_fold_youden$threshold,
                      "Yes", "No")),
      TrueClasses = test_set$momoutcome_swf)
  
  # Confusion matrix using optimal cut-off
  confusion_matrix_fold <- confusionMatrix(predictions_fold$CutoffClasses, 
                                           predictions_fold$TrueClasses,
                                           mode = "everything",
                                           positive = "Yes")
  
  # Store performance metrics
  perf_glmnet_caes_cv$Sens[i] <- confusion_matrix_fold$byClass[1]
  perf_glmnet_caes_cv$Spec[i] <- confusion_matrix_fold$byClass[2]
  perf_glmnet_caes_cv$AUC[i] <- roc_fold$auc
  
  perf_glmnet_caes_cv$PPV[i] <- confusion_matrix_fold$byClass[3]
  perf_glmnet_caes_cv$NPV[i] <- confusion_matrix_fold$byClass[4]
  perf_glmnet_caes_cv$F1[i] <- confusion_matrix_fold$byClass[7]
  
  # Precision, recall, and PRAUC
  pr_curve_fold <- pr.curve(predictions_fold$Yes[predictions_fold$TrueClasses == "Yes"],
                            predictions_fold$Yes[predictions_fold$TrueClasses == "No"],
                            curve = TRUE)
  
  perf_glmnet_caes_cv$PRAUC[i] <- pr_curve_fold$auc.integral
  
  # Brier score
  perf_glmnet_caes_cv$BrierScore[i] <- BrierScore(as.numeric(test_set$momoutcome_swf) - 1, 
                                                  predictions_fold$Yes)
  
  # # Variable importance 
  # if (i == 1) {
  #   var_imp_glmnet_cv <- data.frame(varImp(elastic_net_fold)$importance)
  #   var_imp_glmnet_cv <- cbind(var_imp_glmnet_cv,
  #                              replicate(9, var_imp_glmnet_cv))
  #   colnames(var_imp_glmnet_cv) <- paste0("Fold", 1:N)
  #   var_imp_glmnet_cv[, 2:N] <- NA
  # } else {
  #   var_imp_glmnet_cv[,i] <- varImp(elastic_net_fold)$importance
  # }
}

# End parallel processing
stopCluster(cl)


### ~~~~~~~~~~
### Average cv performance ####
## #~~~~~~~~~~

# Average performance across fold
perf_glmnet_caes_cv <- round(perf_glmnet_caes_cv, 3) %>% 
  rbind(round(apply(perf_glmnet_caes_cv, 2, mean), 3))

# Add column for Fold
perf_glmnet_caes_cv <- perf_glmnet_caes_cv %>% 
  rownames_to_column(var = "Fold")
perf_glmnet_caes_cv$Fold[11] <- "Average"



## ~~~~~~~~~~~~~~~~~
## Vaginal      ####
## ~~~~~~~~~~~~~~~~~

# 10 fold cross validation
set.seed(10012)
folds <- createFolds(dat_impute$momoutcome_swf, k = 10)
N <- length(folds)

# Machine learning parameters 
fitControl <- trainControl(method = "cv", # Cross-fold validation
                           number = 10,  # 10 folds
                           classProbs = TRUE, # Predicted class probabilities 
                           summaryFunction = allStats, # Summary stats
                           savePredictions = TRUE, # Save the predictions
                           allowParallel = TRUE) # Enable parallel processing

# Tuning grid for parameter search
glmnGrid <- expand.grid(alpha = seq(.1, 1, length = 10),
                        lambda = seq(.01, 1, length = 100))

# Empty vector to store results
perf_glmnet_vag_cv <- data.frame(Threshold = rep(NA, N),
                                  Spec = NA,
                                  Sens = NA, 
                                  AUC = NA,
                                  PPV = NA,
                                  NPV = NA,
                                  PRAUC = NA,
                                  F1 = NA,
                                  BrierScore = NA)

### ~~~~~~~~~~~~~~~
### Nested CV  ####
### ~~~~~~~~~~~~~~~

# Number of cores for parallel processing
n_cores <- detectCores() - 1

# create the cluster for caret to use
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

# Begin loop for cross-validation
for (i in 1:N) {
  
  # Training and test set
  train_set <- dat_impute[-folds[[i]], ]
  test_set <- dat_impute[folds[[i]], ]
  
  # Fit model
  set.seed(10002)
  elastic_net_fold <- 
    train(momoutcome_swf ~ . ,
          data = train_set %>% 
            select(predictors,
                   momoutcome_swf) %>% 
            filter(delmode_del_new == "Vaginal"),
          method = "glmnet",
          tuneGrid = glmnGrid,
          preProc = c("center", "scale"), #center and scale variables 
          metric = "ROC",
          trControl = fitControl) 
  
  # Predictions
  predictions_fold <- predict(elastic_net_fold, test_set, "prob")
  
  # ROC
  roc_fold <- roc(test_set$momoutcome_swf, predictions_fold$Yes, ci = TRUE)
  
  # Get optimal cut-offs
  coords_fold <- pROC::coords(roc_fold, "all")
  coords_fold_youden <- pROC::coords(roc_fold, "best", best.method = "youden")
  
  perf_glmnet_vag_cv$Threshold[i] <- coords_fold_youden$threshold
  
  # Classification at optimal cut-off and true classes
  predictions_fold <- predictions_fold %>% 
    mutate(
      CutoffClasses = 
        factor(ifelse(Yes > coords_fold_youden$threshold,
                      "Yes", "No")),
      TrueClasses = test_set$momoutcome_swf)
  
  # Confusion matrix using optimal cut-off
  confusion_matrix_fold <- confusionMatrix(predictions_fold$CutoffClasses, 
                                           predictions_fold$TrueClasses,
                                           mode = "everything",
                                           positive = "Yes")
  
  # Store performance metrics
  perf_glmnet_vag_cv$Sens[i] <- confusion_matrix_fold$byClass[1]
  perf_glmnet_vag_cv$Spec[i] <- confusion_matrix_fold$byClass[2]
  perf_glmnet_vag_cv$AUC[i] <- roc_fold$auc
  
  perf_glmnet_vag_cv$PPV[i] <- confusion_matrix_fold$byClass[3]
  perf_glmnet_vag_cv$NPV[i] <- confusion_matrix_fold$byClass[4]
  perf_glmnet_vag_cv$F1[i] <- confusion_matrix_fold$byClass[7]
  
  # Precision, recall, and PRAUC
  pr_curve_fold <- pr.curve(predictions_fold$Yes[predictions_fold$TrueClasses == "Yes"],
                            predictions_fold$Yes[predictions_fold$TrueClasses == "No"],
                            curve = TRUE)
  
  perf_glmnet_vag_cv$PRAUC[i] <- pr_curve_fold$auc.integral
  
  # Brier score
  perf_glmnet_vag_cv$BrierScore[i] <- BrierScore(as.numeric(test_set$momoutcome_swf) - 1, 
                                                 predictions_fold$Yes)
  
  # # Variable importance 
  # if (i == 1) {
  #   var_imp_glmnet_cv <- data.frame(varImp(elastic_net_fold)$importance)
  #   var_imp_glmnet_cv <- cbind(var_imp_glmnet_cv,
  #                              replicate(9, var_imp_glmnet_cv))
  #   colnames(var_imp_glmnet_cv) <- paste0("Fold", 1:N)
  #   var_imp_glmnet_cv[, 2:N] <- NA
  # } else {
  #   var_imp_glmnet_cv[,i] <- varImp(elastic_net_fold)$importance
  # }
}

# End parallel processing
stopCluster(cl)


### ~~~~~~~~~~
### Average cv performance ####
### ~~~~~~~~~~

# Average performance across fold
perf_glmnet_vag_cv <- round(perf_glmnet_vag_cv, 3) %>% 
  rbind(round(apply(perf_glmnet_vag_cv, 2, mean), 3))

# Add column for Fold
perf_glmnet_vag_cv <- perf_glmnet_vag_cv %>% 
  rownames_to_column(var = "Fold")
perf_glmnet_vag_cv$Fold[11] <- "Average"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SYNTHETIC INFECTIOUS        #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create synthetic data during training

# 10 fold cross validation
set.seed(10012)
folds <- createFolds(dat_impute$momoutcome_swf_infectious, k = 10)
N <- length(folds)

# Machine learning parameters 
fitControl <- trainControl(method = "cv", # Cross-fold validation
                           number = 10,  # 10 folds
                           classProbs = TRUE, # Predicted class probabilities 
                           summaryFunction = allStats, # Summary stats
                           savePredictions = TRUE, # Save the predictions
                           allowParallel = TRUE) # Enable parallel processing

# Tuning grid for parameter search
glmnGrid <- expand.grid(alpha = seq(.1, 1, length = 10),
                        lambda = seq(.01, 1, length = 100))

# Empty vector to store results
perf_glmnet_infectious_cv <- data.frame(Threshold = rep(NA, N),
                                        Spec = NA,
                                        Sens = NA, 
                                        AUC = NA,
                                        PPV = NA,
                                        NPV = NA,
                                        PRAUC = NA,
                                        F1 = NA,
                                        BrierScore = NA)

## ~~~~~~~~~~~~~~~
## Nested CV  ####
## ~~~~~~~~~~~~~~~

# Number of cores for parallel processing
n_cores <- detectCores() - 1

# create the cluster for caret to use
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

# Begin loop for cross-validation
for (i in 1:N) {
  
  # Training and test set
  train_set <- dat_impute[-folds[[i]], ]
  test_set <- dat_impute[folds[[i]], ]
  
  # Fit model
  set.seed(10002)
  elastic_net_fold <- 
    train(momoutcome_swf_infectious ~ . * delmode_del_new,
          data = train_set %>% 
            select(predictors,
                   momoutcome_swf_infectious),
          method = "glmnet",
          tuneGrid = glmnGrid,
          preProc = c("center", "scale"), #center and scale variables 
          metric = "ROC",
          trControl = fitControl) 
  
  # Predictions
  predictions_fold <- predict(elastic_net_fold, test_set, "prob")
  
  # ROC
  roc_fold <- roc(test_set$momoutcome_swf_infectious, predictions_fold$Yes, ci = TRUE)
  
  # Get optimal cut-offs
  coords_fold <- pROC::coords(roc_fold, "all")
  coords_fold_youden <- pROC::coords(roc_fold, "best", best.method = "youden")
  
  perf_glmnet_infectious_cv$Threshold[i] <- coords_fold_youden$threshold
  
  # Classification at optimal cut-off and true classes
  predictions_fold <- predictions_fold %>% 
    mutate(
      CutoffClasses = 
        factor(ifelse(Yes > coords_fold_youden$threshold,
                      "Yes", "No")),
      TrueClasses = test_set$momoutcome_swf_infectious)
  
  # Confusion matrix using optimal cut-off
  confusion_matrix_fold <- confusionMatrix(predictions_fold$CutoffClasses, 
                                           predictions_fold$TrueClasses,
                                           mode = "everything",
                                           positive = "Yes")
  
  # Store performance metrics
  perf_glmnet_infectious_cv$Sens[i] <- confusion_matrix_fold$byClass[1]
  perf_glmnet_infectious_cv$Spec[i] <- confusion_matrix_fold$byClass[2]
  perf_glmnet_infectious_cv$AUC[i] <- roc_fold$auc
  
  perf_glmnet_infectious_cv$PPV[i] <- confusion_matrix_fold$byClass[3]
  perf_glmnet_infectious_cv$NPV[i] <- confusion_matrix_fold$byClass[4]
  perf_glmnet_infectious_cv$F1[i] <- confusion_matrix_fold$byClass[7]
  
  # Precision, recall, and PRAUC
  pr_curve_fold <- pr.curve(predictions_fold$Yes[predictions_fold$TrueClasses == "Yes"],
                            predictions_fold$Yes[predictions_fold$TrueClasses == "No"],
                            curve = TRUE)
  
  perf_glmnet_infectious_cv$PRAUC[i] <- pr_curve_fold$auc.integral
  
  # Brier score
  perf_glmnet_infectious_cv$BrierScore[i] <- BrierScore(as.numeric(test_set$momoutcome_swf_infectious) - 1, 
                                                        predictions_fold$Yes)
  
  # Variable importance 
  if (i == 1) {
    var_imp_glmnet_infectious_cv <- data.frame(varImp(elastic_net_fold)$importance)
    var_imp_glmnet_infectious_cv <- cbind(var_imp_glmnet_infectious_cv,
                                          replicate(9, var_imp_glmnet_infectious_cv))
    colnames(var_imp_glmnet_infectious_cv) <- paste0("Fold", 1:N)
    var_imp_glmnet_infectious_cv[, 2:N] <- NA
  } else {
    var_imp_glmnet_infectious_cv[,i] <- varImp(elastic_net_fold)$importance
  }
}

# End parallel processing
stopCluster(cl)


## ~~~~~~~~~~
## Average cv performance ####
## ~~~~~~~~~~

# Average performance across fold
perf_glmnet_infectious_cv <- round(perf_glmnet_infectious_cv, 3) %>% 
  rbind(round(apply(perf_glmnet_infectious_cv, 2, mean), 3))

# Add column for Fold
perf_glmnet_infectious_cv <- perf_glmnet_infectious_cv %>% 
  rownames_to_column(var = "Fold")
perf_glmnet_infectious_cv$Fold[11] <- "Average"


## ~~~~~~~~~~
## Average variable importance ####
## ~~~~~~~~~~

# Get average rank of variable importance
rank_glmnet_infectious_cv <- apply(var_imp_glmnet_infectious_cv, 2, function(X) rank(-X))
ave_rank_glmnet_infectious_cv <- apply(rank_glmnet_infectious_cv, 1, mean) %>% 
  sort() %>% 
  data.frame() %>% 
  rownames_to_column(var = "Variable") %>% 
  mutate(Variable = factor(Variable, levels = Variable))
colnames(ave_rank_glmnet_infectious_cv)[2] <- "Rank"

# How many times variable was in top 8
count_rank_glmnet_infectious_cv <- apply(rank_glmnet_infectious_cv, 1, function(X) sum(X %in% 1:8)) %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() %>% 
  rownames_to_column(var = "Variable")

# Combine
ave_rank_glmnet_infectious_cv$Selected <- count_rank_glmnet_infectious_cv$.[match(ave_rank_glmnet_infectious_cv$Variable,
                                                                                  count_rank_glmnet_infectious_cv$Variable)]

ave_rank_glmnet_infectious_cv %>% 
  arrange(desc(Selected)) %>% 
  slice(1:10)
