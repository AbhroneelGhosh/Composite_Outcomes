# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 06_INITIAL LOGIT ANALYSIS        #######
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Importing libraries
# For data manipulations
library(tidyverse)
library(Hmisc)
library(reshape2)

# For stats/logit
library(MASS)
library(tidymodels)

# For data viz
library(ggplot2)

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

# Removed 2 columns, "sb20wk_del_new" and "sbcongenital_del_new"
predictors <- predictors[!(predictors %in% exclude)]
length(exclude)

# Identify variables with high amounts (>25%) of missing data for removal
# Use the combined derivation and validation sets for this

# Remove variables
remove_var <- 
  dat_baby %>% 
  dplyr::select(all_of(predictors)) %>% 
  ungroup() %>% 
  summarise_all(~sum(is.na(.))) %>% 
  reshape2::melt() %>% 
  mutate(PCMissing = value / nrow(dat_baby)) %>% 
  filter(PCMissing > 0.25)
remove_var


## ~~~~~~~~~~~~~~~~~~~~~
## Imputation of data ####
## ~~~~~~~~~~~~~~~~~~~~~

dat_impute <- dat_baby %>% 
  dplyr::select(studyid_adm,
         all_of(predictors), 
         babeadmit_swf,
         babedeath_swf,
         babeoutcome_swf) %>% 
  dplyr::select(-remove_var$variable)

set.seed(102)
dat_impute <- VIM::kNN(dat_impute, imp_var = FALSE)

# Removing columns which have only one unique entry
dat_impute <- dat_impute %>% dplyr::select(where(~n_distinct(.) > 1))

# 6092 rows, 144 total variables
dim.data.frame(dat_impute)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MULTIVARIABLE LOGISITIC ANALYSIS     #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating individual datasets containg each outcome and the predictor vars for fitting

# dat_baby_pred_death <- dat_impute[c(predictors, "babedeath_swf")]
# dat_baby_pred_admit <- dat_impute[c(predictors, "babeadmit_swf")]
# dat_baby_pred_comb <- dat_impute[c(predictors, "babeoutcome_swf")]

## ~~~~~~~~~~~~~~~~~~~~~
## Fitting the Models ####
## ~~~~~~~~~~~~~~~~~~~~~

# Building the basic logit model for the outcome babedeath_swf

model_death <- glm(babedeath_swf~.-babeadmit_swf-babeoutcome_swf-studyid_adm, 
                   data = dat_impute, 
                   family = binomial(link = 'logit'))

# Extracting coefficients
# Removing the Intercept
coeffs_death = coefficients(model_death)[2: length(coefficients(model_death))]

# debugging for NA values in our data
# i = colnames(dat_baby_pred_death)[1]
# temp <- dat_baby_pred_death %>% count(meconium_del) 
# unique(dat_baby_pred_death$fetaldemise_adm)
# purrr::discard(dat_baby_pred_death, ~n_distinct(.) == 1)

# Building the basic logit model for the outcome babeadmit_swf

model_admit <- glm(babeadmit_swf~.-babedeath_swf-babeoutcome_swf-studyid_adm, 
                   data = dat_impute, 
                   family = binomial(link = 'logit'))

coeffs_admit = coefficients(model_admit)[2: length(coefficients(model_admit))]

# Building the basic logit model for the combined outcome

model_comb <- glm(babeoutcome_swf~.-babeadmit_swf-babedeath_swf-studyid_adm, 
                   data = dat_impute, 
                   family = binomial(link = 'logit'))

coeffs_comb <- coefficients(model_comb)[2: length(coefficients(model_comb))]

# Note, coefficients for rescusoxy_del_newResuscitation without oxygen, and
#                         gestation_period_cat_newOver 40 weeks are returning NA values
# Redundant (linearly dependent) variables
# Removing NA values from our coeffs for further analysis

coeffs_death <- coeffs_death[!(is.na(coeffs_death))]
coeffs_admit <- coeffs_admit[!(is.na(coeffs_admit))]
coeffs_comb <- coeffs_comb[!(is.na(coeffs_comb))]

## ~~~~~~~~~~~~~~~~~~~~~
## Analysis ####
## ~~~~~~~~~~~~~~~~~~~~~

# Checking how many variables have coefficients of the same sign
# 92 predictors have the same sign
same_dir_logit = ((coeffs_death*coeffs_admit > 0) & 
               (coeffs_admit*coeffs_comb > 0))
cols_same_dir_logit = names(coeffs_death)[same_dir_logit]
length(cols_same_dir_logit)

# Checking how many variables have coefficients within n factors
# 53 predictors have coefficients within 5 factors of each other
fact = 5
diff_factor = ((1/fact) < (coeffs_death/coeffs_admit) & (coeffs_death/coeffs_admit) < fact) & 
  ((1/fact) < (coeffs_admit/coeffs_comb) & (coeffs_admit/coeffs_comb) < fact) &
  ((1/fact) < (coeffs_comb/coeffs_death) & (coeffs_comb/coeffs_death) < fact)

cols_diff_factor = names(coeffs_death)[diff_factor]
length(cols_diff_factor)
cols_diff_factor

# Note that if coefficients are within n factor approximation, they are automatically same sign
# No need to take intersection

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ANALYZING CORRELATIONS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~
## Calculating Correlations ####
## ~~~~~~~~~~~~~~~~~~~~~

# Identifying the columns which are factors in order to unclass them
factor_cols <- dat_impute[, sapply(dat_impute, is.factor)]

# Converting the dataframe into a numeric type for the cor() function
# First unclassing factors, typescasting as numeric and converting back from list to df

dat_corr <- dat_impute %>%
  mutate(across(colnames(factor_cols), ~unclass(.))) %>%
  sapply(., as.numeric) %>%
  data.frame(.)

# Finding the correlation vectors for each of the three outcomes

death_corr <- cor(dat_corr[predictors[predictors %in% colnames(dat_impute)]], 
                  dat_corr$babedeath_swf)

admit_corr <- cor(dat_corr[predictors[predictors %in% colnames(dat_impute)]], 
                  dat_corr$babeadmit_swf)

comb_corr <- cor(dat_corr[predictors[predictors %in% colnames(dat_impute)]], 
                  dat_corr$babeoutcome_swf)

## ~~~~~~~~~~~~~~~~~~~~~
## Analysis ####
## ~~~~~~~~~~~~~~~~~~~~~

# Finding which variable have the same signed correlation
# 59 predictors with the same sign
same_sign = ((death_corr*admit_corr > 0) & 
               (admit_corr*comb_corr > 0))
cols_samesign = predictors[predictors %in% colnames(dat_impute)][same_sign]
length(cols_samesign)

# Finding which of these variables have correlation between 0.5 of each other
# 140 predictors within 0.5
diff_0.5 = ((abs(death_corr-admit_corr) < 0.5) & 
              (abs(admit_corr-comb_corr) < 0.5) &
              (abs(death_corr-comb_corr) < 0.5))
cols_diff_0.5 = predictors[predictors %in% colnames(dat_impute)][diff_0.5]
length(cols_diff_0.5)

# Taking intersection
# 59, i.e. the columns with same sign
cols_both = intersect(cols_samesign, cols_diff_0.5)
length(cols_both)
cols_both

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VISUALIZATION      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~
## Finding form indices ####
## ~~~~~~~~~~~~~~~~~~~~~

#Finding the start indices for the different forms

forms = c('admission_subject_vars',
          'admission_vars',
          'pregnancy_history_vars',
          'conds_during_pregrancy_vars',
          'pregnancy_history2_vars',
          'anc_vars',
          'ses_vars',
          'delivery_maternal_vars',
          'delivery_neonatal_vars',
          'discharge_maternal_vars',
          'discharge_neonatal_vars')

form_index_logit = c(1, 9, 31, 51, 71, 82, 98, 113, 146, 155, 180)
form_index_logit <- c(form_index_logit, length(coeffs_admit))

form_index_corr <- c(length(admission_subject_vars[admission_subject_vars %in% colnames(dat_impute)]),
                    length(admission_vars[admission_vars %in% colnames(dat_impute)]),
                    length(pregnancy_history_vars[pregnancy_history_vars %in% colnames(dat_impute)]),
                    length(ses_vars[ses_vars %in% colnames(dat_impute)]),
                    length(delivery_maternal_vars[delivery_maternal_vars %in% colnames(dat_impute)]),
                    length(delivery_neonatal_vars[delivery_neonatal_vars %in% colnames(dat_impute)]),
                    length(discharge_maternal_vars[discharge_maternal_vars %in% colnames(dat_impute)]),
                    length(discharge_neonatal_vars[discharge_neonatal_vars %in% colnames(dat_impute)]) )

form_index_corr <- c(1, 1+cumsum(form_index_corr)[-length(form_index_corr)])

form_index_corr <- c(1, 8, 25, 42, 62, 78, 87, 102, 111, 131)
forms_corr <- c('admission_subject_vars',
                'admission_vars',
                'pregnancy_history_vars',
                'conds_during_pregrancy_vars',
                'pregnancy_history2_vars',
                'ses_vars',
                'delivery_maternal_vars',
                'delivery_neonatal_vars',
                'discharge_maternal_vars',
                'discharge_neonatal_vars')
# length(death_corr)
# form_index_corr

## ~~~~~~~~~~~~~~~~~~~~~
## Visualization of coeffs (Basic) ####
## ~~~~~~~~~~~~~~~~~~~~~

# Comparing means of abs values of different coefficients
df_coeff_means <- data.frame(cat = c('death', 'admit', 'combined'), 
                             vals = c(mean(abs(coeffs_death)), mean(abs(coeffs_admit)), mean(abs(coeffs_comb))))

#Death appears almost double, admit and combined are more comparable.
ggplot(df_coeff_means, aes(x = cat, y = vals, fill = cat)) +
  geom_bar(stat = 'identity')

# Plotting the coefficients of all models (all outcomes) along an axis

df_coeff_lines <- data.frame(time = seq(1, length(coeffs_admit)),
                             death = coeffs_death,
                             admit = coeffs_admit,
                             combined = coeffs_comb)

df_coeff_lines %>%
  ggplot( aes(x=time, y=death)) +
  geom_line(color = '#245e91') +
  geom_point(shape = 21, color = 'black', fill = '#6fa8dc', size = 2) +
  geom_line( aes(x=time, y=admit), color = '#a94949') +
  geom_point( aes(x=time, y=admit), shape = 21, color = 'black', fill = '#e06666', size = 2) +
  geom_line( aes(x=time, y=combined), color = '#42911f') +
  geom_point( aes(x=time, y=combined), shape = 21, color = 'black', fill = '#73c052', size = 2)
  

# Comparing the coefficients of all 3 outcome - logit models over predictors (linear time)

plot(df_coeff_lines$time, df_coeff_lines$death, type = 'h', col = '#6fa8dc', lwd = 2) +
  lines(df_coeff_lines$time, df_coeff_lines$admit, type = 'h', col = '#e06666', lwd = 2) +
  lines(df_coeff_lines$time, df_coeff_lines$combined, type = 'h', col = '#73c052', lwd = 2)
# Cofficients for death appears more erratic, more outliers (lesser incidence)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Grouped Bar plots for coeffs####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plotting the percentages/ relative comparisons of the three coefficients for variables given start/end index
# Takes care of scaling, etc easier to view and understand

# Taking a start and end index
indstart = 3
indend = 20
indint = indend-indstart+1

# Building dataframe
outcome = c(rep('death', indint), rep('admit', indint), rep('combined', indint))
coeffs = c(coeffs_death[indstart:indend], coeffs_admit[indstart:indend], coeffs_comb[indstart:indend])
labs = c(names(coeffs_death)[indstart:indend], names(coeffs_admit)[indstart:indend], names(coeffs_comb)[indstart:indend])

df_plot = data.frame(outcome, coeffs, labs)

# Plot with flipped axes to read labels
ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  theme_light()

# Grouped Bar Plot
ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip()


# Iterating over the different forms and saving the barplots for each form
# Adding labels, titles and theme to the graphs

for (i in 1:(length(forms)-1)) {
  indstart = as.integer(form_index_logit[i])
  indend = as.integer(form_index_logit[i+1])-1
  indint = indend - indstart + 1
  
  outcome = c(rep('death', indint), rep('admit', indint), rep('combined', indint))
  coeffs = c(coeffs_death[indstart:indend], coeffs_admit[indstart:indend], coeffs_comb[indstart:indend])
  labs = c(names(coeffs_death)[indstart:indend], names(coeffs_admit)[indstart:indend], names(coeffs_comb)[indstart:indend])
  
  df_plot = data.frame(outcome, coeffs, labs)
  
  plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
    geom_bar(stat="identity", position='fill') +
    coord_flip() +
    labs(colours = 'Outcome') +
    labs(title = forms[i], subtitle = 'Relative proportions of logit coefficients') +
    labs(y = 'Coefficient proportions') + 
    labs(x = 'Variables') + 
    theme_light()
  
  ggplot2::ggsave(paste(forms[i],'rel.png'), plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Relative_logit', device = 'png', width=1920/72, height = 1080/72, dpi = 72)
  
  plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() +
    labs(colours = 'Outcome') +
    labs(title = forms[i], subtitle = 'Values of logit coefficients') +
    labs(x = 'Variables') + 
    labs(y = 'Coefficients') + 
    theme_light()
  
  ggplot2::ggsave(paste(forms[i],'abs.png'), plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Absolute_logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Grouped Bar plots for corrln ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plotting the percentages/relative comparisons of the three correlations for variables given start/end index

# Taking indices
indstart = 1
indend = 50
indint = indend-indstart+1

#Creating a dataframe
outcome = c(rep('death', indint), rep('admit', indint), rep('combined', indint))
coeffs = c(death_corr[indstart:indend], admit_corr[indstart:indend], comb_corr[indstart:indend])
labs = rep(predictors[predictors %in% colnames(dat_impute)][indstart:indend])

df_plot = data.frame(outcome, coeffs, labs)

# Relative proportions plot
ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip()

# Grouped correlations plot
ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CONFIDENCE INTERVALS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Using the confint function to find CIs for coefficients of logit for death based on profile likelihood
# conf_death <- confint(model_death)
#Waiting for profiling to be done... Takes time (approx 25 mins)

# conf_admit <- confint(model_admit)
# conf_comb <- confint(model_comb)

#Using the .default parameter for Wald CIs
#Normality assumption, but faster and more efficient
conf_wald_death <- confint.default(model_death)
conf_wald_admit <- confint.default(model_admit)
conf_wald_comb <- confint.default(model_comb)

conf_wald_death <- na.omit(conf_wald_death)
conf_wald_admit <- na.omit(conf_wald_admit)
conf_wald_comb <- na.omit(conf_wald_comb)

dim(conf_wald_admit)
dim(conf_wald_death)
dim(conf_wald_comb)


admit_start <- conf_wald_admit[,1]
admit_end <- conf_wald_admit[,2]
death_start <- conf_wald_death[,1]
death_end <- conf_wald_death[,2]


overlap = c()
for (i in 1:length(admit_start)) {
  if (admit_start[i] < death_start[i]) {
    overlap = c(overlap, ifelse(admit_end[i]-death_start[i] > 0, admit_end[i]-death_start[i], 0))
  }
  if (admit_start[i] >= death_start[i]) {
    overlap = c(overlap, ifelse(death_end[i]-admit_start[i] > 0, death_end[i]-admit_start[i], 0))
  }
}

length(overlap) # 183 total predictors
sum(overlap>0) # 180 have intersection in CIs

# 163 by taking > min of both
sum(overlap> 1*(((death_end - death_start)>(admit_end - admit_start))*(admit_end - admit_start) + ((death_end - death_start)<=(admit_end - admit_start))*(death_end - death_start)))
sum(overlap> 0.5*(death_end - death_start)) # 114 have overlap > 0.5 times death CI
sum(overlap > 1*(admit_end - admit_start)) # 161 have overlap > admit CI


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CHECKING FOR SIGNIFICANCE IN LOGISITIC      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Checking individual significance ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating individual datasets containg each outcome and the predictor vars for fitting

dat_baby_pred_death <- dat_impute[c(predictors, "babedeath_swf")]
dat_baby_pred_admit <- dat_impute[c(predictors, "babeadmit_swf")]
dat_baby_pred_comb <- dat_impute[c(predictors, "babeoutcome_swf")]

# Checking statistical significance for each of the individual predictor vairables
# Filtering those with p-values <= 0.1 in individual logit models

index = c()
for (i in 1: (length(colnames(dat_baby_pred_death))-1)) {
  model_death_new <- glm(babedeath_swf~., 
                     data = dat_baby_pred_death[, c(i, length(colnames(dat_baby_pred_death)))], 
                     family = binomial(link = 'logit'))
  if (summary(model_death_new)$coefficients[2,4] <= 0.1) {
    index = c(index, TRUE)
  }
  else {
    index = c(index, FALSE)
  }
}

# 26 predictors out of 140 selected 
sum(index)
length(colnames(dat_baby_pred_death))-1

# Modifying our data to include only significant predictors
index = c(index, TRUE)
dat_baby_pred_death <- dat_baby_pred_death[, index]
#dim(dat_baby_pred_death)

# Building the final Logit model for outcome death and its summary

model_death_final = glm(babedeath_swf~., 
                        data = dat_baby_pred_death, 
                        family = binomial(link = 'logit'))
summary(model_death_final)


# Proceeding similarly for readmit outcome 

index = c()
for (i in 1: (length(colnames(dat_baby_pred_admit))-1)) {
  model_admit_new <- glm(babeadmit_swf~., 
                         data = dat_baby_pred_admit[, c(i, length(colnames(dat_baby_pred_admit)))], 
                         family = binomial(link = 'logit'))
  if (summary(model_admit_new)$coefficients[2,4] <= 0.1) {
    index = c(index, TRUE)
  }
  else {
    index = c(index, FALSE)
  }
}

# 44 out of 140 predictors selected
sum(index)
length(colnames(dat_baby_pred_admit))-1

index = c(index, TRUE)
dat_baby_pred_admit <- dat_baby_pred_admit[, index]
#dim(dat_baby_pred_death)

# Final admit ouctome logit model
model_admit_final = glm(babeadmit_swf~., 
                        data = dat_baby_pred_admit, 
                        family = binomial(link = 'logit'))
summary(model_admit_final)

# Proceeding similarly for the combined outcome

index = c()
for (i in 1: (length(colnames(dat_baby_pred_comb))-1)) {
  model_comb_new <- glm(babeoutcome_swf~., 
                         data = dat_baby_pred_comb[, c(i, length(colnames(dat_baby_pred_comb)))], 
                         family = binomial(link = 'logit'))
  if (summary(model_comb_new)$coefficients[2,4] <= 0.1) {
    index = c(index, TRUE)
  }
  else {
    index = c(index, FALSE)
  }
}

# 43 out of 140 predictors selected
sum(index)
length(colnames(dat_baby_pred_comb))-1

index = c(index, TRUE)
dat_baby_pred_comb <- dat_baby_pred_comb[, index]
#dim(dat_baby_pred_death)

# Final combined outcome model
model_comb_final <- glm(babeoutcome_swf~., 
                        data = dat_baby_pred_comb, 
                        family = binomial(link = 'logit'))
summary(model_comb_final)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Building the final logistic ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Finding the columns which are significant in all 3 models
cols_final <- intersect(intersect(colnames(dat_baby_pred_admit), colnames(dat_baby_pred_death)), colnames(dat_baby_pred_comb))

# 37 final columns
length(cols_final)

# Final logistic models for comparing coefficients

model_death_final <- glm(babedeath_swf~., 
                        data = dat_baby_pred_death[, c(cols_final, "babedeath_swf")], 
                        family = binomial(link = 'logit'))

model_admit_final <- glm(babeadmit_swf~., 
                       data = dat_baby_pred_admit[, c(cols_final, "babeadmit_swf")], 
                       family = binomial(link = 'logit'))

model_comb_final <- glm(babeoutcome_swf~., 
                        data = dat_baby_pred_comb[, c(cols_final, "babeoutcome_swf")], 
                        family = binomial(link = 'logit'))

# Finding coefficients of these models

coeffs_death_final <- coef(model_death_final)[2: length(coef(model_death_final))]
coeffs_admit_final <- coef(model_admit_final)[2: length(coef(model_admit_final))]
coeffs_comb_final <- coef(model_comb_final)[2: length(coef(model_comb_final))]


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data Visualization ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Taking a start and end index
indstart = 1
indend = length(coeffs_admit_final)
indint = indend-indstart+1

# Building dataframe
outcome = c(rep('death', indint), rep('admit', indint), rep('combined', indint))
coeffs = c(coeffs_death_final[indstart:indend], 
           coeffs_admit_final[indstart:indend], 
           coeffs_comb_final[indstart:indend])

labs = c(names(coeffs_death_final)[indstart:indend], 
         names(coeffs_admit_final)[indstart:indend], 
         names(coeffs_comb_final)[indstart:indend])

df_plot = data.frame(outcome, coeffs, labs)

# Plot with flipped axes to read labels
ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  theme_light()

# Grouped Bar Plot
ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UNIVARIATE LOGISTIC      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initializing ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating individual datasets containg each outcome and the predictor vars for fitting

dat_baby_pred_death <- dat_impute[c(predictors, "babedeath_swf")]
dat_baby_pred_admit <- dat_impute[c(predictors, "babeadmit_swf")]
dat_baby_pred_comb <- dat_impute[c(predictors, "babeoutcome_swf")]

# Initializing the coefficient, p-value, confidence interval vectors
coeffs_death_univ <- c()
death_pvals_univ <- c()
death_confint_start <- c()
death_confint_end <- c()

coeffs_admit_univ <- c()
admit_pvals_univ <- c()
admit_confint_start <- c()
admit_confint_end <- c()

coeffs_comb_univ <- c()
comb_pvals_univ <- c()
comb_confint_start <- c()
comb_confint_end <- c()

predictor_names <- c()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculating coeffs, p-vals, CIs ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Iterating over each predictor to fit a univariable logistic model for death
for (i in 1: (length(colnames(dat_baby_pred_death))-1)) {
  model_death_temp <- glm(babedeath_swf~., 
                         data = dat_baby_pred_death[, c(i, length(colnames(dat_baby_pred_death)))], 
                         family = binomial(link = 'logit'))
  
  # Storing coefficients (- the Intercept), predictor names
  for (j in 2 : length(coef(model_death_temp))) {
    coeffs_death_univ <- c(coeffs_death_univ, coef(model_death_temp)[j])
    predictor_names <- c(predictor_names, names(coef(model_death_temp))[j])
  }
  
  # Storing p-vals, conf ints
  death_pvals_univ <- c(death_pvals_univ, summary(model_death_temp)$coefficients[-1,4])
  death_confint_start <- c(death_confint_start, confint(model_death_temp)[-1, 1])
  death_confint_end <- c(death_confint_end, confint(model_death_temp)[-1, 2])
}

# check all their lengths are the same - 191 predictors
length(coeffs_death_univ)
length(death_pvals_univ)
length(death_confint_start)
length(death_confint_end)
length(predictor_names)


# Proceeding similarly for the outcome readmission
for (i in 1: (length(colnames(dat_baby_pred_admit))-1)) {
  model_admit_temp <- glm(babeadmit_swf~., 
                          data = dat_baby_pred_admit[, c(i, length(colnames(dat_baby_pred_admit)))], 
                          family = binomial(link = 'logit'))
  
  # for (j in 2 : length(coef(model_admit_temp))) {
  coeffs_admit_univ <- c(coeffs_admit_univ, coef(model_admit_temp)[-1])
    # predictor_names <- c(predictor_names, names(coef(model_death_temp))[j])
  # }
  
  admit_pvals_univ <- c(admit_pvals_univ, summary(model_admit_temp)$coefficients[-1,4])
  conf_temp <- confint(model_admit_temp)
  admit_confint_start <- c(admit_confint_start, conf_temp[-1, 1])
  admit_confint_end <- c(admit_confint_end, conf_temp[-1, 2])
}

# Again, all of length 191
length(coeffs_admit_univ)
length(admit_pvals_univ)
length(admit_confint_start)
length(admit_confint_end)
length(predictor_names)


# Proceeding similarly for the combined outcome
for (i in 1: (length(colnames(dat_baby_pred_comb))-1)) {
  model_comb_temp <- glm(babeoutcome_swf~., 
                          data = dat_baby_pred_comb[, c(i, length(colnames(dat_baby_pred_comb)))], 
                          family = binomial(link = 'logit'))
  
  # for (j in 2 : length(coef(model_admit_temp))) {
  coeffs_comb_univ <- c(coeffs_comb_univ, coef(model_comb_temp)[-1])
  # predictor_names <- c(predictor_names, names(coef(model_death_temp))[j])
  # }
  
  comb_pvals_univ <- c(comb_pvals_univ, summary(model_comb_temp)$coefficients[-1,4])
  conf_temp <- confint(model_comb_temp)
  comb_confint_start <- c(comb_confint_start, conf_temp[-1, 1])
  comb_confint_end <- c(comb_confint_end, conf_temp[-1, 2])
}

length(coeffs_comb_univ)
length(comb_pvals_univ)
length(comb_confint_start)
length(comb_confint_end)
length(predictor_names)

# Fixing COnfidence Interval names

names(death_confint_start) <- names(coeffs_death_univ)
names(death_confint_end) <- names(coeffs_death_univ)
names(admit_confint_start) <- names(coeffs_admit_univ)
names(admit_confint_end) <- names(coeffs_admit_univ)
names(comb_confint_start) <- names(coeffs_comb_univ)
names(comb_confint_end) <- names(coeffs_comb_univ)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Odds Ratios ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

odds_death <- exp(coeffs_death_univ)
odds_admit <- exp(coeffs_admit_univ)
odds_comb <- exp(coeffs_comb_univ)

# CIs

death_oddsCI_start <- exp(death_confint_start)
death_oddsCI_end <- exp(death_confint_end)
admit_oddsCI_start <- exp(admit_confint_start)
admit_oddsCI_end <- exp(admit_confint_end)
comb_oddsCI_start <- exp(comb_confint_start)
comb_oddsCI_end <- exp(comb_confint_end)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FINAL COEFFICIENTS DATA VIZ.      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Form indices (repeated) ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initializing the instruments/categories
forms = c('admission_subject_vars',
          'admission_vars',
          'pregnancy_history_vars',
          'conds_during_pregrancy_vars',
          'pregnancy_history2_vars',
          'anc_vars',
          'ses_vars',
          'delivery_maternal_vars',
          'delivery_neonatal_vars',
          'discharge_maternal_vars',
          'discharge_neonatal_vars')

# Manually find and enter the indices for the instruments
form_index_logit = c(1, 8, 30, 51, 71, 82, 91, 106, 131, 141, 166)
form_index_logit <- c(form_index_logit, length(coeffs_admit_univ)+1)

# Checking how many variables have coefficients of the same sign

same_dir_logit = (coeffs_death_univ*coeffs_admit_univ > 0)
cols_same_dir_logit = predictor_names[same_dir_logit]
length(cols_same_dir_logit)
# 78 predictors have logit coefficients in the same dir

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Grouped Bar plots for Logit coeffs ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plotting Grouped Barplots for each of the instruments, and saving them

# Variables having coefficients for death and readmit in the same direction
for (i in 1:(length(forms))) {
  
  # Working out relevant indices and list of columns with same dir 
  indstart = as.integer(form_index_logit[i])
  indend = as.integer(form_index_logit[i+1])-1
  cols_temp = predictor_names[indstart:indend]
  cols_temp <- cols_temp[cols_temp %in% cols_same_dir_logit]
  indint = length(cols_temp)
  
  # Creating the dataframe for the plot
  outcome = c(rep('death', indint), rep('admit', indint), rep('combined', indint))
  coeffs = c(coeffs_death_univ[cols_temp], coeffs_admit_univ[cols_temp], coeffs_comb_univ[cols_temp])
  #labs = c(names(coeffs_death_univ)[cols_temp], names(coeffs_admit)[cols_temp], names(coeffs_comb)[cols_temp])
  labs = rep(cols_temp, 3)
  
  df_plot = data.frame(outcome, coeffs, labs)
  
  # Plot 1: Relative percentages of the different coefficients
  plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
    geom_bar(stat="identity", position='fill') +
    coord_flip() +
    labs(colours = 'Outcome') +
    labs(title = forms[i], subtitle = 'Relative proportions of logit coefficients') +
    labs(x = 'Coefficient percentages') + 
    labs(y = 'Variables') + 
    theme_light()
  
  ggplot2::ggsave(paste(forms[i],'samedir_univ_rel.png'), plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Univariate/Relative_logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)
  
  # Plot 2: Absolute coefficients (grouped)
  plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() +
    labs(colours = 'Outcome') +
    labs(title = forms[i], subtitle = 'Values of logit coefficients') +
    labs(x = 'Coefficients') + 
    labs(y = 'Variables') + 
    theme_light()
  
  ggplot2::ggsave(paste(forms[i],'samedir_univ_abs.png'), plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Univariate/Absolute_logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)
}


# Variables having coefficients for death and readmit in the opposite direction

for (i in 1:(length(forms))) {
  indstart = as.integer(form_index_logit[i])
  indend = as.integer(form_index_logit[i+1])-1
  cols_temp = predictor_names[indstart:indend]
  cols_temp <- cols_temp[!(cols_temp %in% cols_same_dir_logit)]
  indint = length(cols_temp)
  
  outcome = c(rep('death', indint), rep('admit', indint), rep('combined', indint))
  coeffs = c(coeffs_death_univ[cols_temp], coeffs_admit_univ[cols_temp], coeffs_comb_univ[cols_temp])
  #labs = c(names(coeffs_death_univ)[cols_temp], names(coeffs_admit)[cols_temp], names(coeffs_comb)[cols_temp])
  labs = rep(cols_temp, 3)
  
  df_plot = data.frame(outcome, coeffs, labs)
  
  plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
    geom_bar(stat="identity", position='fill') +
    coord_flip() +
    labs(colours = 'Outcome') +
    labs(title = forms[i], subtitle = 'Relative proportions of logit coefficients') +
    labs(x = 'Coefficient percentages') + 
    labs(y = 'Variables') + 
    theme_light()
  
  ggplot2::ggsave(paste(forms[i],'diffdir_univ_rel.png'), plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Univariate/Relative_logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)
  
  plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() +
    labs(colours = 'Outcome') +
    labs(title = forms[i], subtitle = 'Values of logit coefficients') +
    labs(x = 'Coefficients') + 
    labs(y = 'Variables') + 
    theme_light()
  
  ggplot2::ggsave(paste(forms[i],'diffdir_univ_abs.png'), plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Univariate/Absolute_logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Grouped Bar plots for odds ratios ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

same_dir_odds = ((odds_death>1) & (coeffs_admit_univ>1)) | ((odds_death<1) & (coeffs_admit_univ<1))
cols_same_dir_odds = predictor_names[same_dir_odds]
length(cols_same_dir_odds)

# Plotting Grouped Barplots for each of the instruments, and saving them

# Variables having coefficients for death and readmit in the same direction
for (i in 1:(length(forms))) {
  
  # Working out relevant indices and list of columns with same dir 
  indstart = as.integer(form_index_logit[i])
  indend = as.integer(form_index_logit[i+1])-1
  cols_temp = predictor_names[indstart:indend]
  cols_temp <- cols_temp[cols_temp %in% cols_same_dir_odds]
  indint = length(cols_temp)
  
  # Creating the dataframe for the plot
  outcome = c(rep('death', indint), rep('admit', indint), rep('combined', indint))
  coeffs = c(odds_death[cols_temp], odds_admit[cols_temp], odds_comb[cols_temp])
  #labs = c(names(coeffs_death_univ)[cols_temp], names(coeffs_admit)[cols_temp], names(coeffs_comb)[cols_temp])
  labs = rep(cols_temp, 3)
  
  df_plot = data.frame(outcome, coeffs, labs)
  
  # Plot 1: Relative percentages of the different coefficients
  plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
    geom_bar(stat="identity", position='fill') +
    coord_flip() +
    labs(colours = 'Outcome') +
    labs(title = forms[i], subtitle = 'Relative proportions of Odds Ratios') +
    labs(x = 'Variables') + 
    labs(y = 'Odds Ratio Proportions') + 
    theme_dark()
  
  ggplot2::ggsave(paste(forms[i],'samedir_odds_rel.png'), plot = plot1, path = 'Plots/Univariate/Relative_odds', device = 'png', width=860/72, height = 540/72, dpi = 72)
  
  # Plot 2: Absolute coefficients (grouped)
  plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() +
    labs(colours = 'Outcome') +
    labs(title = forms[i], subtitle = 'Values of Odds Ratios') +
    labs(x = 'Variables') + 
    labs(y = 'Odds Ratios') + 
    theme_dark()
  
  ggplot2::ggsave(paste(forms[i],'samedir_odds_abs.png'), plot = plot2, path = 'Plots/Univariate/Absolute_odds', device = 'png', width=860/72, height = 540/72, dpi = 72)
}


# Variables having coefficients for death and readmit in the opposite direction

for (i in 1:(length(forms))) {
  indstart = as.integer(form_index_logit[i])
  indend = as.integer(form_index_logit[i+1])-1
  cols_temp = predictor_names[indstart:indend]
  cols_temp <- cols_temp[!(cols_temp %in% cols_same_dir_odds)]
  indint = length(cols_temp)
  
  outcome = c(rep('death', indint), rep('admit', indint), rep('combined', indint))
  coeffs = c(odds_death[cols_temp], odds_admit[cols_temp], odds_comb[cols_temp])
  #labs = c(names(coeffs_death_univ)[cols_temp], names(coeffs_admit)[cols_temp], names(coeffs_comb)[cols_temp])
  labs = rep(cols_temp, 3)
  
  df_plot = data.frame(outcome, coeffs, labs)
  
  plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
    geom_bar(stat="identity", position='fill') +
    coord_flip() +
    labs(colours = 'Outcome') +
    labs(title = forms[i], subtitle = 'Relative proportions of Odds Ratios') +
    labs(x = 'Coefficient percentages') + 
    labs(y = 'Variables') + 
    theme_dark()
  
  ggplot2::ggsave(paste(forms[i],'diffdir_odds_rel.png'), plot = plot1, path = 'Plots/Univariate/Relative_odds', device = 'png', width=860/72, height = 540/72, dpi = 72)
  
  plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() +
    labs(colours = 'Outcome') +
    labs(title = forms[i], subtitle = 'Values of Odds Ratios') +
    labs(x = 'Variables') + 
    labs(y = 'Odds Ratios') + 
    theme_dark()
  
  ggplot2::ggsave(paste(forms[i],'diffdir_odds_abs.png'), plot = plot2, path = 'Plots/Univariate/Absolute_odds', device = 'png', width=860/72, height = 540/72, dpi = 72)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting logit coefficients over time ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating the dataframe
df_coeff_lines <- data.frame(time = seq(1, length(coeffs_admit_univ)),
                             death = coeffs_death_univ,
                             admit = coeffs_admit_univ,
                             combined = coeffs_comb_univ)

# Plotting
df_coeff_lines %>%
  ggplot( aes(x=time, y=death)) +
  geom_line(color = '#245e91') +
  geom_point(shape = 21, color = 'black', fill = '#6fa8dc', size = 2) +
  geom_line( aes(x=time, y=admit), color = '#a94949') +
  geom_point( aes(x=time, y=admit), shape = 21, color = 'black', fill = '#e06666', size = 2) +
  geom_line( aes(x=time, y=combined), color = '#42911f') +
  geom_point( aes(x=time, y=combined), shape = 21, color = 'black', fill = '#73c052', size = 2)


# Comparing the coefficients of all 3 outcome - logit models over predictors (linear time)

plot(df_coeff_lines$time, df_coeff_lines$death, type = 'h', col = '#6fa8dc', lwd = 2) +
  lines(df_coeff_lines$time, df_coeff_lines$admit, type = 'h', col = '#e06666', lwd = 2) +
  lines(df_coeff_lines$time, df_coeff_lines$combined, type = 'h', col = '#73c052', lwd = 2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting Odds Ratio CIs ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# First plotting the confidence intervals for the first 20 predictors

# Create dataframe
df_odds <- data.frame(death = odds_death[1:20],
                      admit = odds_admit[1:20],
                      comb = odds_comb[1:20],
                      death_CI_start = death_oddsCI_start[1:20],
                      death_CI_end = death_oddsCI_end[1:20],
                      admit_CI_start = admit_oddsCI_start[1:20],
                      admit_CI_end = admit_oddsCI_end[1:20],
                      comb_CI_start = comb_oddsCI_start[1:20],
                      comb_CI_end = comb_oddsCI_end[1:20],
                      pred = predictor_names[1:20])

# Plot conf intervals for death
df_odds %>% ggplot( aes(x = pred, y = death)) +
  geom_point(shape = 21, color = 'black', fill = '#245e91', size = 2) +
  geom_errorbar(aes(ymin = death_CI_start, ymax = death_CI_end), width = 0.1, color = '#6fa8dc') +
  coord_flip() +
  theme_dark()

# Plot conf intervals for admit
df_odds %>% ggplot( aes(x = pred, y = admit)) +
  geom_point(shape = 21, color = 'black', fill = '#a94949', size = 2) +
  geom_errorbar(aes(ymin = admit_CI_start, ymax = admit_CI_end), width = 0.1, color = '#e06666') +
  coord_flip() +
  theme_dark()

# Plot conf intervals for comb
df_odds %>% ggplot( aes(x = pred, y = comb)) +
  geom_point(shape = 21, color = 'black', fill = '#42911f', size = 2) +
  geom_errorbar(aes(ymin = comb_CI_start, ymax = comb_CI_end), width = 0.1, color = '#73c052') +
  coord_flip() +
  theme_dark()

# df_odds <- data.frame(odds = c(odds_death, odds_admit, odds_comb),
#                       CI_start = c(death_oddsCI_start, admit_oddsCI_start, comb_oddsCI_start),
#                       CI_end = c(death_oddsCI_end, admit_oddsCI_end, comb_oddsCI_end),
#                       pred = rep(predictor_names, 3),
#                       outcome = c(rep('death', length(odds_death)), rep('admit', length(odds_admit)), rep('comb', length(odds_comb))))

# Iterating over each form/instrument and generating confidence interval plots

# For Odds Ratios
for(i in 1:(length(forms))) {
  # temp_index = c(form_index_logit[i]:(form_index_logit[i+1] -1),
  #                    (length(form_index_logit)+form_index_logit[i]):(length(form_index_logit)+(form_index_logit[i]-1)),
  #                    (2*length(form_index_logit)+form_index_logit[i]):(2*length(form_index_logit)+(form_index_logit[i]-1)))
  
  # Finding indices
  indstart = as.integer(form_index_logit[i])
  indend = as.integer(form_index_logit[i+1])-1
  cols_temp = predictor_names[indstart:indend]
  indint = length(cols_temp)
  
  # Creating dataframe
  df_odds <- data.frame(odds = c(odds_death[cols_temp], odds_admit[cols_temp], odds_comb[cols_temp]),
                        CI_start = c(death_oddsCI_start[cols_temp], admit_oddsCI_start[cols_temp], comb_oddsCI_start[cols_temp]),
                        CI_end = c(death_oddsCI_end[cols_temp], admit_oddsCI_end[cols_temp], comb_oddsCI_end[cols_temp]),
                        pred = rep(cols_temp, 3),
                        outcome = c(rep('death', indint), rep('admit', indint), rep('comb', indint)))
  
  # Plotting CIs
  plot1 <- df_odds %>% ggplot( aes(x = pred, y = odds, colour = outcome, group = outcome)) +
    geom_point()+
    geom_errorbar(aes(ymin = CI_start, ymax = CI_end)) +
    coord_flip()+
    theme_dark()
  
  #Saving the plot
  ggplot2::ggsave(paste(forms[i],'odds_withCI.png'), plot = plot1, path = 'Plots/Univariate/Confidence Intervals/Odds Ratio', device = 'png', width=860/72, height = 540/72, dpi = 72)
  
}

# df_coeffs <- data.frame(coeffs = c(coeffs_death_univ, coeffs_admit_univ, coeffs_comb_univ),
#                       CI_start = c(death_confint_start, admit_confint_start, comb_confint_start),
#                       CI_end = c(death_confint_end, admit_confint_end, comb_confint_end),
#                       pred = rep(predictor_names, 3),
#                       outcome = c(rep('death', length(coeffs_death_univ)), rep('admit', length(coeffs_admit_univ)), rep('comb', length(coeffs_comb_univ))))


# For Logit coefficients
for(i in 1:(length(forms))) {
  
  # Finding indices
  indstart = as.integer(form_index_logit[i])
  indend = as.integer(form_index_logit[i+1])-1
  cols_temp = predictor_names[indstart:indend]
  indint = length(cols_temp)
  
  # Creating Dataframe
  df_coeffs <- data.frame(coeffs = c(coeffs_death_univ[cols_temp], coeffs_admit_univ[cols_temp], coeffs_comb_univ[cols_temp]),
                        CI_start = c(death_confint_start[cols_temp], admit_confint_start[cols_temp], comb_confint_start[cols_temp]),
                        CI_end = c(death_confint_end[cols_temp], admit_confint_end[cols_temp], comb_confint_end[cols_temp]),
                        pred = rep(cols_temp, 3),
                        outcome = c(rep('death', indint), rep('admit', indint), rep('comb', indint)))
  
  # Plotting CIs
  plot1 <- df_coeffs %>% ggplot( aes(x = pred, y = coeffs, colour = outcome, group = outcome)) +
    geom_point()+
    geom_errorbar(aes(ymin = CI_start, ymax = CI_end)) +
    coord_flip() +
    theme_light()
  
  # Saving the plot
  ggplot2::ggsave(paste(forms[i],'coeffs_withCI.png'), plot = plot1, path = 'Plots/Univariate/Confidence Intervals/Logit Coefficients', device = 'png', width=860/72, height = 540/72, dpi = 72)
  
}

# df_debug <- data.frame(pred = rep(c('a', 'b', 'c'),3),
#                        coeffs = c(1,5,10,3,3,3,0,-5,-8),
#                        CI_start = c(0,4,9,2,2,2,-1,-6,-9),
#                        CI_end = c(2,7,12,5,5,5,2,-3,-6),
#                        outcome = c(rep('death',3), rep('admit', 3), rep('comb', 3)))
# df_debug %>% ggplot( aes(x = pred, y = coeffs, colour = outcome, group = outcome)) +
#   geom_point()+
#   geom_errorbar(aes(ymin = CI_start, ymax = CI_end)) +
#   coord_flip()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Matt's Plot ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

vars <- c("time_tohosp_adm_newmore than 1hr", "hiv_status_newChecked", "weightbb_del", "sesindex_sum")

outcome = c(rep('mortality', 4), rep('readmission', 4), rep('combined outcome', 4))
coeffs = c(coeffs_death_univ[vars], coeffs_admit_univ[vars], coeffs_comb_univ[vars])
#labs = c(names(coeffs_death_univ)[cols_temp], names(coeffs_admit)[cols_temp], names(coeffs_comb)[cols_temp])
labs = rep(c("Travel time to hospital > 1 hr", "HIV history of mother", "Weight of baby at delivery", "Socio-economic index sum"), 3)

df_plot = data.frame(outcome, coeffs, labs)

plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = 'Relative proportions of logit coefficients') +
  labs(x = 'Variables') + 
  labs(y = 'Coefficient percentages') 
  #theme_light()

ggplot2::ggsave('Figure.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots', device = 'png', width=1280/72, height = 720/72, dpi = 144)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE WORKSPACE             ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Only keep final dataframes
to_keep <- c("dat_impute", "dat_baby", "dat_baby_pred_death", "dat_baby_pred_admit", "dat_baby_pred_comb", "analysis_date")
rm(list = setdiff(ls(), to_keep))
save.image(paste0("Cleaned Data/Cleaned_Data (", analysis_date, ").RData"))