# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 07_FILTERING OUT PREDICTORS        #######
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

load("Cleaned Data/Cleaned_Data (2024-05-23).RData")

# Analysis date
analysis_date <- "2024-05-28"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UNIVARIATE LOGISTIC      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initializing ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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

# Fixing pvalue names

names(death_pvals_univ) <- names(coeffs_death_univ)
names(admit_pvals_univ) <- names(coeffs_admit_univ)
names(comb_pvals_univ) <- names(coeffs_comb_univ)

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
# HANDLING NA CONFIDENCE INTERVALS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Checking how many NA values are there in the CIs
sum(is.na(death_confint_start)) #22
sum(is.na(death_confint_end)) #1
sum(is.na(admit_confint_start)) #15
sum(is.na(admit_confint_end)) #1
sum(is.na(comb_confint_start)) #14
sum(is.na(comb_confint_end)) #1

# placenta_del is the column returning NA for 97.5% CI
predictor_names[is.na(comb_confint_end) | is.na(admit_confint_end) | is.na(death_confint_end)]

# 23 columns returning NA in the beginning
cols_NA_CI <- predictor_names[is.na(death_confint_start) | is.na(admit_confint_start) | is.na(comb_confint_start)]
length(cols_NA_CI)

# Adding placenta_delNone and checking the names of the columns
cols_NA_CI <- c(cols_NA_CI, predictor_names[is.na(comb_confint_end)])
cols_NA_CI

# Checking p-values
death_pvals_univ[cols_NA_CI]
admit_pvals_univ[cols_NA_CI]
comb_pvals_univ[cols_NA_CI]

# Checking how many predictors have their p-values <0.9 in atleast 1 of the 3 outcome logit models
cols_NA_CI <- cols_NA_CI[(death_pvals_univ[cols_NA_CI] < 0.9) | (admit_pvals_univ[cols_NA_CI] < 0.9) | (comb_pvals_univ[cols_NA_CI] < 0.9)]
length(cols_NA_CI) #9

cols_NA_CI

# Checking to see if the individual logitmodel raised any error
model_temp <- glm(babeadmit_swf~., 
                  data = dat_impute[, c("delay_adm___4", "babeadmit_swf")], 
                  family = binomial(link = 'logit'))
summary(model_temp)
# No warnings

# Note all 9 columns are Yes/No
# Checking if it has <20 incidence

freq = c()
for (i in 4:8) {
  freq = c(freq, table(dat_impute[paste("medhx_adm___", i, sep = "")])[2])
}
freq
# All medhx_adm columns with NA values (4-8) have <20 incidence

freq = c()
for (i in c(1,3,13,15:18)) {
  freq = c(freq, table(dat_impute[paste("preghx_adm___", i, sep = "")])[2])
}
freq
# All preghx_adm columns with NA values have <20 incidence


table(dat_impute$delay_adm___4)[2] < 20
table(dat_impute$placenta_adm___1)[2] < 20
table(dat_impute$placenta_adm___2)[2] < 20
table(dat_impute$placenta_adm___97)[2] < 20
table(dat_impute$marry_ses)[5] # Widowed has only 9
table(dat_impute$cord_delay_del)[2] < 20
table(dat_impute$destination_mat)[4:6] # <20 all categories with NA
table(dat_impute$abx_mat___99)[2] < 20
table(dat_impute$placenta_del) # <20 other incidences
# All true

# Removing all of these columns
cols_NA_CI_total <- c(predictor_names[is.na(death_confint_start) | is.na(admit_confint_start) | is.na(comb_confint_start)], 
                predictor_names[is.na(comb_confint_end) | is.na(admit_confint_end) | is.na(death_confint_end)])

#dat_filter <- dat_impute[, !(colnames %in% cols_NA_CI_total)]
predictor_names <- predictor_names[!(predictor_names %in% cols_NA_CI_total)]

# Updating variables to exclude vars with NA CIs for further analysis

coeffs_death_univ <- coeffs_death_univ[predictor_names]
coeffs_admit_univ <- coeffs_admit_univ[predictor_names]
coeffs_comb_univ <- coeffs_comb_univ[predictor_names]

death_confint_start <- death_confint_start[predictor_names]
death_confint_end <- death_confint_end[predictor_names]
admit_confint_start <- admit_confint_start[predictor_names]
admit_confint_end <- admit_confint_end[predictor_names]
comb_confint_start <- comb_confint_start[predictor_names]
comb_confint_end <- comb_confint_end[predictor_names]

death_pvals_univ <- death_pvals_univ[predictor_names]
admit_pvals_univ <- admit_pvals_univ[predictor_names]
comb_pvals_univ <- comb_pvals_univ[predictor_names]

odds_death <- exp(coeffs_death_univ)
odds_admit <- exp(coeffs_admit_univ)
odds_comb <- exp(coeffs_comb_univ)

death_oddsCI_start <- exp(death_confint_start)
death_oddsCI_end <- exp(death_confint_end)
admit_oddsCI_start <- exp(admit_confint_start)
admit_oddsCI_end <- exp(admit_confint_end)
comb_oddsCI_start <- exp(comb_confint_start)
comb_oddsCI_end <- exp(comb_confint_end)

# NA CI Column List

cols_NA <- c("delay_adm___4",
             "medhx_adm___4",              
             "medhx_adm___5",
             "medhx_adm___6",
             "medhx_adm___7",
             "medhx_adm___8",              
             "medhx_adm___9",
             "preghx_adm___1",             
             "preghx_adm___3",
             "preghx_adm___13",            
             "preghx_adm___15",
             "preghx_adm___16",            
             "preghx_adm___17",
             "preghx_adm___18",            
             "placenta_adm___1",
             "placenta_adm___2",           
             "placenta_adm___97",
             "cord_delay_del",              
             "abx_mat___99")

# Note "destination_matHome of friend"     
#       "destination_matMother not yet sure" 
#       "destination_matOther"
#       "marry_sesWidowed" still have NA CIs need to correct through regrouping


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILTERING ADMISSION SUBJECT VARS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

adm_subj_vars = colnames(dat_baby_pred_death)[1:6]
form_index <- c(1, 8, 29, 44, 57, 65, 74, 88, 112, 121, 142, 152)

# Checking how many predictors have large CIs starting before -50 or ending after 50
sum(death_confint_start[1:7] < -50)
sum(death_confint_end[1:7] > 50)
sum(admit_confint_start[1:7] < -50)
sum(admit_confint_end[1:7] > 50)
sum(comb_confint_start[1:7] < -50)
sum(comb_confint_end[1:7] > 50)

# Checking how many predictors have p-value above 0.9 (high p-val)
sum(death_pvals_univ[1:7] > 0.9)
sum(admit_pvals_univ[1:7] > 0.9) #1
sum(comb_pvals_univ[1:7] > 0.9)

admit_pvals_univ[1:7][(admit_pvals_univ[1:7] > 0.9)]
# "admdeldiff_new"

table(dat_impute$agemomyears_cat_new)
# <18 has 135 rows which is reference
# >35 has 557 rows
# Both > 100

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILTERING ADMISSION VARS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Checking how many predictors have large CIs starting before -50 or ending after 50
sum(death_confint_start[form_index[2]:(form_index[3]-1)] < -50) #3
sum(death_confint_end[form_index[2]:(form_index[3]-1)] > 50)    #5
sum(admit_confint_start[form_index[2]:(form_index[3]-1)] < -50)
sum(admit_confint_end[form_index[2]:(form_index[3]-1)] > 50)
sum(comb_confint_start[form_index[2]:(form_index[3]-1)] < -50)
sum(comb_confint_end[form_index[2]:(form_index[3]-1)] > 50)

# Which predictors have large CIs 
predictor_names[form_index[2]:(form_index[3]-1)][(death_confint_start[form_index[2]:(form_index[3]-1)] < -50)]
# "delay_adm___1Checked"  "delay_adm___3Checked"  "delay_adm___98Checked"
predictor_names[form_index[2]:(form_index[3]-1)][(death_confint_end[form_index[2]:(form_index[3]-1)] > 50)]
# "transport_admMotorcycle"   
# "transport_admPublic transport (bus, taxi)"                     
# "transport_admPrivate transport (special hire, private vehicle)"
# "transport_admAmbulance"                                        
# "takevitals_admYes"

# Checking how many predictors have p-value above 0.9 (high p-val)
sum(death_pvals_univ[form_index[2]:(form_index[3]-1)] > 0.9) #9
sum(admit_pvals_univ[form_index[2]:(form_index[3]-1)] > 0.9)
sum(comb_pvals_univ[form_index[2]:(form_index[3]-1)] > 0.9) #1

# Which predictors have large p-value
predictor_names[form_index[2]:(form_index[3]-1)][(death_pvals_univ[form_index[2]:(form_index[3]-1)] > 0.9)]
# "transport_admMotorcycle"                                       
# "transport_admPublic transport (bus, taxi)"                     
# "transport_admPrivate transport (special hire, private vehicle)"
# "transport_admAmbulance"                                        
# "transport_admOther"                                            
# "delay_adm___1Checked"                                          
# "delay_adm___3Checked"                                          
# "delay_adm___98Checked"
# "takevitals_admYes"

predictor_names[form_index[2]:(form_index[3]-1)][(admit_pvals_univ[form_index[2]:(form_index[3]-1)] > 0.9)]
# sys_bp_adm
predictor_names[form_index[2]:(form_index[3]-1)][(comb_pvals_univ[form_index[2]:(form_index[3]-1)] > 0.9)]
# dia_bp_adm
comb_pvals_univ["dia_bp_adm"] # .955
death_pvals_univ["dia_bp_adm"] # .407
admit_pvals_univ["dia_bp_adm"] # .798

# Checking incidence of problematic vars

table(dat_impute$transport_adm)
# 99  3784  766 1073  346 24

table(dat_impute$delay_adm___1)  #39
table(dat_impute$delay_adm___2)  #464
table(dat_impute$delay_adm___3)  #210
table(dat_impute$delay_adm___98) #108
table(dat_impute$delay_adm___99) #779

table(dat_impute$takevitals_adm) #96

table(dat_impute$time_tohosp_adm_new) # 2969  2303  820
table(dat_impute$isreferral_adm) #significant (2548)
table(dat_impute$csection_adm) #significant (504)
table(dat_impute$distress_adm) # 39

# Suggestions
# Remove transport_adm - pvals/confints
# Remove delay_adm___1/3/98, 2/99 are Cost of Transport/None may be useful predictors (economic/indicator)
# dia_bp_adm? What happens to sys_bp_adm then?
# Remove takevitals_adm
# distress_adm?

cols_remove <- c('transport_adm', 'delay_adm___1', 'delay_adm___3', 'delay_adm___98', 'takevitals_adm')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILTERING PREGNANCY HISTORY VARS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pregnancy History Vars 1 ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ind = 3

# Checking how many predictors have large CIs starting before -50 or ending after 50
sum(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #2
sum(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    
sum(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)
sum(admit_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)
sum(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)
sum(comb_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)

# Which predictors have large CI
death_confint_start[form_index[ind]:(form_index[ind+1]-1)][(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# medhx_adm___3  medhx_adm___98

coeffs_death_univ[c("medhx_adm___3Checked", "medhx_adm___98Checked")]

# Checking how many predictors have p-value above 0.9 (high p-val)
sum(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #2
sum(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #1
sum(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)

# Which predictors have large p-val
death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# medhx_adm___3  medhx_adm___98
admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# gravid_adm

# Checking incidence 

table(dat_impute$gravid_adm)

table(dat_impute$medhx_adm___2) #63
table(dat_impute$medhx_adm___3) #166
table(dat_impute$medhx_adm___4) #12
table(dat_impute$medhx_adm___5) #8
table(dat_impute$medhx_adm___6) #5
table(dat_impute$medhx_adm___7) #10
table(dat_impute$medhx_adm___8) #3
table(dat_impute$medhx_adm___9) #6
table(dat_impute$medhx_adm___98) #132

# Suggestions
# Remove medhx_adm___3, medhx_adm___98
# Consider removing gravid_adm as parity_adm stores similar more relevant info
# Remove medhx_adm___3/4/5/6/7/8/9/98 - 1/99 stores HIV (before preg)/None so useful info?
# Remove medhx_adm___2?

medhx_remove = c()
for (i in c(3:9, 98)) {
  medhx_remove = c(medhx_remove, paste("medhx_adm___", i, sep = ""))
}
cols_remove <- c(cols_remove, medhx_remove )

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Conditions during pregrancy Vars 1 ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ind = 4

sum(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #6
sum(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    
sum(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)
sum(admit_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)
sum(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)
sum(comb_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)

death_confint_start[form_index[ind]:(form_index[ind+1]-1)][(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# preghx_adm___2/4/5/6/8/14Checked

sum(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #6
sum(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #1
sum(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)  #2

death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# same as confint
admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# preghx_adm___8Checked 
comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# preghx_adm___4/6Checked

# Checking incidence of problematic preghx vars through iteration
freq = c()
for (i in c(2, 4:12, 14, 98, 99)) {
  freq = c(freq, table(dat_impute[paste("preghx_adm___", i, sep = "")])[2])
}
freq
predictor_names[form_index[ind]:(form_index[ind+1]-1)][freq>50]

# Suggestions
# keep 7/9/10/11/12/98/99

preghx_remove = c()
for (i in c(2, 4:6, 8, 14)) {
  preghx_remove = c(preghx_remove, paste("preghx_adm___", i, sep = ""))
}
cols_remove <- c(cols_remove, preghx_remove)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pregnancy History Vars 2 ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ind = 5

sum(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #1
sum(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    
sum(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)
sum(admit_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)
sum(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)
sum(comb_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)

death_confint_start[form_index[ind]:(form_index[ind+1]-1)][(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# prevadm_adm_new1 - 7 days (in the past week)

sum(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #3
sum(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)
sum(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)

death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# medhbp_admYes,   prevadm_adm_new1 - 7 days (in the past week)/ prevadm_adm_new7 - 28 days (from one week to one month ago

table(dat_impute$prevadm_adm_new)
# 5292  190  155  337  118
#       1-7  7-28
table(dat_impute$medhbp_adm) # 163

# Suggestions
# Merge prev_adm_new 1-7 and 7-28 days? Categorical so if deleted need to delete prev_adm_new whole
# Remove medhbhp_adm?

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ANC Vars ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ind = 6
predictor_names[form_index[ind]:(form_index[ind+1]-1)]

sum(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #4
sum(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    #1
sum(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #1
sum(admit_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)
sum(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)  #1
sum(comb_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)

death_confint_start[form_index[ind]:(form_index[ind+1]-1)][(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# numberanc_adm_new>8    ancprovider_adm___1/3/7Checked
death_confint_end[form_index[ind]:(form_index[ind+1]-1)][(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)]
# ancprovider_adm___5Checked
admit_confint_start[form_index[ind]:(form_index[ind+1]-1)][(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# ancprovider_adm___7Checked
comb_confint_start[form_index[ind]:(form_index[ind+1]-1)][(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# ancprovider_adm___7Checked

sum(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #5
sum(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #1
sum(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)  #2

death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# numberanc_adm_new>8    ancprovider_adm___1/3/5/7Checked
admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# ancprovider_adm___7Checked
comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# numberanc_adm_new>8     ancprovider_adm___7Checked

# Checking incidence of required ancprovider vars through iteration
freq = c()
for (i in 1:7) {
  freq = c(freq, table(dat_impute[paste("ancprovider_adm___", i, sep = "")])[2])
}
freq
# ancprovider_adm___1/3/5/7 - 66/28/6032/28 entries respectively - insignificant
# ancprovider_adm___4 - 23

# Incidence of numberanc
table(dat_impute$numberanc_adm_new) #108 (>8 category)

# As good incidence, checking if coeff is also large
coeffs_death_univ['numberanc_adm_new>8']
death_confint_start['numberanc_adm_new>8'] # -196 i.e. too large

# Suggestions 
# Remove ancprovider_adm___1/3/4/5/7
# numberanc_adm_new - Either remove or regroup > 8 to something 

anc_remove = c()
for (i in c(1, 3:5, 7)) {
  anc_remove = c(anc_remove, paste("ancprovider_adm___", i, sep = ""))
}
cols_remove <- c(cols_remove, anc_remove)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILTERING SES VARS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ind = 7
predictor_names[form_index[ind]:(form_index[ind+1]-1)]

sum(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #1
sum(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    #3
sum(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) 
sum(admit_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)
sum(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)  
sum(comb_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)

death_confint_start[form_index[ind]:(form_index[ind+1]-1)][(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# marry_sesMarried polygamous
death_confint_end[form_index[ind]:(form_index[ind+1]-1)][(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)]
# schoolyrs_ses_new P4-P7/S1-S6/Post secondary

sum(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #4
sum(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) 
sum(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)  #2

death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# Same as confint
comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# marry_sesMarried polygamous    sesindex_catHigh SES

# Checking incidence
table(dat_impute$marry_ses) # Married polygamous 602
table(dat_impute$schoolyrs_ses_new) # significant

# Checking to see if the coeff is large, given good incidence
coeffs_death_univ['marry_sesMarried polygamous']
death_confint_start['marry_sesMarried polygamous']

# Suggestions
# Remove schoolyrs_ses_new
# Regroup or remove marry_ses
# Maybe keep marry_ses as it is as death model is erratic and may be useful for comb?

cols_remove <- c(cols_remove, 'schoolyrs_ses_new')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILTERING DELIVERY MATERNAL VARS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ind = 8
predictor_names[form_index[ind]:(form_index[ind+1]-1)]

sum(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #4
sum(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    #1 (#9~but vagexam_del replaced by vagexam_del_new)
sum(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #2
sum(admit_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    #2
sum(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)  #1
sum(comb_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)     #3

death_confint_start[form_index[ind]:(form_index[ind+1]-1)][(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# prom_del_new4 - 8 hours/8 - 12/12 - 24      induce_delYes
death_confint_end[form_index[ind]:(form_index[ind+1]-1)][(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)]
# vagexam_del1/2/3/4/5/6/7/>9~removed       placenta_delPlacenta Abruption
admit_confint_start[form_index[ind]:(form_index[ind+1]-1)][(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# prom_del_new0 - 4/4 - 8 hours
admit_confint_end[form_index[ind]:(form_index[ind+1]-1)][(admit_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)]
# placenta_delPlacenta accreta/ Retained placenta
comb_confint_start[form_index[ind]:(form_index[ind+1]-1)][(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# prom_del_new 4 - 8 hours
comb_confint_end[form_index[ind]:(form_index[ind+1]-1)][(comb_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)]
# placenta_del All

sum(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #19
sum(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #5
sum(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)  #4

death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)]
# Too many, will deal with them in prediction modelling anyways

table(dat_impute$prom_del_new) 
# except >24 hours all are <= 50 (4-8 hrs is 50) - too many insignificant categories

table(dat_impute$vagexam_del_new) # Consistent (>50), other than 9
death_confint_end[grep('^vagexam', predictor_names)]
# Checking how large the confint is for vagexam vars, given good incidence

# Checking incidence and the coefficient for induce_del (good incidence)
table(dat_impute$induce_del)
death_confint_start['induce_delYes']
coeffs_death_univ['induce_delYes']

# Suggestions
# Remove placenta_del as very low incidence - NA/large CIs for many categories
# Remove vagexam_del or replace with vagexam_del_new
# Remove or regroup prom_del_new
# Remove induce_del

cols_remove <- c(cols_remove, 'placenta_del', 'induce_del')
#cols_add <- c('vagexam_del_new')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILTERING DELIVERY NEONATAL VARS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ind = 9
predictor_names[form_index[ind]:(form_index[ind+1]-1)]

sum(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #1
sum(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    
sum(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) 
sum(admit_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    
sum(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)  
sum(comb_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)     

death_confint_start[form_index[ind]:(form_index[ind+1]-1)][(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# rescusoxy_del_newResuscitation with oxygen

sum(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #1
sum(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) 
sum(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)  #1

death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# rescusoxy_del_newResuscitation with oxygen
comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# apgar1_del

table(dat_impute$rescusoxy_del_new)
# Rescusitation with oxygen - 67
table(dat_impute$apgar1_del)
table(dat_impute$weightbb_del_cat_new) # consistent

# Suggestions
# Recombine Rescusitation as a single variable or remove?
# apgar1_del? - prediction modelling


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILTERING DISCHARGE MATERNAL VARS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ind = 10
predictor_names[form_index[ind]:(form_index[ind+1]-1)]

sum(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #1
sum(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    
sum(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) 
sum(admit_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    
sum(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)  
sum(comb_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)

death_confint_start[form_index[ind]:(form_index[ind+1]-1)][(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# support_mat___99Checked

sum(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #2
sum(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #2
sum(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)  #1

death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# dpb_mat    support_mat___99Checked
admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# destination_matHome of relative   bf_matNo
comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# bf_matNo

# Checking incidence of support_mat vars through iteration
freq = c()
for (i in c(1:5, 99)) {
  freq = c(freq, table(dat_impute[paste("support_mat___", i, sep = "")])[2])
}
freq
# all other than support_mat___99 have atleast 100 Checked

table(dat_impute$support_mat___99) #46
table(dat_impute$destination_mat)  #267 - Home of relative
table(dat_impute$dbp_mat) # Keep but consider categorizing
table(dat_impute$bf_mat)  # 184 (>50 incidence)

# Suggestions
# Remove destination_mat as other categories also returned NA values earlier
# Or consider regrouping to "Own home", Home of parent", "Other"
# Remove support_mat___99
# Other variables may be handled by Elastic Net prediction modelling

cols_remove <- c(cols_remove, 'support_mat___99')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILTERING DISCHARGE NEONATAL VARS      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ind = 11
predictor_names[form_index[ind]:(form_index[ind+1]-1)]

sum(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #3
sum(death_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    
sum(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50) #1
sum(admit_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)    #1
sum(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)  #1
sum(comb_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50)

death_confint_start[form_index[ind]:(form_index[ind+1]-1)][(death_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# jaundice_neo_newYes   eyedischarge_neoYes     abx_neoYes
admit_confint_start[form_index[ind]:(form_index[ind+1]-1)][(admit_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# eyedischarge_neoYes
admit_confint_end[form_index[ind]:(form_index[ind+1]-1)][(admit_confint_end[form_index[ind]:(form_index[ind+1]-1)] > 50) ]
# bf_neoYes
comb_confint_start[form_index[ind]:(form_index[ind+1]-1)][(comb_confint_start[form_index[ind]:(form_index[ind+1]-1)] < -50)]
# eyedischarge_neoYes

sum(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #4
sum(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9) #2
sum(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)  #2

death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(death_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# jaundice_neo_newYes   eyedischarge_neoYes     abx_neoYes
# mean_hr_neo_new
admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(admit_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# eyedischarge_neoYes   bf_neoYes
comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)][(comb_pvals_univ[form_index[ind]:(form_index[ind+1]-1)] > 0.9)]
# eyedischarge_neoYes   rr_neo

table(dat_impute$eyedischarge_neo) #27 Yes
table(dat_impute$jaundice_neo_new) #76 Yes
table(dat_impute$abx_neo)          #46 Yes
table(dat_impute$bf_neo)           #55 No

# Checking coefficients in case of good incidence
coeffs_death_univ['jaundice_neo_newYes']
death_confint_start['jaundice_neo_newYes'] # too large, discard even though significant
coeffs_admit_univ['bf_neoYes']
admit_confint_end['bf_neoYes'] # too large, discard (124, coefficient is 13)

# Suggestions
# Remove jaundice_neo_new, bf_neo, eyedischarge_neo, abx_neo (CI too large/incidence)
# Let prediction modelling cut down variables with p-value too high

cols_remove <- c(cols_remove, 'jaundice_neo_new', 'bf_neo', 'eyedischarge_neo', 'abx_neo')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATING FILTERED DATA      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Removing the columns with NA or large confidence intervals

filtered_cols <- colnames(dat_impute)[!(colnames(dat_impute) %in% c(cols_NA, cols_remove))]
dat_filter <- dat_impute[filtered_cols]
dim(dat_filter) #98 columns #~ (was 102 earlier?)

# Initializing vector variables

coeffs_death_filter <- c()
death_pvals_filter <- c()
death_confint_start_filter <- c()
death_confint_end_filter <- c()

coeffs_admit_filter <- c()
admit_pvals_filter <- c()
admit_confint_start_filter <- c()
admit_confint_end_filter <- c()

coeffs_comb_filter <- c()
comb_pvals_filter <- c()
comb_confint_start_filter <- c()
comb_confint_end_filter <- c()

predictor_names_filter <- c()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculating coeffs, p-vals, CIs ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Iterating over each predictor to fit a univariable logistic model for death

for (i in 1: (length(colnames(dat_filter))-4)) {
  model_comb_temp <- glm(babedeath_swf~., 
                         data = dat_filter[, c(colnames(dat_filter)[i+1], "babedeath_swf")], 
                         family = binomial(link = 'logit'))
  
  # Appending coeffs, pred_names from coef()
  coeffs_death_filter <- c(coeffs_death_filter, coef(model_comb_temp)[-1])
  predictor_names_filter <- c(predictor_names_filter, names(coef(model_comb_temp)[-1]))
  
  # Appending p-vals, CIs from summary()
  death_pvals_filter <- c(death_pvals_filter, summary(model_comb_temp)$coefficients[-1,4])
  conf_temp <- confint(model_comb_temp)
  death_confint_start_filter <- c(death_confint_start_filter, conf_temp[-1, 1])
  death_confint_end_filter <- c(death_confint_end_filter, conf_temp[-1, 2])
}

# Proceeding similarly for the readmit outcome

for (i in 1: (length(colnames(dat_filter))-4)) {
  model_admit_temp <- glm(babeadmit_swf~., 
                          data = dat_filter[, c(colnames(dat_filter)[i+1], "babeadmit_swf")], 
                          family = binomial(link = 'logit'))
  
  coeffs_admit_filter <- c(coeffs_admit_filter, coef(model_admit_temp)[-1])

  admit_pvals_filter <- c(admit_pvals_filter, summary(model_admit_temp)$coefficients[-1,4])
  conf_temp <- confint(model_admit_temp)
  admit_confint_start_filter <- c(admit_confint_start_filter, conf_temp[-1, 1])
  admit_confint_end_filter <- c(admit_confint_end_filter, conf_temp[-1, 2])
}

# Proceeding similarly for the combined outcome

for (i in 1: (length(colnames(dat_filter))-4)) {
  model_comb_temp <- glm(babeoutcome_swf~., 
                         data = dat_filter[, c(colnames(dat_filter)[i+1], "babeoutcome_swf")], 
                         family = binomial(link = 'logit'))

  coeffs_comb_filter <- c(coeffs_comb_filter, coef(model_comb_temp)[-1])

  comb_pvals_filter <- c(comb_pvals_filter, summary(model_comb_temp)$coefficients[-1,4])
  conf_temp <- confint(model_comb_temp)
  comb_confint_start_filter <- c(comb_confint_start_filter, conf_temp[-1, 1])
  comb_confint_end_filter <- c(comb_confint_end_filter, conf_temp[-1, 2])
}

# Fixing Confidence Interval names

names(death_confint_start_filter) <- names(coeffs_death_filter)
names(death_confint_end_filter) <- names(coeffs_death_filter)
names(admit_confint_start_filter) <- names(coeffs_admit_filter)
names(admit_confint_end_filter) <- names(coeffs_admit_filter)
names(comb_confint_start_filter) <- names(coeffs_comb_filter)
names(comb_confint_end_filter) <- names(coeffs_comb_filter)

# Fixing pvalue names

names(death_pvals_filter) <- names(coeffs_death_filter)
names(admit_pvals_filter) <- names(coeffs_admit_filter)
names(comb_pvals_filter) <- names(coeffs_comb_filter)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Odds Ratios ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Exponentiating to get odds ratios

odds_death_filter <- exp(coeffs_death_filter)
odds_admit_filter <- exp(coeffs_admit_filter)
odds_comb_filter <- exp(coeffs_comb_filter)

# CIs

death_oddsCI_start <- exp(death_confint_start_filter)
death_oddsCI_end <- exp(death_confint_end_filter)
admit_oddsCI_start <- exp(admit_confint_start_filter)
admit_oddsCI_end <- exp(admit_confint_end_filter)
comb_oddsCI_start <- exp(comb_confint_start_filter)
comb_oddsCI_end <- exp(comb_confint_end_filter)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILTERED DATA VIZ.      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Finding predictors where same/different sign

pred_same_sign_pos <- predictor_names_filter[(coeffs_death_filter>0) & (coeffs_admit_filter>0)]
pred_same_sign_neg <- predictor_names_filter[(coeffs_death_filter<0) & (coeffs_admit_filter<0)]
pred_diff_sign_posneg <- predictor_names_filter[(coeffs_death_filter>0) & (coeffs_admit_filter<0)]
pred_diff_sign_negpos <- predictor_names_filter[(coeffs_death_filter<0) & (coeffs_admit_filter>0)]

length(pred_same_sign_pos)
length(pred_same_sign_neg)
length(pred_diff_sign_posneg)
length(pred_diff_sign_negpos)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting relative values of logit coefficients ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Same sign positive
# Creating the dataframe for the plot
outcome = c(rep('death', length(pred_same_sign_pos)), rep('admit', length(pred_same_sign_pos)), rep('combined', length(pred_same_sign_pos)))
coeffs = c(coeffs_death_filter[pred_same_sign_pos], coeffs_admit_filter[pred_same_sign_pos], coeffs_comb_filter[pred_same_sign_pos])
labs = rep(pred_same_sign_pos, 3)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the different coefficients
plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - positive", subtitle = 'Relative proportions of logit coefficients') +
  labs(x = 'Coefficient percentages') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('samedir_pos_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)

plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - positive", subtitle = 'Values of logit coefficients') +
  labs(x = 'Coefficients') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('samedir_pos_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Same sign negative
# Creating the dataframe for the plot
outcome = c(rep('death', length(pred_same_sign_neg)), rep('admit', length(pred_same_sign_neg)), rep('combined', length(pred_same_sign_neg)))
coeffs = c(coeffs_death_filter[pred_same_sign_neg], coeffs_admit_filter[pred_same_sign_neg], coeffs_comb_filter[pred_same_sign_neg])
labs = rep(pred_same_sign_neg, 3)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the different coefficients
plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - negative", subtitle = 'Relative proportions of logit coefficients') +
  labs(x = 'Coefficient percentages') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('samedir_neg_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)

plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - negative", subtitle = 'Values of logit coefficients') +
  labs(x = 'Coefficients') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('samedir_neg_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Different sign - death positive and admit  negative
# Creating the dataframe for the plot
outcome = c(rep('death', length(pred_diff_sign_posneg)), rep('admit', length(pred_diff_sign_posneg)), rep('combined', length(pred_diff_sign_posneg)))
coeffs = c(coeffs_death_filter[pred_diff_sign_posneg], coeffs_admit_filter[pred_diff_sign_posneg], coeffs_comb_filter[pred_diff_sign_posneg])
labs = rep(pred_diff_sign_posneg, 3)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the different coefficients
plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Death positive, Readmit negative", subtitle = 'Relative proportions of logit coefficients') +
  labs(x = 'Coefficient percentages') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('diffdir_posneg_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)

plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Death positive, Readmit negative", subtitle = 'Values of logit coefficients') +
  labs(x = 'Coefficients') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('diffdir_posneg_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Different sign - death negative and admit positive
# Creating the dataframe for the plot
outcome = c(rep('death', length(pred_diff_sign_negpos)), rep('admit', length(pred_diff_sign_negpos)), rep('combined', length(pred_diff_sign_negpos)))
coeffs = c(coeffs_death_filter[pred_diff_sign_negpos], coeffs_admit_filter[pred_diff_sign_negpos], coeffs_comb_filter[pred_diff_sign_negpos])
labs = rep(pred_diff_sign_negpos, 3)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the different coefficients
plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Death negative, Readmit positive", subtitle = 'Relative proportions of logit coefficients') +
  labs(x = 'Coefficient percentages') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('diffdir_negpos_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)

ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Death negative, Readmit positive", subtitle = 'Values of logit coefficients') +
  labs(x = 'Coefficients') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('diffdir_negpos_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Logit', device = 'png', width=1280/72, height = 720/72, dpi = 72)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting odds ratios ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Same sign positive
# Creating the dataframe for the plot
outcome = c(rep('death', length(pred_same_sign_pos)), rep('admit', length(pred_same_sign_pos)), rep('combined', length(pred_same_sign_pos)))
coeffs = c(odds_death_filter[pred_same_sign_pos], odds_admit_filter[pred_same_sign_pos], odds_comb_filter[pred_same_sign_pos])
labs = rep(pred_same_sign_pos, 3)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the different odds
plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - positive", subtitle = 'Relative proportions of odds ratios') +
  labs(x = 'Odds Ratio percentages') + 
  labs(y = 'Variables') + 
  theme_dark()

ggplot2::ggsave('samedir_pos_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Odds', device = 'png', width=1280/72, height = 720/72, dpi = 72)

plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - positive", subtitle = 'Values of odds ratios') +
  labs(x = 'Odds Ratios') + 
  labs(y = 'Variables') + 
  theme_dark()

ggplot2::ggsave('samedir_pos_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Odds', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Same sign negative
# Creating the dataframe for the plot
outcome = c(rep('death', length(pred_same_sign_neg)), rep('admit', length(pred_same_sign_neg)), rep('combined', length(pred_same_sign_neg)))
coeffs = c(odds_death_filter[pred_same_sign_neg], odds_admit_filter[pred_same_sign_neg], odds_comb_filter[pred_same_sign_neg])
labs = rep(pred_same_sign_neg, 3)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the different coefficients
plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - negative", subtitle = 'Relative proportions of odds ratios') +
  labs(x = 'Odds Ratio percentages') + 
  labs(y = 'Variables') + 
  theme_dark()

ggplot2::ggsave('samedir_neg_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Odds', device = 'png', width=1280/72, height = 720/72, dpi = 72)

plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - negative", subtitle = 'Values of odds ratios') +
  labs(x = 'Odds Ratios') + 
  labs(y = 'Variables') + 
  theme_dark()

ggplot2::ggsave('samedir_neg_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Odds', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Different sign - death positive and admit  negative
# Creating the dataframe for the plot
outcome = c(rep('death', length(pred_diff_sign_posneg)), rep('admit', length(pred_diff_sign_posneg)), rep('combined', length(pred_diff_sign_posneg)))
coeffs = c(odds_death_filter[pred_diff_sign_posneg], odds_admit_filter[pred_diff_sign_posneg], odds_comb_filter[pred_diff_sign_posneg])
labs = rep(pred_diff_sign_posneg, 3)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the different coefficients
plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Death positive, Readmit negative", subtitle = 'Relative proportions of odds ratios') +
  labs(x = 'Odds ratio percentages') + 
  labs(y = 'Variables') + 
  theme_dark()

ggplot2::ggsave('diffdir_posneg_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Odds', device = 'png', width=1280/72, height = 720/72, dpi = 72)

plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Death positive, Readmit negative", subtitle = 'Values of odds ratios') +
  labs(x = 'Odds ratios') + 
  labs(y = 'Variables') + 
  theme_dark()

ggplot2::ggsave('diffdir_posneg_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Odds', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Different sign - death negative and admit positive
# Creating the dataframe for the plot
outcome = c(rep('death', length(pred_diff_sign_negpos)), rep('admit', length(pred_diff_sign_negpos)), rep('combined', length(pred_diff_sign_negpos)))
coeffs = c(odds_death_filter[pred_diff_sign_negpos], odds_admit_filter[pred_diff_sign_negpos], odds_comb_filter[pred_diff_sign_negpos])
labs = rep(pred_diff_sign_negpos, 3)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the different coefficients
plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Death negative, Readmit positive", subtitle = 'Relative proportions of odds ratios') +
  labs(x = 'Odds ratio percentages') + 
  labs(y = 'Variables') + 
  theme_dark()

ggplot2::ggsave('diffdir_negpos_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Odds', device = 'png', width=1280/72, height = 720/72, dpi = 72)

plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Death negative, Readmit positive", subtitle = 'Values of odds ratios') +
  labs(x = 'Odds Ratios') + 
  labs(y = 'Variables') + 
  theme_dark()

ggplot2::ggsave('diffdir_negpos_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/Odds', device = 'png', width=1280/72, height = 720/72, dpi = 72)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting odds ratio CIs ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Teporarily removing categories with large CI for visualization
# temp_remove <- c("prom_del_new12 - 24 hours", 
#                  "vagexam_del1", "vagexam_del2", "vagexam_del3", "vagexam_del4",
#                  "vagexam_del5", "vagexam_del6", "vagexam_del7", "vagexam_del8",
#                  "vagexam_del9", "vagexam_del>9",
#                  "marry_sesWidowed",
#                  "destination_matOther")
# 
# odds_death_filter <- odds_death_filter[!(predictor_names_filter %in% temp_remove)]
# odds_admit_filter <- odds_admit_filter[!(predictor_names_filter %in% temp_remove)]
# odds_comb_filter <- odds_comb_filter[!(predictor_names_filter %in% temp_remove)]
# 
# death_oddsCI_start <- death_oddsCI_start[! (predictor_names_filter %in% temp_remove)]
# death_oddsCI_end <- death_oddsCI_end[! (predictor_names_filter %in% temp_remove)]
# admit_oddsCI_start <- admit_oddsCI_start[! (predictor_names_filter %in% temp_remove)]
# admit_oddsCI_end <- admit_oddsCI_end[! (predictor_names_filter %in% temp_remove)]
# comb_oddsCI_start <- comb_oddsCI_start[! (predictor_names_filter %in% temp_remove)]
# comb_oddsCI_end <- comb_oddsCI_end[! (predictor_names_filter %in% temp_remove)]

# Identifying columns that are continuous
cols_continuos <- c("admlabdiff_new", "best_hr_mat", 
                    "mean_hr_neo_new", "admdisdiff_new", 
                    "agemomyears_calc", "deldisdiff_new",
                    "labdisdiff__new", "height_neo",
                    "dia_bp_adm", "mean_spo2_neo_new",
                    "admdeldiff_new", "dbp_mat",
                    "hem_mat", "hr_adm", "labdeldiff_new",
                    "rr_mat", "rr_neo",
                    "sbp_mat", "sys_bp_adm")

odds_death_filter[cols_continuos] <- odds_death_filter[cols_continuos]^20
death_oddsCI_start[cols_continuos] <- death_oddsCI_start[cols_continuos]^20
death_oddsCI_end[cols_continuos] <- death_oddsCI_end[cols_continuos]^20

odds_admit_filter[cols_continuos] <- odds_admit_filter[cols_continuos]^20
admit_oddsCI_start[cols_continuos] <- admit_oddsCI_start[cols_continuos]^20
admit_oddsCI_end[cols_continuos] <- admit_oddsCI_end[cols_continuos]^20

odds_comb_filter[cols_continuos] <- odds_comb_filter[cols_continuos]^20
comb_oddsCI_start[cols_continuos] <- comb_oddsCI_start[cols_continuos]^20
comb_oddsCI_end[cols_continuos] <- comb_oddsCI_end[cols_continuos]^20

#Same sign - positive
# Creating dataframe
df_odds <- data.frame(odds = c(odds_death_filter[pred_same_sign_pos], odds_admit_filter[pred_same_sign_pos], odds_comb_filter[pred_same_sign_pos]),
                      CI_start = c(death_oddsCI_start[pred_same_sign_pos], admit_oddsCI_start[pred_same_sign_pos], comb_oddsCI_start[pred_same_sign_pos]),
                      CI_end = c(death_oddsCI_end[pred_same_sign_pos], admit_oddsCI_end[pred_same_sign_pos], comb_oddsCI_end[pred_same_sign_pos]),
                      pred = rep(factor(pred_same_sign_pos,levels = sort(pred_same_sign_pos, decreasing = TRUE)), 3),
                      outcome = c(rep('death', length(pred_same_sign_pos)), rep('admit', length(pred_same_sign_pos)), rep('comb', length(pred_same_sign_pos))))

# Plotting CIs
plot1 <- df_odds %>% ggplot( aes(x = pred, y = odds, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width =0.5)) +
  geom_hline(yintercept = 1) +
  # geom_jitter() +
  coord_flip()+
  labs(title = "Same direction positive", subtitle = 'Confidence intervals for odds ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'Odds Ratios') + 
  # theme_dark()+
  scale_y_continuous(limits = c(0,30))

ggplot2::ggsave('samedir_pos.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 72)

#Same sign - negative
# Creating dataframe
df_odds <- data.frame(odds = c(odds_death_filter[pred_same_sign_neg], odds_admit_filter[pred_same_sign_neg], odds_comb_filter[pred_same_sign_neg]),
                      CI_start = c(death_oddsCI_start[pred_same_sign_neg], admit_oddsCI_start[pred_same_sign_neg], comb_oddsCI_start[pred_same_sign_neg]),
                      CI_end = c(death_oddsCI_end[pred_same_sign_neg], admit_oddsCI_end[pred_same_sign_neg], comb_oddsCI_end[pred_same_sign_neg]),
                      pred = rep(factor(pred_same_sign_neg,levels = sort(pred_same_sign_neg, decreasing = TRUE)), 3),
                      outcome = c(rep('death', length(pred_same_sign_neg)), rep('admit', length(pred_same_sign_neg)), rep('comb', length(pred_same_sign_neg))))

temp_ind = (dim(df_odds)[1])/3

# Plotting CIs
plot1 <- df_odds[c(1:20,(temp_ind+1):(temp_ind+20), (2*temp_ind+1):(2*temp_ind+20)), ] %>% 
  ggplot( aes(x = pred, y = odds, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Same direction negative", subtitle = 'Confidence intervals for odds ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'Odds Ratios') + 
  # theme_dark()+
  scale_y_continuous(limits = c(0,20))

plot2 <- df_odds[c(21:temp_ind, (temp_ind+21):(2*temp_ind), (2*temp_ind+21):(3*temp_ind)),] %>% 
  ggplot( aes(x = pred, y = odds, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Same direction negative", subtitle = 'Confidence intervals for odds ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'Odds Ratios') + 
  # theme_dark()+
  scale_y_continuous(limits = c(0,20))

ggplot2::ggsave('samedir_neg1.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 72)
ggplot2::ggsave('samedir_neg2.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 72)

#Different sign - death positive, readmit negative
# Creating dataframe
df_odds <- data.frame(odds = c(odds_death_filter[pred_diff_sign_posneg], odds_admit_filter[pred_diff_sign_posneg], odds_comb_filter[pred_diff_sign_posneg]),
                      CI_start = c(death_oddsCI_start[pred_diff_sign_posneg], admit_oddsCI_start[pred_diff_sign_posneg], comb_oddsCI_start[pred_diff_sign_posneg]),
                      CI_end = c(death_oddsCI_end[pred_diff_sign_posneg], admit_oddsCI_end[pred_diff_sign_posneg], comb_oddsCI_end[pred_diff_sign_posneg]),
                      pred = rep(factor(pred_diff_sign_posneg,levels = sort(pred_diff_sign_posneg, decreasing = TRUE)), 3),
                      outcome = c(rep('death', length(pred_diff_sign_posneg)), rep('admit', length(pred_diff_sign_posneg)), rep('comb', length(pred_diff_sign_posneg))))

temp_ind = (dim(df_odds)[1])/3

# Plotting CIs
plot1 <- df_odds[c(1:20,(temp_ind+1):(temp_ind+20), (2*temp_ind+1):(2*temp_ind+20)), ] %>% 
  ggplot( aes(x = pred, y = odds, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death positive, Readmit negative", subtitle = 'Confidence intervals for odds ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'Odds Ratios') + 
  # theme_dark()+
  scale_y_continuous(limits = c(0,35))

plot2 <- df_odds[c(21:temp_ind, (temp_ind+21):(2*temp_ind), (2*temp_ind+21):(3*temp_ind)),] %>% 
  ggplot( aes(x = pred, y = odds, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death positive, Readmit negative", subtitle = 'Confidence intervals for odds ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'Odds Ratios') + 
  # theme_dark()+
  scale_y_continuous(limits = c(0,35))

ggplot2::ggsave('diffdir_posneg1.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 72)
ggplot2::ggsave('diffdir_posneg2.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 72)

#Different sign - death negative, readmit positive
# Creating dataframe
df_odds <- data.frame(odds = c(odds_death_filter[pred_diff_sign_negpos], odds_admit_filter[pred_diff_sign_negpos], odds_comb_filter[pred_diff_sign_negpos]),
                      CI_start = c(death_oddsCI_start[pred_diff_sign_negpos], admit_oddsCI_start[pred_diff_sign_negpos], comb_oddsCI_start[pred_diff_sign_negpos]),
                      CI_end = c(death_oddsCI_end[pred_diff_sign_negpos], admit_oddsCI_end[pred_diff_sign_negpos], comb_oddsCI_end[pred_diff_sign_negpos]),
                      pred = rep(factor(pred_diff_sign_negpos,levels = sort(pred_diff_sign_negpos, decreasing = TRUE)), 3),
                      outcome = c(rep('death', length(pred_diff_sign_negpos)), rep('admit', length(pred_diff_sign_negpos)), rep('comb', length(pred_diff_sign_negpos))))

temp_ind = (dim(df_odds)[1])/3

# Plotting CIs
plot1 <- df_odds[c(1:20,(temp_ind+1):(temp_ind+20), (2*temp_ind+1):(2*temp_ind+20)), ] %>% 
  ggplot( aes(x = pred, y = odds, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death negative, Readmit positive", subtitle = 'Confidence intervals for odds ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'Odds Ratios') + 
  # theme_dark()+
  scale_y_continuous(limits = c(0,20))

plot2 <- df_odds[c(21:temp_ind, (temp_ind+21):(2*temp_ind), (2*temp_ind+21):(3*temp_ind)),] %>% 
  ggplot( aes(x = pred, y = odds, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death negative, Readmit positive", subtitle = 'Confidence intervals for odds ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'Odds Ratios') + 
  # theme_dark()+
  scale_y_continuous(limits = c(0,20))

ggplot2::ggsave('diffdir_negpos1.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 72)
ggplot2::ggsave('diffdir_negpos2.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Filtered/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE WORKSPACE             ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Only keep final dataframes
# Keep dat_baby as useful for Survival Analysis
to_keep <- c("dat_filter", "dat_baby", "analysis_date")
rm(list = setdiff(ls(), to_keep))
save.image(paste0("Cleaned Data/Cleaned_Data (", analysis_date, ").RData"))