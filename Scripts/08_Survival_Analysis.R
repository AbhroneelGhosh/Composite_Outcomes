# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 08_SURVIVAL ANALYSIS        #######
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Importing libraries
# For data manipulations
library(tidyverse)
library(Hmisc)
library(reshape2)
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

load("Cleaned Data/Cleaned_Data (2024-05-28).RData")

# Analysis date
analysis_date <- "2024-05-31"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA PREPROCESSING              ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## One-Hot Encoding, Adding the days_to_outcome vars ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# One hot encoding

# Removing the outcome variables and studyid_adm column
dat_surv <- dat_filter[, !(colnames(dat_filter) %in% c("studyid_adm", "babedeath_swf", "babeadmit_swf", "babeoutcome_swf"))]

# Encoding
dummy <- dummyVars("~.", data = dat_surv)
dat_surv <- data.frame(predict(dummy, newdata=dat_surv))
dat_surv[1:10, 1:15]

# Add the outcome columns back

dat_surv["babedeath_swf"] = dat_filter$babedeath_swf
dat_surv["babeadmit_swf"] = dat_filter$babeadmit_swf
dat_surv["babeoutcome_swf"] = dat_filter$babeoutcome_swf

# Including the Time to Death/Time to admit/Time to outcome columns from dat_baby

dat_surv["babedeath_swf_days"] = dat_baby$babedeath_swf_days
dat_surv["babeadmit_swf_days"] = dat_baby$babeadmit_swf_days
dat_surv["babeoutcome_swf_days"] = dat_baby$babeoutcome_swf_days

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Discrepancy between babeoutcome, babeoutcome_days ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 7 extra rows where babeoutcome_swf_days is NULL, though babeoutcome_swf is Yes
sum(is.na(dat_surv$babeoutcome_swf_days)) #5821
sum(dat_surv$babeoutcome_swf == 'No') #5814

# Checking for discrepancy in admit

# Discrepancy between babeadmit in dat_baby and dat_filter datasets
# 35 values of NA were imputed into babeadmit_swf in dat_filter
sum(is.na(dat_filter$babeadmit_swf)) #0
sum(is.na(dat_baby$babeadmit_swf))   #35

sum(!is.na(dat_baby$babeadmit_swf) & dat_baby$babeadmit_swf == 'Yes') #242
sum(dat_filter$babeadmit_swf == 'Yes') #242
# The number of Yes stays consistent in both datasets
# 35 rows changed from NA to No in babeadmit_swf (dat_baby -> dat_filter)

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

dat_surv <- dat_surv[!(dat_filter$studyid_adm %in% studyid_remove) , ]
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

table(dat_surv$status)
table(dat_surv$time)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FITTING THE FINE GRAY MODEL            ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting the estimated CIF ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cif <- cuminc(ftime = dat_surv$time, fstatus = dat_surv$status)
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

# Removing outcomes other than time, status

dat_surv <- dat_surv[, !(colnames(dat_surv) %in% c("babedeath_swf", "babedeath_swf_days", 
                                                       "babeadmit_swf", "babeadmit_swf_days", 
                                                       "babeoutcome_swf", "babeoutcome_swf_days"))]
colnames(dat_surv)
#181 One Hot Encoded predictors, time and status


# Fitting a fine gray model using first 4 predictors

# dat_temp <- dat_surv[, c(1:4, length(colnames(dat_surv))-1, length(colnames(dat_surv)))]
# SH <- FGR(formula = Hist(time,status)~admdeldiff_new+admdisdiff_new+admlabdiff_new+labdisdiff_new, 
#           data = dat_surv[, c(1:4, length(colnames(dat_surv))-1, length(colnames(dat_surv)))],
#           cause = 1)
# 
# SH <- FGR(formula = "Hist(time,status)~.", data = dat_temp)

model_death <- crr(dat_surv$time, dat_surv$status, dat_surv[, 1:4], failcode = 1)
model_death$coef

model_admit <- crr(dat_surv$time, dat_surv$status, dat_surv[, 1:4], failcode = 2)
model_admit$coef

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

cols_cat_start <- c(6,9,35,42,68,73,82,94,98,109,119,124,137,149,165)
cols_cat_end <- c(8,11,38,45,72,75,86,96,103,111,121,126,139,154,167)

cols_cat <- c()
for (i in 1:length(cols_cat_start)) {
  cols_cat <- c(cols_cat, cols_cat_start[i]:cols_cat_end[i])
}
cols_notcat <- setdiff((1:(length(colnames(dat_surv))-2)), cols_cat)


# Iterating over categorical predictors and storing values

for (i in 1:length(cols_cat_start)) {
  
  # Univariable Fine-Gray Model for death
  model_death <- crr(dat_surv$time, dat_surv$status, cov1 = dat_surv[, (cols_cat_start[i]+1):cols_cat_end[i]], failcode = 1)
  model_temp <- tidy(model_death, conf.int = TRUE)
  # Storing the required values for death outcome
  coeffs_death_surv <- c(coeffs_death_surv, model_temp$estimate)
  death_pvals_surv <- c(death_pvals_surv, model_temp$p.value)
  death_CIstart_coeff <- c(death_CIstart_coeff, model_temp$conf.low)
  death_CIend_coeff <- c(death_CIend_coeff, model_temp$conf.high)
  
  # Model for readmit 
  model_admit <- crr(dat_surv$time, dat_surv$status, dat_surv[, (cols_cat_start[i]+1):cols_cat_end[i]], failcode = 2)
  model_temp <- tidy(model_admit, conf.int = TRUE)
  # Storing values
  coeffs_admit_surv <- c(coeffs_admit_surv, model_temp$estimate)
  admit_pvals_surv <- c(admit_pvals_surv, model_temp$p.value)
  admit_CIstart_coeff <- c(admit_CIstart_coeff, model_temp$conf.low)
  admit_CIend_coeff <- c(admit_CIend_coeff, model_temp$conf.high)
}

# Naming with categorical predictor names

predictor_names <- colnames(dat_surv)[!(colnames(dat_surv) %in% c("status", "time"))]
length(predictor_names)

names_cat <- c()
for (i in 1:length(cols_cat_start)) {
  names_cat <- c(names_cat, (cols_cat_start[i]+1):cols_cat_end[i])
}

names(coeffs_death_surv) <- predictor_names[names_cat]
names(coeffs_admit_surv) <- predictor_names[names_cat]
names(death_pvals_surv) <- predictor_names[names_cat]
names(admit_pvals_surv) <- predictor_names[names_cat]

names(death_CIstart_coeff) <- predictor_names[names_cat]
names(death_CIend_coeff) <- predictor_names[names_cat]
names(admit_CIstart_coeff) <- predictor_names[names_cat]
names(admit_CIend_coeff) <- predictor_names[names_cat]

# Iterating over non-categorical predictors and storing values

for (i in cols_notcat) {
  
  # Univariable Fine-Gray Model for death
  model_death <- crr(dat_surv$time, dat_surv$status, cov1 = dat_surv[, i], failcode = 1)
  model_temp <- tidy(model_death, conf.int = TRUE)
  # Storing the required values for death outcome
  coeffs_death_surv <- c(coeffs_death_surv, model_temp$estimate)
  death_pvals_surv <- c(death_pvals_surv, model_temp$p.value)
  death_CIstart_coeff <- c(death_CIstart_coeff, model_temp$conf.low)
  death_CIend_coeff <- c(death_CIend_coeff, model_temp$conf.high)
  
  # Model for readmit 
  model_admit <- crr(dat_surv$time, dat_surv$status, dat_surv[, i], failcode = 2)
  model_temp <- tidy(model_admit, conf.int = TRUE)
  # Storing values
  coeffs_admit_surv <- c(coeffs_admit_surv, model_temp$estimate)
  admit_pvals_surv <- c(admit_pvals_surv, model_temp$p.value)
  admit_CIstart_coeff <- c(admit_CIstart_coeff, model_temp$conf.low)
  admit_CIend_coeff <- c(admit_CIend_coeff, model_temp$conf.high)
}

# Naming with non-categorical predictor names

names(coeffs_death_surv) <- c(predictor_names[names_cat], predictor_names[cols_notcat])
names(coeffs_admit_surv) <- c(predictor_names[names_cat], predictor_names[cols_notcat])
names(death_pvals_surv) <- c(predictor_names[names_cat], predictor_names[cols_notcat])
names(admit_pvals_surv) <- c(predictor_names[names_cat], predictor_names[cols_notcat])

names(death_CIstart_coeff) <- c(predictor_names[names_cat], predictor_names[cols_notcat])
names(death_CIend_coeff) <- c(predictor_names[names_cat], predictor_names[cols_notcat])
names(admit_CIstart_coeff) <- c(predictor_names[names_cat], predictor_names[cols_notcat])
names(admit_CIend_coeff) <- c(predictor_names[names_cat], predictor_names[cols_notcat])

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
# DATA VIZ.          ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Identifying predictors
predictor_names <- c(predictor_names[names_cat], predictor_names[cols_notcat])
length(predictor_names)

# Extracting lists based on the sign of coefficients
pred_same_sign_pos <- (coeffs_death_surv>0) & (coeffs_admit_surv>0)
pred_same_sign_neg <- (coeffs_death_surv<0) & (coeffs_admit_surv<0)
pred_diff_sign_posneg <- (coeffs_death_surv>0) & (coeffs_admit_surv<0)
pred_diff_sign_negpos <- (coeffs_death_surv<0) & (coeffs_admit_surv>0)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting coefficients ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Note for different directions, as only death/readmit no sense in just relative barplot
# Instead make relative for their absolute magnitudes

# Same sign positive
# Creating the dataframe for the plot
outcome = c(rep('death', sum(pred_same_sign_pos)), rep('admit', sum(pred_same_sign_pos)))
coeffs = c(coeffs_death_surv[pred_same_sign_pos], coeffs_admit_surv[pred_same_sign_pos])
labs = rep(predictor_names[pred_same_sign_pos], 2)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the different coefficients
plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - positive", subtitle = 'Relative proportions of Fine-Gray coefficients') +
  labs(x = 'Coefficient percentages') + 
  labs(y = 'Variables') + 
  theme_light()

# Saving to folder Survival
ggplot2::ggsave('samedir_pos_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Coeffs', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Plot 2: Values of the actual Fine-Gray coefficients
plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - positive", subtitle = 'Values of Fine-Gray coefficients') +
  labs(x = 'Coefficients') + 
  labs(y = 'Variables') + 
  theme_light()

# Saving ...
ggplot2::ggsave('samedir_pos_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Coeffs', device = 'png', width=1280/72, height = 720/72, dpi = 72)


# Same sign negative
# Creating the dataframe for the plot
outcome = c(rep('death', sum(pred_same_sign_neg)), rep('admit', sum(pred_same_sign_neg)))
coeffs = c(coeffs_death_surv[pred_same_sign_neg], coeffs_admit_surv[pred_same_sign_neg])
labs = rep(predictor_names[pred_same_sign_neg], 2)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the different coefficients
plot1 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - negative", subtitle = 'Relative proportions of Fine-Gray coefficients') +
  labs(x = 'Coefficient percentages') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('samedir_neg_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Coeffs', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Plot 2: Values of the actual Fine-Gray coefficients
plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - negative", subtitle = 'Values of Fine-Gray coefficients') +
  labs(x = 'Coefficients') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('samedir_neg_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Coeffs', device = 'png', width=1280/72, height = 720/72, dpi = 72)


# Different sign - Death positive, readmit negative
# Creating the dataframe for the plot
outcome = c(rep('death', sum(pred_diff_sign_posneg)), rep('admit', sum(pred_diff_sign_posneg)))
coeffs = c(coeffs_death_surv[pred_diff_sign_posneg], coeffs_admit_surv[pred_diff_sign_posneg])
labs = rep(predictor_names[pred_diff_sign_posneg], 2)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the abs magnitude of different coefficients
plot1 <- ggplot(df_plot, aes(x=labs, y=abs(coeffs), fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Opposite sign - Death +ve, Readmit -ve", subtitle = 'Relative proportions of absolute Fine-Gray coefficients') +
  labs(x = 'Coefficient percentages') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('diffdir_posneg_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Coeffs', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Plot 2: Values of the actual Fine-Gray coefficients
plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Opposite sign - Death +ve, Readmit -ve", subtitle = 'Values of Fine-Gray coefficients') +
  labs(x = 'Coefficients') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('diffdir_posneg_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Coeffs', device = 'png', width=1280/72, height = 720/72, dpi = 72)


# Different sign - Death negative, readmit positive
# Creating the dataframe for the plot
outcome = c(rep('death', sum(pred_diff_sign_negpos)), rep('admit', sum(pred_diff_sign_negpos)))
coeffs = c(coeffs_death_surv[pred_diff_sign_negpos], coeffs_admit_surv[pred_diff_sign_negpos])
labs = rep(predictor_names[pred_diff_sign_negpos], 2)

df_plot = data.frame(outcome, coeffs, labs)

# Plot 1: Relative percentages of the abs magnitude of different coefficients
plot1 <- ggplot(df_plot, aes(x=labs, y=abs(coeffs), fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Opposite sign - Death -ve, Readmit +ve", subtitle = 'Relative proportions of absolute Fine-Gray coefficients') +
  labs(x = 'Coefficient percentages') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('diffdir_negpos_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Coeffs', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Plot 2: Values of the actual Fine-Gray coefficients
plot2 <- ggplot(df_plot, aes(x=labs, y=coeffs, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Opposite sign - Death -ve, Readmit +ve", subtitle = 'Values of Fine-Gray coefficients') +
  labs(x = 'Coefficients') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('diffdir_negpos_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Coeffs', device = 'png', width=1280/72, height = 720/72, dpi = 72)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting Odds Ratios ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Same sign positive
# Creating the dataframe for the plot
outcome = c(rep('death', sum(pred_same_sign_pos)), rep('admit', sum(pred_same_sign_pos)))
odds = c(ratios_death[pred_same_sign_pos], ratios_admit[pred_same_sign_pos])
labs = rep(predictor_names[pred_same_sign_pos], 2)

df_plot = data.frame(outcome, odds, labs)

# Plot 1: Relative percentages of the odds ratios
# Add lines for 0.35/0.65 for easy comparing
plot1 <- ggplot(df_plot, aes(x=labs, y=odds, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  geom_hline(yintercept = 0.65) +
  geom_hline(yintercept = 0.35) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - positive", subtitle = 'Relative proportions of Fine-Gray SubHazard ratios') +
  labs(x = 'SubHazards ratio percentages') + 
  labs(y = 'Variables') + 
  theme_light()

# Saving to the Survival folder
ggplot2::ggsave('samedir_pos_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Ratios', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Plot 2: Values of the actual Sub-hazards ratios
plot2 <- ggplot(df_plot, aes(x=labs, y=odds, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - positive", subtitle = 'Values of Fine-Gray SubHazards ratios') +
  labs(x = 'SubHazards ratios') + 
  labs(y = 'Variables') +
  scale_y_continuous(limits = c(0,5)) +
  theme_light()

# Saving ...
ggplot2::ggsave('samedir_pos_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Ratios', device = 'png', width=1280/72, height = 720/72, dpi = 72)


# Same sign negative
# Creating the dataframe for the plot
outcome = c(rep('death', sum(pred_same_sign_neg)), rep('admit', sum(pred_same_sign_neg)))
odds = c(ratios_death[pred_same_sign_neg], ratios_admit[pred_same_sign_neg])
labs = rep(predictor_names[pred_same_sign_neg], 2)

df_plot = data.frame(outcome, odds, labs)

# Plot 1: Relative percentages of the different hazard ratios
# Add lines for 0.35/0.65 for easy comparing
plot1 <- ggplot(df_plot, aes(x=labs, y=odds, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  geom_hline(yintercept = 0.65) +
  geom_hline(yintercept = 0.35) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - negative", subtitle = 'Relative proportions of Fine-Gray SubHazards ratios') +
  labs(x = 'SubHazards ratio percentages') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('samedir_neg_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Ratios', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Plot 2: Values of the actual Sub-hazards ratios
plot2 <- ggplot(df_plot, aes(x=labs, y=odds, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Same sign - negative", subtitle = 'Values of Fine-Gray SubHazards ratios') +
  labs(x = 'SubHazards ratios') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('samedir_neg_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Ratios', device = 'png', width=1280/72, height = 720/72, dpi = 72)


# Different sign - Death positive, readmit negative
# Creating the dataframe for the plot
outcome = c(rep('death', sum(pred_diff_sign_posneg)), rep('admit', sum(pred_diff_sign_posneg)))
ratios = c(ratios_death[pred_diff_sign_posneg], ratios_admit[pred_diff_sign_posneg])
labs = rep(predictor_names[pred_diff_sign_posneg], 2)

df_plot = data.frame(outcome, ratios, labs)

# Plot 1: Relative percentages of the two subhazards ratios
plot1 <- ggplot(df_plot, aes(x=labs, y=ratios, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  geom_hline(yintercept = 0.65) +
  geom_hline(yintercept = 0.35) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Opposite sign - Death +ve, Readmit -ve", subtitle = 'Relative proportions of Fine-Gray SubHazards ratios') +
  labs(x = 'SubHazards ratio percentages') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('diffdir_posneg_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Ratios', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Plot 2: Values of the actual Sub-hazards ratios
plot2 <- ggplot(df_plot, aes(x=labs, y=ratios, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Opposite sign - Death +ve, Readmit -ve", subtitle = 'Values of Fine-Gray SubHazards ratios') +
  labs(x = 'SubHazards ratios') + 
  labs(y = 'Variables') + 
  scale_y_continuous(limits = c(0,10)) +
  theme_light()

ggplot2::ggsave('diffdir_posneg_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Ratios', device = 'png', width=1280/72, height = 720/72, dpi = 72)


# Different sign - Death negative, readmit positive
# Creating the dataframe for the plot
outcome = c(rep('death', sum(pred_diff_sign_negpos)), rep('admit', sum(pred_diff_sign_negpos)))
ratios = c(ratios_death[pred_diff_sign_negpos], ratios_admit[pred_diff_sign_negpos])
labs = rep(predictor_names[pred_diff_sign_negpos], 2)

df_plot = data.frame(outcome, ratios, labs)

# Plot 1: Relative percentages of the two subhazards ratios
plot1 <- ggplot(df_plot, aes(x=labs, y=ratios, fill=outcome)) + 
  geom_bar(stat="identity", position='fill') +
  geom_hline(yintercept = 0.65) +
  geom_hline(yintercept = 0.35) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Opposite sign - Death -ve, Readmit +ve", subtitle = 'Relative proportions of Fine-Gray SubHazards ratios') +
  labs(x = 'SubHazards ratio percentages') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('diffdir_negpos_rel.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Ratios', device = 'png', width=1280/72, height = 720/72, dpi = 72)

# Plot 2: Values of the actual Sub-hazards ratios
plot2 <- ggplot(df_plot, aes(x=labs, y=ratios, fill=outcome)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  coord_flip() +
  labs(colours = 'Outcome') +
  labs(title = "Opposite sign - Death -ve, Readmit +ve", subtitle = 'Values of Fine-Gray SubHazards ratios') +
  labs(x = 'SubHazards ratios') + 
  labs(y = 'Variables') + 
  theme_light()

ggplot2::ggsave('diffdir_negpos_abs.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/Ratios', device = 'png', width=1280/72, height = 720/72, dpi = 72)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting Hazard Ratios with CIs ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Naming the ratio vectors
# names(ratios_death) <- predictor_names
# names(ratios_death_start) <- predictor_names
# names(ratios_death_end) <- predictor_names
# 
# names(ratios_admit) <- predictor_names
# names(ratios_admit_start) <- predictor_names
# names(ratios_admit_end) <- predictor_names

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

#Same sign - positive
# Creating dataframe
df_ratios <- data.frame(ratios = c(ratios_death[pred_same_sign_pos], ratios_admit[pred_same_sign_pos]),
                      CI_start = c(ratios_death_start[pred_same_sign_pos], ratios_admit_start[pred_same_sign_pos]),
                      CI_end = c(ratios_death_end[pred_same_sign_pos], ratios_admit_end[pred_same_sign_pos]),
                      pred = rep(factor(predictor_names[pred_same_sign_pos], levels = sort(predictor_names[pred_same_sign_pos], decreasing = TRUE)), 2),
                      outcome = c(rep('death', sum(pred_same_sign_pos)), rep('admit', sum(pred_same_sign_pos))))

temp_ind = (dim(df_ratios)[1])/2

# Plotting CIs
plot1 <- df_ratios %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Same direction positive", subtitle = 'Confidence intervals for SubHazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark() +
  scale_y_continuous(limits = c(0,30))

plot2 <- df_ratios[c(31:temp_ind, (temp_ind+31):(2*temp_ind)),] %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Same direction positive", subtitle = 'Confidence intervals for SubHazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark() +
  scale_y_continuous(limits = c(0,30))

# Saving to Survival folder
ggplot2::ggsave('samedir_pos.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 144)

#Same sign - negative
# Creating dataframe
df_ratios <- data.frame(ratios = c(ratios_death[pred_same_sign_neg], ratios_admit[pred_same_sign_neg]),
                      CI_start = c(ratios_death_start[pred_same_sign_neg], ratios_admit_start[pred_same_sign_neg]),
                      CI_end = c(ratios_death_end[pred_same_sign_neg], ratios_admit_end[pred_same_sign_neg]),
                      pred = rep(factor(predictor_names[pred_same_sign_neg], levels = sort(predictor_names[pred_same_sign_neg], decreasing = TRUE)), 2),
                      outcome = c(rep('death', sum(pred_same_sign_neg)), rep('admit', sum(pred_same_sign_neg))))

df_ratios <- df_ratios %>% 
  arrange(outcome, pred)
temp_ind = (dim(df_ratios)[1])/2

# Plotting CIs
plot1 <- df_ratios[c(1:20,(temp_ind+1):(temp_ind+20)), ] %>%
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Same direction negative", subtitle = 'Confidence intervals for SubHazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark() +
  scale_y_continuous(limits = c(0,20))

plot2 <- df_ratios[c(21:temp_ind, (temp_ind+21):(2*temp_ind)),] %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Same direction negative", subtitle = 'Confidence intervals for SubHazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark() +
  scale_y_continuous(limits = c(0,20))

ggplot2::ggsave('samedir_neg2.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 144)
ggplot2::ggsave('samedir_neg1.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 144)

#Different sign - death positive, readmit negative
# Creating dataframe
df_ratios <- data.frame(ratios = c(ratios_death[pred_diff_sign_posneg], ratios_admit[pred_diff_sign_posneg]),
                      CI_start = c(ratios_death_start[pred_diff_sign_posneg], ratios_admit_start[pred_diff_sign_posneg]),
                      CI_end = c(ratios_death_end[pred_diff_sign_posneg], ratios_admit_end[pred_diff_sign_posneg]),
                      pred = rep(factor(predictor_names[pred_diff_sign_posneg], levels = sort(predictor_names[pred_diff_sign_posneg], decreasing = TRUE)), 2),
                      outcome = c(rep('death', sum(pred_diff_sign_posneg)), rep('admit', sum(pred_diff_sign_posneg))))

df_ratios <- df_ratios %>% 
  arrange(outcome, pred)
temp_ind = (dim(df_ratios)[1])/2

# Plotting CIs
plot1 <- df_ratios[c(1:25,(temp_ind+1):(temp_ind+25)), ] %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death positive, Readmit negative", subtitle = 'Confidence intervals for Subhazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark()+
  scale_y_continuous(limits = c(0,30))

plot2 <- df_ratios[c(26:temp_ind, (temp_ind+26):(2*temp_ind)),] %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death positive, Readmit negative", subtitle = 'Confidence intervals for Subhazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark()+
  scale_y_continuous(limits = c(0,30))

ggplot2::ggsave('diffdir_posneg2.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 144)
ggplot2::ggsave('diffdir_posneg1.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 144)

#Different sign - death negative, readmit positive
# Creating dataframe
df_ratios <- data.frame(ratios = c(ratios_death[pred_diff_sign_negpos], ratios_admit[pred_diff_sign_negpos]),
                      CI_start = c(ratios_death_start[pred_diff_sign_negpos], ratios_admit_start[pred_diff_sign_negpos]),
                      CI_end = c(ratios_death_end[pred_diff_sign_negpos], ratios_admit_end[pred_diff_sign_negpos]),
                      pred = rep(factor(predictor_names[pred_diff_sign_negpos], sort(predictor_names[pred_diff_sign_negpos], decreasing = TRUE)), 2),
                      outcome = c(rep('death', sum(pred_diff_sign_negpos)), rep('admit', sum(pred_diff_sign_negpos))))

df_ratios <- df_ratios %>% 
  arrange(outcome, pred)
temp_ind = (dim(df_ratios)[1])/2

# Plotting CIs
plot1 <- df_ratios[c(1:30,(temp_ind+1):(temp_ind+30)), ] %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death negative, Readmit positive", subtitle = 'Confidence intervals for Subhazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark()+
  scale_y_continuous(limits = c(0,20))

plot2 <- df_ratios[c(31:temp_ind, (temp_ind+31):(2*temp_ind)),] %>% 
  ggplot( aes(x = pred, y = ratios, colour = outcome, group = outcome)) +
  geom_point(position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1) +
  coord_flip()+
  labs(title = "Death negative, Readmit positive", subtitle = 'Confidence intervals for Subhazards ratios') +
  labs(x = 'Predictors') + 
  labs(y = 'SubHazards Ratios') + 
  #theme_dark()+
  scale_y_continuous(limits = c(0,20))

ggplot2::ggsave('diffdir_negpos2.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 144)
ggplot2::ggsave('diffdir_negpos1.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs', device = 'png', width=1280/72, height = 720/72, dpi = 144)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ANALYSING SUBHAZARDS RATIOS          ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Naming the vectors for subhazards ratios

names(ratios_death) <- predictor_names
names(ratios_death_start) <- predictor_names
names(ratios_death_end) <- predictor_names

names(ratios_admit) <- predictor_names
names(ratios_admit_start) <- predictor_names
names(ratios_admit_end) <- predictor_names

# Columns with large confidence intervals (start < .01 or end > 100)

ratios_death_end[ratios_death_end > 100] # 3 predictors
ratios_death_start[ratios_death_start < 0.01] # 17 predictors
(ratios_death_end/ratios_death_start)[ratios_death_end/ratios_death_start > 50]

ratios_admit_end[ratios_admit_end > 100] # 0 predictors
ratios_admit_start[ratios_admit_start < 0.01] # 6 predictors
(ratios_admit_end/ratios_admit_start)[ratios_admit_end/ratios_admit_start > 50] # 5 predictors

# Finding predictors where the two hazards ratios are within n factors of each other

n = 1.5
cols_factor <- predictor_names[((ratios_death/ratios_admit) < n) &
                                 ((ratios_admit/ratios_death) < n)]
length(cols_factor) # 59 columns for n = 1.5
cols_factor
sum(cols_factor %in% (predictor_names[pred_same_sign_neg | pred_same_sign_pos])) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FITTING COX REGRESSION FOR A COMBINED OUTCOME          ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Recreating the babeoutcome column ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dim(dat_surv)
dat_cox <- dat_surv %>%
  mutate(babeoutcome_swf = case_when(status != 0 ~ 1,
                                     status == 0 ~ 0))
table(dat_cox$babeoutcome_swf)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## KM Survival Curve ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

surv_Object <- Surv(dat_cox$time, dat_cox$babeoutcome_swf)
comb_fit <- survfit(formula = surv_Object ~ sexbb_del_new.Male, data = dat_cox)
summary(comb_fit)

plot1 <- ggsurvplot(comb_fit, data = dat_cox, conf.int = TRUE, pval = TRUE, fun = "event",
           legend.labs = c("Female", "Male"), legend.title = "Sex of the baby",
           legend = c(0.8, 0.8),
           ylim = c(0, 0.1), xlab = "Time (days from birth)", title = "KM Mortality Curve - Combined Outcome")

ggsave(paste0('Combined Outcome KM Survival Plot.png', analysis_date, ".png"), path = 'Results', device = 'png', width=1280/72, height = 720/72, dpi = 144)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Cox regression and odds ratios ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initializing vector variables

coeffs_comb_surv <- c()
comb_pvals_surv <- c()
comb_CIstart_coeff <- c()
comb_CIend_coeff <- c()

# Iterating over every categorical (>2 cats) predictor variable and storing the values

for (i in 1:length(cols_cat_start)) {
  
  # Univariable Fine-Gray Model for death
  model_comb <- coxph(Surv(time, babeoutcome_swf) ~ ., 
                      data = dat_cox[, c((cols_cat_start[i]+1):cols_cat_end[i], dim(dat_cox)[2] -1, dim(dat_cox)[2])])
  model_temp <- tidy(model_comb, conf.int = TRUE)
  # Storing the required values for death outcome
  coeffs_comb_surv <- c(coeffs_comb_surv, model_temp$estimate)
  comb_pvals_surv <- c(comb_pvals_surv, model_temp$p.value)
  comb_CIstart_coeff <- c(comb_CIstart_coeff, model_temp$conf.low)
  comb_CIend_coeff <- c(comb_CIend_coeff, model_temp$conf.high)
}

# Iterating over every non-categorical predictor variable and storing the values

for (i in cols_notcat) {
  
  # Univariable Fine-Gray Model for death
  model_comb <- coxph(Surv(time, babeoutcome_swf) ~ ., 
                      data = dat_cox[, c(i, dim(dat_cox)[2] -1, dim(dat_cox)[2])])
  model_temp <- tidy(model_comb, conf.int = TRUE)
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
predictor_names <- colnames(dat_surv)[!(colnames(dat_surv) %in% c("status", "time"))]
length(predictor_names)

names(coeffs_comb_surv) <- c(predictor_names[names_cat], predictor_names[cols_notcat])
names(comb_pvals_surv) <- c(predictor_names[names_cat], predictor_names[cols_notcat])
names(comb_CIstart_coeff) <- c(predictor_names[names_cat], predictor_names[cols_notcat])
names(comb_CIend_coeff) <- c(predictor_names[names_cat], predictor_names[cols_notcat])

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
predictor_names <- c(predictor_names[names_cat], predictor_names[cols_notcat])
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
## Plotting new CIs ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Naming the ratio vectors
# names(ratios_death) <- predictor_names
# names(ratios_death_start) <- predictor_names
# names(ratios_death_end) <- predictor_names
# 
# names(ratios_admit) <- predictor_names
# names(ratios_admit_start) <- predictor_names
# names(ratios_admit_end) <- predictor_names
# 
# names(ratios_comb) <- predictor_names
# names(ratios_comb_start) <- predictor_names
# names(ratios_comb_end) <- predictor_names

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

# ratios_death[cols_continuos] <- ratios_death[cols_continuos]^20
# ratios_death_start[cols_continuos] <- ratios_death_start[cols_continuos]^20
# ratios_death_end[cols_continuos] <- ratios_death_end[cols_continuos]^20
# 
# ratios_admit[cols_continuos] <- ratios_admit[cols_continuos]^20
# ratios_admit_start[cols_continuos] <- ratios_admit_start[cols_continuos]^20
# ratios_admit_end[cols_continuos] <- ratios_admit_end[cols_continuos]^20

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
ggplot2::ggsave('samedir_pos.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs New', device = 'png', width=1280/72, height = 720/72, dpi = 144)

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

ggplot2::ggsave('samedir_neg2.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs New', device = 'png', width=1280/72, height = 720/72, dpi = 144)
ggplot2::ggsave('samedir_neg1.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs New', device = 'png', width=1280/72, height = 720/72, dpi = 144)

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

ggplot2::ggsave('diffdir_posneg2.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs New', device = 'png', width=1280/72, height = 720/72, dpi = 144)
ggplot2::ggsave('diffdir_posneg1.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs New', device = 'png', width=1280/72, height = 720/72, dpi = 144)

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

ggplot2::ggsave('diffdir_negpos2.png', plot = plot1, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs New', device = 'png', width=1280/72, height = 720/72, dpi = 144)
ggplot2::ggsave('diffdir_negpos1.png', plot = plot2, path = 'C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Plots/Survival/CIs New', device = 'png', width=1280/72, height = 720/72, dpi = 144)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIGURES FOR EXPORT             ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Admit and Death CIFs ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cif <- cuminc(ftime = dat_surv$time, fstatus = dat_surv$status)

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TABLES FOR EXPORT             ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Admit and Death SHRs ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Vars = colnames(dat_surv[, 1:(dim(dat_surv)[2]-2)])
surv_ana_results <- data.frame(Variables = Vars,
                               Death_SHRs = round(ratios_death[Vars], 3),
                               Death_CIs = sprintf("%.3f to %.3f", ratios_death_start[Vars], ratios_death_end[Vars]),
                               Death_Pvals = death_pvals_surv[Vars],
                               Admit_SHRs = round(ratios_admit[Vars], 3),
                               Admit_CIs = sprintf("%.3f to %.3f", ratios_admit_start[Vars], ratios_admit_end[Vars]),
                               Admit_Pvals = admit_pvals_surv[Vars]
                               )
# surv_ana_results[2] = round(ratios_death[surv_ana_results$Variables], 3)
# surv_ana_results[surv_ana_results$Variables %in% names(ratios_death), 3] = sprintf("%.3f to %.3f", ratios_death_start, ratios_death_end)
# surv_ana_results[surv_ana_results$Variables %in% names(ratios_death), 4] = death_pvals_surv
# surv_ana_results[surv_ana_results$Variables %in% names(ratios_admit), 5] = round(ratios_admit, 3)
# surv_ana_results[surv_ana_results$Variables %in% names(ratios_admit), 6] = sprintf("%.3f to %.3f", ratios_admit_start, ratios_admit_end)
# surv_ana_results[surv_ana_results$Variables %in% names(ratios_admit), 7] = admit_pvals_surv

surv_ana_results[1:20, ]

# Exporting the table
write.xlsx(surv_ana_results, paste0("Results/Death and Readmit SHRs (", analysis_date, ").xlsx"),
           colWidths = "auto", rowNames = FALSE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Comb HRs ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Vars = colnames(dat_surv[, 1:(dim(dat_surv)[2]-2)])
surv_ana_results <- data.frame(Variables = Vars,
                               Comb_SHRs = round(ratios_comb[Vars], 3),
                               Comb_CIs = sprintf("%.3f to %.3f", ratios_comb_start[Vars], ratios_comb_end[Vars]),
                               Comb_Pvals = comb_pvals_surv[Vars]
                               )
surv_ana_results[1:20, ]

# Exporting the table
write.xlsx(surv_ana_results, paste0("Results/Comb HRs (", analysis_date, ").xlsx"),
           colWidths = "auto", rowNames = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE WORKSPACE             ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create lists of new columns and store for prediction modelling
pred_same_sign <- c(predictor_names[pred_same_sign_pos], predictor_names[pred_same_sign_neg])

# Only keep final dataframes
to_keep <- c("dat_surv", "dat_cox", "dat_baby", "pred_same_sign", "analysis_date")
rm(list = setdiff(ls(), to_keep))
save.image(paste0("Cleaned Data/Cleaned_Data (", analysis_date, ").RData"))
