# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 03_DEMOGRAPHICS         ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Demographics and univariable odds ratios

library(tidyverse)
library(Hmisc)
# library(table1)
library(openxlsx)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA               ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("Cleaned Data/Cleaned_Data (2024-04-26).RData")

# Analysis date
analysis_date <- "2024-06-04"

# Load new outcomes
 # new_outcomes <- read.xlsx("Data/MB Maternal Admission Classifications and Model Outcomes for VN.xlsx", 
 #                           sheet = 1, check.names = TRUE) %>% 
 #   filter(!(Notes %in% "duplicate"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HELPER FUNCTIONS          #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Demographic function
DemographicSummary <- function(Data,
                               Vars,
                               SummariseMean = NA,
                               SummariseMedian = NA,
                               SummariseYesNo = NA,
                               SummariseCat = NA,
                               YesLabels = c("Yes", "yes", "Checked", 1, "Male", "Vaginal", "Single"),
                               OrderLevels = c(""),
                               Title = NA) {
  
  # Data: dat_mom or dat_baby
  # Vars: vector of variables to summarise
  # SummariseMean: Vector of variables to summarise as Mean (SD)
  # SummariseMedian: Vector of variables to summarise as Median (Q1, Q3)
  # SummariseYesNo: Vector of variables to summarise as single row (usually yes/no variables)
  # SummariseCat: Vector of variables to summarise for each level of the category
  # YesLabels: For SummariseYesNo variables, which label codes for yes (positive cases)
  # OrderLevels: For categorical variables that are not yes/no, 
  #   vector of variables to summarise in the order that the factor levels are in
  #   Otherwise, the categories will be ordered alphabetically. 
  # Title: Name of domain
  
  # Filter out only the columns being summarised
  Data_reduced <- Data %>% 
    dplyr::select(Vars)
  
  # Initialise dataframe and calculate n missing
  demographic_summary <- 
    data.frame(
      Variable = Vars,
      Outcome = NA,
      NMissing = apply(Data_reduced, 2, 
                       function(X) paste0(sum(is.na(X)), " (",
                                          round(sum(is.na(X) / length(X)) * 100, 2), 
                                          "%)")
      )
    )
  
  # Check if variable is listed in SummariseMean, Median, YesNo, or Cat
  # If is not been explicitly stated, 
  #  use mean if numeric, yes/no if binary, or cat if categorical by default
  check_cols <- setdiff(Vars, c(SummariseMean, SummariseMedian,
                                SummariseYesNo, SummariseCat))
  
  if (length(check_cols > 0)) {
    for (i in 1:length(check_cols)) {
      if (is.numeric(Data_reduced[, check_cols[i]])) {
        SummariseMean <- c(SummariseMean, check_cols[i])
      } else {
        # Check if binary
        is_binary <- names(table(Data_reduced[, check_cols[i]]))
        is_binary <- length(is_binary) == 2
        if (is_binary) {
          SummariseYesNo = c(SummariseYesNo, check_cols[i])
        } else {
          SummariseCat = c(SummariseCat, check_cols[i])
        }
      }
    }
  }
  
  
  # Summarise
  Data_reduced <- data.frame(Data_reduced)
  for (i in 1:length(Vars)) {
    
    if (Vars[i] %in% SummariseMean) {
      Mean <- round(mean(Data_reduced[,i], na.rm = TRUE), 1)
      SD <- round(sd(Data_reduced[,i], na.rm = TRUE), 1)
      # Report mean as "Mean (SD)"
      demographic_summary$Outcome[i] <- paste0(Mean, " (", SD, ")")
      
    } else if (Vars[i] %in% SummariseMedian) {
      
      Median <- round(median(Data_reduced[,i], na.rm = TRUE), 1)
      Q1 <- round(quantile(Data_reduced[,i], na.rm = TRUE)[2], 1)
      Q3 <- round(quantile(Data_reduced[,i], na.rm = TRUE)[4], 1)
      # Report median as "Median (Q1, Q3)"
      demographic_summary$Outcome[i] <- paste0(Median, " (",
                                               Q1, ", ",
                                               Q3, ")")
      
    }
  }
  
  # For categorical variables, we'll need to add rows
  for (i in 1:length(Vars)) {
    # Yes/No Variables or other binary variables to collapse into single row
    if (Vars[i] %in% SummariseYesNo) {
      which_row <- which(demographic_summary$Variable == Vars[i])
      
      # Count values for Yes or 1
      # categories <- names(table(Data[, Vars[i]]))[2]
      N <- sum(Data_reduced[,i] %in% YesLabels, na.rm = TRUE)
      Percentage <- round(N / nrow(Data_reduced) * 100, 1)
      # Report categorical variables as n (%)
      demographic_summary$Outcome[which_row] <- paste0(N, " (", Percentage, "%)")
      
      # # Insert additional row if missing
      # if (sum(is.na(Data_reduced[,i])) > 0) {
      #   demographic_summary <- demographic_summary %>%
      #     add_row(.after = which_row)
      #   
      #   demographic_summary$Variable[which_row + 1] <- "Missing"
      #   N <- sum(is.na(Data_reduced[,i]))
      #   Percentage <- round(N / nrow(Data_reduced) * 100, 1)
      #   demographic_summary$Outcome[which_row + 1] <- paste0(N, " (", Percentage, "%)")
      # }
      
    } else if (Vars[i] %in% SummariseCat) {
      # Categories with more than 2 levels or not Yes/No
      which_row <- which(demographic_summary$Variable == Vars[i])
      
      # All possible category values
      if (Vars[i] %in% OrderLevels) {
        Categories <- levels(Data_reduced[,i])
      } else {
        Categories <- as.character(sort(unique(Data_reduced[,i])))
      }
      
      for (j in 1:length(Categories)) {
        
        # Insert new row for each possible category
        demographic_summary <- demographic_summary %>% 
          add_row(.after = which_row + j - 1)
        
        # Report categorical variables as n (%)
        N <- sum(Data_reduced[,i] == Categories[j], na.rm = TRUE)
        Percentage <- round(N / nrow(Data_reduced) * 100, 1)
        
        demographic_summary$Variable[which_row + j] <- Categories[j]
        demographic_summary$Outcome[which_row + j] <- paste0(N, " (", Percentage, "%)")
      }
      
      # # Insert additional row if missing
      # if (sum(is.na(Data_reduced[,i])) > 0) {
      #   demographic_summary <- demographic_summary %>%
      #     add_row(.after = which_row + length(Categories))
      #   
      #   demographic_summary$Variable[which_row + length(Categories) + 1] <- "Missing"
      #   N <- sum(is.na(Data_reduced[,i]))
      #   Percentage <- round(N / nrow(Data_reduced) * 100, 1)
      #   demographic_summary$Outcome[which_row + length(Categories) + 1] <- paste0(N, " (", Percentage, "%)")
      # }
    }
  }
  
  # Add title
  if (!is.na(Title)) {
    demographic_summary <- demographic_summary %>% 
      add_row(.before = 1, 
              Variable = Title)
  }
  
  # Remove rownames
  rownames(demographic_summary) <- NULL
  
  # Add sample size
  demographic_summary <- demographic_summary %>% 
    add_row(.before = 1, 
            Variable = "N",
            Outcome = as.character(nrow(Data)))
  
  return(demographic_summary)
}

# Univariable models
UnivariableModels <- function(Data = dat_mom,
                              Vars, 
                              Outcome = "momadmit_swf",
                              Domain = "") {
  
  # Need library(broom)
  if ( !("broom" %in% .packages())) {
    require(broom)
  }
  
  # Data: data
  # Vars: list of variables, use the domains
  # Outcome: pd_mortality
  # Domain: Heading for the output
  
  # For maternal outcomes using maternal variables, remove duplicates
  # This is applicable for mum+baby, not smart discharges
  # Data <- Data[,c("studyid_adm", Vars, Outcome)]
  # Data <- unique(Data)
  
  Data <- Data %>% 
    dplyr::select(all_of(Vars), 
           all_of(Outcome))
  
  # Univariable models
  uni_formula <- sapply(Vars, function(X) {as.formula(paste0(Outcome, " ~ ", X))})
  uni_models <- lapply(uni_formula, function(X) {glm(X, data = Data, family = "binomial")})
  
  # Tidy up output
  uni_export <- lapply(uni_models, function(X) tidy(X, exp = TRUE, conf.int = TRUE))
  uni_export <- do.call(rbind, uni_export)
  uni_export <- uni_export %>%
    mutate(OR = paste0(round(estimate, 2), " (",  round(conf.low, 2), ", ", round(conf.high, 2), ")"),
           p.value = round(p.value, 3)) %>%
    dplyr::select(term, OR, p.value) %>%
    filter(term != "(Intercept)") %>% 
    data.frame()
  
  # For factor variables, if more than two levels
  #   1) Add new row to indicate reference group
  #   2) Add global p-value
  
  for (i in 1:length(Vars)) {
    if (is.factor(Data[,Vars[i], drop = TRUE]) & 
        n_distinct(Data[,Vars[i]], na.rm = TRUE) > 2) {
      
      # Identify which row the first category is in
      which_row <- grep(Vars[i], uni_export$term)[1]
      
      # Identify the reference group
      reference_group <- levels(Data[,Vars[i], drop = TRUE])[1]
      
      # # Determine where to put the row for the reference group
      # cat_levels_order <- sort(levels(Data[,Vars[i], drop = TRUE]))
      # which_row_cat <- which(cat_levels_order == reference_group)
      # which_row_cat <- which_row + which_row_cat - 1
      which_row_cat <- which_row
      
      # Get p-value from corresponding model
      p_value <- anova(uni_models[[i]], test = "Chisq")
      
      # Add row for reference group
      uni_export <- uni_export %>% 
        add_row(.before = which_row_cat, 
                term = paste0(Vars[i], reference_group),
                OR = "ref.",
                p.value = round(p_value$`Pr(>Chi)`[2], 3))
      
      # Add another row for row heading
      uni_export <- uni_export %>% 
        add_row(.before = which_row,
                term = Vars[i],
                p.value = NA)
    }
  }
  
  # Add title
  uni_export <- uni_export %>% 
    add_row(.before = 1,
            term = Domain)
  
  # Set p to <0.001 if = 0
  uni_export <- uni_export %>% 
    mutate(p.value = ifelse(p.value == 0, "<0.001", as.character(p.value)))
  
  return(uni_export)
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

# All data: 9497 moms, 7444 babies
nrow(dat_mom_raw)
nrow(dat_baby_raw)
n_distinct(dat_mom$studyid_adm)

## ~~~~~~~~~~~~~~~
## Mum Exclusion  ####
## ~~~~~~~~~~~~~~~

# Admitted to hospital for delivery: 9373 moms, 7508 babies
admit_exclude <- subset(dat_mom, reason_adm_v2 == "No")
dat_mom <- subset(dat_mom, reason_adm_v2 == "Yes" | is.na(reason_adm_v2))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
n_distinct(admit_exclude$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)

# Eligible age: 9366 moms, 7503 babies
age_exclude <- subset(dat_mom, eligibleage_adm_v2 == "No")
dat_mom <- subset(dat_mom, eligibleage_adm_v2 == "Yes" | is.na(eligibleage_adm_v2))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
n_distinct(age_exclude$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)

# Check other exclusion: 7199 moms, 7488 babies
other_exclusion <- table(dat_mom %>% 
                           group_by(studyid_adm) %>% 
                           slice(1) %>% 
                           pull(exclusion_adm_v2))
dat_mom <- subset(dat_mom, exclusion_adm_v2 == "No exclusion criteria apply" | is.na(exclusion_adm_v2))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
other_exclusion
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)

# Consent form filled: 7197 moms, 7486 babies
#  Note: Combine refused consent and did not fill consent form for the flow diagram
consent_exclude <- subset(dat_mom, consentform_adm_v2 == "No")
dat_mom <- subset(dat_mom, consentform_adm_v2 == "Yes" | is.na(consentform_adm_v2))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
n_distinct(consent_exclude$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)

# Remove miscarriages (stillbirth before 20 weeks): 7154 moms, 7441 babies
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

# Mother died giving birth: 7130 moms, 7439 babies
# Note: missing discharge status irrelevant since they are alive at follow-up
died_exclude <- subset(dat_mom, dischstat_mat == "Died")
dat_mom <- subset(dat_mom, dischstat_mat != "Died" | is.na(dischstat_mat))
dat_baby <- subset(dat_baby, studyid_adm %in% dat_mom$studyid_adm)
n_distinct(died_exclude$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)


# Discharged against medical advice: 7098 moms, 7439 babies
dama_exclude <- subset(dat_mom, dischstat_mat == "Against medical advice")
dat_mom <- subset(dat_mom, dischstat_mat != "Against medical advice" | is.na(dischstat_mat))
n_distinct(dama_exclude$studyid_adm)
n_distinct(dat_mom$studyid_adm)
nrow(dat_baby)


# Complete follow-up for mothers: N = 7029 moms, 7332 babies
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
mom_died <- subset(dat_mom, momdeath_swf == "Yes")
mom_admitted <- subset(dat_mom, momadmit_swf == "Yes")

n_distinct(mom_survived$studyid_adm)
n_distinct(mom_died$studyid_adm)
n_distinct(mom_admitted$studyid_adm)


## ~~~~~~~~~~~~~~~
## Baby exclusion  ####
## ~~~~~~~~~~~~~~~

# Only apply these criteria for the babies

# Total births: 7439 births
nrow(dat_baby)

# Remove stillbirths: 7152 babies
stillbirths_exclude <- subset(dat_baby, sb20wk_del_new == "Yes")
dat_baby <- subset(dat_baby, sb20wk_del_new == "No")
nrow(stillbirths_exclude)
nrow(dat_baby)

# Exclude if baby died at birth or during admission: 7015
died_baby_exclude <- subset(dat_baby, babedeath_dis == "Baby Died")
admitted <- subset(dat_baby, dispbb_neo %in% c("Admitted", "Discharged, but readmitted after study nurse assessment")) # To do a sub-analysis later on this cohort
dat_baby <- subset(dat_baby, babedeath_dis == "Baby Survived")
nrow(died_baby_exclude)
nrow(dat_baby)

# Not admitted, referred, etc.: 6176 babies
admitted_exclude <- table(dat_baby$dispbb_neo)
dat_baby <- subset(dat_baby, dispbb_neo == "Discharged" | is.na(dispbb_neo))
admitted_exclude
nrow(dat_baby)

# Check if still unknown reason: 6151 babies
unknown_baby_exclude <- subset(dat_baby, (is.na(dispbb_neo)))
dat_baby <- subset(dat_baby, !is.na(dispbb_neo))
nrow(unknown_baby_exclude)
nrow(dat_baby)

# Mother discharged against medical advice: 6131 babies
dama_baby_exclude <- subset(dat_baby, dischstat_mat == "Against medical advice")
dat_baby <- subset(dat_baby, dischstat_mat != "Against medical advice" | is.na(dischstat_mat))
nrow(dama_baby_exclude)
nrow(dat_baby)

# Follow-up: N = 6072
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
# DATA MANIPULATIONS        #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Only for demographics
dat_mom <- dat_mom %>% 
  mutate(
    
    # Combine HIV before and during pregnancy
    hiv_status_before_during = factor(case_when(medhx_adm___1 == "Checked" ~ "HIV Before Pregnancy",
                                                preghx_adm___11 == "Checked" ~ "HIV During Pregnancy",
                                                TRUE ~ "No HIV"),
                                      levels = c("No HIV", "HIV Before Pregnancy", "HIV During Pregnancy")),
    
    # Combine eclampsia and pre-eclampsia
    eclampsia = factor(case_when(preghx_adm___2 == "Checked" | 
                                   preghx_adm___3 == "Checked" ~ "Checked", 
                                 TRUE ~ "Unchecked"),
                       levels = c("Unchecked", "Checked")),
    
    # Reverese medhx, preghx and symp "none" so that
    #  Checked codes for any diagnosis
    medhx_adm___99_reverse = factor(ifelse(medhx_adm___99 == "Checked", "Unchecked", "Checked"), levels = c("Unchecked", "Checked")),
    preghx_adm___99_reverse = factor(ifelse(preghx_adm___99 == "Checked", "Unchecked", "Checked"), levels = c("Unchecked", "Checked")),
    symp_mat___99_reverse = factor(ifelse(preghx_adm___99 == "Checked", "Unchecked", "Checked"), levels = c("Unchecked", "Checked")),
    
    # Make gestation period NA if not known, and only keep if <37 or >=37
    gestation_period_37_new = factor(case_when(gestation_period_cat_new == "Under 37 weeks" ~ "Checked",
                                               TRUE ~ "Unchecked"),
                                     levels = c("Unchecked", "Checked")),
    
    # # Discharge against medical advice
    # dama_mat = factor(case_when(dischstat_mat == "Against medical advice" ~ "Checked",
    #                             TRUE ~ "Unchecked"),
    #                   levels = c("Unchecked", "Checked")),
    
    # Destination own home
    destination_own_home_mat = factor(case_when(destination_mat == "Own home" ~ "Checked",
                                                TRUE ~ "Unchecked"),
                                      levels = c("Unchecked", "Checked")),
    
    # Prior history of poor milk production
    bf_mat_new = factor(case_when(bf_mat %in% c("No", "Not Applicable (first baby)") ~ "No",
                                  bf_mat == "Yes" ~ "Yes",
                                  TRUE ~ NA_character_),
                        levels = c("No", "Yes")),
    
    # Temperature categories
    mean_temp_neo_new_cat = factor(case_when(mean_temp_neo_new < 36.5 ~ "Hypothermic",
                                             mean_temp_neo_new > 37.5 ~ "Fever",
                                             mean_temp_neo_new >= 36.5 & 
                                               mean_temp_neo_new <= 37.5 ~ "Normal",
                                             TRUE ~ NA_character_),
                                   levels = c("Normal", "Hypothermic", "Fever")),
    
    # Hypoxia
    hypoxia = factor(case_when(mean_spo2_neo_new < 95 ~ "Checked",
                               mean_spo2_neo_new >= 95 ~ "Unchecked"),
                     levels = c("Unchecked", "Checked"))
    
  )

# Baby data manipulations
dat_baby <- dat_baby %>% 
  mutate(    
    
    # Combine HIV before and during pregnancy
    hiv_status_before_during = factor(case_when(medhx_adm___1 == "Checked" ~ "HIV Before Pregnancy",
                                                preghx_adm___11 == "Checked" ~ "HIV During Pregnancy",
                                                TRUE ~ "No HIV"),
                                      levels = c("No HIV", "HIV Before Pregnancy", "HIV During Pregnancy")),
    
    # Combine eclampsia and pre-eclampsia
    eclampsia = factor(case_when(preghx_adm___2 == "Checked" | 
                                   preghx_adm___3 == "Checked" ~ "Checked", 
                                 TRUE ~ "Unchecked"),
                       levels = c("Unchecked", "Checked")),
    
    # Reverese medhx, preghx and symp "none" so that
    #  Checked codes for any diagnosis
    medhx_adm___99_reverse = factor(ifelse(medhx_adm___99 == "Checked", "Unchecked", "Checked"), levels = c("Unchecked", "Checked")),
    preghx_adm___99_reverse = factor(ifelse(preghx_adm___99 == "Checked", "Unchecked", "Checked"), levels = c("Unchecked", "Checked")),
    symp_mat___99_reverse = factor(ifelse(preghx_adm___99 == "Checked", "Unchecked", "Checked"), levels = c("Unchecked", "Checked")),
    
    # Make gestation period NA if not known, and only keep if <37 or >=37
    gestation_period_37_new = factor(case_when(gestation_period_cat_new == "Under 37 weeks" ~ "Checked",
                                               TRUE ~ "Unchecked"),
                                     levels = c("Unchecked", "Checked")),
    
    # # Discharge against medical advice
    # dama_mat = factor(case_when(dischstat_mat == "Against medical advice" ~ "Checked",
    #                             TRUE ~ "Unchecked"),
    #                   levels = c("Unchecked", "Checked")),
    
    # Destination own home
    destination_own_home_mat = factor(case_when(destination_mat == "Own home" ~ "Checked",
                                                TRUE ~ "Unchecked"),
                                      levels = c("Unchecked", "Checked")),
    
    # Prior history of poor milk production
    bf_mat_new = factor(case_when(bf_mat %in% c("No", "Not Applicable (first baby)") ~ "No",
                                  bf_mat == "Yes" ~ "Yes",
                                  TRUE ~ NA_character_),
                        levels = c("No", "Yes")),
    
    # Temperature categories
    mean_temp_neo_new_cat = factor(case_when(mean_temp_neo_new < 36.5 ~ "Hypothermic",
                                             mean_temp_neo_new > 37.5 ~ "Fever",
                                             mean_temp_neo_new >= 36.5 & 
                                               mean_temp_neo_new <= 37.5 ~ "Normal",
                                             TRUE ~ NA_character_),
                                   levels = c("Normal", "Hypothermic", "Fever")),
    
    # Hypoxia
    hypoxia = factor(case_when(mean_spo2_neo_new < 95 ~ "Checked",
                               mean_spo2_neo_new >= 95 ~ "Unchecked"),
                     levels = c("Unchecked", "Checked"))
  )


# Create days til event for KM curves
# Need to set to max follow-up if no event
dat_mom <- dat_mom %>% 
  mutate(momoutcome_swf_days_km = ifelse(momoutcome_swf == "No" | 
                                           momoutcome_swf_days > 42, 42, momoutcome_swf_days),
         momoutcome_swf_days_km = ifelse(momoutcome_swf_days_km < 0, NA, momoutcome_swf_days_km),
         momseek_swf_days_km = ifelse(momseek_swf == "No" | 
                                        momseek_swf_days > 42, 42, momseek_swf_days),
         momseek_swf_days_km = ifelse(momseek_swf_days_km < 0, NA, momseek_swf_days_km))
         # mominfectionrelated_days_km = ifelse(Infection.related == 0 | 
         #                                        (momadmit_swf_days > 42 & Infection.related == 1), 42,
         #                                      momadmit_swf_days))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEMOGRAPHIC VARIABLES     #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Demographic and Social    ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

demographic_and_social <- c(
  "agemomyears_calc",
  "livfather_ses",
  "schoolyrs_ses_new",
  "nutri_adm",
  "child_death_ses",
  "sesindex_sum",
  "time_tohosp_adm_new",
  "transport_adm_new",
  "delay_adm___99", # No delay
  "delay_adm___1",  # Terrain
  "delay_adm___2",  # Transport cost
  "delay_adm___3",  # General transport delay
  "delay_adm___4",  # Weather 
  "delay_adm___98"  # Other
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pregnancy and Health History  ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pregnancy_history <- c(
  "parity_adm",
  "pregnancy_loss_new",
  "medhx_adm___99_reverse",
  "preghx_adm___99_reverse",
  "csect_adm",
  "hiv_status_before_during",
  "isreferral_adm",
  "prevadm_adm",
  "numberanc_adm_new"
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Delivery Maternal         ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

delivery_mom_vars <- c(
  "delmode_del_new",
  "gestation_period_37_new",
  "meconium_del",
  "vagexam_del_new",
  "prom_del",
  "numbabe_del_new"
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Delivery Neonatal         ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

delivery_baby_vars <- c(
  "sexbb_del_new",
  "weightbb_del",
  "height_neo",
  "apgar1_del",
  "apgar5_del",
  "rescusoxy_del_new"
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Discharge Maternal        ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

discharge_mom_vars <- c(
  "adm_mat",
  "sbp_mat",
  "dbp_mat",
  "rr_mat",
  "temp_mat",
  "best_spo2_mat",
  "best_hr_mat",
  "hem_mat",
  "destination_own_home_mat",
  grep("^support_mat", colnames(dat_mom), value = TRUE),
  "bf_mat_new",
  "symp_mat___99_reverse",
  "dispbb_neo_new"
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Discharge Neonatal        ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

discharge_baby_vars <- c(
  "poop_neo",
  "pee_neo",
  "bf_neo",
  "jaundice_neo_new",
  "eyedischarge_neo",
  "mean_temp_neo_new",
  "mean_temp_neo_new_cat",
  "mean_spo2_neo_new",
  "hypoxia",
  "mean_hr_neo_new",
  "rr_neo",
  "abx_neo"
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Outcomes                  ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

outcome_vars <- c(
  # "momadmit_swf",
  # "momalive_swf",
  # "momoutcome_swf",
  # "Direct.Obstetric",
  # "Direct.Obstetric.and.Infection.related",
  "babeadmit_swf",
  "babealive_swf",
  "babeoutcome_swf"
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Unique Mom Dataset        ####
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Mom dataset currently contains repeated rows for each child they
#  they gave birth to. This is needed for modelling since 
#  baby characteristics are being used as predictors for 
#  mom's outcomes
# For demographics, we want only one row per mother
dat_mom_unique <- dat_mom %>% 
  dplyr::select(studyid_adm,
         all_of(demographic_and_social),
         all_of(pregnancy_history),
         all_of(delivery_mom_vars),
         all_of(discharge_mom_vars[discharge_mom_vars != "dispbb_neo_new"])) %>% 
  unique()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEMOGRAPHICS              #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Table summary
demographics_summary <- 
  rbind(
    # Maternal Demographics
    DemographicSummary(dat_mom_unique, 
                       Vars = demographic_and_social, 
                       SummariseMean = c("agemomyears_calc",
                                         "parity_adm"), 
                       SummariseMedian = c("sesindex_sum"),
                       SummariseYesNo = c("livfather_ses",
                                          "nutri_adm",
                                          "child_death_ses",
                                          grep("delay", demographic_and_social, value = TRUE)),
                       SummariseCat = c("schoolyrs_ses_new",
                                        "time_tohosp_adm_new",
                                        "transport_adm_new"),
                       Title = "Demographics and Social") %>% 
      add_row(.before = 20, Variable = "Delay"),
    "",
    
    # Pregnancy and Health History
    DemographicSummary(dat_mom_unique, 
                       Vars = pregnancy_history, 
                       SummariseMedian = c("parity_adm",
                                           "pregnancy_loss_new"),
                       SummariseYesNo = c("medhx_adm___99_reverse",
                                          "preghx_adm___99_reverse",
                                          "csect_adm",
                                          "isreferral_adm",
                                          "prevadm_adm"),
                       SummariseCat = c("hiv_status_before_during",
                                        "numberanc_adm_new"),
                       Title = "Pregnancy and Health"),
    "",
    
    # Delivery Maternal Variables
    DemographicSummary(dat_mom_unique, 
                       Vars = delivery_mom_vars, 
                       SummariseMean = c(""), 
                       SummariseMedian = c(""),
                       SummariseYesNo = c("delmode_del_new",
                                          "gestation_period_cat_new",
                                          "meconium_del",
                                          "prom_del"),
                       SummariseCat = c("vagexam_del_new",
                                        "numbabe_del"),
                       Title = "Delivery Maternal"),
    "",
    
    # Delivery Neonatal Variables
    DemographicSummary(dat_baby, 
                       Vars = delivery_baby_vars, 
                       SummariseMean = c(""), 
                       SummariseMedian = c("weightbb_del",
                                           "height_neo",
                                           "apgar1_del",
                                           "apgar5_del"),
                       SummariseYesNo = c("sexbb_del_new"),
                       SummariseCat = c("rescusoxy_del_new"),
                       Title = "Delivery Neonatal"),
    "",
    
    # Discharge Maternal Variables
    DemographicSummary(dat_mom_unique, 
                       Vars = discharge_mom_vars[discharge_mom_vars != "dispbb_neo_new"], 
                       SummariseMean = c("sbp_mat",
                                         "dbp_mat",
                                         "rr_mat",
                                         "temp_mat",
                                         "best_spo2_mat",
                                         "best_hr_mat",
                                         "hem_mat"), 
                       SummariseMedian = c(""),
                       SummariseYesNo = c("adm_mat",
                                          "destination_own_home_mat",
                                          grep("^support_mat", discharge_mom_vars, value = TRUE),
                                          "bf_mat",
                                          "symp_mat___99_reverse"),
                       SummariseCat = c("dispbb_neo_new"),
                       Title = "Discharge Maternal") %>% 
      add_row(.before = 12, Variable = "Support"),
    # Disposition of the baby (need to use the repeated rows)
    #  of the mother data for this
    DemographicSummary(dat_mom, 
                       Vars = "dispbb_neo_new") %>% 
      slice(-1),
    
    # Discharge neonatal vars
    DemographicSummary(dat_baby, 
                       Vars = discharge_baby_vars, 
                       SummariseMean = c("mean_temp_neo_new",
                                         "mean_spo2_neo_new",
                                         "mean_hr_neo_new",
                                         "rr_neo"), 
                       SummariseMedian = c(""),
                       SummariseYesNo = c("poop_neo",
                                          "pee_neo",
                                          "bf_neo",
                                          "jaundice_neo_new",
                                          "eyedischarge_neo",
                                          "hypoxia",
                                          "abx_neo"),
                       SummariseCat = c("mean_temp_neo_new_cat"),
                       Title = "Discharge Neonatal") %>% 
      slice(-8)
  )


demographics_summary



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE TABLE 1 EXPORT     #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

demographic_export <- cbind(
  # demographics_summary,
  mom_uni_or %>% 
    dplyr::select(OR, p.value),
  baby_uni_or %>% 
    dplyr::select(OR, p.value)
)

# Export
write.xlsx(demographics_summary, paste0("Results/Demographics (", analysis_date, ").xlsx"),
           colWidths = "auto", rowNames = FALSE)

write.xlsx(demographic_export, paste0("Results/Univariable Models (", analysis_date, ").xlsx"),
           colWidths = "auto", rowNames = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADMISSION AND DEATH       #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check babies admitted immediately after birth 
#  then died post-discharge

# N = 843 admitted # 865
nrow(admitted)

# Non-missing admission disposition: N = 690 # 701
admitted_disposition <- subset(admitted, !is.na(admitdisposition_neo))
nrow(admitted_disposition)

table(admitted_disposition$admitdisposition_neo)
table(admitted_disposition$admitdisposition_neo) / nrow(admitted_disposition) * 100

# Discharged alive: N = 608
admitted_discharged <- subset(admitted, admitdisposition_neo == "Discharged")
nrow(admitted_discharged)

# Died at 6 week follow-up
table(admitted_discharged$babedeath_swf)
summary(admitted_discharged$babedeath_swf_days)
summary(subset(admitted_discharged, babealive_swf == "No")$babedeath_swf_days)
table(admitted_discharged$babedeathplace_swf)

# Readmitted at 6 week follow-up
table(admitted_discharged$babeadmit_swf)

# Admission disposition by sepsis diagnosis
table(admitted_disposition$admitdiagnosis_neo___1)

table(admitted_disposition$admitdisposition_neo,
      admitted_disposition$admitdiagnosis_neo___1)

table(admitted_disposition$admitdisposition_neo,
      admitted_disposition$admitdiagnosis_neo___1)[,1] / table(admitted_disposition$admitdiagnosis_neo___1)[1] * 100

table(admitted_disposition$admitdisposition_neo,
      admitted_disposition$admitdiagnosis_neo___1)[,2] / table(admitted_disposition$admitdiagnosis_neo___1)[2] * 100


# 6-week follow-up by sepsis diagnosis
table(admitted_discharged$admitdiagnosis_neo___1)

table(admitted_discharged$babedeath_swf,
      admitted_discharged$admitdiagnosis_neo___1)

table(admitted_discharged$babedeath_swf,
      admitted_discharged$admitdiagnosis_neo___1)[2,] / table(admitted_discharged$admitdiagnosis_neo___1) * 100

table(admitted_discharged$babeadmit_swf,
      admitted_discharged$admitdiagnosis_neo___1)

table(admitted_discharged$babeadmit_swf,
      admitted_discharged$admitdiagnosis_neo___1)[2,] / table(admitted_discharged$admitdiagnosis_neo___1) * 100

