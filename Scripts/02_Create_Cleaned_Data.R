# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CLEAN MOM + BABY DATA   ###########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script converts the raw Mom + Baby data downloaded from REDCap
# and converts it into a cleaned data file

# Download from REDCap and select the following: 
#  Export format: R Statistical Software
#  Remove unvalidated text fields

# Save the csv file in the Data folder
# Don't technically need to save the R script again but edit the 
#  01_Redcap_Labels.r file so the data filename is correct

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD LIBRARIES    #################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Core set of libraries for data manipulation
library(tidyverse)
library(xlsx)
library(expss)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA          ################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read redcap data
# Remember to change the file path of the data in the Redcap_Labels script 
source("C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/01_Redcap_Labels.r")
redcap_date <- "2024-04-26"

# Rename the file
dat_raw <- data
rm(data)

# Dimensions (rows, columns)
dim(dat_raw)

# Brief summary of each column
glimpse(dat_raw)

# Load updated symptoms and conditions for mother
#  "Other" free text columns converted by Yash
dat_mom_admit_symptoms <- read.xlsx("C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Data/Maternal Text Outcomes V3 Edit .xlsx", sheetIndex = 1)
dat_mom_admit_cond <- read.xlsx("C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Data/Maternal Text Outcomes V3 Edit .xlsx", sheetIndex = 2)
dat_mom_seek_symptoms <- read.xlsx("C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Data/Maternal Text Outcomes V3 Edit .xlsx", sheetIndex = 3)
dat_mom_seek_cond <- read.xlsx("C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Data/Maternal Text Outcomes V3 Edit .xlsx", sheetIndex = 4)

# Fix dat_mom_seek_symptoms
dat_mom_seek_symptoms$New1[dat_mom_seek_symptoms$New1 == "New - Mastitis related compliant "] <- "New - Mastitis related complaint"
dat_mom_seek_symptoms$New1[dat_mom_seek_symptoms$New1 == "New - New - Mastitis related compliant "] <- "New - Mastitis related complaint"
dat_mom_seek_symptoms$New2[dat_mom_seek_symptoms$New2 == "New - Mastitis related compliant "] <- "New - Mastitis related complaint"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GLOBAL DATA MANIPULATIONS #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Keep raw data, create new clean data frame
dat_clean <- dat_raw

## ~~~~~~~~~~~~~~~~~~~
## Replace factors cols ####
## ~~~~~~~~~~~~~~~~~~~

# REDCap automatically creates factor versions of each variable
# Replace the original version of variables with the factor versions

# Note that some of the levels are not consistent (levels are sometimes 1,0 or 0,1)
#  This is how it's coded in REDCap though so just need to be careful...

# Identify the factor version of variables created in the REDCap R script
factor_vars <- grep("\\.factor", colnames(dat_clean), value = TRUE)
nonfactor_equivalent <- gsub("\\.factor", "", factor_vars)

# Replace the non-factor version with the factor version of variables
# Make sure to preserve the labels before overwriting
#  Note: Factor version of columns do not have labels
label_vars <- sapply(dat_clean %>% 
                       select(-all_of(factor_vars)), 
                     label)

dat_clean[, nonfactor_equivalent] <- dat_clean[, factor_vars]
dat_clean <- dat_clean %>% 
  select(-all_of(factor_vars))

# Add the labels back
label(dat_clean) <- as.list(label_vars)


## ~~~~~~~~~~~~~~~~~~~
## Remove autopsy cols ####
## ~~~~~~~~~~~~~~~~~~~

# Identify which row each form starts/ends
form_index <- data.frame(EndIndex = grep("complete", colnames(dat_clean)),
                         ColName = grep("complete", colnames(dat_clean), value = TRUE)) %>% 
  mutate(StartIndex = c(0, EndIndex[-length(EndIndex)]) + 1)

# Verbal autopsy columns not needed (word of Matt)
verbal_autopsy_cols <- form_index$StartIndex[form_index$ColName == "verbal_autopsy_mother_complete"] : 
  form_index$EndIndex[form_index$ColName == "verbal_autopsy_newborn_complete"]

dat_clean <- dat_clean %>% 
  select(-all_of(verbal_autopsy_cols))


## ~~~~~~~~~~~~~~~~~~
## Preserve labels ####
## ~~~~~~~~~~~~~~~~~~

# Subsequent data manipulations may remove labels since tidyverse
#  functions are not compatible with labels and will get overwritten
# Need to make sure to store the labels so we can re-apply them again later
label_vars <- sapply(dat_clean, label)


## ~~~~~~~~~~~~~~~~~~~
## Replace blanks ####
## ~~~~~~~~~~~~~~~~~~~

# Shouldn't be necessary since we are using factor version of columns

# Replace blank values with NA
dat_clean <- dat_clean %>% 
  # Apply function na_if across all character columns
  mutate(across(where(is.character), ~na_if(., "")))

## ~~~~~~~~~~~~~~~~~~
## NA if unknown ####
## ~~~~~~~~~~~~~~~~~~

# Replace Not sure, Unsure, don't know, doesn't know with NA

# Columns where 3 codes for NA
not_sure_3 <- c("distress_adm",
                "prom_del",
                "sbcongenital_del",
                "cord_delay_del",
                "poop_neo",
                "pee_neo")

# Columns where 5 codes for NA
not_sure_5 <- c("csect_delay_del")

# Columns where 6 codes for NA
not_sure_6 <- c("schoolyrs_ses")

# Columns where 99 codes for NA
not_sure_99 <- c("medhbp_adm",
                 "medarv_adm",
                 "nutri_adm",
                 "episiotomy_del",
                 "tear_del",
                 "induce_del",
                 "pph_del",
                 "transfx_del",
                 "obstruct_del",
                 "meconium_del",
                 "csecturgency_del",
                 "abx_del",
                 "sb_del",
                 "sbsex_del",
                 "rescus_del")

# All not sures, unknowns, or don't knows
not_sure <- c(not_sure_3,
              not_sure_5,
              not_sure_6,
              not_sure_99)

# Replace with NA
dat_clean <- dat_clean %>% 
  mutate(across(all_of(not_sure), ~recode_factor(., 
                                         "Dont know" = NA_character_,
                                         "Not sure" = NA_character_,
                                         "Not Sure" = NA_character_,
                                         "Unsure" = NA_character_)))

# # If using coded versions of variables: 
# dat_clean <- dat_clean %>% 
#   mutate_if(names(.) %in% not_sure_3, funs(na_if(., 3))) %>% 
#   mutate_if(names(.) %in% not_sure_5, funs(na_if(., 5))) %>% 
#   mutate_if(names(.) %in% not_sure_6, funs(na_if(., 6))) %>% 
#   mutate_if(names(.) %in% not_sure_99, funs(na_if(., 99)))


## ~~~~~~~~~~~~~~~~~~
## Convert to dates ####
## ~~~~~~~~~~~~~~~~~~

date_cols <- grep("date", colnames(dat_clean), value = TRUE)
date_cols

# Only duedate3_adm is actually a date
date_cols <- date_cols[!(date_cols %in% c("duedate_adm", 
                                          "duedate2_adm"))]

dat_clean <- dat_clean %>% 
  mutate(across(all_of(date_cols), ~as.Date(.)))


## ~~~~~~~~~~~~~~~~~~
## Re-apply labels ####
## ~~~~~~~~~~~~~~~~~~

label(dat_clean) <- as.list(label_vars)


## ~~~~~~~~~~~~~~~~~~
## Add REDCap repeat instrument ####
## ~~~~~~~~~~~~~~~~~~

# First row for REDCap repeat instrument is blank/NA for each patient
# This corresponds to mom data

dat_clean <- dat_clean %>% 
  mutate(redcap_repeat_instrument = sjmisc::replace_na(redcap_repeat_instrument, value = "maternal"))


## ~~~~~~~~~~~~~~~~~~~
## Reverse factor levels  ####
## ~~~~~~~~~~~~~~~~~~~

# Currently, for yes/no variables, yes is the reference group
#  We need to make it so no is the reference

# Identify yes/no columns
factor_levels <- lapply(dat_clean, levels)
yes_no_cols <- sapply(factor_levels, identical, c("Yes", "No"))
yes_no_cols <- names(yes_no_cols[yes_no_cols])

dat_clean <- dat_clean %>% 
  mutate(across(all_of(yes_no_cols), ~relevel(., ref = "No") ))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMBINE DATA FROM REPEAT INSTRUMENTS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For baby details, make delivery neonatal redcap repeat instrument
#  and discharge interview neonatal redcap repeat instrument
#  have the same data

# Identify which row each form starts/ends
form_index <- data.frame(EndIndex = grep("complete", colnames(dat_clean)),
                         ColName = grep("complete", colnames(dat_clean), value = TRUE)) %>% 
  mutate(StartIndex = c(0, EndIndex[-length(EndIndex)]) + 1)

# Delivery neonatal columns
delivery_neonatal_cols <- form_index$StartIndex[form_index$ColName == "delivery_neonatal_complete"] : 
  form_index$EndIndex[form_index$ColName == "delivery_neonatal_complete"]

delivery_neonatal_cols <- colnames(dat_clean)[delivery_neonatal_cols]

# Fill down the delivery neonatal columns
dat_clean <- dat_clean %>% 
  group_by(studyid_adm) %>% 
  fill(all_of(delivery_neonatal_cols), .direction = "down") %>% 
  ungroup()

# Set to NA if 6-week follow-up visit
dat_clean[dat_clean$redcap_repeat_instrument == "11. Six Week Follow Up Neonatal", 
          delivery_neonatal_cols] <- NA


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPLIT INTO MOM AND BABY   #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Filter out the non-pilot data
non_pilot <- dat_clean %>% 
  filter(is_pilot_adm == "No") %>% 
  pull(studyid_adm)

dat_clean <- dat_clean %>% 
  filter(studyid_adm %in% non_pilot)

# Remove autopsy rows
dat_clean <- dat_clean %>% 
  filter(redcap_repeat_instrument != "Verbal Autopsy - Newborn")

# Split into mom, baby, baby follow-up
dat_split <- dat_clean %>% 
  group_split(redcap_repeat_instrument) 
names(dat_split) <- lapply(dat_split, function(X) as.character(X$redcap_repeat_instrument[1]))

# Assign datasets and remove the variables with completely missing values
# For babies, use discharge interview row
#  We filled down the delivery neonatal row earlier 
#  so discharge interview should have all the data
dat_mom_raw <- dat_split$maternal %>%  
  purrr::discard(~sum(is.na(.x)) / length(.x) * 100 == 100)

dat_baby_raw <-  dat_split$`8. Discharge Interview Neonatal` %>% 
  purrr::discard(~sum(is.na(.x)) / length(.x) * 100 == 100)

dat_fu <- dat_split$`11. Six Week Follow Up Neonatal` %>% 
  purrr::discard(~sum(is.na(.x)) / length(.x) * 100 == 100)

# Create combined baby dataset with follow-up
dat_baby_raw <- dat_baby_raw %>%  
  full_join(dat_fu %>% 
              dplyr::select(-redcap_repeat_instrument), 
            by = c("studyid_adm" = "studyid_adm", 
                   "redcap_repeat_instance" = "redcap_repeat_instance"), 
            copy = T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MOM DATA MANIPULATIONS    #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~
## Create new columns ####
## ~~~~~~~~~~~~~~~~~~~

dat_mom_raw <- dat_mom_raw %>% 
  mutate(
    
    ### ~~~~~~~~~~~~~~~~~~~~~~
    ### Admission Subject Details ####
    ### ~~~~~~~~~~~~~~~~~~~~~~
    
    # Mum age categorical
    agemomyears_cat_new = factor(case_when(agemomyears_calc < 18 ~ "Less 18 years",
                                           agemomyears_calc >= 18 & agemomyears_calc <= 35 ~ "18 - 35 years",
                                           agemomyears_calc > 35 ~ "Over 35 years",
                                           TRUE ~ NA_character_),
                                 levels = c("Less 18 years", "18 - 35 years", "Over 35 years")),
    
    # Time between admission and delivery: delivery - admission 
    admittime_adm_new = ymd_hm(paste(admitdate_adm_v2, admittime_adm_v2)),
    deltime_del_new = ymd_hm(paste(deldate_del, deltime_del)),
    
    admdeldiff_new = admittime_adm_new %--% deltime_del_new / dhours(1),
    
    # Time between admission and discharge: discharge - admission 
    dischtime_mat_new = ymd_hm(paste(dischdate_mat, dischtime_mat)),
    admdisdiff_new =  admittime_adm_new %--% dischtime_mat_new / dhours(1),
    
    # Time between labour and admission: labour - admission
    labour_time_new = ymd_hm(paste(labordate_adm, labortime_adm)),
    admlabdiff_new = labour_time_new %--% admittime_adm_new / dhours(1),
    
    # Time between labour and discharge: discharge - labour
    labdisdiff_new = labour_time_new %--% dischtime_mat_new / dhours(1),
    
    ### ~~~~~~~~~~~~~~~
    ### Admission  ####
    ### ~~~~~~~~~~~~~~~
    
    # Combine time to hospital categories
    time_tohosp_adm_new = factor(case_when(time_tohosp_adm %in% c("more than 1hrs and up to 2hrs",
                                                                  "more than 2hrs and less than 4hrs", 
                                                                  "4 hours or more") ~ "more than 1hr",
                                           TRUE ~ as.character(time_tohosp_adm)),
                                 levels = c("less than 30 minutes",
                                            "30 minutes - 1 hr",
                                            "more than 1hr")),
    
    
    # Combine modes of transport
    transport_adm_new = factor(case_when(transport_adm %in% c("Walk", "Motorcycle", "Other") ~ "Non-motorised transport",
                                         transport_adm %in% c("Public transport (bus, taxi)", "Private transport (special hire, private vehicle)") ~ "Motorised transport",
                                         transport_adm %in% c("Ambulance") ~ "Ambulance"),
                               levels = c("Motorised transport", "Non-motorised transport", "Ambulance")),
    
    # Combined isreferral and referral source
    isreferral_adm_new = factor(case_when(isreferral_adm == "No" ~ "Not referred",
                                          isreferral_adm == "Yes" ~ as.character(referralsrc_adm),
                                          TRUE ~ NA_character_),
                                levels = c("Not referred",
                                           levels(referralsrc_adm))),
    
    # Time between labour and delivery: delivery - labour 
    labdeldiff_new = labour_time_new %--% deltime_del_new / dhours(1),
    
    # Time between delivery and discharge: discharge - delivery
    deldisdiff_new = deltime_del_new %--% dischtime_mat_new / dhours(1),
    
    ### ~~~~~~~~~~~~~~~~~~~~~~
    ### Pregnancy History ####
    ### ~~~~~~~~~~~~~~~~~~~~~~
    
    # Approximate pregnancy date and gestation period
    preg_date_new = ymd(duedate3_adm) - 280,
    gestation_period_new = as.duration(as.period(interval(end = deldate_del,
                                                          start = preg_date_new))) / ddays(1),
    gestation_period_cat_new = factor(case_when(duedate_adm == "No" ~ "Due date not known",
                                                gestation_period_new / 7 < 37 ~ "Under 37 weeks",
                                                gestation_period_new / 7 >= 37 & gestation_period_new / 7 <= 40 ~ "37 - 40 weeks",
                                                gestation_period_new / 7 > 40  ~ "Over 40 weeks"),
                                      levels = c("Due date not known",
                                                 "Under 37 weeks", 
                                                 "37 - 40 weeks", 
                                                 "Over 40 weeks")), 
    
    # Pregnancy loss: gravid - parity
    pregnancy_loss_new = gravid_adm - parity_adm,
    
    # Combine binary previous c-section with how long ago last c-section
    csect_adm_new = factor(case_when(csect_adm == "No" ~ "None",
                                     csect_adm == "Yes" ~ as.character(csect2_adm),
                                     TRUE ~ NA_character_),
                           levels = c("None", 
                                      levels(csect2_adm))),
    
    # # Combine high blood pressure medication with diagnoses
    # medhbp_adm_new = case_when(),
    
    # Combine HIV before pregnancy (medhx) and during pregnancy (preghx)
    hiv_status_new = factor(case_when(medhx_adm___1 == "Checked" | preghx_adm___11 == "Checked" ~ "Checked",
                                      TRUE ~ "Unchecked"),
                            levels = c("Unchecked", "Checked")),
    
    # Combine HIV and HIV medication
    hiv_status_medarv_new = factor(case_when(hiv_status_new == "Unchecked" ~ "No HIV",
                                             hiv_status_new == "Checked" & 
                                               medarv_adm == "Yes" ~ "HIV with ARV",
                                             hiv_status_new == "Checked" & 
                                               medarv_adm == "No" ~ "HIV without ARV",
                                             TRUE ~ NA_character_),
                                   levels = c("No HIV", "HIV with ARV", "HIV without ARV")),
    
    # Combine no previous admission and if previously admitted, how long ago
    prevadm_adm_new = factor(case_when(prevadm_adm == "No" ~ "Not admitted",
                                       TRUE ~ as.character(lngth_prevadm_adm)),
                             levels = c("Not admitted", 
                                        levels(dat_mom_raw$lngth_prevadm_adm))),
    
    # Create ANC visits categories
    numberanc_adm_new = factor(case_when(numberanc_adm %in% c("0", "1", "2", "3") ~ "<4",
                                         numberanc_adm %in% c("4", "5", "6", "7", "8") ~ "4-8",
                                         numberanc_adm %in% c(">8") ~ ">8"),
                               levels = c("<4", "4-8", ">8")),
    
    ### ~~~~~~~~~~~~~~~~~~~~~~
    ### SES and Demographics ####
    ### ~~~~~~~~~~~~~~~~~~~~~~
    
    # Combine single/widowed/divorced
    marry_ses_new = factor(case_when(marry_ses %in% c("Single", "Separated/divorced", "Widowed") ~ "Single",
                                     TRUE ~ as.character(marry_ses)),
                           levels = c("Single", "Married monogamous", "Married polygamous")),
    
    # Combine education categories
    schoolyrs_ses_new = factor(case_when(schoolyrs_ses == "No school" ~ "< = P3",
                                         TRUE ~ as.character(schoolyrs_ses)),
                               levels = c("< = P3",
                                          "P4-P7",
                                          "S1-S6",
                                          "Post secondary (including post S4 technical school)")),
    
    # Socioeconomic index; see scoring table
    # https://reproductive-health-journal.biomedcentral.com/articles/10.1186/s12978-020-01034-2/tables/4
    # Note that since we only calculate score for one country, we can use the 1-10 scale
    sesindex_sum = 
      (sesindex_flooring___3 == "Checked") +  # Finished flooring
      (sesindex_toilet___1 == "Checked") +    # Flushing toilet
      (sesindex_cooking___1 == "Checked" | 
         sesindex_cooking___2 == "Checked") + # Cooking electricity/gas
      (sesindex_safewater %in% 
         c("Piped water", "Borehole",
           "Protected spring", "Well/dam",
           "Rainwater")) +                    # Improved drinking water
      (sesindex_assets___1 == "Checked") +    # Electricity
      (sesindex_assets___2 == "Checked") +    # Television
      (sesindex_assets___4 == "Checked") +    # Refrigerator
      (sesindex_assets2___1 == "Checked") +   # Smart phone
      (sesindex_assets2___4 == "Checked") +   # Motorcycle
      (sesindex_assets2___5 == "Checked"),    # Car/truck
    # NOTE: Since most of these are checkbox fields, there won't be any NAs even if missing
    # sesindex_safewater is not a checkbox field so we can use this to check for missing data
    sesindex_sum = ifelse(is.na(sesindex_safewater), NA, sesindex_sum), 
    
    # Categorical SES
    sesindex_cat = factor(case_when(sesindex_sum <= 2 ~ "Low SES", 
                                    sesindex_sum >= 3 & sesindex_sum <= 4 ~ "Mod SES",
                                    sesindex_sum > 4 ~ "High SES",
                                    TRUE ~ NA_character_),
                          levels = c("Low SES", "Mod SES", "High SES")),
    
    ### ~~~~~~~~~~~~~~~~~~~~~~
    ### Delivery Maternal ####
    ### ~~~~~~~~~~~~~~~~~~~~~~
    
    # Singleton and multiple
    numbabe_del_new = factor(case_when(numbabe_del == 1 ~ "Single",
                                       numbabe_del > 1 ~ "Multiple",
                                       TRUE ~ NA_character_),
                             levels = c("Single", "Multiple")),
    
    # Combine prom and rom
    prom_del_new = factor(case_when(prom_del == "No" ~ "No PROM",
                                    prom_del == "Yes" ~ as.character(rom_del),
                                    TRUE ~ NA_character_),
                          levels = c("No PROM", 
                                     levels(rom_del))),
    
    # Time between delivery and discharge: discharge - delivery 
    deldisdiff_new = deltime_del_new %--% dischtime_mat_new / dhours(1),
    
    # Combine delivery modes into vaginal or caesarean
    delmode_del_new = factor(
      case_when(delmode_del %in% c("Vaginal", "Assisted vaginal (vacuum or forceps)") ~ "Vaginal",
                delmode_del %in% c("Caesarean (with labour)", "Caesarean (without labour)") ~ "Caesarean",
                TRUE ~ NA_character_),
      levels = c("Vaginal", "Caesarean")),
    
    # Combine tear_del and degreetear_del
    degreetear_del_new = factor(case_when(tear_del == "No" ~ "0",
                                          degreetear_del %in% c("2", "3", "4") ~ ">=2",
                                          TRUE ~ as.character(degreetear_del)),
                                levels = c("0", "1", ">=2")),
    
    # Combine blood transfusion with units of blood
    transfx_del_new = case_when(transfx_del == "No" ~ 0,
                                transfx_del == "Yes" ~ as.numeric(as.character(unittrans_del)),
                                TRUE ~ NA_real_),
    
    # Emergency c-section done without delay
    # https://www.pregnancybirthbaby.org.au/emergency-caesarean
    csecturgency_del_new = 
      factor(case_when(csecturgency_del %in% "Immediate threat to life of woman or fetus" & 
                         csect_delay_del %in% "No delay" ~ "Timely C-section",
                       csecturgency_del %in% "Maternal or fetal compromise which is not immediately life-threatening" & 
                         csect_delay_del %in% c("No delay", "30-60m") ~ "Timely C-section",
                       csecturgency_del %in% c("Needing early delivery but no maternal or fetal compromise",
                                               "At a time to suit the patient and maternity team") ~ "Timely C-section",
                       csecturgency_del %in% "Immediate threat to life of woman or fetus" & 
                         csect_delay_del %in% c("30-60m", "61m - 120m", ">120m") ~ "Delayed C-section",
                       csecturgency_del %in% "Maternal or fetal compromise which is not immediately life-threatening" & 
                         csect_delay_del %in% c("61m - 120m", ">120m") ~ "Delayed C-section",
                       delmode_del %in% c("Vaginal", "Assisted vaginal (vacuum or forceps)") ~ "Vaginal Birth",
                       TRUE ~ NA_character_),
             levels = c("Vaginal Birth", "Delayed C-section", "Timely C-section")),
    
    # abx_del create new category for NA when delmode is vaginal
    abx_del = factor(case_when(is.na(abx_del) & 
                                 delmode_del %in% c("Vaginal", 
                                                    "Assisted vaginal (vacuum or forceps)") ~ "Vaginal birth",
                               TRUE ~ as.character(abx_del)),
                     levels = c("No", 
                                "Vaginal birth",
                                "Yes - but after incision", 
                                "Yes - within 1 hour of incision")),
    
    # anticoag_del create new category for NA when delmode is vaginal
    anticoag_del = factor(case_when(is.na(anticoag_del) & 
                                      delmode_del %in% c("Vaginal", 
                                                         "Assisted vaginal (vacuum or forceps)") ~ "Vaginal birth",
                                    TRUE ~ as.character(abx_del)),
                          levels = c("No", 
                                     "Vaginal birth",
                                     "Yes")),
    
    # Categorise vaginal exams
    vagexam_del_new = factor(case_when(vagexam_del %in% c("0", "1", "2") ~ "0-2",
                                       vagexam_del %in% c("3", "4", "5") ~ "3-5",
                                       vagexam_del %in% c("6", "7", "8", "9", ">9") ~ ">5"),
                             levels = c("0-2", "3-5", ">5")),
    
    
    ### ~~~~~~~~~~~~~~~~~~~~~~
    ### Six Week Follow-Up ####
    ### ~~~~~~~~~~~~~~~~~~~~~~
    
    # Reverse admission factor levels
    momadmit_swf = relevel(momadmit_swf, ref = "No"),
    
    # Convert alive to death
    momdeath_swf = factor(case_when(momalive_swf == "Yes" ~ "No",
                                    momalive_swf == "No" ~ "Yes",
                                    TRUE ~ NA),
                          levels = c("No", "Yes")),
    
    # Combine deaths and re-admissions
    momoutcome_swf = factor(case_when(momadmit_swf == "Yes" | 
                                        momdeath_swf == "Yes" ~ "Yes",
                                      TRUE ~ "No"),
                            levels = c("No", "Yes")),
    
    # Time between discharge and careseeking: careseeking - discharge 
    momseek_swf_days = as.numeric(momseekdate_swf - dischdate_mat),
    
    # Time between discharge and mother's outcome
    momadmit_swf_days = as.numeric(momadmitdate_swf - dischdate_mat),
    momdeath_swf_days = as.numeric(momdeathdate_swf - dischdate_mat),
    momoutcome_swf_days = coalesce(momadmit_swf_days, momdeath_swf_days)
    
  ) %>% 
  select(-redcap_repeat_instrument)


## ~~~~~~~~~~~~~~~~~~~
## Fix time between events ####
## ~~~~~~~~~~~~~~~~~~~

# Can't have negative times and remove outliers
dat_mom_raw <- dat_mom_raw %>% 
  mutate(admdeldiff_new = ifelse(admdeldiff_new < 0 | admdeldiff_new > 1000, NA, admdeldiff_new),
         admdisdiff_new = ifelse(admdisdiff_new < 0 | admdisdiff_new > 1000, NA, admdisdiff_new),
         admlabdiff_new = ifelse(admlabdiff_new < 0 | admlabdiff_new > 1000, NA, admlabdiff_new),
         labdisdiff_new = ifelse(labdisdiff_new < 0 | labdisdiff_new > 1000, NA, labdisdiff_new),
         labdeldiff_new = ifelse(labdeldiff_new < 0 | labdeldiff_new > 1000, NA, labdeldiff_new),
         deldisdiff_new = ifelse(deldisdiff_new < 0 | deldisdiff_new > 1000, NA, deldisdiff_new))


## ~~~~~~~~~~~~~~~~~~~
## Update conditions and symptoms ####
## ~~~~~~~~~~~~~~~~~~~

# Convert free text from "Other" conditions or symptoms to new categories
#  New categories created by Yash

# Make studyid_adm labelled first 
label(dat_mom_admit_cond$studyid_adm) <- "Study ID"
label(dat_mom_admit_symptoms$studyid_adm) <- "Study ID"
label(dat_mom_seek_cond$studyid_adm) <- "Study ID"
label(dat_mom_seek_symptoms$studyid_adm) <- "Study ID"

### ~~~~~~~~~~~~~~~~~~~~~
### Admit Conditions ####
### ~~~~~~~~~~~~~~~~~~~~~

dat_mom_raw <- dat_mom_raw %>% 
  left_join(dat_mom_admit_cond %>% 
              select(-momadmitcondother_swf),
            relationship = "many-to-many") %>% 
  mutate(
    # Fistula
    momadmitcond_swf___4 = factor(case_when(New1 %in% "Existing - Fistula" |
                                              New2 %in% "Existing - Fistula" ~ "Checked",
                                            TRUE ~ as.character(momadmitcond_swf___4)),
                                  levels = c("Unchecked", "Checked")),
    
    # Retained placenta
    momadmitcond_swf___7 = factor(case_when(New1 %in% "Existing - Retained placenta" |
                                              New2 %in% "Existing - Retained placenta" ~ "Checked",
                                            TRUE ~ as.character(momadmitcond_swf___7)),
                                  levels = c("Unchecked", "Checked")),
    
    # Heavy bleeding
    momadmitcond_swf___8 = factor(case_when(New1 %in% "Existing - Heavy bleeding" |
                                              New2 %in% "Existing - Heavy bleeding" ~ "Checked",
                                            TRUE ~ as.character(momadmitcond_swf___8)),
                                  levels = c("Unchecked", "Checked")),
    
    # Other infection
    momadmitcond_swf___12 = factor(case_when(New1 %in% "Existing - Other infection" |
                                               New2 %in% "Existing - Other infection" ~ "Checked",
                                             TRUE ~ as.character(momadmitcond_swf___12)),
                                   levels = c("Unchecked", "Checked")),
    
    # PPD/Psychosis
    momadmitcond_swf___15 = factor(case_when(New1 %in% "Existing - PPD/psychosis" |
                                               New2 %in% "Existing - PPD/psychosis" ~ "Checked",
                                             TRUE ~ as.character(momadmitcond_swf___15)),
                                   levels = c("Unchecked", "Checked")),
    
    # Need another surgery
    momadmitcond_swf___16 = factor(case_when(New1 %in% "Existing - Needed another surgery" |
                                               New2 %in% "Existing - Needed another surgery" ~ "Checked",
                                             TRUE ~ as.character(momadmitcond_swf___16)),
                                   levels = c("Unchecked", "Checked")),
    
    # NEW - Cesearean wound disruption (non-infectious)
    momadmitcond_swf___19 = factor(case_when(New1 %in% "New - Cesearean wound disruption (non infectious)" |
                                               New2 %in% "New - Cesearean wound disruption (non infectious)" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # NEW - Constipation
    momadmitcond_swf___20 = factor(case_when(New1 %in% "New - Constipation" |
                                               New2 %in% "New - Constipation" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # NEW - Dehydration
    momadmitcond_swf___21 = factor(case_when(New1 %in% "New - Dehydration " |
                                               New2 %in% "New - Dehydration " ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # NEW - Mastitis and related concerns
    momadmitcond_swf___22 = factor(case_when(New1 %in% "New - Mastitis and related concerns" |
                                               New2 %in% "New - Mastitis and related concerns" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # NEW - MSK Complaint
    momadmitcond_swf___23 = factor(case_when(New1 %in% "New - MSK Complaint" |
                                               New2 %in% "New - MSK Complaint" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # NEW - Peptic ulcer disease
    momadmitcond_swf___24 = factor(case_when(New1 %in% "New - Peptic Ulcer Disease" |
                                               New2 %in% "New - Peptic Ulcer Disease" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # NEW - Perineal disruption
    momadmitcond_swf___25 = factor(case_when(New1 %in% "New - Perineal  disruption" |
                                               New2 %in% "New - Perineal  disruption" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # NEW - Pulmonary embolism
    momadmitcond_swf___26 = factor(case_when(New1 %in% "New - Pulmonary embolism" |
                                               New2 %in% "New - Pulmonary embolism" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # NEW - Retained products of conception
    momadmitcond_swf___27 = factor(case_when(New1 %in% "New - Retained products of conception" |
                                               New2 %in% "New - Retained products of conception" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # NEW - Varicose veins
    momadmitcond_swf___28 = factor(case_when(New1 %in% "New - Varicose Veins" |
                                               New2 %in% "New - Varicose Veins" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked"))
  ) %>% 
  select(-starts_with("New"))


### ~~~~~~~~~~~~~~~~~~~~~
### Admit Symptoms   ####
### ~~~~~~~~~~~~~~~~~~~~~

dat_mom_raw <- dat_mom_raw %>% 
  left_join(dat_mom_admit_symptoms %>% 
              select(-momadmitsympother_swf)) %>% 
  mutate(
    # Abnormal tiredness
    momadmitsymp_swf___4 = factor(case_when(New1 %in% "Existing - Abnormal tiredness" |
                                              New2 %in% "Existing - Abnormal tiredness" | 
                                              New3 %in% "Existing - Abnormal tiredness" ~ "Checked",
                                            TRUE ~ as.character(momadmitsymp_swf___4)),
                                  levels = c("Unchecked", "Checked")),
    # Severe headache >24 hours
    momadmitsymp_swf___9 = factor(case_when(New1 %in% "Existing - Severe headache >24hrs" |
                                              New2 %in% "Existing - Severe headache >24hrs" | 
                                              New3 %in% "Existing - Severe headache >24hrs" ~ "Checked",
                                            TRUE ~ as.character(momadmitsymp_swf___9)),
                                  levels = c("Unchecked", "Checked")),
    
    # New - Anorexia
    momadmitsymp_swf___22 = factor(case_when(New1 %in% "New - Anorexia" |
                                               New2 %in% "New - Anorexia" | 
                                               New3 %in% "New - Anorexia" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # New - Cesarean wound disruption
    momadmitsymp_swf___23 = factor(case_when(New1 %in% "New - Cesarean wound disruption" |
                                               New2 %in% "New - Cesarean wound disruption" | 
                                               New3 %in% "New - Cesarean wound disruption" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # New - Dizziness
    momadmitsymp_swf___24 = factor(case_when(New1 %in% "New - Dizziness" |
                                               New2 %in% "New - Dizziness" | 
                                               New3 %in% "New - Dizziness" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # New - Dysuria
    momadmitsymp_swf___25 = factor(case_when(New1 %in% "New - Dysuria " |
                                               New2 %in% "New - Dysuria " | 
                                               New3 %in% "New - Dysuria " ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # New - Epigastric pain
    momadmitsymp_swf___26 = factor(case_when(New1 %in% "New - Epigastric pain " |
                                               New2 %in% "New - Epigastric pain " | 
                                               New3 %in% "New - Epigastric pain " ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # New - Heartburn
    momadmitsymp_swf___27 = factor(case_when(New1 %in% "New - Heartburn" |
                                               New2 %in% "New - Heartburn" | 
                                               New3 %in% "New - Heartburn" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    # New - MSK complaint
    momadmitsymp_swf___28 = factor(case_when(New1 %in% "New - MSK Complaint" |
                                               New2 %in% "New - MSK Complaint" | 
                                               New3 %in% "New - MSK Complaint" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    
    # New - Painful of swollen breasts
    momadmitsymp_swf___29 = factor(case_when(New1 %in% "New - Painful or swollen breasts " |
                                               New2 %in% "New - Painful or swollen breasts " | 
                                               New3 %in% "New - Painful or swollen breasts " ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
    # New - Palmar pallor
    momadmitsymp_swf___30 = factor(case_when(New1 %in% "New - Palmar Pallor" |
                                               New2 %in% "New - Palmar Pallor" | 
                                               New3 %in% "New - Palmar Pallor" ~ "Checked",
                                             TRUE ~ "Unchecked"),
                                   levels = c("Unchecked", "Checked")),
  ) %>% 
  select(-starts_with("New"))


### ~~~~~~~~~~~~~~~~~~~~~
### Seek Conditions ####
### ~~~~~~~~~~~~~~~~~~~~~

dat_mom_raw <- dat_mom_raw %>% 
  left_join(dat_mom_seek_cond %>% 
              select(-momseekcondother_swf)) %>% 
  mutate(
    # Heavy bleeding
    momseekcond_swf___8 = factor(case_when(New1 %in% "Existing - Heavy bleeding" |
                                             New2 %in% "Existing - Heavy bleeding" ~ "Checked",
                                           TRUE ~ as.character(momseekcond_swf___8)),
                                 levels = c("Unchecked", "Checked")),
    
    # Surgical site infection
    momseekcond_swf___10 = factor(case_when(New1 %in% "Existing - Surgical site infection" |
                                              New2 %in% "Existing - Surgical site infection" ~ "Checked",
                                            TRUE ~ as.character(momseekcond_swf___10)), 
                                  levels = c("Unchecked", "Checked")),
    
    # Other infection
    momseekcond_swf___12 = factor(case_when(New1 %in% "Existing - Other infection" |
                                              New2 %in% "Existing - Other infection" ~ "Checked",
                                            TRUE ~ as.character(momseekcond_swf___12)), 
                                  levels = c("Unchecked", "Checked")),
    
    # PPD/psychosis
    momseekcond_swf___15 = factor(case_when(New1 %in% "Existing - PPD/psychosis" |
                                              New2 %in% "Existing - PPD/psychosis" ~ "Checked",
                                            TRUE ~ as.character(momseekcond_swf___15)), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Abnormal tiredness
    momseekcond_swf___19 = factor(case_when(New1 %in% "New - Abnormal tiredness" |
                                              New2 %in% "New - Abnormal tiredness" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Abnormal vaginal discharge syndrome
    momseekcond_swf___20 = factor(case_when(New1 %in% "New - Abnormal vaginal discharge syndrome" |
                                              New2 %in% "New - Abnormal vaginal discharge syndrome" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Allergy
    momseekcond_swf___21 = factor(case_when(New1 %in% "New - Allergy" |
                                              New2 %in% "New - Allergy" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Cesearean wound disruption (non infectious)
    momseekcond_swf___22 = factor(case_when(New1 %in% "New - Cesearean wound disruption (non infectious)" |
                                              New2 %in% "New - Cesearean wound disruption (non infectious)" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Constipation
    momseekcond_swf___23 = factor(case_when(New1 %in% "New - Constipation" |
                                              New2 %in% "New - Constipation" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Failure to pass urine
    momseekcond_swf___24 = factor(case_when(New1 %in% "New - Failure to pass urine" |
                                              New2 %in% "New - Failure to pass urine" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Family planning
    momseekcond_swf___25 = factor(case_when(New1 %in% "New - Family Planning" |
                                              New2 %in% "New - Family Planning" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Gastroesophageal reflux disease
    momseekcond_swf___26 = factor(case_when(New1 %in% "New - Gastroesophageal reflux disease" |
                                              New2 %in% "New - Gastroesophageal reflux disease" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Heart disease
    momseekcond_swf___27 = factor(case_when(New1 %in% "New - Heart Disease" |
                                              New2 %in% "New - Heart Disease" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Hematoma
    momseekcond_swf___28 = factor(case_when(New1 %in% "New - Hematoma" |
                                              New2 %in% "New - Hematoma" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Hypertension
    momseekcond_swf___29 = factor(case_when(New1 %in% "New - Hypertension" |
                                              New2 %in% "New - Hypertension" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Mastitis and related concerns
    momseekcond_swf___30 = factor(case_when(New1 %in% "New - Mastitis and related concerns" |
                                              New2 %in% "New - Mastitis and related concerns" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - MSK complaint
    momseekcond_swf___31 = factor(case_when(New1 %in% "New - MSK Complaint" |
                                              New2 %in% "New - MSK Complaint" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Oral complaint
    momseekcond_swf___32 = factor(case_when(New1 %in% "New - Oral complaint" |
                                              New2 %in% "New - Oral complaint" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Peptic ulcer disease
    momseekcond_swf___33 = factor(case_when(New1 %in% "New - Peptic Ulcer Disease" |
                                              New2 %in% "New - Peptic Ulcer Disease" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Perineal disruption
    momseekcond_swf___34 = factor(case_when(New1 %in% "New - Perineal  disruption" |
                                              New2 %in% "New - Perineal  disruption" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Perineal repair
    momseekcond_swf___35 = factor(case_when(New1 %in% "New - Perineal repair" |
                                              New2 %in% "New - Perineal repair" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Postpartum paralysis 
    momseekcond_swf___36 = factor(case_when(New1 %in% "New - Postpartum paralysis " |
                                              New2 %in% "New - Postpartum paralysis " ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Prolapsed rectum
    momseekcond_swf___37 = factor(case_when(New1 %in% "New - Prolapsed rectum" |
                                              New2 %in% "New - Prolapsed rectum" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Prolapsed uterus
    momseekcond_swf___38 = factor(case_when(New1 %in% "New - Prolapsed uterus" |
                                              New2 %in% "New - Prolapsed uterus" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Suture removal
    momseekcond_swf___39 = factor(case_when(New1 %in% "New - Suture removal" |
                                              New2 %in% "New - Suture removal" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
  ) %>% 
  select(-starts_with("New"))


### ~~~~~~~~~~~~~~~~~~~~~
### Seek Symptoms    ####
### ~~~~~~~~~~~~~~~~~~~~~

dat_mom_raw <- dat_mom_raw %>% 
  left_join(dat_mom_seek_symptoms %>% 
              select(-momseeksympother_swf)) %>% 
  mutate(
    # Heavy vaginal bleeding
    momseeksymp_swf___1 = factor(case_when(New1 %in% "Existing - Heavy vaginal bleeding" |
                                             New2 %in% "Existing - Heavy vaginal bleeding" | 
                                             New3 %in% "Existing - Heavy vaginal bleeding" ~ "Checked",
                                           TRUE ~ as.character(momseeksymp_swf___1)),
                                 levels = c("Unchecked", "Checked")),
    
    # Abnormal tiredness
    momseeksymp_swf___4 = factor(case_when(New1 %in% "Existing - Abnormal tiredness" |
                                             New2 %in% "Existing - Abnormal tiredness" | 
                                             New3 %in% "Existing - Abnormal tiredness" ~ "Checked",
                                           TRUE ~ as.character(momseeksymp_swf___4)),
                                 levels = c("Unchecked", "Checked")),
    
    # Shortness of breath
    momseeksymp_swf___6 = factor(case_when(New1 %in% "Existing - Shortness of breath" |
                                             New2 %in% "Existing - Shortness of breath" | 
                                             New3 %in% "Existing - Shortness of breath" ~ "Checked",
                                           TRUE ~ as.character(momseeksymp_swf___6)),
                                 levels = c("Unchecked", "Checked")),
    
    # Severe headache <24 hours
    momseeksymp_swf___8 = factor(case_when(New1 %in% "Existing - Severe headache <24 hrs" |
                                             New2 %in% "Existing - Severe headache <24 hrs" | 
                                             New3 %in% "Existing - Severe headache <24 hrs" ~ "Checked",
                                           TRUE ~ as.character(momseeksymp_swf___8)),
                                 levels = c("Unchecked", "Checked")),
    
    # Abdominal pain
    momseeksymp_swf___10 = factor(case_when(New1 %in% "Existing - Abdominal pain" |
                                              New2 %in% "Existing - Abdominal pain" | 
                                              New3 %in% "Existing - Abdominal pain" ~ "Checked",
                                            TRUE ~ as.character(momseeksymp_swf___10)),
                                  levels = c("Unchecked", "Checked")),
    
    # Abdominal tenderness
    momseeksymp_swf___11 = factor(case_when(New1 %in% "Existing - Abdominal tenderness\nwhen touched" |
                                              New2 %in% "Existing - Abdominal tenderness\nwhen touched" | 
                                              New3 %in% "Existing - Abdominal tenderness\nwhen touched" ~ "Checked",
                                            TRUE ~ as.character(momseeksymp_swf___11)),
                                  levels = c("Unchecked", "Checked")),
    
    # Foul smelling vaginal discharge
    momseeksymp_swf___12 = factor(case_when(New1 %in% "Existing - Foul smelling vaginal discharge" |
                                              New2 %in% "Existing - Foul smelling vaginal discharge" | 
                                              New3 %in% "Existing - Foul smelling vaginal discharge" ~ "Checked",
                                            TRUE ~ as.character(momseeksymp_swf___12)),
                                  levels = c("Unchecked", "Checked")),
    
    # Fever <7 days
    momseeksymp_swf___13 = factor(case_when(New1 %in% "Existing - Fever/ body hotness < 7days" |
                                              New2 %in% "Existing - Fever/ body hotness < 7days" | 
                                              New3 %in% "Existing - Fever/ body hotness < 7days" ~ "Checked",
                                            TRUE ~ as.character(momseeksymp_swf___13)),
                                  levels = c("Unchecked", "Checked")),
    
    # Diarrhea <14 days
    momseeksymp_swf___15 = factor(case_when(New1 %in% "Existing - Diarrhea < 14 days" |
                                              New2 %in% "Existing - Diarrhea < 14 days" | 
                                              New3 %in% "Existing - Diarrhea < 14 days" ~ "Checked",
                                            TRUE ~ as.character(momseeksymp_swf___15)),
                                  levels = c("Unchecked", "Checked")),
    
    # Vomiting everything
    momseeksymp_swf___17 = factor(case_when(New1 %in% "Existing - Vomiting everything" |
                                              New2 %in% "Existing - Vomiting everything" | 
                                              New3 %in% "Existing - Vomiting everything" ~ "Checked",
                                            TRUE ~ as.character(momseeksymp_swf___17)),
                                  levels = c("Unchecked", "Checked")),
    
    # New - Abdominal swelling
    momseeksymp_swf___22 = factor(case_when(New1 %in% "New - Abdominal swelling" |
                                              New2 %in% "New - Abdominal swelling" | 
                                              New3 %in% "New - Abdominal swelling" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Anorexia
    momseeksymp_swf___23 = factor(case_when(New1 %in% "New - Anorexia" |
                                              New2 %in% "New - Anorexia" | 
                                              New3 %in% "New - Anorexia" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Boil on the thigh
    momseeksymp_swf___24 = factor(case_when(New1 %in% "New - Boil on the thigh" |
                                              New2 %in% "New - Boil on the thigh" | 
                                              New3 %in% " - Boil on the thigh" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Cesearean wound disruption
    momseeksymp_swf___25 = factor(case_when(New1 %in% "New - Cesarean wound disruption" |
                                              New2 %in% "New - Cesarean wound disruption" | 
                                              New3 %in% "New - Cesarean wound disruption" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Chest Pain
    momseeksymp_swf___26 = factor(case_when(New1 %in% "New - Chest Pain" |
                                              New2 %in% "New - Chest Pain" | 
                                              New3 %in% "New - Chest Pain" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Cold and flu symptoms
    momseeksymp_swf___27 = factor(case_when(New1 %in% "New - Cold and flu symptoms" |
                                              New2 %in% "New - Cold and flu symptoms" | 
                                              New3 %in% "New - Cold and flu symptoms" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Constipation
    momseeksymp_swf___28 = factor(case_when(New1 %in% "New - Constipation" |
                                              New2 %in% "New - Constipation" | 
                                              New3 %in% "New - Constipation" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Dizziness
    momseeksymp_swf___29 = factor(case_when(New1 %in% "New - Dizziness" |
                                              New2 %in% "New - Dizziness" | 
                                              New3 %in% "New - Dizziness" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Dysuria 
    momseeksymp_swf___30 = factor(case_when(New1 %in% "New - Dysuria " |
                                              New2 %in% "New - Dysuria " | 
                                              New3 %in% "New - Dysuria " ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Epigastric pain 
    momseeksymp_swf___31 = factor(case_when(New1 %in% "New - Epigastric pain " |
                                              New2 %in% "New - Epigastric pain " | 
                                              New3 %in% "New - Epigastric pain " ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - failure to pass urine
    momseeksymp_swf___32 = factor(case_when(New1 %in% "New - failure to pass urine" |
                                              New2 %in% "New - failure to pass urine" | 
                                              New3 %in% "New - failure to pass urine" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Flatulence
    momseeksymp_swf___33 = factor(case_when(New1 %in% "New - Flatulence" |
                                              New2 %in% "New - Flatulence" | 
                                              New3 %in% "New - Flatulence" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Generalized edema 
    momseeksymp_swf___34 = factor(case_when(New1 %in% "New - Generalized edema " |
                                              New2 %in% "New - Generalized edema " | 
                                              New3 %in% "New - Generalized edema " ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Heart Palpitations 
    momseeksymp_swf___35 = factor(case_when(New1 %in% "New - Heart Palpitations " |
                                              New2 %in% "New - Heart Palpitations " | 
                                              New3 %in% "New - Heart Palpitations " ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Heartburn
    momseeksymp_swf___36 = factor(case_when(New1 %in% "New - Heartburn" |
                                              New2 %in% "New - Heartburn" | 
                                              New3 %in% "New - Heartburn" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Mastitis related complaint
    momseeksymp_swf___37 = factor(case_when(New1 %in% "New - Mastitis related complaint" |
                                              New2 %in% "New - Mastitis related complaint" | 
                                              New3 %in% "New - Mastitis related complaint" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Mood changes 
    momseeksymp_swf___38 = factor(case_when(New1 %in% "New - Mood changes " |
                                              New2 %in% "New - Mood changes " | 
                                              New3 %in% "New - Mood changes " ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - MSK Complaint
    momseeksymp_swf___39 = factor(case_when(New1 %in% "New - MSK Complaint" |
                                              New2 %in% "New - MSK Complaint" | 
                                              New3 %in% "New - MSK Complaint" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Nausea
    momseeksymp_swf___40 = factor(case_when(New1 %in% "New - Nausea" |
                                              New2 %in% "New - Nausea" | 
                                              New3 %in% "New - Nausea" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Oral complaint
    momseeksymp_swf___41 = factor(case_when(New1 %in% "New - Oral complaint" |
                                              New2 %in% "New - Oral complaint" | 
                                              New3 %in% "New - Oral complaint" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Pain in the left eye
    momseeksymp_swf___42 = factor(case_when(New1 %in% "New - Pain in the left eye" |
                                              New2 %in% "New - Pain in the left eye" | 
                                              New3 %in% "New - Pain in the left eye" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Pain or swelling in breasts 
    momseeksymp_swf___43 = factor(case_when(New1 %in% "New - Pain or swelling in breasts " |
                                              New2 %in% "New - Pain or swelling in breasts " | 
                                              New3 %in% "New - Pain or swelling in breasts " ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Palmar Pallor
    momseeksymp_swf___44 = factor(case_when(New1 %in% "New - Palmar Pallor" |
                                              New2 %in% "New - Palmar Pallor" | 
                                              New3 %in% "New - Palmar Pallor" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Perineal disruption
    momseeksymp_swf___45 = factor(case_when(New1 %in% "New - Perineal disruption" |
                                              New2 %in% "New - Perineal disruption" | 
                                              New3 %in% "New - Perineal disruption" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Perineal irritation
    momseeksymp_swf___46 = factor(case_when(New1 %in% "New - Perineal irritation" |
                                              New2 %in% "New - Perineal irritation" | 
                                              New3 %in% "New - Perineal irritation" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Peripheral edema
    momseeksymp_swf___47 = factor(case_when(New1 %in% "New - Peripheral edema" |
                                              New2 %in% "New - Peripheral edema" | 
                                              New3 %in% "New - Peripheral edema" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Postpartum paralysis 
    momseeksymp_swf___48 = factor(case_when(New1 %in% "New - Postpartum paralysis " |
                                              New2 %in% "New - Postpartum paralysis " | 
                                              New3 %in% "New - Postpartum paralysis " ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Prolapsed rectum
    momseeksymp_swf___49 = factor(case_when(New1 %in% "New - Prolapsed rectum" |
                                              New2 %in% "New - Prolapsed rectum" | 
                                              New3 %in% "New - Prolapsed rectum" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Prolapsed uterus
    momseeksymp_swf___50 = factor(case_when(New1 %in% "New - Prolapsed uterus" |
                                              New2 %in% "New - Prolapsed uterus" | 
                                              New3 %in% "New - Prolapsed uterus" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Sour Taste
    momseeksymp_swf___51 = factor(case_when(New1 %in% "New - Sour Taste" |
                                              New2 %in% "New - Sour Taste" | 
                                              New3 %in% "New - Sour Taste" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Suture removal
    momseeksymp_swf___52 = factor(case_when(New1 %in% "New - Suture removal" |
                                              New2 %in% "New - Suture removal" | 
                                              New3 %in% "New - Suture removal" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Syphilitic nails
    momseeksymp_swf___53 = factor(case_when(New1 %in% "New - Syphilitic nails" |
                                              New2 %in% "New - Syphilitic nails" | 
                                              New3 %in% "New - Syphilitic nails" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked")),
    
    # New - Yellow skin
    momseeksymp_swf___54 = factor(case_when(New1 %in% "New - Yellow skin" |
                                              New2 %in% "New - Yellow skin" | 
                                              New3 %in% "New - Yellow skin" ~ "Checked",
                                            TRUE ~ "Unchecked"), 
                                  levels = c("Unchecked", "Checked"))
    
  ) %>% 
  select(-starts_with("New"))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BABY DATA MANIPULATIONS    ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~
## Create new columns ####
## ~~~~~~~~~~~~~~~~~~~

dat_baby_raw <- dat_baby_raw %>%
  
  mutate(
    
    ### ~~~~~~~~~~~~~~~~~~~~~~
    ### Delivery Neonatal ####
    ### ~~~~~~~~~~~~~~~~~~~~~~
    
    # Stillbirth binary column (currently only conditionally entered)
    sb20wk_del_new = factor(case_when(sb20wk_del == "Yes" ~ "Yes",
                                      TRUE ~ "No"),
                            levels = c("No", "Yes")),
    
    # Congenital stillbirth binary column (currently only conditionally entered)
    sbcongenital_del_new = factor(case_when(sbcongenital_del == "Yes" ~ "Yes",
                                            TRUE ~ "No"),
                                  levels = c("No", "Yes")),
    
    # Combine sex of stillborn and sex of baby
    sexbb_del_new = factor(case_when(infantstatus_del == "No" ~ as.character(sbsex_del),
                                     infantstatus_del == "Yes" ~ as.character(sexbb_del),
                                     TRUE ~ NA_character_),
                           levels = c("Male", "Female")),
    
    # Group resuscitation yes/no with resuscitation type oxygen
    rescusoxy_del_new = factor(case_when(rescus_del == "No" ~ "Not resuscitated",
                                         rescus_del == "Yes" & resustype_del___2 == "Checked" ~ "Resuscitation with oxygen",
                                         rescus_del == "Yes" & resustype_del___2 == "Unchecked" ~ "Resuscitation without oxygen",
                                         TRUE ~ NA_character_),
                               levels = c("Not resuscitated", "Resuscitation with oxygen", "Resuscitation without oxygen")),
    
    # Categorical weight
    weightbb_del_cat_new = factor(case_when(weightbb_del <= 2.5 ~ "<=2.5kg",
                                            weightbb_del > 2.5 ~ ">2.5kg",
                                            TRUE ~ NA_character_),
                                  levels = c("<=2.5kg", ">2.5kg")),
    
    ### ~~~~~~~~~~~~~~~~~~~~~~
    ### Discharge Interview Neonatal ####
    ### ~~~~~~~~~~~~~~~~~~~~~~
    
    # Combined disposition referred with other 
    dispbb_neo_new = factor(case_when(dispbb_neo == "Referred" ~ "Other",
                                      TRUE ~ as.character(dispbb_neo)),
                            levels = c("Discharged", "Admitted",
                                       "Died", "Discharged, but readmitted after study nurse assessment",
                                       "Other")),
    
    # Single variable describing whether child died at any point before discharge
    #  check discharge and admitted
    babedeath_dis = factor(case_when(dispbb_neo == "Died" | 
                                       admitdisposition_neo  == "Died" ~ "Baby Died",
                                     TRUE ~ "Baby Survived"),
                           levels = c("Baby Died", "Baby Survived")),
    
    # Jaundice combine categories
    jaundice_neo_new = factor(case_when(jaundice_neo %in% c("Yes", "Yes (a little)") ~ "Yes",
                                        TRUE ~ as.character(jaundice_neo)),
                              levels = c("No", "Yes")),
    
    # Mean temperature
    mean_temp_neo_new = (temp1_neo + temp2_neo) / 2,
    
    # Mean best foot/rhand spo2
    mean_spo2_neo_new = (foot_best_spo2_neo + rhand_best_spo2_neo) / 2,
    
    # Mean foot/rhand hr
    mean_hr_neo_new = (foot_best_hr_neo + rhand_best_hr_neo) / 2,
    
    
    
    ### ~~~~~~~~~~~~~~~~~~~~~~
    ### Six Week Follow-Up ####
    ### ~~~~~~~~~~~~~~~~~~~~~~
    
    # Time between delivery and death
    deldate_del = dat_mom_raw$deldate_del[match(studyid_adm, dat_mom_raw$studyid_adm)],
    deldeathdiff_new = as.period(interval(end = ymd(babedeathdate_swf),
                                          start = ymd(deldate_del)))$day,
    
    # Time between discharge and death
    dischdate_mat = dat_mom_raw$dischdate_mat[match(studyid_adm, dat_mom_raw$studyid_adm)],
    disdeathdiff_new = as.period(interval(end = ymd(babedeathdate_swf),
                                          start = ymd(dischdate_mat)))$day,
    
    # Reverse admission factor levels 
    babeadmit_swf = relevel(babeadmit_swf, ref = "No"),
    
    # Convert alive to death
    babedeath_swf = factor(case_when(babealive_swf == "Yes" ~ "No",
                                     babealive_swf == "No" ~ "Yes",
                                     TRUE ~ NA),
                           levels = c("No", "Yes")),
    
    # Combine deaths and re-admission
    babeoutcome_swf = factor(case_when(babeadmit_swf == "Yes" | 
                                         babedeath_swf == "Yes" ~ "Yes",
                                       TRUE ~ "No"),
                             levels = c("No", "Yes")),
    
    # Time between discharge and baby's outcome
    babeadmit_swf_days = unlab(as.numeric(babeadmitage_swf)),
    babedeath_swf_days = as.numeric(babedeathdate_swf - dispdate_neo),
    babeoutcome_swf_days = coalesce(babeadmit_swf_days, babedeath_swf_days)
    
  ) %>% 
  select(-redcap_repeat_instrument,
         -deldate_del,
         -dischdate_mat)

## ~~~~~~~~~~~~~~~~~~~
## Fix time between events ####
## ~~~~~~~~~~~~~~~~~~~

# Can't have negative times and remove outliers
dat_baby_raw <- dat_baby_raw %>% 
  mutate(deldeathdiff_new = ifelse(deldeathdiff_new < 0 | deldeathdiff_new > 1000, NA, deldeathdiff_new),
         disdeathdiff_new = ifelse(disdeathdiff_new < 0 | disdeathdiff_new > 1000, NA, disdeathdiff_new))
         


## ~~~~~~~~~~~~~~~~~~~
## Set neonatal predictors to 0 ####
## ~~~~~~~~~~~~~~~~~~~

# For mother's prediction models, need to set neonatal
#   predictors (e.g., temp) to 0 if the baby was not alive
#   These are conditionally missing data

# Note that there may be some genuine missing data - we do not 
#  want to set these to 0

### ~~~~~~~~~~~~~~~~~~~~~~
### Delivery Neonatal ####
### ~~~~~~~~~~~~~~~~~~~~~~

# Delivery neonatal variables only entered if baby born alive
#  i.e., infantstatus_del = Yes
delivery_neonatal_cont_vars <- c(
  "apgar1_del",
  "apgar5_del",
  "weightbb_del",
  "height_neo"
)

delivery_neonatal_cat_vars <- c(
  "cord_delay_del",
  "weightbb_del_cat_new",
  "rescus_del"
)

# Store labels
delivery_neonatal_labels <- sapply(dat_baby_raw[, c(delivery_neonatal_cont_vars,
                                                    delivery_neonatal_cat_vars)], 
                                   label)

# Set values to 0
dat_baby_raw <- dat_baby_raw %>% 
  # Again, need to remove the useless labels because they clash with tidyverse
  mutate(across(all_of(c(delivery_neonatal_cont_vars, 
                         delivery_neonatal_cat_vars)), ~ unclass(.)),
         across(all_of(delivery_neonatal_cont_vars), 
                ~ case_when(infantstatus_del == "No" ~ 0,
                            TRUE ~ .)),
         cord_delay_del = factor(case_when(infantstatus_del == "No" ~ 1,
                                           TRUE ~ cord_delay_del),
                                 labels = c("No", "Yes")),
         weightbb_del_cat_new = factor(case_when(infantstatus_del == "No" ~ 1, 
                                                 TRUE ~ weightbb_del_cat_new),
                                       labels = c("<=2.5kg", ">2.5kg")),
         rescus_del = factor(case_when(infantstatus_del == "No" ~ 1,
                                       TRUE ~ rescus_del),
                             labels = c("No", "Yes")))

# Re-apply labels
label(dat_baby_raw[, c(delivery_neonatal_cont_vars, 
                       delivery_neonatal_cat_vars)]) <- 
  as.list(delivery_neonatal_labels)


### ~~~~~~~~~~~~~~~~~~~~~~
### Discharge Neonatal ####
### ~~~~~~~~~~~~~~~~~~~~~~

# Discharge neonatal variables only entered if baby discharged alive
#  i.e, dispbb_neo = Discharged AND babedisch_neo = Yes
discharge_neonatal_cont_vars <- c(
  "mean_temp_neo_new",
  "rr_neo",
  "mean_spo2_neo_new",
  "mean_hr_neo_new"
)

discharge_neonatal_cat_vars <- c(
  "poop_neo",
  "pee_neo",
  "bf_neo",
  "jaundice_neo_new",
  "eyedischarge_neo",
  "abx_neo"
)

# Store labels
discharge_neonatal_labels <- sapply(dat_baby_raw[, c(discharge_neonatal_cont_vars,
                                                     discharge_neonatal_cat_vars,
                                                     "babedisch_neo")], 
                                   label)

# Set values to 0
dat_baby_raw <- dat_baby_raw %>% 
  # Again, need to remove the useless labels because they clash with tidyverse
  mutate(across(all_of(c(discharge_neonatal_cont_vars, 
                         discharge_neonatal_cat_vars,
                         "babedisch_neo")), ~ unclass(.)),
         
         # Set babedisch_neo to No (currently missing if dispbb_neo != discharged)
         babedisch_neo = factor(case_when(dispbb_neo != "Discharged" ~ 1,
                                          TRUE ~ babedisch_neo),
                                labels = c("No", "Yes")),
         
         # Set values to 0 based on dispbb_neo and babedisch_neo
         across(all_of(discharge_neonatal_cont_vars), 
                ~ case_when( !(dispbb_neo == "Discharged" & 
                                 babedisch_neo == "Yes") ~ 0,
                            TRUE ~ .)),

         # All categorical variables here are yes/no
         # Set them to No if baby not discharged
         across(all_of(discharge_neonatal_cat_vars), 
                ~ factor(case_when( !(dispbb_neo == "Discharged" & 
                                 babedisch_neo == "Yes") ~ 1,
                             TRUE ~ .),
                         labels = c("No", "Yes"))))

# Re-apply labels
label(dat_baby_raw[, c(discharge_neonatal_cont_vars,
                       discharge_neonatal_cat_vars,
                       "babedisch_neo")]) <- 
  as.list(discharge_neonatal_labels)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE NEW LABELS          ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Mother's data
label(dat_mom_raw$admdeldiff_new) <- "Time between admission and delivery"
label(dat_mom_raw$admdisdiff_new) <- "Time between admission and discharge"
label(dat_mom_raw$admlabdiff_new) <- "Time between labour and admission"
label(dat_mom_raw$labdisdiff_new) <- "Time between labour and discharge"
label(dat_mom_raw$agemomyears_cat_new) <- "Age of mother, categorical"
label(dat_mom_raw$isreferral_adm_new) <- "Referral source"
label(dat_mom_raw$labdeldiff_new) <- "Time between labour and delivery"
label(dat_mom_raw$deldisdiff_new) <- "Time between delivery and discharge"
label(dat_mom_raw$delmode_del_new) <- "What was the mode of delivery?"
label(dat_mom_raw$pregnancy_loss_new) <- "Pregnancy loss"
label(dat_mom_raw$gestation_period_cat_new) <- "Gestation period"
label(dat_mom_raw$csect_adm_new) <- "How long ago was previous c-section"
label(dat_mom_raw$hiv_status_new) <- "HIV status"
label(dat_mom_raw$hiv_status_medarv_new) <- "HIV status"
label(dat_mom_raw$prevadm_adm_new) <- "How long ago was previous admission"
label(dat_mom_raw$numberanc_adm_new) <- "How many ANC visits did you attend (best estimate if unsure)? "
label(dat_mom_raw$schoolyrs_ses_new) <- "What is the highest level of education you (mother) have completed?"
label(dat_mom_raw$sesindex_sum) <- "SES index score"
label(dat_mom_raw$sesindex_cat) <- "SES index category"
label(dat_mom_raw$numbabe_del_new) <- "How many babies were delivered?"
label(dat_mom_raw$prom_del_new) <- "How many hours were membranes ruptured?"
label(dat_mom_raw$degreetear_del_new) <- "Degree of tearing"
label(dat_mom_raw$transfx_del_new) <- "Units of blood transfused"
label(dat_mom_raw$csecturgency_del_new) <- "Was c-section performed in timely manner?"
label(dat_mom_raw$vagexam_del_new) <- "Number of vaginal exams (self reported best estimate)"
label(dat_mom_raw$time_tohosp_adm_new) <- "How long did it take you to travel to this hospital?"
label(dat_mom_raw$momdeath_swf) <- "Did the mother die at follow-up?"
label(dat_mom_raw$momoutcome_swf) <- "Did the mother have an outcome (admission or death) at follow-up?"
label(dat_mom_raw$momseek_swf_days) <- "Days until care-seeking"
label(dat_mom_raw$momadmit_swf_days) <- "Days until re-admission"
label(dat_mom_raw$momdeath_swf_days) <- "Days until mother's death"
label(dat_mom_raw$momoutcome_swf_days) <- "Days until mother's death or re-admission"

label(dat_mom_raw$momadmitcond_swf___19) <- "Cesearean wound disruption (non-infectious)"
label(dat_mom_raw$momadmitcond_swf___20) <- "Constipation"
label(dat_mom_raw$momadmitcond_swf___21) <- "Dehydration"
label(dat_mom_raw$momadmitcond_swf___22) <- "Mastitis and related concerns"
label(dat_mom_raw$momadmitcond_swf___23) <- "MSK Complaint"
label(dat_mom_raw$momadmitcond_swf___24) <- "Peptic ulcer disease"
label(dat_mom_raw$momadmitcond_swf___25) <- "Perineal disruption"
label(dat_mom_raw$momadmitcond_swf___26) <- "Pulmonary embolism"
label(dat_mom_raw$momadmitcond_swf___27) <- "Retained products of conception"
label(dat_mom_raw$momadmitcond_swf___28) <- "Varicose Veins"

label(dat_mom_raw$momadmitsymp_swf___22) <- "Anorexia"
label(dat_mom_raw$momadmitsymp_swf___23) <- "Cesarean wound disruption"
label(dat_mom_raw$momadmitsymp_swf___24) <- "Dizziness"
label(dat_mom_raw$momadmitsymp_swf___25) <- "Dysuria"
label(dat_mom_raw$momadmitsymp_swf___26) <- "Epigastric pain"
label(dat_mom_raw$momadmitsymp_swf___27) <- "Heartburn"
label(dat_mom_raw$momadmitsymp_swf___28) <- "MSK Complaint"
label(dat_mom_raw$momadmitsymp_swf___29) <- "Painful or swollen breasts"
label(dat_mom_raw$momadmitsymp_swf___30) <- "Palmor Pallor"

label(dat_mom_raw$momseekcond_swf___19) <- "Abnormal tiredness"
label(dat_mom_raw$momseekcond_swf___20) <- "Abnormal vaginal discharge syndrome"
label(dat_mom_raw$momseekcond_swf___21) <- "Allergy"
label(dat_mom_raw$momseekcond_swf___22) <- "Cesearean wound disruption (non infectious)"
label(dat_mom_raw$momseekcond_swf___23) <- "Constipation"
label(dat_mom_raw$momseekcond_swf___24) <- "Failure to pass urine"
label(dat_mom_raw$momseekcond_swf___25) <- "Family Planning"
label(dat_mom_raw$momseekcond_swf___26) <- "Gastroesophageal reflux disease"
label(dat_mom_raw$momseekcond_swf___27) <- "Heart disease"
label(dat_mom_raw$momseekcond_swf___28) <- "Hematoma"
label(dat_mom_raw$momseekcond_swf___29) <- "Hypertension"
label(dat_mom_raw$momseekcond_swf___30) <- "Mastitis and related concerns"
label(dat_mom_raw$momseekcond_swf___31) <- "MSK Complaint"
label(dat_mom_raw$momseekcond_swf___32) <- "Oral complaint"
label(dat_mom_raw$momseekcond_swf___33) <- "Peptic Ulcer Disease"
label(dat_mom_raw$momseekcond_swf___34) <- "Perineal disruption"
label(dat_mom_raw$momseekcond_swf___35) <- "Perineal repair"
label(dat_mom_raw$momseekcond_swf___36) <- "Postpartum paralysis"
label(dat_mom_raw$momseekcond_swf___37) <- "Prolapsed rectum"
label(dat_mom_raw$momseekcond_swf___38) <- "Prolapsed uterus"
label(dat_mom_raw$momseekcond_swf___39) <- "Suture removal"

label(dat_mom_raw$momseeksymp_swf___22) <- "Abdominal swelling"
label(dat_mom_raw$momseeksymp_swf___23) <- "Anorexia"
label(dat_mom_raw$momseeksymp_swf___24) <- "Boil on the thigh"
label(dat_mom_raw$momseeksymp_swf___25) <- "Cesearean wound disruption"
label(dat_mom_raw$momseeksymp_swf___26) <- "Chest Pain"
label(dat_mom_raw$momseeksymp_swf___27) <- "Cold and flu symptoms"
label(dat_mom_raw$momseeksymp_swf___28) <- "Constipation"
label(dat_mom_raw$momseeksymp_swf___29) <- "Dizziness"
label(dat_mom_raw$momseeksymp_swf___30) <- "Dysuria"
label(dat_mom_raw$momseeksymp_swf___31) <- "Epigastric pain"
label(dat_mom_raw$momseeksymp_swf___32) <- "failure to pass urine"
label(dat_mom_raw$momseeksymp_swf___33) <- "Flatulence"
label(dat_mom_raw$momseeksymp_swf___34) <- "Generalized edema"
label(dat_mom_raw$momseeksymp_swf___35) <- "Heart Palpitations"
label(dat_mom_raw$momseeksymp_swf___36) <- "Heartburn"
label(dat_mom_raw$momseeksymp_swf___37) <- "Mastitis related complaint"
label(dat_mom_raw$momseeksymp_swf___38) <- "Mood changes"
label(dat_mom_raw$momseeksymp_swf___39) <- "MSK Complaint"
label(dat_mom_raw$momseeksymp_swf___40) <- "Nausea"
label(dat_mom_raw$momseeksymp_swf___41) <- "Oral complaint"
label(dat_mom_raw$momseeksymp_swf___42) <- "Pain in the left eye"
label(dat_mom_raw$momseeksymp_swf___43) <- "Pain or swelling in breasts"
label(dat_mom_raw$momseeksymp_swf___44) <- "Palmar Pallor"
label(dat_mom_raw$momseeksymp_swf___45) <- "Perineal disruption"
label(dat_mom_raw$momseeksymp_swf___46) <- "Perineal irritation"
label(dat_mom_raw$momseeksymp_swf___47) <- "Peripheral edema"
label(dat_mom_raw$momseeksymp_swf___48) <- "Postpartum paralysis"
label(dat_mom_raw$momseeksymp_swf___49) <- "Prolapsed rectum"
label(dat_mom_raw$momseeksymp_swf___50) <- "Prolapsed uterus"
label(dat_mom_raw$momseeksymp_swf___51) <- "Sour Taste"
label(dat_mom_raw$momseeksymp_swf___52) <- "Suture removal"
label(dat_mom_raw$momseeksymp_swf___53) <- "Syphilitic nails"
label(dat_mom_raw$momseeksymp_swf___54) <- "Yellow skin"

# Baby's data
label(dat_baby_raw$sb20wk_del_new) <- "Was this baby a stillborn?"
label(dat_baby_raw$sbcongenital_del_new) <- "Presence of visible congenital malformation"
label(dat_baby_raw$sexbb_del_new) <- "Sex of the baby or stillborn"
label(dat_baby_raw$weightbb_del_cat_new) <- "Weight of the baby, categorical"
label(dat_baby_raw$rescusoxy_del_new) <- "Was resuscitation at birth required?"
label(dat_baby_raw$dispbb_neo_new) <- "Disposition of baby?"
label(dat_baby_raw$babedeath_dis) <- "Did baby survive until discharge?"
label(dat_baby_raw$jaundice_neo_new) <- "Are the babys eyes yellow?"
label(dat_baby_raw$mean_temp_neo_new) <- "Mean neonatal temperature"
label(dat_baby_raw$mean_spo2_neo_new) <- "Mean neonatal SpO2"
label(dat_baby_raw$mean_hr_neo_new) <- "Mean neonatal heart rate"
label(dat_baby_raw$babedeath_swf) <- "Did the baby die at follow-up?"
label(dat_baby_raw$babeoutcome_swf) <- "Did the baby have an outcome (admission or death) at follow-up?"
label(dat_baby_raw$babeadmit_swf_days) <- "Days until re-admission"
label(dat_baby_raw$babedeath_swf_days) <- "Days until baby's death"
label(dat_baby_raw$babeoutcome_swf_days) <- "Days until baby's death or re-admission"



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMBINED DATASET           ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Recombine the data
dat_full_raw <- dat_mom_raw %>% 
  full_join(dat_baby_raw, by = "studyid_adm")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE WORKSPACE             ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Only keep final dataframes
to_keep <- c("dat_raw", "dat_mom_raw", "dat_baby_raw", "dat_full_raw", "redcap_date")
rm(list = setdiff(ls(), to_keep))
save.image(paste0("C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Cleaned Data/Cleaned_Data (", redcap_date, ").RData"))

