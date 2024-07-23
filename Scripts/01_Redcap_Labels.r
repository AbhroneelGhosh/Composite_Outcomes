#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv("C:/Users/home/OneDrive - UBC/IGH/Mom + Baby Project/Data/MomBabyTargetedFollo_DATA_2024-04-26_0907.csv")
#Setting Labels

label(data$studyid_adm)="Study ID"
label(data$redcap_repeat_instrument)="Repeat Instrument"
label(data$redcap_repeat_instance)="Repeat Instance"
label(data$creationdate_adm)="Date of creation"
label(data$uploaddate_adm)="Date of upload"
label(data$appversion_adm)="SD4Mom Version"
label(data$is_pilot_adm)="Enrolled as part of pilot phase?"
label(data$site_adm)="Site of enrollment"
label(data$nurse_adm_v2)="Nurse collecting the data"
label(data$othernurse_adm)="Specify other"
label(data$reason_adm_v2)="Is the woman admitted to the hospital for delivery?"
label(data$eligibleage_adm_v2)="Is the woman over the age of 12?"
label(data$exclusion_adm_v2)="Do any other exclusion criteria apply?"
label(data$consentform_adm_v2)="Is the consent form complete and signed?"
label(data$fetaldemise_adm)="Is this a known intrauterine fetal demise? NOTE: Not an exclusion criteria"
label(data$firstname_adm_v2)="First name"
label(data$lastname_adm_v2)="Last name"
label(data$contactname_adm_v2)="Contact name (next of kin)"
label(data$phone_adm_v2)="Phone number"
label(data$verifyphone_adm_v2)="Please verify phone number (ask mother again)"
label(data$otherphone_adm_v2)="Alternative phone number (different person)"
label(data$verifyotherphone_adm_v2)="Verify alternate phone number"
label(data$chairman_adm_v2)="Name of Chairman"
label(data$chairmanphone_adm_v2)="Chairman phone number"
label(data$mrrh_id_v2)="Hospital patient ID"
label(data$admitdate_adm_v2)="Date of admission"
label(data$admittime_adm_v2)="Time of admission (USE 24 HOUR TIME - HH:MM)"
label(data$districtresidence_adm_v2)="District of residence"
label(data$districtother_adm_v2)="Other district"
label(data$subcountyresidence_adm_v2)="Sub-county of residence"
label(data$subcountyother_adm_v2)="Other sub-county"
label(data$parishresidence_adm_v2)="Parish of residence"
label(data$parishother_adm_v2)="Other parish"
label(data$villageresidence_adm_v2)="Village of residence"
label(data$countyresidence_adm_v2)="County/Constituency of residence"
label(data$landmarkresidence_adm_v2)="Landmark nearby to residence"
label(data$chairmanresidence_adm_v2)="Chairman name"
label(data$dobknown_adm_v2)="Is exact date of birth known?"
label(data$dob_adm_v2)="Date of birth"
label(data$dob_estage_adm_v2)="What is her estimated age in years?"
label(data$comment_adm_v2)="Comment"
label(data$admission_subject_details_complete)="Complete?"
label(data$time_tohosp_adm)="How long did it take you to travel to this hospital?"
label(data$transport_adm)="Primary mode of transportation to the hospital?"
label(data$delay_adm___1)="Were you delayed >1 hour by any of the following (choose all that apply): (choice=Terrain (swamp, flood, water transport delay, etc.))"
label(data$delay_adm___2)="Were you delayed >1 hour by any of the following (choose all that apply): (choice=Cost of transport)"
label(data$delay_adm___3)="Were you delayed >1 hour by any of the following (choose all that apply): (choice=General transport delay (waiting for the bus, schedules, travel at night, etc.))"
label(data$delay_adm___4)="Were you delayed >1 hour by any of the following (choose all that apply): (choice=Significant rain/threat of rain)"
label(data$delay_adm___98)="Were you delayed >1 hour by any of the following (choose all that apply): (choice=Other)"
label(data$delay_adm___99)="Were you delayed >1 hour by any of the following (choose all that apply): (choice=None)"
label(data$isreferral_adm)="Is this visit a referral?"
label(data$referralsrc_adm)="Referral source"
label(data$csection_adm)="Is the mother here for elective c-section?"
label(data$labordate_adm)="Date labor started:"
label(data$labortime_adm)="Time labor started:"
label(data$takevitals_adm)="Record BP, Temperature, and HR?"
label(data$sys_bp_adm)="Systolic blood pressure at admission"
label(data$dia_bp_adm)="Diastolic blood pressure at admission"
label(data$temp_adm)="Temperature at admission"
label(data$hr_adm)="Heart rate at admission"
label(data$distress_adm)="Was the woman in distress upon arrival?"
label(data$admission_complete)="Complete?"
label(data$medhx_adm___1)="Has the women been diagnosed with ANY of the following conditions BEFORE pregnancy (choose all that apply) (choice=HIV (diagnosed BEFORE pregnancy))"
label(data$medhx_adm___2)="Has the women been diagnosed with ANY of the following conditions BEFORE pregnancy (choose all that apply) (choice=High blood pressure (diagnosed BEFORE pregnancy))"
label(data$medhx_adm___3)="Has the women been diagnosed with ANY of the following conditions BEFORE pregnancy (choose all that apply) (choice=Prior infertility (>1yr attempting to get pregnant but unsuccesful))"
label(data$medhx_adm___4)="Has the women been diagnosed with ANY of the following conditions BEFORE pregnancy (choose all that apply) (choice=Diabetes (diagnosed BEFORE pregnancy))"
label(data$medhx_adm___5)="Has the women been diagnosed with ANY of the following conditions BEFORE pregnancy (choose all that apply) (choice=Kidney disease)"
label(data$medhx_adm___6)="Has the women been diagnosed with ANY of the following conditions BEFORE pregnancy (choose all that apply) (choice=Sickle cell)"
label(data$medhx_adm___7)="Has the women been diagnosed with ANY of the following conditions BEFORE pregnancy (choose all that apply) (choice=Hepatitis B/C)"
label(data$medhx_adm___8)="Has the women been diagnosed with ANY of the following conditions BEFORE pregnancy (choose all that apply) (choice=Tuberculosis)"
label(data$medhx_adm___9)="Has the women been diagnosed with ANY of the following conditions BEFORE pregnancy (choose all that apply) (choice=Chronic mental illness)"
label(data$medhx_adm___98)="Has the women been diagnosed with ANY of the following conditions BEFORE pregnancy (choose all that apply) (choice=Other diagnoses not listed here)"
label(data$medhx_adm___99)="Has the women been diagnosed with ANY of the following conditions BEFORE pregnancy (choose all that apply) (choice=No prior diagnoses)"
label(data$duedate_adm)="Does the woman know her approximate due date?"
label(data$duedate3_adm)="Due date (if known)"
label(data$duedate2_adm)="How was the due date defined?"
label(data$gravid_adm)="How many pregnancies has she had in her lifetime (including this one)?"
label(data$parity_adm)="How many pregnancies has she had with a baby born greater than 500g at birth delivered after 20 weeks gestation?"
label(data$csect_adm)="Has woman had a prior c-section?"
label(data$csect2_adm)="How long ago was most recent c-section?"
label(data$preghx_adm___1)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Gestational diabetes)"
label(data$preghx_adm___2)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Pre-eclampsia)"
label(data$preghx_adm___3)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Eclampsia)"
label(data$preghx_adm___4)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Gestational hypertension)"
label(data$preghx_adm___5)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Antepartum hemorrhage/ vaginal bleeding (1st trimester))"
label(data$preghx_adm___6)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Antepartum hemorrhage/ vaginal bleeding (2nd trimester))"
label(data$preghx_adm___7)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Antepartum hemorrhage/ vaginal bleeding (3rd trimester))"
label(data$preghx_adm___8)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=PPROM)"
label(data$preghx_adm___9)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Preterm labour)"
label(data$preghx_adm___10)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Malaria)"
label(data$preghx_adm___11)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=HIV (diagnosed DURING))"
label(data$preghx_adm___12)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Urinary tract infection)"
label(data$preghx_adm___13)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Tuberculosis)"
label(data$preghx_adm___14)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Anemia)"
label(data$preghx_adm___15)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=COVID-19 (suspected))"
label(data$preghx_adm___16)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=COVID-19 (posititive test))"
label(data$preghx_adm___17)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Hepatitis B/C)"
label(data$preghx_adm___18)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Mental Health Illness)"
label(data$preghx_adm___98)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=Other Infection)"
label(data$preghx_adm___99)="Has the woman been diagnosed with any of the following conditions DURING THIS pregnancy? (select all that apply) (choice=None)"
label(data$placenta_adm___1)="Any known/suspected placental disorder (this pregnancy) (choice=Yes, placenta previa)"
label(data$placenta_adm___2)="Any known/suspected placental disorder (this pregnancy) (choice=Yes, placental abruption)"
label(data$placenta_adm___97)="Any known/suspected placental disorder (this pregnancy) (choice=Yes, other (specify))"
label(data$placenta_adm___98)="Any known/suspected placental disorder (this pregnancy) (choice=No)"
label(data$placenta_adm___99)="Any known/suspected placental disorder (this pregnancy) (choice=Not sure)"
label(data$placentaother_adm)="Specify other"
label(data$medhbp_adm)="Is she currently taking medication for high blood pressure?"
label(data$medarv_adm)="Is she currently taking ARVs for HIV treatment?"
label(data$covidvax_adm)="Has woman been vaccinated against COVID-19"
label(data$prevadm_adm)="Has the woman been admitted to hospital DURING this pregnancy for any reason?"
label(data$lngth_prevadm_adm)="How long ago was this admission?"
label(data$rs_prevadm_adm___1)="What was the reason for this admission? (choice=Gestational diabetes)"
label(data$rs_prevadm_adm___2)="What was the reason for this admission? (choice=Pre-eclampsia)"
label(data$rs_prevadm_adm___3)="What was the reason for this admission? (choice=Eclampsia)"
label(data$rs_prevadm_adm___4)="What was the reason for this admission? (choice=Gestational hypertension)"
label(data$rs_prevadm_adm___5)="What was the reason for this admission? (choice=Antepartum hemorrhage/ vaginal bleeding)"
label(data$rs_prevadm_adm___6)="What was the reason for this admission? (choice=PPROM)"
label(data$rs_prevadm_adm___7)="What was the reason for this admission? (choice=Preterm labour)"
label(data$rs_prevadm_adm___8)="What was the reason for this admission? (choice=Malaria)"
label(data$rs_prevadm_adm___9)="What was the reason for this admission? (choice=HIV)"
label(data$rs_prevadm_adm___10)="What was the reason for this admission? (choice=Urinary tract infection)"
label(data$rs_prevadm_adm___11)="What was the reason for this admission? (choice=Tuberculosis)"
label(data$rs_prevadm_adm___12)="What was the reason for this admission? (choice=Anemia)"
label(data$rs_prevadm_adm___13)="What was the reason for this admission? (choice=Abuse/Fight)"
label(data$rs_prevadm_adm___14)="What was the reason for this admission? (choice=motor accident (car, boda, etc.))"
label(data$rs_prevadm_adm___98)="What was the reason for this admission? (choice=Other Infection)"
label(data$rs_prevadm_adm___99)="What was the reason for this admission? (choice=Other (specify))"
label(data$rs_prevadm2_adm)="Specify other"
label(data$prevadm2_adm)="For how many days was the woman admitted"
label(data$uti_abx_adm)="Was she given IV antibiotics/antimalarials during the admission? "
label(data$numberanc_adm)="How many ANC visits did you attend (best estimate if unsure)?"
label(data$ancprovider_adm___1)="Which types of healthcare providers provided your ANC care (choose all that apply) (choice=Obstetrician)"
label(data$ancprovider_adm___2)="Which types of healthcare providers provided your ANC care (choose all that apply) (choice=Medical Officer)"
label(data$ancprovider_adm___3)="Which types of healthcare providers provided your ANC care (choose all that apply) (choice=Clinical Officer)"
label(data$ancprovider_adm___4)="Which types of healthcare providers provided your ANC care (choose all that apply) (choice=Nurse)"
label(data$ancprovider_adm___5)="Which types of healthcare providers provided your ANC care (choose all that apply) (choice=midwife)"
label(data$ancprovider_adm___6)="Which types of healthcare providers provided your ANC care (choose all that apply) (choice=Traditional Birth Attendant)"
label(data$ancprovider_adm___7)="Which types of healthcare providers provided your ANC care (choose all that apply) (choice=Other)"
label(data$comment_adm)="Comment"
label(data$pregnancy_history_complete)="Complete?"
label(data$housenum_ses)="How many people will be living in your household (including you AND new baby)?"
label(data$numchild_ses)="How many children are you living with in your household (including the new baby)?"
label(data$marry_ses)="What is your current marital status"
label(data$livfather_ses)="Do you live with the father of this baby?"
label(data$schoolyrs_ses)="What is the highest level of education you (mother) have completed?"
label(data$nutri_adm)="During your pregnancy, did you or any household member have to eat less food than you felt you needed?"
label(data$sesindex_flooring___1)="Which types of flooring does ANY room in your home have (choose ALL that apply)?  (choice=Earth/Dung/Sand)"
label(data$sesindex_flooring___2)="Which types of flooring does ANY room in your home have (choose ALL that apply)?  (choice=Temporary carpet)"
label(data$sesindex_flooring___3)="Which types of flooring does ANY room in your home have (choose ALL that apply)?  (choice=Permanent flooring (tile, finished wood))"
label(data$sesindex_flooring___4)="Which types of flooring does ANY room in your home have (choose ALL that apply)?  (choice=Cement)"
label(data$sesindex_flooring___98)="Which types of flooring does ANY room in your home have (choose ALL that apply)?  (choice=Other)"
label(data$sesindex_toilet___1)="What kind of toilet do members of your household usually use? (choice=Flush toilet system (sitting or squatting))"
label(data$sesindex_toilet___2)="What kind of toilet do members of your household usually use? (choice=Ventilated Improved Pit (VIP) Latrine)"
label(data$sesindex_toilet___3)="What kind of toilet do members of your household usually use? (choice=Composting toilet)"
label(data$sesindex_toilet___4)="What kind of toilet do members of your household usually use? (choice=Pit latrine)"
label(data$sesindex_toilet___5)="What kind of toilet do members of your household usually use? (choice=Bucket toilet)"
label(data$sesindex_toilet___6)="What kind of toilet do members of your household usually use? (choice=No toilet)"
label(data$sesindex_toilet___98)="What kind of toilet do members of your household usually use? (choice=Other)"
label(data$sesindex_toiletshared)="Do you share this toilet with anyone outside your household (i.e. those NOT living together with you)?"
label(data$sesindex_cooking___1)="Which of the following energy sources for cooking food do you use (choose all that apply)? (choice=Electricity)"
label(data$sesindex_cooking___2)="Which of the following energy sources for cooking food do you use (choose all that apply)? (choice=Petrol/diesel/propane)"
label(data$sesindex_cooking___3)="Which of the following energy sources for cooking food do you use (choose all that apply)? (choice=Kerosene/paraffin)"
label(data$sesindex_cooking___4)="Which of the following energy sources for cooking food do you use (choose all that apply)? (choice=Coal/charcoal)"
label(data$sesindex_cooking___5)="Which of the following energy sources for cooking food do you use (choose all that apply)? (choice=Wood)"
label(data$sesindex_cooking___6)="Which of the following energy sources for cooking food do you use (choose all that apply)? (choice=Straw/shrubs/grass)"
label(data$sesindex_cooking___7)="Which of the following energy sources for cooking food do you use (choose all that apply)? (choice=Biomass or woodchips)"
label(data$sesindex_cooking___8)="Which of the following energy sources for cooking food do you use (choose all that apply)? (choice=Dung)"
label(data$sesindex_cooking___9)="Which of the following energy sources for cooking food do you use (choose all that apply)? (choice=Other)"
label(data$sesindex_safewater)="What is the main source of drinking water for members of your household? "
label(data$sesindex_otherwater)="Specify other:"
label(data$sesindex_safewaterdistance)="Does it take you more than 30 minutes to walk from your home to the water source and back? "
label(data$sesindex_assets___1)="Does your household have any of the following items:  (choice=Electricity (from the grid))"
label(data$sesindex_assets___2)="Does your household have any of the following items:  (choice=A television)"
label(data$sesindex_assets___3)="Does your household have any of the following items:  (choice=A computer)"
label(data$sesindex_assets___4)="Does your household have any of the following items:  (choice=A refrigerator)"
label(data$sesindex_assets___99)="Does your household have any of the following items:  (choice=None)"
label(data$sesindex_assets2___1)="Do you (or someone in your household) own any of the following items:  (choice=A smart phone)"
label(data$sesindex_assets2___2)="Do you (or someone in your household) own any of the following items:  (choice=A non-smart phone)"
label(data$sesindex_assets2___3)="Do you (or someone in your household) own any of the following items:  (choice=A bicycle)"
label(data$sesindex_assets2___4)="Do you (or someone in your household) own any of the following items:  (choice=A motorcycle)"
label(data$sesindex_assets2___5)="Do you (or someone in your household) own any of the following items:  (choice=A car or truck)"
label(data$sesindex_assets2___99)="Do you (or someone in your household) own any of the following items:  (choice=None)"
label(data$sesindex_room)="How many rooms does your home contain?"
label(data$child_death_ses)="Have you had any children who have died?"
label(data$comment_ses)="Comment"
label(data$ses_and_demographics_complete)="Complete?"
label(data$numbabe_del)="How many babies were delivered?"
label(data$prom_del)="Rupture of membranes more than 24hrs before delivery?"
label(data$rom_del)="For how many hours were membranes ruptured prior to delivery?"
label(data$deldate_del)="Delivery date"
label(data$deltime_del)="Delivery time (USE 24 HOUR TIME - HH:MM)"
label(data$delmode_del)="What was the mode of delivery?"
label(data$episiotomy_del)="Was the woman given an episiotomy?"
label(data$tear_del)="Was there any noted vaginal or perineal tearing?"
label(data$degreetear_del)="What was the recorded degree of tearing?"
label(data$induce_del)="Was the start of labour induced?"
label(data$inductype_del___1)="Which method of induction was used (select all that apply)? (choice=Membrane strip and sweep)"
label(data$inductype_del___2)="Which method of induction was used (select all that apply)? (choice=Manual rupture of membranes)"
label(data$inductype_del___3)="Which method of induction was used (select all that apply)? (choice=Foley catheter)"
label(data$inductype_del___4)="Which method of induction was used (select all that apply)? (choice=Prostaglandins)"
label(data$inductype_del___5)="Which method of induction was used (select all that apply)? (choice=Oxytocin)"
label(data$inductype_del___98)="Which method of induction was used (select all that apply)? (choice=Other)"
label(data$pph_del)="Was a PPH noted?"
label(data$transfx_del)="Was the woman given a blood transfusion?"
label(data$unittrans_del)="How many units of blood?"
label(data$obstruct_del)="Was obstructed labour noted? "
label(data$meconium_del)="Was any meconium noted in the amniotic fluid during labour?"
label(data$vagexam_del)="Number of vaginal exams (self reported best estimate)"
label(data$placenta_del)="Placental complications noted?"
label(data$man_placenta_del)="Manual removal of placenta"
label(data$other_del)="Other major event or conditions during delivery?"
label(data$csecturgency_del)="Surgical urgency (per clinical team)"
label(data$csect_delay_del)="Estimated decision to delivery time"
label(data$heme_del)="Pre-surgery hemoglobin level"
label(data$abx_del)="Were prophylactic antibiotics administered?"
label(data$csect_abx_del)="List all antibiotics used for prophylaxis"
label(data$otherabx_del)="Specify other antibiotic"
label(data$anticoag_del)="Were anticoagulants administered?"
label(data$delivery_maternal_complete)="Complete?"
label(data$infantstatus_del)="Was the baby born alive?"
label(data$sb20wk_del)="Is stillbirth assumed to be >20 weeks gestation?"
label(data$sb_del)="Type of stillbirth"
label(data$sbcongenital_del)="Presence of visible congenital malformation"
label(data$sbsex_del)="Sex of the stillborn baby"
label(data$sexbb_del)="Sex of the baby"
label(data$apgar1_del)="Apgar score after 1 minute"
label(data$apgar5_del)="Apgar score after 5 minutes"
label(data$cord_delay_del)="Did the patient experience delayed cord clamping (> 1 minute)?"
label(data$weightbb_del)="Weight of the baby (kg)"
label(data$height_neo)="Length (cm)"
label(data$rescus_del)="Any rescucitation at birth?"
label(data$resustype_del___1)="What type of resuscitation did the baby receive after birth? (check all that apply) (choice=Stimulation only)"
label(data$resustype_del___2)="What type of resuscitation did the baby receive after birth? (check all that apply) (choice=Oxygen)"
label(data$resustype_del___3)="What type of resuscitation did the baby receive after birth? (check all that apply) (choice=Bag mask)"
label(data$resustype_del___4)="What type of resuscitation did the baby receive after birth? (check all that apply) (choice=Suction)"
label(data$resustype_del___5)="What type of resuscitation did the baby receive after birth? (check all that apply) (choice=Drugs)"
label(data$resustype_del___6)="What type of resuscitation did the baby receive after birth? (check all that apply) (choice=Warming)"
label(data$resustype_del___7)="What type of resuscitation did the baby receive after birth? (check all that apply) (choice=Chest compression)"
label(data$resustype_del___8)="What type of resuscitation did the baby receive after birth? (check all that apply) (choice=Dont know)"
label(data$resustype_del___98)="What type of resuscitation did the baby receive after birth? (check all that apply) (choice=Other)"
label(data$resustype_del___99)="What type of resuscitation did the baby receive after birth? (check all that apply) (choice=None)"
label(data$delivery_neonatal_complete)="Complete?"
label(data$disnurse_mat)="Nurse collecting the data"
label(data$disnurseother_mat)="Specify other nurse:"
label(data$adm_mat)="Was mother admitted to higher level of care due to complications AFTER delivery"
label(data$dischstat_mat)="Discharge status"
label(data$sbp_mat)="Discharge systolic blood pressure"
label(data$dbp_mat)="Discharge diastolic blood pressure"
label(data$rr_mat)="Respiratory rate"
label(data$temp_mat)="Temporal artery temperature"
label(data$best_spo2_mat)="Best SpO2"
label(data$best_hr_mat)="Best Heart rate"
label(data$best_oxyradg_mat)="Rad-G: Best SpO2 from Rad-G"
label(data$best_hrradg_mat)="Rad-G: best heart rate (PR) from Rad-G"
label(data$tempradg_mat)="Rad-G: Temporal artery temperature from Rad-G"
label(data$rrradg_mat)="Rad-G: Respiratory rate (RRp) from Rad-G"
label(data$hem_mat)="Hematocrit (%)"
label(data$destination_mat)="Discharge destination"
label(data$dischdate_mat)="Date of discharge"
label(data$deathdate_mat)="(Temporary) Date of death"
label(data$dischtime_mat)="Time of discharge (USE 24 HOUR TIME)"
label(data$support_mat___1)="Please indicate all those from whom you have substantial support at home after discharge (choose all that apply) (choice=Mother)"
label(data$support_mat___2)="Please indicate all those from whom you have substantial support at home after discharge (choose all that apply) (choice=Mother-in-law)"
label(data$support_mat___3)="Please indicate all those from whom you have substantial support at home after discharge (choose all that apply) (choice=Husband/partner/father)"
label(data$support_mat___4)="Please indicate all those from whom you have substantial support at home after discharge (choose all that apply) (choice=Other relative (sister/cousin, etc))"
label(data$support_mat___5)="Please indicate all those from whom you have substantial support at home after discharge (choose all that apply) (choice=Friend)"
label(data$support_mat___99)="Please indicate all those from whom you have substantial support at home after discharge (choose all that apply) (choice=No substantial support at home)"
label(data$bf_mat)="Any prior history of poor milk production?"
label(data$symp_mat___1)="Symptoms currently present (check all that apply) (choice=Headache (continuous pain in the front of the head or behind the eyes))"
label(data$symp_mat___2)="Symptoms currently present (check all that apply) (choice=Visual changes (spots, flashing lights or blurry vision))"
label(data$symp_mat___3)="Symptoms currently present (check all that apply) (choice=Chest pain (causing difficulty breathing))"
label(data$symp_mat___4)="Symptoms currently present (check all that apply) (choice=Shortness of breath)"
label(data$symp_mat___5)="Symptoms currently present (check all that apply) (choice=Nausea with vomiting)"
label(data$symp_mat___6)="Symptoms currently present (check all that apply) (choice=Abdominal pain on the right side)"
label(data$symp_mat___7)="Symptoms currently present (check all that apply) (choice=Foul smelling vaginal discharge)"
label(data$symp_mat___8)="Symptoms currently present (check all that apply) (choice=Stiff neck)"
label(data$symp_mat___9)="Symptoms currently present (check all that apply) (choice=Cough)"
label(data$symp_mat___10)="Symptoms currently present (check all that apply) (choice=Difficulty emptying bladder)"
label(data$symp_mat___99)="Symptoms currently present (check all that apply) (choice=None)"
label(data$abx_mat___1)="During this admission was woman given antibiotics? (choice=Yes - oral antibiotics)"
label(data$abx_mat___2)="During this admission was woman given antibiotics? (choice=Yes - IV antibiotics)"
label(data$abx_mat___3)="During this admission was woman given antibiotics? (choice=No antibiotics)"
label(data$abx_mat___99)="During this admission was woman given antibiotics? (choice=Not sure)"
label(data$followup_date_mat)="Scheduled follow-up date (for phone follow-up with research team)"
label(data$comment_mat)="Comment"
label(data$best_sqi_mat)="Best SQI"
label(data$best_spo2trends_mat)="Best SpO2 Trends file"
label(data$best_spo2raw_mat)="Best SpO2 Raw bin file"
label(data$add_sqi_mat)="Additional SQI"
label(data$add_spo2trends_mat)="Additional SpO2 Trends file"
label(data$add_spo2raw_mat)="Additional SpO2 Raw bin file"
label(data$rrtaps_mat)="Respiratory rate taps"
label(data$discharge_interview_maternal_complete)="Complete?"
label(data$dispbb_neo)="Disposition of baby?"
label(data$dispbb_other_neo)="Specify other:"
label(data$dispdate_neo)="Disposition date"
label(data$admitsite_neo)="Where was the child admitted?"
label(data$admitdisposition_neo)="Admission disposition"
label(data$admitdispdate_neo)="Admission disposition date"
label(data$admitdiagnosis_neo___1)="Admission diagnosis (choice=Sepsis)"
label(data$admitdiagnosis_neo___2)="Admission diagnosis (choice=Birth Asphyxia)"
label(data$admitdiagnosis_neo___3)="Admission diagnosis (choice=Congenital)"
label(data$admitdiagnosis_neo___4)="Admission diagnosis (choice=Jaundice)"
label(data$admitdiagnosis_neo___5)="Admission diagnosis (choice=prematurity/low birth weight)"
label(data$admitdiagnosis_neo___99)="Admission diagnosis (choice=Other (specify))"
label(data$admitdiagnosisother_neo)="Other admission diagnosis:"
label(data$babedisch_neo)="Is the baby being discharged home with mom?"
label(data$poop_neo)="Has your baby pooped?"
label(data$pee_neo)="Has your baby peed?"
label(data$bf_neo)="Is baby latching and suckling well when breastfeeding?"
label(data$jaundice_neo)="Are the babys eyes yellow?"
label(data$eyedischarge_neo)="Does the baby have a discharge in their eyes?"
label(data$temp1_neo)="Temporal artery temperature 1"
label(data$temp2_neo)="Temporal artery temperature 2"
label(data$rr_neo)="Respiratory rate"
label(data$foot_o2src_neo)="Foot Oxygen Saturation Source"
label(data$foot_best_spo2_neo)="Best Foot SpO2"
label(data$foot_best_hr_neo)="Best Foot Heart rate"
label(data$foot_best_oxyradg_neo)="Rad-G: Best Foot SpO2 from Rad-G"
label(data$foot_best_hrradg_neo)="Rad-G: Best Foot Heart rate (PR) from Rad-G"
label(data$tempfootradg_neo)="Rad-G: Foot Temporal artery temperature from Rad-G"
label(data$rrfootradg_neo)="Rad-G: Foot Respiratory rate (RRp) from Rad-G"
label(data$rhand_o2src_neo)="Right Hand Oxygen Saturation Source"
label(data$rhand_best_spo2_neo)="Best Right Hand SpO2"
label(data$rhand_best_hr_neo)="Best Right Hand Heart rate"
label(data$rhand_best_oxyradg_neo)="Rad-G: Best Right Hand SpO2 from Rad-G"
label(data$rhand_best_hrradg_neo)="Rad-G: Best Right Hand Heart rate (PR) from Rad-G"
label(data$temphandradg_neo)="Rad-G: Hand Temporal artery temperature from Rad-G"
label(data$rrhandradg_neo)="Rad-G: Hand Respiratory rate (RRp) from Rad-G"
label(data$abx_neo)="Was the baby discharged on antibiotics?"
label(data$comment_neo)="Comment"
label(data$foot_best_sqi_neo)="Best Foot SQI"
label(data$foot_best_spo2trends_neo)="Best Foot SpO2 Trends file"
label(data$foot_best_spo2raw_neo)="Best Foot SpO2 Raw bin file"
label(data$foot_add_sqi_neo)="Additional Foot SQI"
label(data$foot_add_spo2trends_neo)="Additional Foot SpO2 Trends file"
label(data$foot_add_spo2raw_neo)="Additional Foot SpO2 Raw bin file"
label(data$rhand_best_sqi_neo)="Best Right Hand SQI"
label(data$rhand_best_spo2trends_neo)="Best Right Hand SpO2 Trends file"
label(data$rhand_best_spo2raw_neo)="Best Right Hand SpO2 Raw bin file"
label(data$rhand_add_sqi_neo)="Additional Right Hand SQI"
label(data$rhand_add_spo2trends_neo)="Additional Right Hand SpO2 Trends file"
label(data$rhand_add_spo2raw_neo)="Additional Right Hand SpO2 Raw bin file"
label(data$rrtaps_neo)="Respiratory rate taps"
label(data$discharge_interview_neonatal_complete)="Complete?"
label(data$physical_swf___1)="Check this box if this is a physical (in-person) follow-up (choice=)"
label(data$respondent_swf)="Respondent during the interview?"
label(data$otherrespondent_swf)="Specify other"
label(data$home_swf)="Is mother based at the same home that you were in prior to delivery (6 weeks after birth)?"
label(data$momalive_swf)="Is the woman alive at the time of follow-up interview?"
label(data$momdeathdate_swf)="On what date did she die?"
label(data$momdeathplace_swf)="Where did she die?"
label(data$momadmit_swf)="Was the mother admitted for one or more nights at a facility for any reason after being home post-delivery?"
label(data$nummomadmit_swf)="How many times was the mother admitted in the first 6 weeks after delivery?"
label(data$momadmitdate_swf)="Date of first admission"
label(data$momnightsadm_swf)="For how many nights was she admitted?"
label(data$momadmitpathway_swf___1)="What was the care-seeking pathway to the first admission? (choice=Routine PNC visit identified problem)"
label(data$momadmitpathway_swf___2)="What was the care-seeking pathway to the first admission? (choice=Self referral)"
label(data$momadmitpathway_swf___3)="What was the care-seeking pathway to the first admission? (choice=Both self-referral and PNC visit resulted in care seeking)"
label(data$momadmitpathway_swf___98)="What was the care-seeking pathway to the first admission? (choice=Other)"
label(data$momadmitpathwayother_swf)="Other pathway:"
label(data$momadmitsymp_swf___1)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Heavy vaginal bleeding)"
label(data$momadmitsymp_swf___2)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Bleeding from nose/ eyes/ ears)"
label(data$momadmitsymp_swf___3)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Petechiae (small red dots and bruises across the skin))"
label(data$momadmitsymp_swf___4)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Abnormal tiredness)"
label(data$momadmitsymp_swf___5)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Convulsions)"
label(data$momadmitsymp_swf___6)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Shortness of breath)"
label(data$momadmitsymp_swf___7)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Changes in vision)"
label(data$momadmitsymp_swf___8)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Severe headache < 24 hrs)"
label(data$momadmitsymp_swf___9)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Severe headache >24 hrs)"
label(data$momadmitsymp_swf___10)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Abdominal pain)"
label(data$momadmitsymp_swf___11)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Abdominal tenderness when touched)"
label(data$momadmitsymp_swf___12)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Foul smelling vaginal discharge)"
label(data$momadmitsymp_swf___13)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Fever/ body hotness < 7days)"
label(data$momadmitsymp_swf___14)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Fever/ body hotness >7days)"
label(data$momadmitsymp_swf___15)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Diarrhea < 14 days)"
label(data$momadmitsymp_swf___16)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Diarrhea >14 days)"
label(data$momadmitsymp_swf___17)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Vomiting everything)"
label(data$momadmitsymp_swf___18)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Cough < 14 days)"
label(data$momadmitsymp_swf___19)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Cough >14 days)"
label(data$momadmitsymp_swf___20)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Yellow eyes)"
label(data$momadmitsymp_swf___21)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Loss of consciousness)"
label(data$momadmitsymp_swf___98)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Other (specify))"
label(data$momadmitsymp_swf___99)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=None)"
label(data$momadmitsympother_swf)="Other symptom"
label(data$momadmitcond_swf___1)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Pre-eclampsia)"
label(data$momadmitcond_swf___2)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Eclampsia)"
label(data$momadmitcond_swf___3)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Diabetes)"
label(data$momadmitcond_swf___4)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Fistula)"
label(data$momadmitcond_swf___5)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=HIV related illness)"
label(data$momadmitcond_swf___6)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Malaria)"
label(data$momadmitcond_swf___7)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Retained placenta)"
label(data$momadmitcond_swf___8)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Heavy bleeding)"
label(data$momadmitcond_swf___9)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Anemia)"
label(data$momadmitcond_swf___10)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Surgical site infection)"
label(data$momadmitcond_swf___11)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Puerperal sepsis)"
label(data$momadmitcond_swf___12)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Other infection)"
label(data$momadmitcond_swf___13)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Diarrhea)"
label(data$momadmitcond_swf___14)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Vomiting)"
label(data$momadmitcond_swf___15)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=PPD/psychosis)"
label(data$momadmitcond_swf___16)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Needed another surgery)"
label(data$momadmitcond_swf___17)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Needed blood transfusion)"
label(data$momadmitcond_swf___18)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Needed oxygen)"
label(data$momadmitcond_swf___98)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Other (specify))"
label(data$momadmitcond_swf___99)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=None)"
label(data$momadmitcondother_swf)="Other condition:"
label(data$momseek_swf)="Did she seek care at a facility at any time after being home post-discharge that DID NOT result in an admission?"
label(data$momseekdate_swf)="Date of first care seeking visit:"
label(data$momseekpathway_swf___1)="What was the care-seeking pathway for this visit? (choice=Routine PNC visit identified problem)"
label(data$momseekpathway_swf___2)="What was the care-seeking pathway for this visit? (choice=Self referral)"
label(data$momseekpathway_swf___3)="What was the care-seeking pathway for this visit? (choice=Both self-referral and PNC visit resulted in care seeking)"
label(data$momseekpathway_swf___98)="What was the care-seeking pathway for this visit? (choice=Other)"
label(data$momseekpathwayother_swf)="Other pathway:"
label(data$momseeksymp_swf___1)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Heavy vaginal bleeding)"
label(data$momseeksymp_swf___2)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Bleeding from nose/ eyes/ ears)"
label(data$momseeksymp_swf___3)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Petechiae (small red dots and bruises across the skin))"
label(data$momseeksymp_swf___4)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Abnormal tiredness)"
label(data$momseeksymp_swf___5)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Convulsions)"
label(data$momseeksymp_swf___6)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Shortness of breath)"
label(data$momseeksymp_swf___7)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Changes in vision)"
label(data$momseeksymp_swf___8)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Severe headache < 24 hrs)"
label(data$momseeksymp_swf___9)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Severe headache >24 hrs)"
label(data$momseeksymp_swf___10)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Abdominal pain)"
label(data$momseeksymp_swf___11)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Abdominal tenderness when touched)"
label(data$momseeksymp_swf___12)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Foul smelling vaginal discharge)"
label(data$momseeksymp_swf___13)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Fever/ body hotness < 7days)"
label(data$momseeksymp_swf___14)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Fever/ body hotness >7days)"
label(data$momseeksymp_swf___15)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Diarrhea < 14 days)"
label(data$momseeksymp_swf___16)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Diarrhea >14 days)"
label(data$momseeksymp_swf___17)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Vomiting everything)"
label(data$momseeksymp_swf___18)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Cough < 14 days)"
label(data$momseeksymp_swf___19)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Cough >14 days)"
label(data$momseeksymp_swf___20)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Yellow eyes)"
label(data$momseeksymp_swf___21)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Loss of consciousness)"
label(data$momseeksymp_swf___98)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Other (specify))"
label(data$momseeksymp_swf___99)="What were the symptoms she experienced during this illness (check all that apply)? (choice=None)"
label(data$momseeksympother_swf)="Other symptom:"
label(data$momseekcond_swf___1)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Pre-eclampsia)"
label(data$momseekcond_swf___2)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Eclampsia)"
label(data$momseekcond_swf___3)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Diabetes)"
label(data$momseekcond_swf___4)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Fistula)"
label(data$momseekcond_swf___5)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=HIV)"
label(data$momseekcond_swf___6)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Malaria)"
label(data$momseekcond_swf___7)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Retained placenta)"
label(data$momseekcond_swf___8)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Heavy bleeding)"
label(data$momseekcond_swf___9)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Anemia)"
label(data$momseekcond_swf___10)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Surgical site infection)"
label(data$momseekcond_swf___11)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Puerperal sepsis)"
label(data$momseekcond_swf___12)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Other infection)"
label(data$momseekcond_swf___13)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Diarrhea)"
label(data$momseekcond_swf___14)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Vomiting)"
label(data$momseekcond_swf___15)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=PPD/psychosis)"
label(data$momseekcond_swf___16)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Needed another surgery)"
label(data$momseekcond_swf___17)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Needed blood transfusion)"
label(data$momseekcond_swf___18)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Needed oxygen)"
label(data$momseekcond_swf___98)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Other (specify))"
label(data$momseekcond_swf___99)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=None)"
label(data$momseekcondother_swf)="Other condition:"
label(data$pnc_mom_swf)="How many PNC visits were conducted in the first 6 weeks after delivery to assess the condition of the mother (NOT for the baby)?"
label(data$mompnc1_swf)="How many days after delivery did the first visit take place?"
label(data$mompnc2_swf)="How many days after delivery did the second visit take place?"
label(data$mompnc3_swf)="How many days after delivery did the third visit take place?"
label(data$comment_swf)="Comment"
label(data$six_week_follow_up_maternal_complete)="Complete?"
label(data$babephysical_swf___1)="Check this box if this is a physical (in-person) follow-up (choice=)"
label(data$babealive_swf)="Is the newborn alive at the time of follow-up interview?"
label(data$babedeathdate_swf)="On what date did the baby die?"
label(data$babedeathplace_swf)="Where did the baby die?"
label(data$babedeathplaceother_swf)="Specify other"
label(data$babeadmit_swf)="Was the newborn admitted for one or more nights at a facility for any reason after being home post-delivery?"
label(data$numbabeadmit_swf)="How many times was the baby admitted in the first 6 weeks after birth"
label(data$babeadmitage_swf)="How many days old was the baby during the first admission?"
label(data$babenightsadm_swf)="For how many nights were they admitted?"
label(data$babeadmitpathway_swf___1)="What was the care-seeking pathway to this admission? (choice=Routine well-baby visit identified problem)"
label(data$babeadmitpathway_swf___2)="What was the care-seeking pathway to this admission? (choice=Self referral due to parental concern)"
label(data$babeadmitpathway_swf___3)="What was the care-seeking pathway to this admission? (choice=Both self-referral and well-baby visit resulted in care seeking)"
label(data$babeadmitpathway_swf___98)="What was the care-seeking pathway to this admission? (choice=Other)"
label(data$babeadmitpathwayother_swf)="Other pathway:"
label(data$babeadmitsymp_swf___1)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Skin pustules)"
label(data$babeadmitsymp_swf___2)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Respiratory distress)"
label(data$babeadmitsymp_swf___3)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Watery stool)"
label(data$babeadmitsymp_swf___4)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Fever/hotness of body)"
label(data$babeadmitsymp_swf___5)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Bilious vomit)"
label(data$babeadmitsymp_swf___6)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=projectile vomit)"
label(data$babeadmitsymp_swf___7)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Not waking up to feed or abnormally sleepy)"
label(data$babeadmitsymp_swf___8)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Swelling of both feet)"
label(data$babeadmitsymp_swf___9)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Changes in urine color)"
label(data$babeadmitsymp_swf___10)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Making less urine than usual)"
label(data$babeadmitsymp_swf___11)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Blood in stool)"
label(data$babeadmitsymp_swf___12)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Seizure/convulsions)"
label(data$babeadmitsymp_swf___13)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Coma)"
label(data$babeadmitsymp_swf___14)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Yellow soles)"
label(data$babeadmitsymp_swf___15)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Difficulty wtih breastfeeding)"
label(data$babeadmitsymp_swf___16)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Cough)"
label(data$babeadmitsymp_swf___17)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Umbilical cord problem (pain, discharge, etc.))"
label(data$babeadmitsymp_swf___18)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Not gaining weight)"
label(data$babeadmitsymp_swf___98)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Other)"
label(data$babeadmitsymp_swf___99)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=None)"
label(data$babeadmitsympother_swf)="Other symptom:"
label(data$transfusion_swf)="Was baby transfused during any admission post-discharge"
label(data$babeadmitcond_swf___1)="Were you told the child had any of the following conditions? (choice=Sepsis)"
label(data$babeadmitcond_swf___2)="Were you told the child had any of the following conditions? (choice=Respiratory illness)"
label(data$babeadmitcond_swf___3)="Were you told the child had any of the following conditions? (choice=Oncologic disease (cancer))"
label(data$babeadmitcond_swf___4)="Were you told the child had any of the following conditions? (choice=Congenital abnormality)"
label(data$babeadmitcond_swf___5)="Were you told the child had any of the following conditions? (choice=Endocrine/ metabolic disease)"
label(data$babeadmitcond_swf___6)="Were you told the child had any of the following conditions? (choice=Failure to thrive)"
label(data$babeadmitcond_swf___7)="Were you told the child had any of the following conditions? (choice=Trauma/ injury)"
label(data$babeadmitcond_swf___8)="Were you told the child had any of the following conditions? (choice=HIV)"
label(data$babeadmitcond_swf___9)="Were you told the child had any of the following conditions? (choice=Sickle Cell)"
label(data$babeadmitcond_swf___10)="Were you told the child had any of the following conditions? (choice=Anemia)"
label(data$babeadmitcond_swf___11)="Were you told the child had any of the following conditions? (choice=Dehydration)"
label(data$babeadmitcond_swf___98)="Were you told the child had any of the following conditions? (choice=Other)"
label(data$babeadmitcond_swf___99)="Were you told the child had any of the following conditions? (choice=None)"
label(data$babeadmitcondother_swf)="Other condition:"
label(data$matsymp_babeoutcome)="Was the mother experiencing any signifcant illness at the time when baby was admitted?"
label(data$babefeed_outcome)="Is the baby being exclusively breastfed"
label(data$babeseek_swf)="Did you seek care for the newborn at a facility at any time after being home post-discharge that DID NOT result in an admission?"
label(data$babeseekage_swf)="How many days old was the baby during the first such visit?"
label(data$babeseekpathway_swf___1)="What was the care-seeking pathway for this visit? (choice=Routine well-baby visit identified problem)"
label(data$babeseekpathway_swf___2)="What was the care-seeking pathway for this visit? (choice=Self referral due to parental concern)"
label(data$babeseekpathway_swf___3)="What was the care-seeking pathway for this visit? (choice=Both self-referral and well-baby visit resulted in care seeking)"
label(data$babeseekpathway_swf___98)="What was the care-seeking pathway for this visit? (choice=Other)"
label(data$babeseekpathwayother_swf)="Other pathway:"
label(data$pnc_baby_swf)="How many well-baby visits were conducted in the first 6 weeks (i.e. post-natal care visits for the baby) during which your baby was physically examined"
label(data$babepnc1_swf)="How many days old was this baby at the first visit?"
label(data$babepnc2_swf)="How many days old was this baby at the second visit?"
label(data$babepnc3_swf)="How many days old was this baby at the third visit?"
label(data$comment2_swf)="Comment"
label(data$six_week_follow_up_neonatal_complete)="Complete?"
label(data$momchecks_swf)="Did you know you needed to attend a routine wellness check for yourself after being sent home?"
label(data$babechecks_swf)="Did you know you needed to attend a routine wellness check for your baby after being sent home?"
label(data$wellnesstimesthink_swf)="How many times do you think you need to attend a routine wellness check for you or your baby after being sent home?"
label(data$wellnesssource_swf___1)="How did you know to do this? (choice=Information from a healthcare worker at discharge)"
label(data$wellnesssource_swf___2)="How did you know to do this? (choice=Previous pregnancy or experience)"
label(data$wellnesssource_swf___3)="How did you know to do this? (choice=Guidance from a family member or friend)"
label(data$wellnesssource_swf___4)="How did you know to do this? (choice=Date listed on the discharge form)"
label(data$wellnesssource_swf___98)="How did you know to do this? (choice=Other)"
label(data$wellnesssourceother_swf)="Specify any other ways you knew to do this."
label(data$wellnesstimes_swf)="How many times were you told to seek routine wellness checks?"
label(data$wellnesswhen_swf___1)="What did the information you received tell you about when you should attend a routine wellness check (time periods relate to days/hours after birth)? (choice=1 day (24 hours))"
label(data$wellnesswhen_swf___2)="What did the information you received tell you about when you should attend a routine wellness check (time periods relate to days/hours after birth)? (choice=3 days (48-72 hours))"
label(data$wellnesswhen_swf___3)="What did the information you received tell you about when you should attend a routine wellness check (time periods relate to days/hours after birth)? (choice=6 days)"
label(data$wellnesswhen_swf___4)="What did the information you received tell you about when you should attend a routine wellness check (time periods relate to days/hours after birth)? (choice=7-14 days)"
label(data$wellnesswhen_swf___5)="What did the information you received tell you about when you should attend a routine wellness check (time periods relate to days/hours after birth)? (choice=6 weeks)"
label(data$wellnesswhen_swf___98)="What did the information you received tell you about when you should attend a routine wellness check (time periods relate to days/hours after birth)? (choice=Other)"
label(data$wellnesswhen_swf___99)="What did the information you received tell you about when you should attend a routine wellness check (time periods relate to days/hours after birth)? (choice=None)"
label(data$wellnesswhenother_swf)="Specify the other times you were told to do this."
label(data$wellnessreasonnot_swf___1)="Is there any reason you may not have attended? (choice=I did not think it was necessary)"
label(data$wellnessreasonnot_swf___2)="Is there any reason you may not have attended? (choice=I did not know)"
label(data$wellnessreasonnot_swf___3)="Is there any reason you may not have attended? (choice=Cost of transport)"
label(data$wellnessreasonnot_swf___4)="Is there any reason you may not have attended? (choice=Distance to nearest facility)"
label(data$wellnessreasonnot_swf___5)="Is there any reason you may not have attended? (choice=Bad weather or roads)"
label(data$wellnessreasonnot_swf___6)="Is there any reason you may not have attended? (choice=No one to look after other children (child care))"
label(data$wellnessreasonnot_swf___7)="Is there any reason you may not have attended? (choice=Previous bad experience with the health workers)"
label(data$wellnessreasonnot_swf___8)="Is there any reason you may not have attended? (choice=Fears or insecurities)"
label(data$wellnessreasonnot_swf___9)="Is there any reason you may not have attended? (choice=Decision is made by my husband/partner or someone else in the family)"
label(data$wellnessreasonnot_swf___10)="Is there any reason you may not have attended? (choice=Cost of care at the facility)"
label(data$wellnessreasonnot_swf___98)="Is there any reason you may not have attended? (choice=Other)"
label(data$wellnessreasonnot_swf___99)="Is there any reason you may not have attended? (choice=None)"
label(data$wellnessreasonnotother_swf)="Specify other reason you may not have attended."
label(data$wellnessdiscuss_swf___1)="Were any of the following discussed with you by a healthcare worker regarding seeking care for you and your baby? (choice=Seek care for illness or complications)"
label(data$wellnessdiscuss_swf___2)="Were any of the following discussed with you by a healthcare worker regarding seeking care for you and your baby? (choice=Attend family planning at 6 weeks)"
label(data$wellnessdiscuss_swf___3)="Were any of the following discussed with you by a healthcare worker regarding seeking care for you and your baby? (choice=Attend vaccination visit at 6 weeks)"
label(data$wellnessdiscuss_swf___4)="Were any of the following discussed with you by a healthcare worker regarding seeking care for you and your baby? (choice=Attend routine wellness checks throughout the 6 week postnatal period)"
label(data$wellnessdiscuss_swf___5)="Were any of the following discussed with you by a healthcare worker regarding seeking care for you and your baby? (choice=Healthcare workers would complete home visits for routine wellness checks)"
label(data$wellnessdiscuss_swf___99)="Were any of the following discussed with you by a healthcare worker regarding seeking care for you and your baby? (choice=None)"
label(data$six_week_follow_up_wellness_checks_complete)="Complete?"
label(data$confirm_studyid_va_m)="Confirm Study ID"
label(data$date_va_m)="Date of verbal autopsy: "
label(data$id10007_m)="What is the sex of the VA respondent?"
label(data$id10008_m)="What is your/the respondents relationship to the deceased?"
label(data$id10009_m)="Did you/the respondent live with the deceased in the period leading to her death?"
label(data$id10013_m)="Did the respondent give consent?"
label(data$ageatdeath)="Mothers age at death (in years):"
label(data$id10487_m)="In the two weeks before death, did she live with or visit someone who had any COVID-19 symptoms or a positive COVID-19 test?"
label(data$id10488_m)="In the two weeks before death, did she travel to an area where COVID-19 is known to be present?"
label(data$momadmit_va)="Was the mother admitted for one or more nights at a facility for any reason after being home post-delivery?"
label(data$nummomadmit_va)="How many times was the mother admitted in the first 6 weeks after delivery?"
label(data$momadmitdate_va)="Date of first admission"
label(data$momnightsadm_va)="For how many nights was she admitted?"
label(data$momadmitpathway_va___1)="What was the care-seeking pathway to the first admission? (choice=Routine PNC visit identified problem)"
label(data$momadmitpathway_va___2)="What was the care-seeking pathway to the first admission? (choice=Self referral)"
label(data$momadmitpathway_va___3)="What was the care-seeking pathway to the first admission? (choice=Both self-referral and PNC visit resulted in care seeking)"
label(data$momadmitpathway_va___98)="What was the care-seeking pathway to the first admission? (choice=Other)"
label(data$momadmitpathwayother_va)="Other pathway:"
label(data$momadmitsymp_va___1)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Heavy vaginal bleeding)"
label(data$momadmitsymp_va___2)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Bleeding from nose/ eyes/ ears)"
label(data$momadmitsymp_va___3)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Petechiae (small red dots and bruises across the skin))"
label(data$momadmitsymp_va___4)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Abnormal tiredness)"
label(data$momadmitsymp_va___5)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Convulsions)"
label(data$momadmitsymp_va___6)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Shortness of breath)"
label(data$momadmitsymp_va___7)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Changes in vision)"
label(data$momadmitsymp_va___8)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Severe headache < 24 hrs)"
label(data$momadmitsymp_va___9)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Severe headache >24 hrs)"
label(data$momadmitsymp_va___10)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Abdominal pain)"
label(data$momadmitsymp_va___11)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Abdominal tenderness when touched)"
label(data$momadmitsymp_va___12)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Foul smelling vaginal discharge)"
label(data$momadmitsymp_va___13)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Fever/ body hotness < 7days)"
label(data$momadmitsymp_va___14)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Fever/ body hotness >7days)"
label(data$momadmitsymp_va___15)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Diarrhea < 14 days)"
label(data$momadmitsymp_va___16)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Diarrhea >14 days)"
label(data$momadmitsymp_va___17)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Vomiting everything)"
label(data$momadmitsymp_va___18)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Cough < 14 days)"
label(data$momadmitsymp_va___19)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Cough >14 days)"
label(data$momadmitsymp_va___20)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Yellow eyes)"
label(data$momadmitsymp_va___21)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Loss of consciousness)"
label(data$momadmitsymp_va___98)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=Other (specify))"
label(data$momadmitsymp_va___99)="What were the symptoms she experienced during this illness?  (check all that apply) (choice=None)"
label(data$momadmitsympother_va)="Other symptom"
label(data$momadmitcond_va___1)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Pre-eclampsia)"
label(data$momadmitcond_va___2)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Eclampsia)"
label(data$momadmitcond_va___3)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Diabetes)"
label(data$momadmitcond_va___4)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Fistula)"
label(data$momadmitcond_va___5)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=HIV related illness)"
label(data$momadmitcond_va___6)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Malaria)"
label(data$momadmitcond_va___7)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Retained placenta)"
label(data$momadmitcond_va___8)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Heavy bleeding)"
label(data$momadmitcond_va___9)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Anemia)"
label(data$momadmitcond_va___10)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Surgical site infection)"
label(data$momadmitcond_va___11)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Puerperal sepsis)"
label(data$momadmitcond_va___12)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Other infection)"
label(data$momadmitcond_va___13)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Diarrhea)"
label(data$momadmitcond_va___14)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Vomiting)"
label(data$momadmitcond_va___15)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=PPD/psychosis)"
label(data$momadmitcond_va___16)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Needed another surgery)"
label(data$momadmitcond_va___17)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Needed blood transfusion)"
label(data$momadmitcond_va___18)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Needed oxygen)"
label(data$momadmitcond_va___98)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=Other (specify))"
label(data$momadmitcond_va___99)="Was she told she had any of the following conditions during this visit? (check all that apply) (choice=None)"
label(data$momadmitcondother_va)="Other condition:"
label(data$momseek_va)="Did she seek care at a facility at any time after being home post-delivery that DID NOT result in an admission?"
label(data$momseekdate_va)="Date of first care seeking visit:"
label(data$momseekpathway_va___1)="What was the care-seeking pathway for this visit? (choice=Routine PNC visit identified problem)"
label(data$momseekpathway_va___2)="What was the care-seeking pathway for this visit? (choice=Self referral)"
label(data$momseekpathway_va___3)="What was the care-seeking pathway for this visit? (choice=Both self-referral and PNC visit resulted in care seeking)"
label(data$momseekpathway_va___98)="What was the care-seeking pathway for this visit? (choice=Other)"
label(data$momseekpathwayother_va)="Other pathway:"
label(data$momseeksymp_va___1)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Heavy vaginal bleeding)"
label(data$momseeksymp_va___2)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Bleeding from nose/ eyes/ ears)"
label(data$momseeksymp_va___3)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Petechiae (small red dots and bruises across the skin))"
label(data$momseeksymp_va___4)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Abnormal tiredness)"
label(data$momseeksymp_va___5)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Convulsions)"
label(data$momseeksymp_va___6)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Shortness of breath)"
label(data$momseeksymp_va___7)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Changes in vision)"
label(data$momseeksymp_va___8)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Severe headache < 24 hrs)"
label(data$momseeksymp_va___9)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Severe headache >24 hrs)"
label(data$momseeksymp_va___10)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Abdominal pain)"
label(data$momseeksymp_va___11)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Abdominal tenderness when touched)"
label(data$momseeksymp_va___12)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Foul smelling vaginal discharge)"
label(data$momseeksymp_va___13)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Fever/ body hotness < 7days)"
label(data$momseeksymp_va___14)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Fever/ body hotness >7days)"
label(data$momseeksymp_va___15)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Diarrhea < 14 days)"
label(data$momseeksymp_va___16)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Diarrhea >14 days)"
label(data$momseeksymp_va___17)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Vomiting everything)"
label(data$momseeksymp_va___18)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Cough < 14 days)"
label(data$momseeksymp_va___19)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Cough >14 days)"
label(data$momseeksymp_va___20)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Yellow eyes)"
label(data$momseeksymp_va___21)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Loss of consciousness)"
label(data$momseeksymp_va___98)="What were the symptoms she experienced during this illness (check all that apply)? (choice=Other (specify))"
label(data$momseeksymp_va___99)="What were the symptoms she experienced during this illness (check all that apply)? (choice=None)"
label(data$momseeksympother_va)="Other symptom:"
label(data$momseekcond_va___1)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Pre-eclampsia)"
label(data$momseekcond_va___2)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Eclampsia)"
label(data$momseekcond_va___3)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Diabetes)"
label(data$momseekcond_va___4)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Fistula)"
label(data$momseekcond_va___5)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=HIV)"
label(data$momseekcond_va___6)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Malaria)"
label(data$momseekcond_va___7)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Retained placenta)"
label(data$momseekcond_va___8)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Heavy bleeding)"
label(data$momseekcond_va___9)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Anemia)"
label(data$momseekcond_va___10)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Surgical site infection)"
label(data$momseekcond_va___11)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Puerperal sepsis)"
label(data$momseekcond_va___12)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Other infection)"
label(data$momseekcond_va___13)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Diarrhea)"
label(data$momseekcond_va___14)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Vomiting)"
label(data$momseekcond_va___15)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=PPD/psychosis)"
label(data$momseekcond_va___16)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Needed another surgery)"
label(data$momseekcond_va___17)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Needed blood transfusion)"
label(data$momseekcond_va___18)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Needed oxygen)"
label(data$momseekcond_va___98)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=Other (specify))"
label(data$momseekcond_va___99)="Was she told she had any of the following conditions during this visit (check all that apply)? (choice=None)"
label(data$momseekcondother_va)="Other condition:"
label(data$id10077_m)="Did she suffer from any injury or accident that led to her death?"
label(data$id10079_m)="Was it a road traffic accident?"
label(data$id10080_m)="What was her role in the road traffic accident?"
label(data$id10081_m)="What was the counterpart that was hit during the road traffic accident?"
label(data$id10082_m)="Was she injured in a non-road transport accident?"
label(data$id10083_m)="Was she injured in a fall?"
label(data$id10084_m)="Was there any poisoning?"
label(data$id10085_m)="Did she die of drowning?"
label(data$id10086_m)="Was she injured by a bite or sting by venomous animal?"
label(data$id10087_m)="Was she injured by an animal or insect (non-venomous)?"
label(data$id10088_m)="What was the animal/insect?"
label(data$id10089_m)="Was she injured by burns/fire?"
label(data$id10090_m)="Was she subject to violence (homicide, abuse)?"
label(data$id10091_m)="Was she injured by a firearm?"
label(data$id10092_m)="Was she stabbed, cut or pierced?"
label(data$id10093_m)="Was she strangled?"
label(data$id10094_m)="Was she injured by a blunt force?"
label(data$id10095_m)="Was she injured by a force of nature?"
label(data$id10096_m)="Was it electrocution?"
label(data$id10097_m)="Did she encounter any other injury?"
label(data$id10098_m)="Was the injury accidental?"
label(data$id10099_m)="Was the injury self-inflicted?"
label(data$id10100_m)="Was the injury intentionally inflicted by someone else?"
label(data$id10120_m)="For how long was she ill before death? (in days)"
label(data$id10123_m)="Did she die suddenly?"
label(data$id10125_m)="Was there any diagnosis by a health professional of tuberculosis?"
label(data$id10126_m)="Was an HIV test ever positive?"
label(data$id10127_m)="Was there any diagnosis by a health professional of AIDS?"
label(data$id10128_m)="Did she have a recent positive test by a health professional for malaria?"
label(data$id10129_m)="Did she have a recent negative test by a health professional for malaria?"
label(data$id10130_m)="Was there any diagnosis by a health professional of dengue fever?"
label(data$id10131_m)="Was there any diagnosis by a health professional of measles?"
label(data$id10132_m)="Was there any diagnosis by a health professional of high blood pressure?"
label(data$id10133_m)="Was there any diagnosis by a health professional of heart disease?"
label(data$id10134_m)="Was there any diagnosis by a health professional of diabetes?"
label(data$id10135_m)="Was there any diagnosis by a health professional of asthma?"
label(data$id10136_m)="Was there any diagnosis by a health professional of epilepsy?"
label(data$id10137_m)="Was there any diagnosis by a health professional of cancer?"
label(data$id10138_m)="Was there any diagnosis by a health professional of Chronic Obstructive Pulmonary Disease (COPD)?"
label(data$id10139_m)="Was there any diagnosis by a health professional of dementia?"
label(data$id10140_m)="Was there any diagnosis by a health professional of depression?"
label(data$id10141_m)="Was there any diagnosis by a health professional of stroke?"
label(data$id10142_m)="Was there any diagnosis by a health professional of sickle cell disease?"
label(data$id10143_m)="Was there any diagnosis by a health professional of kidney disease?"
label(data$id10144_m)="Was there any diagnosis by a health professional of liver disease?"
label(data$id10482_m)="Was there any diagnosis by a health professional of COVID-19?"
label(data$id10483_m)="Did she have a recent test by a health professional for COVID-19?"
label(data$id10484_m)="What was the result?"
label(data$id10147_m)="Did she have a fever?"
label(data$id10148_m)="How long did the fever last? (in days)"
label(data$id10149_m)="Did the fever continue until death?"
label(data$id10150_m)="How severe was the fever?"
label(data$id10151_m)="What was the pattern of the fever?"
label(data$id10152_m)="Did she have night sweats?"
label(data$id10153_m)="Did she have a cough?"
label(data$id10154_m)="For how long did she have a cough?"
label(data$id10155_m)="Was the cough productive, with sputum?"
label(data$id10156_m)="Was the cough very severe?"
label(data$id10157_m)="Did she cough up blood?"
label(data$id10159_m)="Did she have any difficulty breathing?"
label(data$id10161_m)="For how many days did the difficulty breathing last?"
label(data$id10165_m)="Was the difficulty continuous or on and off?"
label(data$id10166_m)="During the illness that led to death, did she have fast breathing?"
label(data$id10167_m)="For how many days did the fast breathing last?"
label(data$id10168_m)="Did she have breathlessness?"
label(data$id10169_m)="For how many days did she have breathlessness?"
label(data$id10170_m)="Was she unable to carry out daily routines due to breathlessness?"
label(data$id10171_m)="Was she breathless while lying flat?"
label(data$id10173_m)="During the illness that led to death did she have wheezing?"
label(data$id10174_m)="Did she have chest pain?"
label(data$id10175_m)="Was the chest pain severe?"
label(data$id10179_m)="How long did the chest pain last? "
label(data$id10181_m)="Did she have more frequent loose or liquid stools than usual?"
label(data$id10182_m)="For how many days did she have frequent loose or liquid stools?"
label(data$id10186_m)="At any time during the final illness was there blood in the stools?"
label(data$id10187_m)="Was there blood in the stool up until death?"
label(data$id10188_m)="Did she vomit?"
label(data$id10189_m)="To clarify: Did she vomit in the week preceding the death?"
label(data$id10190_m)="How long before death did she vomit? (in days)"
label(data$id10192_m)="Was the vomit black?"
label(data$id10193_m)="Did she have any belly (abdominal) problem?"
label(data$id10194_m)="Did she have belly (abdominal) pain?"
label(data$id10195_m)="Was the belly (abdominal) pain severe?"
label(data$id10197_m)="For how long did she have belly (abdominal) pain? (in days)"
label(data$id10199_m)="Was the pain in the upper or lower belly (abdomen)?"
label(data$id10200_m)="Did she have a more than usually protruding belly (abdomen)?"
label(data$id10201_m)="For how long before death did she have a more than usually protruding belly (abdomen)? (in days)"
label(data$id10203_m)="How rapidly did she develop the protruding belly (abdomen)?"
label(data$id10204_m)="Did she have any mass in the belly (abdomen)?"
label(data$id10205_m)="For how long did she have a mass in the belly (abdomen)? (in days)"
label(data$id10207_m)="Did she have a severe headache?"
label(data$id10208_m)="Did she have a stiff neck during illness that led to death?"
label(data$id10209_m)="How long before death did she have a stiff neck? (in days)"
label(data$id10210_m)="Did she have a painful neck during the illness that led to death?"
label(data$id10211_m)="For how many days before death did she have a painful neck?"
label(data$id10212_m)="Did she have mental confusion?"
label(data$id10213_m)="How long did she have mental confusion? (in days)"
label(data$id10214_m)="Was she unconscious during the illness thatledto death?"
label(data$id10215_m)="Was she unconscious for more than 24 hours before death?"
label(data$id10217_m)="Did the unconsciousness start suddenly, quickly (at least within a single day)?"
label(data$id10218_m)="Did the unconsciousness continue until death?"
label(data$id10219_m)="Did she have convulsions?"
label(data$id10221_m)="For how many minutes did the convulsions last?"
label(data$id10222_m)="Did she become unconscious immediately after the convulsion?"
label(data$id10223_m)="Did she have any urine problems?"
label(data$id10225_m)="Did she go to urinate more often than usual?"
label(data$id10226_m)="During the final illness did she ever pass blood in the urine?"
label(data$id10224_m)="Did she stop urinating?"
label(data$id10227_m)="Did she have sores or ulcers anywhere on the body?"
label(data$id10228_m)="Did she have sores?"
label(data$id10229_m)="Did the sores have clear fluid or pus?"
label(data$id10230_m)="Did she have an ulcer (pit) on the foot?"
label(data$id10231_m)="Did the ulcer on the foot ooze pus?"
label(data$id10232_m)="For how many days did the ulcer on the foot ooze pus?"
label(data$id10233_m)="During the illness that led to death, did she have any skin rash?"
label(data$id10234_m)="For how many days did she have the skin rash?"
label(data$id10235_m___1)="Where was the rash? (choice=Face)"
label(data$id10235_m___2)="Where was the rash? (choice=Trunk or abdomen)"
label(data$id10235_m___3)="Where was the rash? (choice=Extremities)"
label(data$id10235_m___4)="Where was the rash? (choice=Everywhere)"
label(data$id10236_m)="Did she have measles rash (use local term)?"
label(data$id10237_m)="Did she ever have shingles or herpes zoster?"
label(data$id10238_m)="During the illness that led to death, did her skin flake off in patches?"
label(data$id10241_m)="During the illness that led to death, did she bleed from anywhere?"
label(data$id10242_m)="Did she bleed from the nose, mouth or anus?"
label(data$id10243_m)="Did she have noticeable weight loss?"
label(data$id10244_m)="Was she severely thin or wasted?"
label(data$id10245_m)="During the illness that led to death, did she have a whitish rash inside the mouth or on the tongue?"
label(data$id10246_m)="Did she have stiffness of the whole body or was unable to open the mouth?"
label(data$id10247_m)="Did she have puffiness of the face?"
label(data$id10248_m)="For how many days did she have puffiness of the face?"
label(data$id10249_m)="During the illness that led to death, did she have swollen legs or feet?"
label(data$id10250_m)="How many days did the swelling last?"
label(data$id10251_m)="Did she have both feet swollen?"
label(data$id10252_m)="Did she have general puffiness all over her body?"
label(data$id10253_m)="Did she have any lumps?"
label(data$id10254_m)="Did she have any lumps or lesions in the mouth?"
label(data$id10255_m)="Did she have any lumps on the neck?"
label(data$id10256_m)="Did she have any lumps on the armpit?"
label(data$id10257_m)="Did she have any lumps on the groin?"
label(data$id10258_m)="Was she in any way paralysed?"
label(data$id10259_m)="Did she have paralysis of only one side of the body?"
label(data$id10260_m___1)="Which were the limbs or body parts paralysed? (choice=Right side)"
label(data$id10260_m___2)="Which were the limbs or body parts paralysed? (choice=Left side)"
label(data$id10260_m___3)="Which were the limbs or body parts paralysed? (choice=Lower part of body)"
label(data$id10260_m___4)="Which were the limbs or body parts paralysed? (choice=Upper part of body)"
label(data$id10260_m___5)="Which were the limbs or body parts paralysed? (choice=One leg only)"
label(data$id10260_m___6)="Which were the limbs or body parts paralysed? (choice=One arm only)"
label(data$id10260_m___7)="Which were the limbs or body parts paralysed? (choice=Whole body)"
label(data$id10260_m___8)="Which were the limbs or body parts paralysed? (choice=Other)"
label(data$id10261_m)="Did she have difficulty swallowing?"
label(data$id10262_m)="For how many days before death did she have difficulty swallowing?"
label(data$id10263_m)="Was the difficulty with swallowing with solids, liquids, or both?"
label(data$id10264_m)="Did she have pain upon swallowing?"
label(data$id10265_m)="Did she have yellow discoloration of the eyes?"
label(data$id10266_m)="For how many days did she have the yellow discoloration?"
label(data$id10267_m)="Did her hair change in color to a reddish or yellowish color?"
label(data$id10268_m)="Did she look pale (thinning/lack of blood) or have pale palms, eyes or nail beds?"
label(data$id10270_m)="Did she drink a lot more water than usual?"
label(data$id10486_m)="Did she suffer from extreme fatigue? "
label(data$id10485_m)="Did she experience a new loss, change or decreased sense of smell or taste?  "
label(data$id10294_m)="Did she have any swelling or lump in the breast?"
label(data$id10295_m)="Did she have any ulcers (pits) in the breast?"
label(data$id10296_m)="Did she ever have a period or menstruate?"
label(data$id10297_m)="When she had her period, did she have vaginal bleeding in between menstrual periods?"
label(data$id10298_m)="Was the bleeding excessive?"
label(data$id10301_m)="Was there excessive vaginal bleeding in the week prior to death?"
label(data$id10299_m)="Did her menstrual period stop naturally because of menopause or removal of uterus?"
label(data$id10302_m)="At the time of death was her period overdue?"
label(data$id10303_m)="For how many weeks had her period been overdue?"
label(data$id10300_m)="Did she have vaginal bleeding after cessation of menstruation?"
label(data$id10304_m)="Did she have a sharp pain in her belly (abdomen) shortly before death?"
label(data$id10305_m)="Was she pregnant or in labour at the time of death?"
label(data$id10306_m)="Did she die within 6 weeks of delivery, abortion or miscarriage?"
label(data$id10307_m)="Did this woman die more than 6 weeks after being pregnant or delivering a baby?"
label(data$id10309_m)="For how many months was she pregnant?"
label(data$id10312_m)="Did she die during labour or delivery?"
label(data$id10313_m)="Did she die after delivering a baby?"
label(data$id10314_m)="Did she die within 24 hours after delivery?"
label(data$id10315_m)="Did she die within 6 weeks of childbirth?"
label(data$id10316_m)="Did she give birth to a live baby (within 6 weeks of her death)?"
label(data$id10317_m)="Did she die during or after a multiple pregnancy?"
label(data$id10318_m)="Was she breastfeeding the child in the days before death?"
label(data$id10319_m)="How many births, including stillbirths, did she/the mother have before this baby?"
label(data$id10320_m)="Had she had any previous Caesarean section?"
label(data$id10321_m)="During pregnancy, did she suffer from high blood pressure?"
label(data$id10322_m)="Did she have foul smelling vaginal discharge during pregnancy or after delivery?"
label(data$id10323_m)="During the last 3 months of pregnancy, did she suffer from convulsions?"
label(data$id10324_m)="During the last 3 months of pregnancy did she suffer from blurred vision?"
label(data$id10325_m)="Did bleeding occur while she was pregnant?"
label(data$id10326_m)="Was there vaginal bleeding during the first 6 months of pregnancy?"
label(data$id10327_m)="Was there vaginal bleeding during the last 3 months of pregnancy but before labour started?"
label(data$id10328_m)="Did she have excessive bleeding during labour or delivery?"
label(data$id10329_m)="Did she have excessive bleeding after delivery or abortion?"
label(data$id10330_m)="Was the placenta completely delivered?"
label(data$id10331_m)="Did she deliver or try to deliver an abnormally positioned baby?"
label(data$id10332_m)="For how many hours was she in labour?"
label(data$id10333_m)="Did she attempt to terminate the pregnancy?"
label(data$id10334_m)="Did she recently have a pregnancy that ended in an abortion (spontaneous or induced)?"
label(data$id10335_m)="Did she die during an abortion?"
label(data$id10336_m)="Did she die within 6 weeks of having an abortion?"
label(data$id10337_m)="Where did she give birth / complete the miscarriage / perform the abortion?"
label(data$id10338_m)="Did she receive professional assistance during the delivery?"
label(data$id10339_m)="Who delivered the baby / completed the miscarriage?"
label(data$id10342_m)="Was the delivery normal vaginal, without forceps or vacuum?"
label(data$id10343_m)="Was the delivery vaginal, with forceps or vacuum?"
label(data$id10344_m)="Was the delivery a Caesarean section?"
label(data$id10347_m)="Was the baby born more than one month early?"
label(data$id10340_m)="Did she have an operation to remove her uterus shortly before death?"
label(data$id10411_m)="Did she drink alcohol?"
label(data$id10412_m)="Did she use tobacco?"
label(data$id10413_m)="Did she smoke tobacco (cigarette, cigar, pipe, etc.)?"
label(data$id10414_m___1)="What kind of tobacco did she use? (choice=Cigarettes)"
label(data$id10414_m___2)="What kind of tobacco did she use? (choice=Pipe)"
label(data$id10414_m___3)="What kind of tobacco did she use? (choice=Chewing tobacco)"
label(data$id10414_m___4)="What kind of tobacco did she use? (choice=Local form of tobacco)"
label(data$id10414_m___5)="What kind of tobacco did she use? (choice=Other)"
label(data$id10414_m___99)="What kind of tobacco did she use? (choice=Doesnt know)"
label(data$id10414_m___88)="What kind of tobacco did she use? (choice=Refused to answer)"
label(data$id10415_m)="How many cigarettes did she smoke daily?"
label(data$id10416_m)="How many times didshe use tobacco products each day?"
label(data$id10418_m)="Did she receive any treatment for the illness that led to death?"
label(data$id10419_m)="Did she receive oral rehydration salts?"
label(data$id10420_m)="Did she receive (or need) intravenous fluids (drip) treatment?"
label(data$id10421_m)="Did she receive (or need) a blood transfusion?"
label(data$id10422_m)="Did she receive (or need) treatment/food through a tube passed through the nose?"
label(data$id10423_m)="Did she receive (or need) injectable antibiotics?"
label(data$id10424_m)="Did she receive (or need) antiretroviral therapy (ART)?"
label(data$id10425_m)="Did she have (or need) an operation for the illness?"
label(data$id10426_m)="Did she have the operation within 1 month before death?"
label(data$id10427_m)="Was she discharged from hospital very ill?"
label(data$id10432_m)="Was care sought outside the home while she had this illness (that led to death)?"
label(data$id10433_m___1)="Where or from whom did they seek care? (choice=Traditional healer)"
label(data$id10433_m___2)="Where or from whom did they seek care? (choice=Homeopath)"
label(data$id10433_m___3)="Where or from whom did they seek care? (choice=Religious leader)"
label(data$id10433_m___4)="Where or from whom did they seek care? (choice=Government hospital)"
label(data$id10433_m___5)="Where or from whom did they seek care? (choice=Government health center or clinic)"
label(data$id10433_m___6)="Where or from whom did they seek care? (choice=Private hospital)"
label(data$id10433_m___7)="Where or from whom did they seek care? (choice=Community-based practitioner associated with health system)"
label(data$id10433_m___8)="Where or from whom did they seek care? (choice=Trained birth attendant)"
label(data$id10433_m___9)="Where or from whom did they seek care? (choice=Private physician)"
label(data$id10433_m___10)="Where or from whom did they seek care? (choice=Relative, friend (outside household))"
label(data$id10433_m___11)="Where or from whom did they seek care? (choice=Pharmacy)"
label(data$id10433_m___99)="Where or from whom did they seek care? (choice=Doesnt know)"
label(data$id10433_m___88)="Where or from whom did they seek care? (choice=Refused to answer)"
label(data$id10435_m)="Did a health care worker tell you the cause of death?"
label(data$id10436_m)="What did the health care worker say?"
label(data$id10437_m)="Do you have any health records that belonged to the deceased?"
label(data$id10438_m)="Can I see the health records?"
label(data$id10439_check_m)="[Is the date of the most recent (last) visit available?]"
label(data$id10439_m)="[Record the date of the most recent (last) visit]"
label(data$id10440_check_m)="[Is the date of the second most recent visit available?]"
label(data$id10440_m)="[Record the date of the second most recent visit]"
label(data$id10441_check_m)="[Is the date of the last note on the health records available?]"
label(data$id10441_m)="[Record the date of the last note on the health records]"
label(data$id10442_m)="[Record the weight (in kilograms) written at the most recent (last) visit]"
label(data$id10443_m)="[Record the weight (in kilograms) written at the second most recent visit]"
label(data$id10444_m)="[Transcribe the last note on the health records]"
label(data$id10450_m)="In the final days before death, did she travel to a hospital or health facility?"
label(data$id10451_m)="Did she use motorised transport to get to the hospital or health facility?"
label(data$id10452_m)="Were there any problems during admission to the hospital or health facility?"
label(data$id10453_m)="Were there any problems with the way she was treated (medical treatment, procedures, interpersonal attitudes, respect, dignity) in the hospital or health facility?"
label(data$id10454_m)="Were there any problems getting medications or diagnostic tests in the hospital or health facility?"
label(data$id10455_m)="Does it take more than 2 hours to get to the nearest hospital or health facility from the deceaseds household?"
label(data$id10456_m)="In the final days before death, were there any doubts about whether medical care was needed?"
label(data$id10457_m)="In the final days before death, was traditional medicine used?"
label(data$id10458_m)="In the final days before death, did anyone use a telephone or cell phone to call for help?"
label(data$id10459_m)="Over the course of illness, did the total costs of care and treatment prohibit other household payments?"
label(data$id10476_m)="Thank you for your information. Now can you please tell me in your own words about the events that led to the death?"
label(data$id10477_m___1)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Chronic kidney disease)"
label(data$id10477_m___2)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Dialysis)"
label(data$id10477_m___3)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Fever)"
label(data$id10477_m___4)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Heart attack)"
label(data$id10477_m___5)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Heart problem)"
label(data$id10477_m___6)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Jaundice)"
label(data$id10477_m___7)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Liver failure)"
label(data$id10477_m___8)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Malaria)"
label(data$id10477_m___9)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Pneumonia)"
label(data$id10477_m___10)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Renal (kidney) failure)"
label(data$id10477_m___11)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Suicide)"
label(data$id10477_m___12)="[Select any of the following words that were mentioned as present in the narrative.] (choice=None of the above words were mentioned)"
label(data$id10477_m___99)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Dont know)"
label(data$verbal_autopsy_mother_complete)="Complete?"
label(data$confirm_studyid_va_n)="Confirm Study ID"
label(data$date_va_n)="Date of verbal autopsy: "
label(data$id10007)="What is the sex of VA respondent?"
label(data$id10008)="What is your/the respondents relationship to the deceased?"
label(data$id10009)="Did you/the respondent live with the deceased in the period leading to her/his death?"
label(data$id10013)="Did the respondent give consent?"
label(data$ageindays)="Babys age at death (in days):"
label(data$id10487)="In the two weeks before death, did the baby live with or visit someone who had any COVID-19 symptoms or a positive COVID-19 test?"
label(data$id10488)="In the two weeks before death, did the baby travel to an area where COVID-19 is known to be present?"
label(data$babeadmit_va)="Was the newborn admitted for one or more nights at a facility for any reason after being home post-delivery?"
label(data$numbabeadmit_va)="How many times was the baby admitted in the first 6 weeks after birth"
label(data$babeadmitage_va)="How many days old was the baby during the first admission?"
label(data$babenightsadm_va)="For how many nights were they admitted?"
label(data$babeadmitpathway_va___1)="What was the care-seeking pathway to this admission? (choice=Routine well-baby visit identified problem)"
label(data$babeadmitpathway_va___2)="What was the care-seeking pathway to this admission? (choice=Self referral due to parental concern)"
label(data$babeadmitpathway_va___3)="What was the care-seeking pathway to this admission? (choice=Both self-referral and well-baby visit resulted in care seeking)"
label(data$babeadmitpathway_va___98)="What was the care-seeking pathway to this admission? (choice=Other)"
label(data$babeadmitpathwayother_va)="Other pathway:"
label(data$babeadmitsymp_va___1)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Skin pustules)"
label(data$babeadmitsymp_va___2)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Respiratory distress)"
label(data$babeadmitsymp_va___3)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Watery stool)"
label(data$babeadmitsymp_va___4)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Fever/hotness of body)"
label(data$babeadmitsymp_va___5)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Bilious vomit)"
label(data$babeadmitsymp_va___6)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=projectile vomit)"
label(data$babeadmitsymp_va___7)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Not waking up to feed or abnormally sleepy)"
label(data$babeadmitsymp_va___8)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Swelling of both feet)"
label(data$babeadmitsymp_va___9)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Changes in urine color)"
label(data$babeadmitsymp_va___10)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Making less urine than usual)"
label(data$babeadmitsymp_va___11)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Blood in stool)"
label(data$babeadmitsymp_va___12)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Seizure/convulsions)"
label(data$babeadmitsymp_va___13)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Coma)"
label(data$babeadmitsymp_va___14)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Yellow soles)"
label(data$babeadmitsymp_va___15)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Difficulty wtih breastfeeding)"
label(data$babeadmitsymp_va___16)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Cough)"
label(data$babeadmitsymp_va___17)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Umbilical cord problem (pain, discharge, etc.))"
label(data$babeadmitsymp_va___18)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Not gaining weight)"
label(data$babeadmitsymp_va___98)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=Other)"
label(data$babeadmitsymp_va___99)="What were the symptoms the child experienced during this illness (check all that apply)? (choice=None)"
label(data$babeadmitsympother_va)="Other symptom:"
label(data$transfusion_va)="Was baby transfused during any admission post-discharge"
label(data$babeadmitcond_va___1)="Were you told the child had any of the following conditions? (choice=Sepsis)"
label(data$babeadmitcond_va___2)="Were you told the child had any of the following conditions? (choice=Respiratory illness)"
label(data$babeadmitcond_va___3)="Were you told the child had any of the following conditions? (choice=Oncologic disease (cancer))"
label(data$babeadmitcond_va___4)="Were you told the child had any of the following conditions? (choice=Congenital abnormality)"
label(data$babeadmitcond_va___5)="Were you told the child had any of the following conditions? (choice=Endocrine/ metabolic disease)"
label(data$babeadmitcond_va___6)="Were you told the child had any of the following conditions? (choice=Failure to thrive)"
label(data$babeadmitcond_va___7)="Were you told the child had any of the following conditions? (choice=Trauma/ injury)"
label(data$babeadmitcond_va___8)="Were you told the child had any of the following conditions? (choice=HIV)"
label(data$babeadmitcond_va___9)="Were you told the child had any of the following conditions? (choice=Sickle Cell)"
label(data$babeadmitcond_va___10)="Were you told the child had any of the following conditions? (choice=Anemia)"
label(data$babeadmitcond_va___11)="Were you told the child had any of the following conditions? (choice=Dehydration)"
label(data$babeadmitcond_va___98)="Were you told the child had any of the following conditions? (choice=Other)"
label(data$babeadmitcond_va___99)="Were you told the child had any of the following conditions? (choice=None)"
label(data$babeadmitcondother_va)="Other condition:"
label(data$matsymp_va)="Was the mother experiencing any signifcant illness at the time when baby was admitted?"
label(data$babefeed_va)="Was the baby being exclusively breastfed at the time they died?"
label(data$babeseek_va)="Did you seek care for the newborn at a facility at any time after being home post-delivery that DID NOT result in an admission?"
label(data$babeseekage_va)="How many days old was the baby during the first such visit?"
label(data$babeseekpathway_va___1)="What was the care-seeking pathway for this visit? (choice=Routine well-baby visit identified problem)"
label(data$babeseekpathway_va___2)="What was the care-seeking pathway for this visit? (choice=Self referral due to parental concern)"
label(data$babeseekpathway_va___3)="What was the care-seeking pathway for this visit? (choice=Both self-referral and well-baby visit resulted in care seeking)"
label(data$babeseekpathway_va___98)="What was the care-seeking pathway for this visit? (choice=Other)"
label(data$babeseekpathwayother_va)="Other pathway:"
label(data$id10077)="Did the baby suffer from any injury or accident that led to their death?"
label(data$id10079)="Was it a road traffic accident?"
label(data$id10081)="What was the counterpart that was hit during the road traffic accident?"
label(data$id10082)="Was the baby injured in a non-road transport accident?"
label(data$id10083)="Was the baby injured in a fall?"
label(data$id10084)="Was there any poisoning?"
label(data$id10085)="Did the baby die of drowning?"
label(data$id10086)="Was the baby injured by a bite or sting by venomous animal?"
label(data$id10087)="Was the baby injured by an animal or insect (non-venomous)?"
label(data$id10088)="What was the animal/insect?"
label(data$id10089)="Was the baby injured by burns/fire?"
label(data$id10090)="Was the baby subject to violence (homicide, abuse)?"
label(data$id10092)="Was the baby stabbed, cut or pierced?"
label(data$id10093)="Was the baby strangled?"
label(data$id10094)="Was the baby injured by a blunt force?"
label(data$id10095)="Was the baby injured by a force of nature?"
label(data$id10096)="Was it electrocution?"
label(data$id10097)="Did the baby encounter any other injury?"
label(data$id10098)="Was the injury accidental?"
label(data$id10100)="Was the injury intentionally inflicted by someone else?"
label(data$id10408)="Before the illness that led to death, was the baby growing normally?"
label(data$id10101)="How many days old was the baby when the fatal illness started?"
label(data$id10352)="How old was the child when the fatal illness started? (in days or weeks)"
label(data$id10120)="For how long was the baby ill before death? (in days)"
label(data$id10123)="Did the baby die suddenly?"
label(data$id10125)="Was there any diagnosis by a health professional of tuberculosis?"
label(data$id10128)="Did the baby have a recent positive test by a health professional for malaria?"
label(data$id10129)="Did the baby have a recent negative test by a health professional for malaria?"
label(data$id10131)="Was there any diagnosis by a health professional of measles?"
label(data$id10133)="Was there any diagnosis by a health professional of heart disease?"
label(data$id10134)="Was there any diagnosis by a health professional of diabetes?"
label(data$id10135)="Was there any diagnosis by a health professional of asthma?"
label(data$id10136)="Was there any diagnosis by a health professional of epilepsy?"
label(data$id10137)="Was there any diagnosis by a health professional of cancer?"
label(data$id10142)="Was there any diagnosis by a health professional of sickle cell disease?"
label(data$id10143)="Was there any diagnosis by a health professional of kidney disease?"
label(data$id10144)="Was there any diagnosis by a health professional of liver disease?"
label(data$id10147)="Did the baby have a fever?"
label(data$id10147_measure)="Was the babys temperature measured using a device? "
label(data$id10147_temp)="What was the babys temperature? (in Celsius)"
label(data$id10148)="How long did the fever last? (in days)"
label(data$id10149)="Did the fever continue until death?"
label(data$id10150)="How severe was the fever?"
label(data$id10151)="What was the pattern of the fever?"
label(data$id10152)="Did the baby have night sweats?"
label(data$id10153)="Did the baby have a cough?"
label(data$id10154)="For how long did the baby have a cough? (in days)"
label(data$id10155)="Was the cough productive, with sputum?"
label(data$id10156)="Was the cough very severe?"
label(data$id10157)="Did the baby cough up blood?"
label(data$id10158)="Did the baby make a whooping sound when coughing?"
label(data$id10159)="Did the baby have any difficulty breathing?"
label(data$id10161)="For how many days did the difficulty breathing last?"
label(data$id10165)="Was the difficulty continuous or on and off?"
label(data$id10166)="During the illness that led to death, did the baby have fast breathing?"
label(data$id10167)="For how many days did the fast breathing last? (in days)"
label(data$id10168)="Did the baby have breathlessness?"
label(data$id10169)="For how many days did the baby have breathlessness?"
label(data$id10172)="Did you see the lower chest wall/ribs being pulled in as the child breathed in?"
label(data$id10173___1)="During the illness that led to death did their breathing sound like any of the following: (choice=Stridor)"
label(data$id10173___2)="During the illness that led to death did their breathing sound like any of the following: (choice=Grunting)"
label(data$id10173___3)="During the illness that led to death did their breathing sound like any of the following: (choice=Wheezing)"
label(data$id10173___4)="During the illness that led to death did their breathing sound like any of the following: (choice=No)"
label(data$id10173___99)="During the illness that led to death did their breathing sound like any of the following: (choice=Doesnt know)"
label(data$id10173___88)="During the illness that led to death did their breathing sound like any of the following: (choice=Refused to answer)"
label(data$id10181)="Did the baby have more frequent loose or liquid stools than usual?"
label(data$id10182)="How long did the baby have frequent loose or liquid stools? (in days)"
label(data$id10183)="How many stools did the baby have on the day that loose liquid stools were most frequent?"
label(data$id10184)="How many days before death did the frequent loose or liquid stools start?"
label(data$id10185)="Did the frequent loose or liquid stools continue until death?"
label(data$id10186)="At any time during the final illness was there blood in the stools?"
label(data$id10187)="Was there blood in the stool up until death?"
label(data$id10188)="Did the baby vomit?"
label(data$id10194)="To clarify: Did the baby vomit in the week preceding the death?"
label(data$id10190)="How long before death did the baby vomit? (days)"
label(data$id10188_projectile)="Did the baby projectile vomit?"
label(data$id10191)="Was there blood in the vomit?"
label(data$id10188_billious)="Was the vomit green?"
label(data$id10192)="Was the vomit black?"
label(data$id10193)="Did the baby have any belly (abdominal) problem?"
label(data$id10200)="Did the baby have a more than usually protruding belly (abdomen)?"
label(data$id10201)="For how long before death did the baby have a more than usually protruding belly (abdomen)? (in days)"
label(data$id10203)="How rapidly did the baby develop the protruding belly (abdomen)?"
label(data$id10204)="Did the baby have any mass in the belly (abdomen)?"
label(data$id10205)="For how long did the baby have a mass in the belly (abdomen)? (in days)"
label(data$id10214)="Was the baby unconscious during the illness thatledto death?"
label(data$id10215)="Was the baby unconscious for more than 24 hours before death?"
label(data$id10216)="How many hours before death did the unconsciousness start? "
label(data$id10217)="Did the unconsciousness start suddenly, quickly (at least within a single day)?"
label(data$id10218)="Did the unconsciousness continue until death?"
label(data$id10219)="Did the baby have convulsions?"
label(data$id10220)="Did the baby experience any generalized convulsions or fits during the illness that led to death?"
label(data$id10221)="For how many minutes did the convulsions last?"
label(data$id10222)="Did the baby become unconscious immediately after the convulsion?"
label(data$id10223)="Did the baby have any urine problems?"
label(data$id10225)="Did the baby urinate more often than usual?"
label(data$id10226)="During the final illness did the baby ever pass blood in the urine?"
label(data$id10224)="Did the baby stop urinating?"
label(data$id10227)="Did the baby have sores or ulcers anywhere on the body?"
label(data$id10229)="Did the sores have clear fluid or pus?"
label(data$id10230)="Did the baby have an ulcer (pit) on the foot?"
label(data$id10231)="Did the ulcer on the foot ooze pus?"
label(data$id10232)="For how many days did the ulcer on the foot ooze pus?"
label(data$id10233)="During the illness that led to death, did the baby have any skin rash?"
label(data$id10234)="For how many days did the baby have the skin rash?"
label(data$id10235___1)="Where was the rash? (choice=Face)"
label(data$id10235___2)="Where was the rash? (choice=Trunk or abdomen)"
label(data$id10235___3)="Where was the rash? (choice=Extremities)"
label(data$id10235___4)="Where was the rash? (choice=Everywhere)"
label(data$id10236)="Did the baby have measles rash (use local term)?"
label(data$id10238)="During the illness that led to death, did their skin flake off in patches?"
label(data$id10239)="During the illness that led to death, did the baby have areas of the skin that turned black?"
label(data$id10240)="During the illness that led to death, did the baby have areas of the skin with redness and swelling?"
label(data$id10241)="During the illness that led to death, did the baby bleed from anywhere?"
label(data$id10242)="Did the baby bleed from the nose, mouth or anus (bum)?"
label(data$id10243)="Did the baby have noticeable weight loss?"
label(data$id10244)="Was the baby severely thin or wasted?"
label(data$id10245)="During the illness that led to death, did the baby have a whitish rash inside the mouth or on the tongue?"
label(data$id10246)="Did the baby have stiffness of the whole body or was unable to open the mouth?"
label(data$id10247)="Did the baby have puffiness of the face?"
label(data$id10248)="For how many days did the baby have puffiness of the face?"
label(data$id10249)="During the illness that led to death, did the baby have swollen legs or feet?"
label(data$id10250)="How many days did the swelling last?"
label(data$id10251)="Did the baby have both feet swollen?"
label(data$id10252)="Did the baby have general puffiness all over their body?"
label(data$id10253)="Did the baby have any lumps?"
label(data$id10255)="Did the baby have any lumps on the neck?"
label(data$id10256)="Did the baby have any lumps on the armpit?"
label(data$id10257)="Did the baby have any lumps on the groin?"
label(data$id10258)="Was the baby in any way paralysed?"
label(data$id10259)="Did the baby have paralysis of only one side of the body?"
label(data$id10260___1)="Which were the limbs or body parts paralysed? (choice=Right side)"
label(data$id10260___2)="Which were the limbs or body parts paralysed? (choice=Left side)"
label(data$id10260___3)="Which were the limbs or body parts paralysed? (choice=Lower part of body)"
label(data$id10260___4)="Which were the limbs or body parts paralysed? (choice=Upper part of body)"
label(data$id10260___5)="Which were the limbs or body parts paralysed? (choice=One leg only)"
label(data$id10260___6)="Which were the limbs or body parts paralysed? (choice=One arm only)"
label(data$id10260___7)="Which were the limbs or body parts paralysed? (choice=Whole body)"
label(data$id10260___8)="Which were the limbs or body parts paralysed? (choice=Other)"
label(data$id10261)="Did the baby have difficulty swallowing?"
label(data$id10262)="For how many days before death did the baby have difficulty swallowing?"
label(data$id10265)="Did the baby have yellow discoloration of the eyes?"
label(data$id10266)="For how many days did the baby have the yellow discoloration?"
label(data$id10268)="Did the baby look pale (thinning/lack of blood) or have pale palms, eyes or nail beds?"
label(data$id10269)="Did the baby have sunken eyes?"
label(data$id10271)="Was the baby able to suckle or bottle-feed within the first 24 hours after birth?"
label(data$id10272)="Did the baby ever suckle in a normal way?"
label(data$id10273)="Did the baby stop suckling?"
label(data$id10274)="How many days after birth did the baby stop suckling?"
label(data$id10275)="Did the baby have convulsions starting within the first 24 hours of life?"
label(data$id10276)="Did the baby have convulsions starting more than 24 hours after birth?"
label(data$id10277)="Did the babys body become stiff, with the back arched backwards?"
label(data$id10278)="During the illness that led to death, did the baby have a bulging or raised fontanelle? "
label(data$id10279)="During the illness that led to death, did the baby have a sunken fontanelle? "
label(data$id10281)="During the illness that led to death, did the baby become unresponsive or unconscious?"
label(data$id10282)="Did the baby become unresponsive or unconscious soon after birth, within less than 24 hours?"
label(data$id10283)="Did the baby become unresponsive or unconscious more than 24 hours after birth?"
label(data$id10284)="During the illness that led to death, did the baby become cold to touch?"
label(data$id10285)="How many days old was the baby when it started feeling cold to touch?"
label(data$id10286)="During the illness that led to death, did the baby become lethargic after a period of normal activity?"
label(data$id10287)="Did the baby have redness or pus drainage from the umbilical cord stump?"
label(data$id10288)="During the illness that led to death, did the baby have skin ulcer(s) or pits?"
label(data$id10289)="During the illness that led to death, did the baby have yellow skin, palms (hand) or soles (foot)?"
label(data$id10354)="Was the child part of a multiple birth?"
label(data$id10355)="Was the child the first, second, or later in the birth order?"
label(data$id10435)="Did a health care worker tell you the cause of death?"
label(data$id10436)="What did the health care worker say?"
label(data$id10437)="Do you have any health records that belonged to the deceased?"
label(data$id10438)="Can I see the health records?"
label(data$id10439_check)="[Is the date of the most recent (last) visit available?]"
label(data$id10439)="[Record the date of the most recent (last) visit]"
label(data$id10450)="In the final days before death, did the mother travel with the baby to a hospital or health facility?"
label(data$id10451)="Did the baby use motorised transport to get to the hospital or health facility?"
label(data$id10452)="Were there any problems during admission to the hospital or health facility?"
label(data$id10453)="Were there any problems with the way the baby was treated (medical treatment, procedures, interpersonal attitudes, respect, dignity) in the hospital or health facility?"
label(data$id10454)="Were there any problems getting medications or diagnostic tests in the hospital or health facility?"
label(data$id10455)="Does it take more than 2 hours to get to the nearest hospital or health facility from the deceaseds household?"
label(data$id10456)="In the final days before death, were there any doubts about whether medical care was needed?"
label(data$id10457)="In the final days before death, was traditional medicine used?"
label(data$id10458)="In the final days before death, did anyone use a telephone or cell phone to call for help?"
label(data$id10459)="Over the course of illness, did the total costs of care and treatment prohibit other household payments?"
label(data$id10476)="Thank you for your information. Now can you please tell me in your own words about the events that led to the death?"
label(data$id10478___1)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Asphyxia)"
label(data$id10478___2)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Incubator)"
label(data$id10478___3)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Lung problem)"
label(data$id10478___4)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Pneumonia)"
label(data$id10478___5)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Preterm delivery)"
label(data$id10478___6)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Respiratory distress)"
label(data$id10478___7)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Abdomen)"
label(data$id10478___8)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Cancer)"
label(data$id10478___9)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Dehydration)"
label(data$id10478___10)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Dengue fever)"
label(data$id10478___11)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Diarrhea)"
label(data$id10478___12)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Fever)"
label(data$id10478___13)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Heart problems)"
label(data$id10478___14)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Jaundice (yellow skin or eyes))"
label(data$id10478___15)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Rash)"
label(data$id10478___16)="[Select any of the following words that were mentioned as present in the narrative.] (choice=None of the above words were mentioned)"
label(data$id10478___99)="[Select any of the following words that were mentioned as present in the narrative.] (choice=Dont know)"
label(data$verbal_autopsy_newborn_complete)="Complete?"
label(data$hi)="hi"
label(data$barrier_mat)="Describe any barriers observed (e.g., language barrier between mom and healthcare workers)"
label(data$discheduc_mat)="Did mom receive discharge education, if so, types of topics discussed?"
label(data$pnc_mat)="Was a referral made for postnatal follow-up care? If so when?"
label(data$dod_mat)="If the mother died during hospitalization what was the date of death?"
label(data$disch_ad_mat)="If the mother died during hospitalization, who was the infant discharged to?"
label(data$journey_mapping_admission_and_discharge_complete)="Complete?"
label(data$timehosphome_72)="Travel time from hospital to home (in hours)"
label(data$transport_72)="Mode of transport from hospital to home"
label(data$disch_72)="Did the time of discharge affect the trip home?"
label(data$barriers_72)="Were there any barriers encountered on the journey home from the hospital?"
label(data$exp_72)="Did you feel the healthcare worker spent enough time preparing you for discharge from hospital to home?"
label(data$exp2_72)="Were things explained to you in a way that was easy to understand?"
label(data$quality_72)="How would you rate the quality of your discharge experience?"
label(data$journey_mapping_72_hour_follow_up_complete)="Complete?"
label(data$agemomyears_calc)="Age of mother (years)"
label(data$calculated_fields_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_repeat_instrument.factor = factor(data$redcap_repeat_instrument,levels=c("delivery_neonatal","discharge_interview_neonatal","six_week_follow_up_neonatal","verbal_autopsy_newborn"))
data$is_pilot_adm.factor = factor(data$is_pilot_adm,levels=c("1","0"))
data$site_adm.factor = factor(data$site_adm,levels=c("1","2"))
data$nurse_adm_v2.factor = factor(data$nurse_adm_v2,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","98"))
data$reason_adm_v2.factor = factor(data$reason_adm_v2,levels=c("1","0"))
data$eligibleage_adm_v2.factor = factor(data$eligibleage_adm_v2,levels=c("1","0"))
data$exclusion_adm_v2.factor = factor(data$exclusion_adm_v2,levels=c("1","2","3","4","5","6","7","8","9","10"))
data$consentform_adm_v2.factor = factor(data$consentform_adm_v2,levels=c("1","0"))
data$fetaldemise_adm.factor = factor(data$fetaldemise_adm,levels=c("1","0"))
data$dobknown_adm_v2.factor = factor(data$dobknown_adm_v2,levels=c("1","0"))
data$admission_subject_details_complete.factor = factor(data$admission_subject_details_complete,levels=c("0","1","2"))
data$time_tohosp_adm.factor = factor(data$time_tohosp_adm,levels=c("1","2","3","4","5"))
data$transport_adm.factor = factor(data$transport_adm,levels=c("1","2","3","4","5","98"))
data$delay_adm___1.factor = factor(data$delay_adm___1,levels=c("0","1"))
data$delay_adm___2.factor = factor(data$delay_adm___2,levels=c("0","1"))
data$delay_adm___3.factor = factor(data$delay_adm___3,levels=c("0","1"))
data$delay_adm___4.factor = factor(data$delay_adm___4,levels=c("0","1"))
data$delay_adm___98.factor = factor(data$delay_adm___98,levels=c("0","1"))
data$delay_adm___99.factor = factor(data$delay_adm___99,levels=c("0","1"))
data$isreferral_adm.factor = factor(data$isreferral_adm,levels=c("1","0"))
data$referralsrc_adm.factor = factor(data$referralsrc_adm,levels=c("1","2","3","4","5","6","98"))
data$csection_adm.factor = factor(data$csection_adm,levels=c("1","0"))
data$takevitals_adm.factor = factor(data$takevitals_adm,levels=c("1","0"))
data$distress_adm.factor = factor(data$distress_adm,levels=c("1","2","3"))
data$admission_complete.factor = factor(data$admission_complete,levels=c("0","1","2"))
data$medhx_adm___1.factor = factor(data$medhx_adm___1,levels=c("0","1"))
data$medhx_adm___2.factor = factor(data$medhx_adm___2,levels=c("0","1"))
data$medhx_adm___3.factor = factor(data$medhx_adm___3,levels=c("0","1"))
data$medhx_adm___4.factor = factor(data$medhx_adm___4,levels=c("0","1"))
data$medhx_adm___5.factor = factor(data$medhx_adm___5,levels=c("0","1"))
data$medhx_adm___6.factor = factor(data$medhx_adm___6,levels=c("0","1"))
data$medhx_adm___7.factor = factor(data$medhx_adm___7,levels=c("0","1"))
data$medhx_adm___8.factor = factor(data$medhx_adm___8,levels=c("0","1"))
data$medhx_adm___9.factor = factor(data$medhx_adm___9,levels=c("0","1"))
data$medhx_adm___98.factor = factor(data$medhx_adm___98,levels=c("0","1"))
data$medhx_adm___99.factor = factor(data$medhx_adm___99,levels=c("0","1"))
data$duedate_adm.factor = factor(data$duedate_adm,levels=c("1","0"))
data$duedate2_adm.factor = factor(data$duedate2_adm,levels=c("1","2","3"))
data$csect_adm.factor = factor(data$csect_adm,levels=c("1","0"))
data$csect2_adm.factor = factor(data$csect2_adm,levels=c("1","2","3"))
data$preghx_adm___1.factor = factor(data$preghx_adm___1,levels=c("0","1"))
data$preghx_adm___2.factor = factor(data$preghx_adm___2,levels=c("0","1"))
data$preghx_adm___3.factor = factor(data$preghx_adm___3,levels=c("0","1"))
data$preghx_adm___4.factor = factor(data$preghx_adm___4,levels=c("0","1"))
data$preghx_adm___5.factor = factor(data$preghx_adm___5,levels=c("0","1"))
data$preghx_adm___6.factor = factor(data$preghx_adm___6,levels=c("0","1"))
data$preghx_adm___7.factor = factor(data$preghx_adm___7,levels=c("0","1"))
data$preghx_adm___8.factor = factor(data$preghx_adm___8,levels=c("0","1"))
data$preghx_adm___9.factor = factor(data$preghx_adm___9,levels=c("0","1"))
data$preghx_adm___10.factor = factor(data$preghx_adm___10,levels=c("0","1"))
data$preghx_adm___11.factor = factor(data$preghx_adm___11,levels=c("0","1"))
data$preghx_adm___12.factor = factor(data$preghx_adm___12,levels=c("0","1"))
data$preghx_adm___13.factor = factor(data$preghx_adm___13,levels=c("0","1"))
data$preghx_adm___14.factor = factor(data$preghx_adm___14,levels=c("0","1"))
data$preghx_adm___15.factor = factor(data$preghx_adm___15,levels=c("0","1"))
data$preghx_adm___16.factor = factor(data$preghx_adm___16,levels=c("0","1"))
data$preghx_adm___17.factor = factor(data$preghx_adm___17,levels=c("0","1"))
data$preghx_adm___18.factor = factor(data$preghx_adm___18,levels=c("0","1"))
data$preghx_adm___98.factor = factor(data$preghx_adm___98,levels=c("0","1"))
data$preghx_adm___99.factor = factor(data$preghx_adm___99,levels=c("0","1"))
data$placenta_adm___1.factor = factor(data$placenta_adm___1,levels=c("0","1"))
data$placenta_adm___2.factor = factor(data$placenta_adm___2,levels=c("0","1"))
data$placenta_adm___97.factor = factor(data$placenta_adm___97,levels=c("0","1"))
data$placenta_adm___98.factor = factor(data$placenta_adm___98,levels=c("0","1"))
data$placenta_adm___99.factor = factor(data$placenta_adm___99,levels=c("0","1"))
data$medhbp_adm.factor = factor(data$medhbp_adm,levels=c("1","2","99"))
data$medarv_adm.factor = factor(data$medarv_adm,levels=c("1","2","99"))
data$covidvax_adm.factor = factor(data$covidvax_adm,levels=c("1","2","3","4"))
data$prevadm_adm.factor = factor(data$prevadm_adm,levels=c("1","0"))
data$lngth_prevadm_adm.factor = factor(data$lngth_prevadm_adm,levels=c("1","2","3","4"))
data$rs_prevadm_adm___1.factor = factor(data$rs_prevadm_adm___1,levels=c("0","1"))
data$rs_prevadm_adm___2.factor = factor(data$rs_prevadm_adm___2,levels=c("0","1"))
data$rs_prevadm_adm___3.factor = factor(data$rs_prevadm_adm___3,levels=c("0","1"))
data$rs_prevadm_adm___4.factor = factor(data$rs_prevadm_adm___4,levels=c("0","1"))
data$rs_prevadm_adm___5.factor = factor(data$rs_prevadm_adm___5,levels=c("0","1"))
data$rs_prevadm_adm___6.factor = factor(data$rs_prevadm_adm___6,levels=c("0","1"))
data$rs_prevadm_adm___7.factor = factor(data$rs_prevadm_adm___7,levels=c("0","1"))
data$rs_prevadm_adm___8.factor = factor(data$rs_prevadm_adm___8,levels=c("0","1"))
data$rs_prevadm_adm___9.factor = factor(data$rs_prevadm_adm___9,levels=c("0","1"))
data$rs_prevadm_adm___10.factor = factor(data$rs_prevadm_adm___10,levels=c("0","1"))
data$rs_prevadm_adm___11.factor = factor(data$rs_prevadm_adm___11,levels=c("0","1"))
data$rs_prevadm_adm___12.factor = factor(data$rs_prevadm_adm___12,levels=c("0","1"))
data$rs_prevadm_adm___13.factor = factor(data$rs_prevadm_adm___13,levels=c("0","1"))
data$rs_prevadm_adm___14.factor = factor(data$rs_prevadm_adm___14,levels=c("0","1"))
data$rs_prevadm_adm___98.factor = factor(data$rs_prevadm_adm___98,levels=c("0","1"))
data$rs_prevadm_adm___99.factor = factor(data$rs_prevadm_adm___99,levels=c("0","1"))
data$uti_abx_adm.factor = factor(data$uti_abx_adm,levels=c("1","0"))
data$numberanc_adm.factor = factor(data$numberanc_adm,levels=c("0","1","2","3","4","5","6","7","8","9"))
data$ancprovider_adm___1.factor = factor(data$ancprovider_adm___1,levels=c("0","1"))
data$ancprovider_adm___2.factor = factor(data$ancprovider_adm___2,levels=c("0","1"))
data$ancprovider_adm___3.factor = factor(data$ancprovider_adm___3,levels=c("0","1"))
data$ancprovider_adm___4.factor = factor(data$ancprovider_adm___4,levels=c("0","1"))
data$ancprovider_adm___5.factor = factor(data$ancprovider_adm___5,levels=c("0","1"))
data$ancprovider_adm___6.factor = factor(data$ancprovider_adm___6,levels=c("0","1"))
data$ancprovider_adm___7.factor = factor(data$ancprovider_adm___7,levels=c("0","1"))
data$pregnancy_history_complete.factor = factor(data$pregnancy_history_complete,levels=c("0","1","2"))
data$marry_ses.factor = factor(data$marry_ses,levels=c("1","2","3","4","5"))
data$livfather_ses.factor = factor(data$livfather_ses,levels=c("1","0"))
data$schoolyrs_ses.factor = factor(data$schoolyrs_ses,levels=c("1","2","3","4","5","6"))
data$nutri_adm.factor = factor(data$nutri_adm,levels=c("1","2","99"))
data$sesindex_flooring___1.factor = factor(data$sesindex_flooring___1,levels=c("0","1"))
data$sesindex_flooring___2.factor = factor(data$sesindex_flooring___2,levels=c("0","1"))
data$sesindex_flooring___3.factor = factor(data$sesindex_flooring___3,levels=c("0","1"))
data$sesindex_flooring___4.factor = factor(data$sesindex_flooring___4,levels=c("0","1"))
data$sesindex_flooring___98.factor = factor(data$sesindex_flooring___98,levels=c("0","1"))
data$sesindex_toilet___1.factor = factor(data$sesindex_toilet___1,levels=c("0","1"))
data$sesindex_toilet___2.factor = factor(data$sesindex_toilet___2,levels=c("0","1"))
data$sesindex_toilet___3.factor = factor(data$sesindex_toilet___3,levels=c("0","1"))
data$sesindex_toilet___4.factor = factor(data$sesindex_toilet___4,levels=c("0","1"))
data$sesindex_toilet___5.factor = factor(data$sesindex_toilet___5,levels=c("0","1"))
data$sesindex_toilet___6.factor = factor(data$sesindex_toilet___6,levels=c("0","1"))
data$sesindex_toilet___98.factor = factor(data$sesindex_toilet___98,levels=c("0","1"))
data$sesindex_toiletshared.factor = factor(data$sesindex_toiletshared,levels=c("1","0"))
data$sesindex_cooking___1.factor = factor(data$sesindex_cooking___1,levels=c("0","1"))
data$sesindex_cooking___2.factor = factor(data$sesindex_cooking___2,levels=c("0","1"))
data$sesindex_cooking___3.factor = factor(data$sesindex_cooking___3,levels=c("0","1"))
data$sesindex_cooking___4.factor = factor(data$sesindex_cooking___4,levels=c("0","1"))
data$sesindex_cooking___5.factor = factor(data$sesindex_cooking___5,levels=c("0","1"))
data$sesindex_cooking___6.factor = factor(data$sesindex_cooking___6,levels=c("0","1"))
data$sesindex_cooking___7.factor = factor(data$sesindex_cooking___7,levels=c("0","1"))
data$sesindex_cooking___8.factor = factor(data$sesindex_cooking___8,levels=c("0","1"))
data$sesindex_cooking___9.factor = factor(data$sesindex_cooking___9,levels=c("0","1"))
data$sesindex_safewater.factor = factor(data$sesindex_safewater,levels=c("1","2","3","4","5","6","7","98"))
data$sesindex_safewaterdistance.factor = factor(data$sesindex_safewaterdistance,levels=c("1","0"))
data$sesindex_assets___1.factor = factor(data$sesindex_assets___1,levels=c("0","1"))
data$sesindex_assets___2.factor = factor(data$sesindex_assets___2,levels=c("0","1"))
data$sesindex_assets___3.factor = factor(data$sesindex_assets___3,levels=c("0","1"))
data$sesindex_assets___4.factor = factor(data$sesindex_assets___4,levels=c("0","1"))
data$sesindex_assets___99.factor = factor(data$sesindex_assets___99,levels=c("0","1"))
data$sesindex_assets2___1.factor = factor(data$sesindex_assets2___1,levels=c("0","1"))
data$sesindex_assets2___2.factor = factor(data$sesindex_assets2___2,levels=c("0","1"))
data$sesindex_assets2___3.factor = factor(data$sesindex_assets2___3,levels=c("0","1"))
data$sesindex_assets2___4.factor = factor(data$sesindex_assets2___4,levels=c("0","1"))
data$sesindex_assets2___5.factor = factor(data$sesindex_assets2___5,levels=c("0","1"))
data$sesindex_assets2___99.factor = factor(data$sesindex_assets2___99,levels=c("0","1"))
data$sesindex_room.factor = factor(data$sesindex_room,levels=c("1","2","3","4"))
data$child_death_ses.factor = factor(data$child_death_ses,levels=c("1","0"))
data$ses_and_demographics_complete.factor = factor(data$ses_and_demographics_complete,levels=c("0","1","2"))
data$prom_del.factor = factor(data$prom_del,levels=c("1","2","3"))
data$rom_del.factor = factor(data$rom_del,levels=c("1","2","3","4","5"))
data$delmode_del.factor = factor(data$delmode_del,levels=c("1","2","3","4"))
data$episiotomy_del.factor = factor(data$episiotomy_del,levels=c("1","2","99"))
data$tear_del.factor = factor(data$tear_del,levels=c("1","2","99"))
data$degreetear_del.factor = factor(data$degreetear_del,levels=c("1","2","3","4"))
data$induce_del.factor = factor(data$induce_del,levels=c("1","2","99"))
data$inductype_del___1.factor = factor(data$inductype_del___1,levels=c("0","1"))
data$inductype_del___2.factor = factor(data$inductype_del___2,levels=c("0","1"))
data$inductype_del___3.factor = factor(data$inductype_del___3,levels=c("0","1"))
data$inductype_del___4.factor = factor(data$inductype_del___4,levels=c("0","1"))
data$inductype_del___5.factor = factor(data$inductype_del___5,levels=c("0","1"))
data$inductype_del___98.factor = factor(data$inductype_del___98,levels=c("0","1"))
data$pph_del.factor = factor(data$pph_del,levels=c("1","2","99"))
data$transfx_del.factor = factor(data$transfx_del,levels=c("1","2","99"))
data$obstruct_del.factor = factor(data$obstruct_del,levels=c("1","2","99"))
data$meconium_del.factor = factor(data$meconium_del,levels=c("1","2","99"))
data$vagexam_del.factor = factor(data$vagexam_del,levels=c("0","1","2","3","4","5","6","7","8","9","10"))
data$placenta_del.factor = factor(data$placenta_del,levels=c("1","2","3","4","99"))
data$man_placenta_del.factor = factor(data$man_placenta_del,levels=c("1","0"))
data$csecturgency_del.factor = factor(data$csecturgency_del,levels=c("1","2","3","4","99"))
data$csect_delay_del.factor = factor(data$csect_delay_del,levels=c("1","2","3","4","5"))
data$abx_del.factor = factor(data$abx_del,levels=c("1","2","3","99"))
data$csect_abx_del.factor = factor(data$csect_abx_del,levels=c("1","2","3","4","5","6","7","8","9","98","99"))
data$anticoag_del.factor = factor(data$anticoag_del,levels=c("1","0"))
data$delivery_maternal_complete.factor = factor(data$delivery_maternal_complete,levels=c("0","1","2"))
data$infantstatus_del.factor = factor(data$infantstatus_del,levels=c("1","0"))
data$sb20wk_del.factor = factor(data$sb20wk_del,levels=c("1","0"))
data$sb_del.factor = factor(data$sb_del,levels=c("1","2","99"))
data$sbcongenital_del.factor = factor(data$sbcongenital_del,levels=c("1","2","3"))
data$sbsex_del.factor = factor(data$sbsex_del,levels=c("1","2","99"))
data$sexbb_del.factor = factor(data$sexbb_del,levels=c("1","2","98"))
data$cord_delay_del.factor = factor(data$cord_delay_del,levels=c("1","2","3"))
data$rescus_del.factor = factor(data$rescus_del,levels=c("1","2","99"))
data$resustype_del___1.factor = factor(data$resustype_del___1,levels=c("0","1"))
data$resustype_del___2.factor = factor(data$resustype_del___2,levels=c("0","1"))
data$resustype_del___3.factor = factor(data$resustype_del___3,levels=c("0","1"))
data$resustype_del___4.factor = factor(data$resustype_del___4,levels=c("0","1"))
data$resustype_del___5.factor = factor(data$resustype_del___5,levels=c("0","1"))
data$resustype_del___6.factor = factor(data$resustype_del___6,levels=c("0","1"))
data$resustype_del___7.factor = factor(data$resustype_del___7,levels=c("0","1"))
data$resustype_del___8.factor = factor(data$resustype_del___8,levels=c("0","1"))
data$resustype_del___98.factor = factor(data$resustype_del___98,levels=c("0","1"))
data$resustype_del___99.factor = factor(data$resustype_del___99,levels=c("0","1"))
data$delivery_neonatal_complete.factor = factor(data$delivery_neonatal_complete,levels=c("0","1","2"))
data$disnurse_mat.factor = factor(data$disnurse_mat,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","98"))
data$adm_mat.factor = factor(data$adm_mat,levels=c("1","0"))
data$dischstat_mat.factor = factor(data$dischstat_mat,levels=c("1","2","3","99"))
data$destination_mat.factor = factor(data$destination_mat,levels=c("1","2","3","4","5","98"))
data$support_mat___1.factor = factor(data$support_mat___1,levels=c("0","1"))
data$support_mat___2.factor = factor(data$support_mat___2,levels=c("0","1"))
data$support_mat___3.factor = factor(data$support_mat___3,levels=c("0","1"))
data$support_mat___4.factor = factor(data$support_mat___4,levels=c("0","1"))
data$support_mat___5.factor = factor(data$support_mat___5,levels=c("0","1"))
data$support_mat___99.factor = factor(data$support_mat___99,levels=c("0","1"))
data$bf_mat.factor = factor(data$bf_mat,levels=c("1","2","3"))
data$symp_mat___1.factor = factor(data$symp_mat___1,levels=c("0","1"))
data$symp_mat___2.factor = factor(data$symp_mat___2,levels=c("0","1"))
data$symp_mat___3.factor = factor(data$symp_mat___3,levels=c("0","1"))
data$symp_mat___4.factor = factor(data$symp_mat___4,levels=c("0","1"))
data$symp_mat___5.factor = factor(data$symp_mat___5,levels=c("0","1"))
data$symp_mat___6.factor = factor(data$symp_mat___6,levels=c("0","1"))
data$symp_mat___7.factor = factor(data$symp_mat___7,levels=c("0","1"))
data$symp_mat___8.factor = factor(data$symp_mat___8,levels=c("0","1"))
data$symp_mat___9.factor = factor(data$symp_mat___9,levels=c("0","1"))
data$symp_mat___10.factor = factor(data$symp_mat___10,levels=c("0","1"))
data$symp_mat___99.factor = factor(data$symp_mat___99,levels=c("0","1"))
data$abx_mat___1.factor = factor(data$abx_mat___1,levels=c("0","1"))
data$abx_mat___2.factor = factor(data$abx_mat___2,levels=c("0","1"))
data$abx_mat___3.factor = factor(data$abx_mat___3,levels=c("0","1"))
data$abx_mat___99.factor = factor(data$abx_mat___99,levels=c("0","1"))
data$discharge_interview_maternal_complete.factor = factor(data$discharge_interview_maternal_complete,levels=c("0","1","2"))
data$dispbb_neo.factor = factor(data$dispbb_neo,levels=c("1","2","3","4","5","98"))
data$admitsite_neo.factor = factor(data$admitsite_neo,levels=c("1","2"))
data$admitdisposition_neo.factor = factor(data$admitdisposition_neo,levels=c("1","2","3","4"))
data$admitdiagnosis_neo___1.factor = factor(data$admitdiagnosis_neo___1,levels=c("0","1"))
data$admitdiagnosis_neo___2.factor = factor(data$admitdiagnosis_neo___2,levels=c("0","1"))
data$admitdiagnosis_neo___3.factor = factor(data$admitdiagnosis_neo___3,levels=c("0","1"))
data$admitdiagnosis_neo___4.factor = factor(data$admitdiagnosis_neo___4,levels=c("0","1"))
data$admitdiagnosis_neo___5.factor = factor(data$admitdiagnosis_neo___5,levels=c("0","1"))
data$admitdiagnosis_neo___99.factor = factor(data$admitdiagnosis_neo___99,levels=c("0","1"))
data$babedisch_neo.factor = factor(data$babedisch_neo,levels=c("1","0"))
data$poop_neo.factor = factor(data$poop_neo,levels=c("1","2","3"))
data$pee_neo.factor = factor(data$pee_neo,levels=c("1","2","3"))
data$bf_neo.factor = factor(data$bf_neo,levels=c("1","2"))
data$jaundice_neo.factor = factor(data$jaundice_neo,levels=c("0","1","2"))
data$eyedischarge_neo.factor = factor(data$eyedischarge_neo,levels=c("1","0"))
data$foot_o2src_neo.factor = factor(data$foot_o2src_neo,levels=c("1","2"))
data$rhand_o2src_neo.factor = factor(data$rhand_o2src_neo,levels=c("1","2"))
data$abx_neo.factor = factor(data$abx_neo,levels=c("1","0"))
data$discharge_interview_neonatal_complete.factor = factor(data$discharge_interview_neonatal_complete,levels=c("0","1","2"))
data$physical_swf___1.factor = factor(data$physical_swf___1,levels=c("0","1"))
data$respondent_swf.factor = factor(data$respondent_swf,levels=c("1","98"))
data$otherrespondent_swf.factor = factor(data$otherrespondent_swf,levels=c("1","2","3","4","5","6","7","98"))
data$home_swf.factor = factor(data$home_swf,levels=c("1","0"))
data$momalive_swf.factor = factor(data$momalive_swf,levels=c("1","0"))
data$momdeathplace_swf.factor = factor(data$momdeathplace_swf,levels=c("1","2","3","98"))
data$momadmit_swf.factor = factor(data$momadmit_swf,levels=c("1","0"))
data$nummomadmit_swf.factor = factor(data$nummomadmit_swf,levels=c("1","2","3","4"))
data$momadmitpathway_swf___1.factor = factor(data$momadmitpathway_swf___1,levels=c("0","1"))
data$momadmitpathway_swf___2.factor = factor(data$momadmitpathway_swf___2,levels=c("0","1"))
data$momadmitpathway_swf___3.factor = factor(data$momadmitpathway_swf___3,levels=c("0","1"))
data$momadmitpathway_swf___98.factor = factor(data$momadmitpathway_swf___98,levels=c("0","1"))
data$momadmitsymp_swf___1.factor = factor(data$momadmitsymp_swf___1,levels=c("0","1"))
data$momadmitsymp_swf___2.factor = factor(data$momadmitsymp_swf___2,levels=c("0","1"))
data$momadmitsymp_swf___3.factor = factor(data$momadmitsymp_swf___3,levels=c("0","1"))
data$momadmitsymp_swf___4.factor = factor(data$momadmitsymp_swf___4,levels=c("0","1"))
data$momadmitsymp_swf___5.factor = factor(data$momadmitsymp_swf___5,levels=c("0","1"))
data$momadmitsymp_swf___6.factor = factor(data$momadmitsymp_swf___6,levels=c("0","1"))
data$momadmitsymp_swf___7.factor = factor(data$momadmitsymp_swf___7,levels=c("0","1"))
data$momadmitsymp_swf___8.factor = factor(data$momadmitsymp_swf___8,levels=c("0","1"))
data$momadmitsymp_swf___9.factor = factor(data$momadmitsymp_swf___9,levels=c("0","1"))
data$momadmitsymp_swf___10.factor = factor(data$momadmitsymp_swf___10,levels=c("0","1"))
data$momadmitsymp_swf___11.factor = factor(data$momadmitsymp_swf___11,levels=c("0","1"))
data$momadmitsymp_swf___12.factor = factor(data$momadmitsymp_swf___12,levels=c("0","1"))
data$momadmitsymp_swf___13.factor = factor(data$momadmitsymp_swf___13,levels=c("0","1"))
data$momadmitsymp_swf___14.factor = factor(data$momadmitsymp_swf___14,levels=c("0","1"))
data$momadmitsymp_swf___15.factor = factor(data$momadmitsymp_swf___15,levels=c("0","1"))
data$momadmitsymp_swf___16.factor = factor(data$momadmitsymp_swf___16,levels=c("0","1"))
data$momadmitsymp_swf___17.factor = factor(data$momadmitsymp_swf___17,levels=c("0","1"))
data$momadmitsymp_swf___18.factor = factor(data$momadmitsymp_swf___18,levels=c("0","1"))
data$momadmitsymp_swf___19.factor = factor(data$momadmitsymp_swf___19,levels=c("0","1"))
data$momadmitsymp_swf___20.factor = factor(data$momadmitsymp_swf___20,levels=c("0","1"))
data$momadmitsymp_swf___21.factor = factor(data$momadmitsymp_swf___21,levels=c("0","1"))
data$momadmitsymp_swf___98.factor = factor(data$momadmitsymp_swf___98,levels=c("0","1"))
data$momadmitsymp_swf___99.factor = factor(data$momadmitsymp_swf___99,levels=c("0","1"))
data$momadmitcond_swf___1.factor = factor(data$momadmitcond_swf___1,levels=c("0","1"))
data$momadmitcond_swf___2.factor = factor(data$momadmitcond_swf___2,levels=c("0","1"))
data$momadmitcond_swf___3.factor = factor(data$momadmitcond_swf___3,levels=c("0","1"))
data$momadmitcond_swf___4.factor = factor(data$momadmitcond_swf___4,levels=c("0","1"))
data$momadmitcond_swf___5.factor = factor(data$momadmitcond_swf___5,levels=c("0","1"))
data$momadmitcond_swf___6.factor = factor(data$momadmitcond_swf___6,levels=c("0","1"))
data$momadmitcond_swf___7.factor = factor(data$momadmitcond_swf___7,levels=c("0","1"))
data$momadmitcond_swf___8.factor = factor(data$momadmitcond_swf___8,levels=c("0","1"))
data$momadmitcond_swf___9.factor = factor(data$momadmitcond_swf___9,levels=c("0","1"))
data$momadmitcond_swf___10.factor = factor(data$momadmitcond_swf___10,levels=c("0","1"))
data$momadmitcond_swf___11.factor = factor(data$momadmitcond_swf___11,levels=c("0","1"))
data$momadmitcond_swf___12.factor = factor(data$momadmitcond_swf___12,levels=c("0","1"))
data$momadmitcond_swf___13.factor = factor(data$momadmitcond_swf___13,levels=c("0","1"))
data$momadmitcond_swf___14.factor = factor(data$momadmitcond_swf___14,levels=c("0","1"))
data$momadmitcond_swf___15.factor = factor(data$momadmitcond_swf___15,levels=c("0","1"))
data$momadmitcond_swf___16.factor = factor(data$momadmitcond_swf___16,levels=c("0","1"))
data$momadmitcond_swf___17.factor = factor(data$momadmitcond_swf___17,levels=c("0","1"))
data$momadmitcond_swf___18.factor = factor(data$momadmitcond_swf___18,levels=c("0","1"))
data$momadmitcond_swf___98.factor = factor(data$momadmitcond_swf___98,levels=c("0","1"))
data$momadmitcond_swf___99.factor = factor(data$momadmitcond_swf___99,levels=c("0","1"))
data$momseek_swf.factor = factor(data$momseek_swf,levels=c("1","0"))
data$momseekpathway_swf___1.factor = factor(data$momseekpathway_swf___1,levels=c("0","1"))
data$momseekpathway_swf___2.factor = factor(data$momseekpathway_swf___2,levels=c("0","1"))
data$momseekpathway_swf___3.factor = factor(data$momseekpathway_swf___3,levels=c("0","1"))
data$momseekpathway_swf___98.factor = factor(data$momseekpathway_swf___98,levels=c("0","1"))
data$momseeksymp_swf___1.factor = factor(data$momseeksymp_swf___1,levels=c("0","1"))
data$momseeksymp_swf___2.factor = factor(data$momseeksymp_swf___2,levels=c("0","1"))
data$momseeksymp_swf___3.factor = factor(data$momseeksymp_swf___3,levels=c("0","1"))
data$momseeksymp_swf___4.factor = factor(data$momseeksymp_swf___4,levels=c("0","1"))
data$momseeksymp_swf___5.factor = factor(data$momseeksymp_swf___5,levels=c("0","1"))
data$momseeksymp_swf___6.factor = factor(data$momseeksymp_swf___6,levels=c("0","1"))
data$momseeksymp_swf___7.factor = factor(data$momseeksymp_swf___7,levels=c("0","1"))
data$momseeksymp_swf___8.factor = factor(data$momseeksymp_swf___8,levels=c("0","1"))
data$momseeksymp_swf___9.factor = factor(data$momseeksymp_swf___9,levels=c("0","1"))
data$momseeksymp_swf___10.factor = factor(data$momseeksymp_swf___10,levels=c("0","1"))
data$momseeksymp_swf___11.factor = factor(data$momseeksymp_swf___11,levels=c("0","1"))
data$momseeksymp_swf___12.factor = factor(data$momseeksymp_swf___12,levels=c("0","1"))
data$momseeksymp_swf___13.factor = factor(data$momseeksymp_swf___13,levels=c("0","1"))
data$momseeksymp_swf___14.factor = factor(data$momseeksymp_swf___14,levels=c("0","1"))
data$momseeksymp_swf___15.factor = factor(data$momseeksymp_swf___15,levels=c("0","1"))
data$momseeksymp_swf___16.factor = factor(data$momseeksymp_swf___16,levels=c("0","1"))
data$momseeksymp_swf___17.factor = factor(data$momseeksymp_swf___17,levels=c("0","1"))
data$momseeksymp_swf___18.factor = factor(data$momseeksymp_swf___18,levels=c("0","1"))
data$momseeksymp_swf___19.factor = factor(data$momseeksymp_swf___19,levels=c("0","1"))
data$momseeksymp_swf___20.factor = factor(data$momseeksymp_swf___20,levels=c("0","1"))
data$momseeksymp_swf___21.factor = factor(data$momseeksymp_swf___21,levels=c("0","1"))
data$momseeksymp_swf___98.factor = factor(data$momseeksymp_swf___98,levels=c("0","1"))
data$momseeksymp_swf___99.factor = factor(data$momseeksymp_swf___99,levels=c("0","1"))
data$momseekcond_swf___1.factor = factor(data$momseekcond_swf___1,levels=c("0","1"))
data$momseekcond_swf___2.factor = factor(data$momseekcond_swf___2,levels=c("0","1"))
data$momseekcond_swf___3.factor = factor(data$momseekcond_swf___3,levels=c("0","1"))
data$momseekcond_swf___4.factor = factor(data$momseekcond_swf___4,levels=c("0","1"))
data$momseekcond_swf___5.factor = factor(data$momseekcond_swf___5,levels=c("0","1"))
data$momseekcond_swf___6.factor = factor(data$momseekcond_swf___6,levels=c("0","1"))
data$momseekcond_swf___7.factor = factor(data$momseekcond_swf___7,levels=c("0","1"))
data$momseekcond_swf___8.factor = factor(data$momseekcond_swf___8,levels=c("0","1"))
data$momseekcond_swf___9.factor = factor(data$momseekcond_swf___9,levels=c("0","1"))
data$momseekcond_swf___10.factor = factor(data$momseekcond_swf___10,levels=c("0","1"))
data$momseekcond_swf___11.factor = factor(data$momseekcond_swf___11,levels=c("0","1"))
data$momseekcond_swf___12.factor = factor(data$momseekcond_swf___12,levels=c("0","1"))
data$momseekcond_swf___13.factor = factor(data$momseekcond_swf___13,levels=c("0","1"))
data$momseekcond_swf___14.factor = factor(data$momseekcond_swf___14,levels=c("0","1"))
data$momseekcond_swf___15.factor = factor(data$momseekcond_swf___15,levels=c("0","1"))
data$momseekcond_swf___16.factor = factor(data$momseekcond_swf___16,levels=c("0","1"))
data$momseekcond_swf___17.factor = factor(data$momseekcond_swf___17,levels=c("0","1"))
data$momseekcond_swf___18.factor = factor(data$momseekcond_swf___18,levels=c("0","1"))
data$momseekcond_swf___98.factor = factor(data$momseekcond_swf___98,levels=c("0","1"))
data$momseekcond_swf___99.factor = factor(data$momseekcond_swf___99,levels=c("0","1"))
data$pnc_mom_swf.factor = factor(data$pnc_mom_swf,levels=c("0","1","2","3","4"))
data$six_week_follow_up_maternal_complete.factor = factor(data$six_week_follow_up_maternal_complete,levels=c("0","1","2"))
data$babephysical_swf___1.factor = factor(data$babephysical_swf___1,levels=c("0","1"))
data$babealive_swf.factor = factor(data$babealive_swf,levels=c("1","0"))
data$babedeathplace_swf.factor = factor(data$babedeathplace_swf,levels=c("1","2","3","98"))
data$babeadmit_swf.factor = factor(data$babeadmit_swf,levels=c("1","0"))
data$numbabeadmit_swf.factor = factor(data$numbabeadmit_swf,levels=c("1","2","3","4"))
data$babeadmitpathway_swf___1.factor = factor(data$babeadmitpathway_swf___1,levels=c("0","1"))
data$babeadmitpathway_swf___2.factor = factor(data$babeadmitpathway_swf___2,levels=c("0","1"))
data$babeadmitpathway_swf___3.factor = factor(data$babeadmitpathway_swf___3,levels=c("0","1"))
data$babeadmitpathway_swf___98.factor = factor(data$babeadmitpathway_swf___98,levels=c("0","1"))
data$babeadmitsymp_swf___1.factor = factor(data$babeadmitsymp_swf___1,levels=c("0","1"))
data$babeadmitsymp_swf___2.factor = factor(data$babeadmitsymp_swf___2,levels=c("0","1"))
data$babeadmitsymp_swf___3.factor = factor(data$babeadmitsymp_swf___3,levels=c("0","1"))
data$babeadmitsymp_swf___4.factor = factor(data$babeadmitsymp_swf___4,levels=c("0","1"))
data$babeadmitsymp_swf___5.factor = factor(data$babeadmitsymp_swf___5,levels=c("0","1"))
data$babeadmitsymp_swf___6.factor = factor(data$babeadmitsymp_swf___6,levels=c("0","1"))
data$babeadmitsymp_swf___7.factor = factor(data$babeadmitsymp_swf___7,levels=c("0","1"))
data$babeadmitsymp_swf___8.factor = factor(data$babeadmitsymp_swf___8,levels=c("0","1"))
data$babeadmitsymp_swf___9.factor = factor(data$babeadmitsymp_swf___9,levels=c("0","1"))
data$babeadmitsymp_swf___10.factor = factor(data$babeadmitsymp_swf___10,levels=c("0","1"))
data$babeadmitsymp_swf___11.factor = factor(data$babeadmitsymp_swf___11,levels=c("0","1"))
data$babeadmitsymp_swf___12.factor = factor(data$babeadmitsymp_swf___12,levels=c("0","1"))
data$babeadmitsymp_swf___13.factor = factor(data$babeadmitsymp_swf___13,levels=c("0","1"))
data$babeadmitsymp_swf___14.factor = factor(data$babeadmitsymp_swf___14,levels=c("0","1"))
data$babeadmitsymp_swf___15.factor = factor(data$babeadmitsymp_swf___15,levels=c("0","1"))
data$babeadmitsymp_swf___16.factor = factor(data$babeadmitsymp_swf___16,levels=c("0","1"))
data$babeadmitsymp_swf___17.factor = factor(data$babeadmitsymp_swf___17,levels=c("0","1"))
data$babeadmitsymp_swf___18.factor = factor(data$babeadmitsymp_swf___18,levels=c("0","1"))
data$babeadmitsymp_swf___98.factor = factor(data$babeadmitsymp_swf___98,levels=c("0","1"))
data$babeadmitsymp_swf___99.factor = factor(data$babeadmitsymp_swf___99,levels=c("0","1"))
data$transfusion_swf.factor = factor(data$transfusion_swf,levels=c("1","0"))
data$babeadmitcond_swf___1.factor = factor(data$babeadmitcond_swf___1,levels=c("0","1"))
data$babeadmitcond_swf___2.factor = factor(data$babeadmitcond_swf___2,levels=c("0","1"))
data$babeadmitcond_swf___3.factor = factor(data$babeadmitcond_swf___3,levels=c("0","1"))
data$babeadmitcond_swf___4.factor = factor(data$babeadmitcond_swf___4,levels=c("0","1"))
data$babeadmitcond_swf___5.factor = factor(data$babeadmitcond_swf___5,levels=c("0","1"))
data$babeadmitcond_swf___6.factor = factor(data$babeadmitcond_swf___6,levels=c("0","1"))
data$babeadmitcond_swf___7.factor = factor(data$babeadmitcond_swf___7,levels=c("0","1"))
data$babeadmitcond_swf___8.factor = factor(data$babeadmitcond_swf___8,levels=c("0","1"))
data$babeadmitcond_swf___9.factor = factor(data$babeadmitcond_swf___9,levels=c("0","1"))
data$babeadmitcond_swf___10.factor = factor(data$babeadmitcond_swf___10,levels=c("0","1"))
data$babeadmitcond_swf___11.factor = factor(data$babeadmitcond_swf___11,levels=c("0","1"))
data$babeadmitcond_swf___98.factor = factor(data$babeadmitcond_swf___98,levels=c("0","1"))
data$babeadmitcond_swf___99.factor = factor(data$babeadmitcond_swf___99,levels=c("0","1"))
data$matsymp_babeoutcome.factor = factor(data$matsymp_babeoutcome,levels=c("1","0"))
data$babefeed_outcome.factor = factor(data$babefeed_outcome,levels=c("1","0"))
data$babeseek_swf.factor = factor(data$babeseek_swf,levels=c("1","0"))
data$babeseekpathway_swf___1.factor = factor(data$babeseekpathway_swf___1,levels=c("0","1"))
data$babeseekpathway_swf___2.factor = factor(data$babeseekpathway_swf___2,levels=c("0","1"))
data$babeseekpathway_swf___3.factor = factor(data$babeseekpathway_swf___3,levels=c("0","1"))
data$babeseekpathway_swf___98.factor = factor(data$babeseekpathway_swf___98,levels=c("0","1"))
data$pnc_baby_swf.factor = factor(data$pnc_baby_swf,levels=c("0","1","2","3","4"))
data$six_week_follow_up_neonatal_complete.factor = factor(data$six_week_follow_up_neonatal_complete,levels=c("0","1","2"))
data$momchecks_swf.factor = factor(data$momchecks_swf,levels=c("1","0"))
data$babechecks_swf.factor = factor(data$babechecks_swf,levels=c("1","0"))
data$wellnesstimesthink_swf.factor = factor(data$wellnesstimesthink_swf,levels=c("0","1","2","3","4","5"))
data$wellnesssource_swf___1.factor = factor(data$wellnesssource_swf___1,levels=c("0","1"))
data$wellnesssource_swf___2.factor = factor(data$wellnesssource_swf___2,levels=c("0","1"))
data$wellnesssource_swf___3.factor = factor(data$wellnesssource_swf___3,levels=c("0","1"))
data$wellnesssource_swf___4.factor = factor(data$wellnesssource_swf___4,levels=c("0","1"))
data$wellnesssource_swf___98.factor = factor(data$wellnesssource_swf___98,levels=c("0","1"))
data$wellnesstimes_swf.factor = factor(data$wellnesstimes_swf,levels=c("0","1","2","3","4","5"))
data$wellnesswhen_swf___1.factor = factor(data$wellnesswhen_swf___1,levels=c("0","1"))
data$wellnesswhen_swf___2.factor = factor(data$wellnesswhen_swf___2,levels=c("0","1"))
data$wellnesswhen_swf___3.factor = factor(data$wellnesswhen_swf___3,levels=c("0","1"))
data$wellnesswhen_swf___4.factor = factor(data$wellnesswhen_swf___4,levels=c("0","1"))
data$wellnesswhen_swf___5.factor = factor(data$wellnesswhen_swf___5,levels=c("0","1"))
data$wellnesswhen_swf___98.factor = factor(data$wellnesswhen_swf___98,levels=c("0","1"))
data$wellnesswhen_swf___99.factor = factor(data$wellnesswhen_swf___99,levels=c("0","1"))
data$wellnessreasonnot_swf___1.factor = factor(data$wellnessreasonnot_swf___1,levels=c("0","1"))
data$wellnessreasonnot_swf___2.factor = factor(data$wellnessreasonnot_swf___2,levels=c("0","1"))
data$wellnessreasonnot_swf___3.factor = factor(data$wellnessreasonnot_swf___3,levels=c("0","1"))
data$wellnessreasonnot_swf___4.factor = factor(data$wellnessreasonnot_swf___4,levels=c("0","1"))
data$wellnessreasonnot_swf___5.factor = factor(data$wellnessreasonnot_swf___5,levels=c("0","1"))
data$wellnessreasonnot_swf___6.factor = factor(data$wellnessreasonnot_swf___6,levels=c("0","1"))
data$wellnessreasonnot_swf___7.factor = factor(data$wellnessreasonnot_swf___7,levels=c("0","1"))
data$wellnessreasonnot_swf___8.factor = factor(data$wellnessreasonnot_swf___8,levels=c("0","1"))
data$wellnessreasonnot_swf___9.factor = factor(data$wellnessreasonnot_swf___9,levels=c("0","1"))
data$wellnessreasonnot_swf___10.factor = factor(data$wellnessreasonnot_swf___10,levels=c("0","1"))
data$wellnessreasonnot_swf___98.factor = factor(data$wellnessreasonnot_swf___98,levels=c("0","1"))
data$wellnessreasonnot_swf___99.factor = factor(data$wellnessreasonnot_swf___99,levels=c("0","1"))
data$wellnessdiscuss_swf___1.factor = factor(data$wellnessdiscuss_swf___1,levels=c("0","1"))
data$wellnessdiscuss_swf___2.factor = factor(data$wellnessdiscuss_swf___2,levels=c("0","1"))
data$wellnessdiscuss_swf___3.factor = factor(data$wellnessdiscuss_swf___3,levels=c("0","1"))
data$wellnessdiscuss_swf___4.factor = factor(data$wellnessdiscuss_swf___4,levels=c("0","1"))
data$wellnessdiscuss_swf___5.factor = factor(data$wellnessdiscuss_swf___5,levels=c("0","1"))
data$wellnessdiscuss_swf___99.factor = factor(data$wellnessdiscuss_swf___99,levels=c("0","1"))
data$six_week_follow_up_wellness_checks_complete.factor = factor(data$six_week_follow_up_wellness_checks_complete,levels=c("0","1","2"))
data$id10007_m.factor = factor(data$id10007_m,levels=c("1","2","3"))
data$id10008_m.factor = factor(data$id10008_m,levels=c("1","2","3","4","5","6","7","8","88"))
data$id10009_m.factor = factor(data$id10009_m,levels=c("1","0","99","88"))
data$id10013_m.factor = factor(data$id10013_m,levels=c("1","0"))
data$id10487_m.factor = factor(data$id10487_m,levels=c("1","0","99","88"))
data$id10488_m.factor = factor(data$id10488_m,levels=c("1","0","99","88"))
data$momadmit_va.factor = factor(data$momadmit_va,levels=c("1","0"))
data$nummomadmit_va.factor = factor(data$nummomadmit_va,levels=c("1","2","3","4"))
data$momadmitpathway_va___1.factor = factor(data$momadmitpathway_va___1,levels=c("0","1"))
data$momadmitpathway_va___2.factor = factor(data$momadmitpathway_va___2,levels=c("0","1"))
data$momadmitpathway_va___3.factor = factor(data$momadmitpathway_va___3,levels=c("0","1"))
data$momadmitpathway_va___98.factor = factor(data$momadmitpathway_va___98,levels=c("0","1"))
data$momadmitsymp_va___1.factor = factor(data$momadmitsymp_va___1,levels=c("0","1"))
data$momadmitsymp_va___2.factor = factor(data$momadmitsymp_va___2,levels=c("0","1"))
data$momadmitsymp_va___3.factor = factor(data$momadmitsymp_va___3,levels=c("0","1"))
data$momadmitsymp_va___4.factor = factor(data$momadmitsymp_va___4,levels=c("0","1"))
data$momadmitsymp_va___5.factor = factor(data$momadmitsymp_va___5,levels=c("0","1"))
data$momadmitsymp_va___6.factor = factor(data$momadmitsymp_va___6,levels=c("0","1"))
data$momadmitsymp_va___7.factor = factor(data$momadmitsymp_va___7,levels=c("0","1"))
data$momadmitsymp_va___8.factor = factor(data$momadmitsymp_va___8,levels=c("0","1"))
data$momadmitsymp_va___9.factor = factor(data$momadmitsymp_va___9,levels=c("0","1"))
data$momadmitsymp_va___10.factor = factor(data$momadmitsymp_va___10,levels=c("0","1"))
data$momadmitsymp_va___11.factor = factor(data$momadmitsymp_va___11,levels=c("0","1"))
data$momadmitsymp_va___12.factor = factor(data$momadmitsymp_va___12,levels=c("0","1"))
data$momadmitsymp_va___13.factor = factor(data$momadmitsymp_va___13,levels=c("0","1"))
data$momadmitsymp_va___14.factor = factor(data$momadmitsymp_va___14,levels=c("0","1"))
data$momadmitsymp_va___15.factor = factor(data$momadmitsymp_va___15,levels=c("0","1"))
data$momadmitsymp_va___16.factor = factor(data$momadmitsymp_va___16,levels=c("0","1"))
data$momadmitsymp_va___17.factor = factor(data$momadmitsymp_va___17,levels=c("0","1"))
data$momadmitsymp_va___18.factor = factor(data$momadmitsymp_va___18,levels=c("0","1"))
data$momadmitsymp_va___19.factor = factor(data$momadmitsymp_va___19,levels=c("0","1"))
data$momadmitsymp_va___20.factor = factor(data$momadmitsymp_va___20,levels=c("0","1"))
data$momadmitsymp_va___21.factor = factor(data$momadmitsymp_va___21,levels=c("0","1"))
data$momadmitsymp_va___98.factor = factor(data$momadmitsymp_va___98,levels=c("0","1"))
data$momadmitsymp_va___99.factor = factor(data$momadmitsymp_va___99,levels=c("0","1"))
data$momadmitcond_va___1.factor = factor(data$momadmitcond_va___1,levels=c("0","1"))
data$momadmitcond_va___2.factor = factor(data$momadmitcond_va___2,levels=c("0","1"))
data$momadmitcond_va___3.factor = factor(data$momadmitcond_va___3,levels=c("0","1"))
data$momadmitcond_va___4.factor = factor(data$momadmitcond_va___4,levels=c("0","1"))
data$momadmitcond_va___5.factor = factor(data$momadmitcond_va___5,levels=c("0","1"))
data$momadmitcond_va___6.factor = factor(data$momadmitcond_va___6,levels=c("0","1"))
data$momadmitcond_va___7.factor = factor(data$momadmitcond_va___7,levels=c("0","1"))
data$momadmitcond_va___8.factor = factor(data$momadmitcond_va___8,levels=c("0","1"))
data$momadmitcond_va___9.factor = factor(data$momadmitcond_va___9,levels=c("0","1"))
data$momadmitcond_va___10.factor = factor(data$momadmitcond_va___10,levels=c("0","1"))
data$momadmitcond_va___11.factor = factor(data$momadmitcond_va___11,levels=c("0","1"))
data$momadmitcond_va___12.factor = factor(data$momadmitcond_va___12,levels=c("0","1"))
data$momadmitcond_va___13.factor = factor(data$momadmitcond_va___13,levels=c("0","1"))
data$momadmitcond_va___14.factor = factor(data$momadmitcond_va___14,levels=c("0","1"))
data$momadmitcond_va___15.factor = factor(data$momadmitcond_va___15,levels=c("0","1"))
data$momadmitcond_va___16.factor = factor(data$momadmitcond_va___16,levels=c("0","1"))
data$momadmitcond_va___17.factor = factor(data$momadmitcond_va___17,levels=c("0","1"))
data$momadmitcond_va___18.factor = factor(data$momadmitcond_va___18,levels=c("0","1"))
data$momadmitcond_va___98.factor = factor(data$momadmitcond_va___98,levels=c("0","1"))
data$momadmitcond_va___99.factor = factor(data$momadmitcond_va___99,levels=c("0","1"))
data$momseek_va.factor = factor(data$momseek_va,levels=c("1","0"))
data$momseekpathway_va___1.factor = factor(data$momseekpathway_va___1,levels=c("0","1"))
data$momseekpathway_va___2.factor = factor(data$momseekpathway_va___2,levels=c("0","1"))
data$momseekpathway_va___3.factor = factor(data$momseekpathway_va___3,levels=c("0","1"))
data$momseekpathway_va___98.factor = factor(data$momseekpathway_va___98,levels=c("0","1"))
data$momseeksymp_va___1.factor = factor(data$momseeksymp_va___1,levels=c("0","1"))
data$momseeksymp_va___2.factor = factor(data$momseeksymp_va___2,levels=c("0","1"))
data$momseeksymp_va___3.factor = factor(data$momseeksymp_va___3,levels=c("0","1"))
data$momseeksymp_va___4.factor = factor(data$momseeksymp_va___4,levels=c("0","1"))
data$momseeksymp_va___5.factor = factor(data$momseeksymp_va___5,levels=c("0","1"))
data$momseeksymp_va___6.factor = factor(data$momseeksymp_va___6,levels=c("0","1"))
data$momseeksymp_va___7.factor = factor(data$momseeksymp_va___7,levels=c("0","1"))
data$momseeksymp_va___8.factor = factor(data$momseeksymp_va___8,levels=c("0","1"))
data$momseeksymp_va___9.factor = factor(data$momseeksymp_va___9,levels=c("0","1"))
data$momseeksymp_va___10.factor = factor(data$momseeksymp_va___10,levels=c("0","1"))
data$momseeksymp_va___11.factor = factor(data$momseeksymp_va___11,levels=c("0","1"))
data$momseeksymp_va___12.factor = factor(data$momseeksymp_va___12,levels=c("0","1"))
data$momseeksymp_va___13.factor = factor(data$momseeksymp_va___13,levels=c("0","1"))
data$momseeksymp_va___14.factor = factor(data$momseeksymp_va___14,levels=c("0","1"))
data$momseeksymp_va___15.factor = factor(data$momseeksymp_va___15,levels=c("0","1"))
data$momseeksymp_va___16.factor = factor(data$momseeksymp_va___16,levels=c("0","1"))
data$momseeksymp_va___17.factor = factor(data$momseeksymp_va___17,levels=c("0","1"))
data$momseeksymp_va___18.factor = factor(data$momseeksymp_va___18,levels=c("0","1"))
data$momseeksymp_va___19.factor = factor(data$momseeksymp_va___19,levels=c("0","1"))
data$momseeksymp_va___20.factor = factor(data$momseeksymp_va___20,levels=c("0","1"))
data$momseeksymp_va___21.factor = factor(data$momseeksymp_va___21,levels=c("0","1"))
data$momseeksymp_va___98.factor = factor(data$momseeksymp_va___98,levels=c("0","1"))
data$momseeksymp_va___99.factor = factor(data$momseeksymp_va___99,levels=c("0","1"))
data$momseekcond_va___1.factor = factor(data$momseekcond_va___1,levels=c("0","1"))
data$momseekcond_va___2.factor = factor(data$momseekcond_va___2,levels=c("0","1"))
data$momseekcond_va___3.factor = factor(data$momseekcond_va___3,levels=c("0","1"))
data$momseekcond_va___4.factor = factor(data$momseekcond_va___4,levels=c("0","1"))
data$momseekcond_va___5.factor = factor(data$momseekcond_va___5,levels=c("0","1"))
data$momseekcond_va___6.factor = factor(data$momseekcond_va___6,levels=c("0","1"))
data$momseekcond_va___7.factor = factor(data$momseekcond_va___7,levels=c("0","1"))
data$momseekcond_va___8.factor = factor(data$momseekcond_va___8,levels=c("0","1"))
data$momseekcond_va___9.factor = factor(data$momseekcond_va___9,levels=c("0","1"))
data$momseekcond_va___10.factor = factor(data$momseekcond_va___10,levels=c("0","1"))
data$momseekcond_va___11.factor = factor(data$momseekcond_va___11,levels=c("0","1"))
data$momseekcond_va___12.factor = factor(data$momseekcond_va___12,levels=c("0","1"))
data$momseekcond_va___13.factor = factor(data$momseekcond_va___13,levels=c("0","1"))
data$momseekcond_va___14.factor = factor(data$momseekcond_va___14,levels=c("0","1"))
data$momseekcond_va___15.factor = factor(data$momseekcond_va___15,levels=c("0","1"))
data$momseekcond_va___16.factor = factor(data$momseekcond_va___16,levels=c("0","1"))
data$momseekcond_va___17.factor = factor(data$momseekcond_va___17,levels=c("0","1"))
data$momseekcond_va___18.factor = factor(data$momseekcond_va___18,levels=c("0","1"))
data$momseekcond_va___98.factor = factor(data$momseekcond_va___98,levels=c("0","1"))
data$momseekcond_va___99.factor = factor(data$momseekcond_va___99,levels=c("0","1"))
data$id10077_m.factor = factor(data$id10077_m,levels=c("1","0","99","88"))
data$id10079_m.factor = factor(data$id10079_m,levels=c("1","0","99","88"))
data$id10080_m.factor = factor(data$id10080_m,levels=c("1","2","3","4","5","6","99","88"))
data$id10081_m.factor = factor(data$id10081_m,levels=c("1","2","3","4","5","6","7","99","88"))
data$id10082_m.factor = factor(data$id10082_m,levels=c("1","0","99","88"))
data$id10083_m.factor = factor(data$id10083_m,levels=c("1","0","99","88"))
data$id10084_m.factor = factor(data$id10084_m,levels=c("1","0","99","88"))
data$id10085_m.factor = factor(data$id10085_m,levels=c("1","0","99","88"))
data$id10086_m.factor = factor(data$id10086_m,levels=c("1","0","99","88"))
data$id10087_m.factor = factor(data$id10087_m,levels=c("1","0","99","88"))
data$id10088_m.factor = factor(data$id10088_m,levels=c("1","2","3","4","99","88"))
data$id10089_m.factor = factor(data$id10089_m,levels=c("1","0","99","88"))
data$id10090_m.factor = factor(data$id10090_m,levels=c("1","0","99","88"))
data$id10091_m.factor = factor(data$id10091_m,levels=c("1","0","99","88"))
data$id10092_m.factor = factor(data$id10092_m,levels=c("1","0","99","88"))
data$id10093_m.factor = factor(data$id10093_m,levels=c("1","0","99","88"))
data$id10094_m.factor = factor(data$id10094_m,levels=c("1","0","99","88"))
data$id10095_m.factor = factor(data$id10095_m,levels=c("1","0","99","88"))
data$id10096_m.factor = factor(data$id10096_m,levels=c("1","0","99","88"))
data$id10097_m.factor = factor(data$id10097_m,levels=c("1","0","99","88"))
data$id10098_m.factor = factor(data$id10098_m,levels=c("1","0","99","88"))
data$id10099_m.factor = factor(data$id10099_m,levels=c("1","0","99","88"))
data$id10100_m.factor = factor(data$id10100_m,levels=c("1","0","99","88"))
data$id10123_m.factor = factor(data$id10123_m,levels=c("1","0","99","88"))
data$id10125_m.factor = factor(data$id10125_m,levels=c("1","0","99","88"))
data$id10126_m.factor = factor(data$id10126_m,levels=c("1","0","99","88"))
data$id10127_m.factor = factor(data$id10127_m,levels=c("1","0","99","88"))
data$id10128_m.factor = factor(data$id10128_m,levels=c("1","0","99","88"))
data$id10129_m.factor = factor(data$id10129_m,levels=c("1","0","99","88"))
data$id10130_m.factor = factor(data$id10130_m,levels=c("1","0","99","88"))
data$id10131_m.factor = factor(data$id10131_m,levels=c("1","0","99","88"))
data$id10132_m.factor = factor(data$id10132_m,levels=c("1","0","99","88"))
data$id10133_m.factor = factor(data$id10133_m,levels=c("1","0","99","88"))
data$id10134_m.factor = factor(data$id10134_m,levels=c("1","0","99","88"))
data$id10135_m.factor = factor(data$id10135_m,levels=c("1","0","99","88"))
data$id10136_m.factor = factor(data$id10136_m,levels=c("1","0","99","88"))
data$id10137_m.factor = factor(data$id10137_m,levels=c("1","0","99","88"))
data$id10138_m.factor = factor(data$id10138_m,levels=c("1","0","99","88"))
data$id10139_m.factor = factor(data$id10139_m,levels=c("1","0","99","88"))
data$id10140_m.factor = factor(data$id10140_m,levels=c("1","0","99","88"))
data$id10141_m.factor = factor(data$id10141_m,levels=c("1","0","99","88"))
data$id10142_m.factor = factor(data$id10142_m,levels=c("1","0","99","88"))
data$id10143_m.factor = factor(data$id10143_m,levels=c("1","0","99","88"))
data$id10144_m.factor = factor(data$id10144_m,levels=c("1","0","99","88"))
data$id10482_m.factor = factor(data$id10482_m,levels=c("1","0","99","88"))
data$id10483_m.factor = factor(data$id10483_m,levels=c("1","0","99","88"))
data$id10484_m.factor = factor(data$id10484_m,levels=c("1","2","3","99","88"))
data$id10147_m.factor = factor(data$id10147_m,levels=c("1","0","99","88"))
data$id10149_m.factor = factor(data$id10149_m,levels=c("1","0","99","88"))
data$id10150_m.factor = factor(data$id10150_m,levels=c("1","2","3","99","88"))
data$id10151_m.factor = factor(data$id10151_m,levels=c("1","2","3","99","88"))
data$id10152_m.factor = factor(data$id10152_m,levels=c("1","0","99","88"))
data$id10153_m.factor = factor(data$id10153_m,levels=c("1","0","99","88"))
data$id10155_m.factor = factor(data$id10155_m,levels=c("1","0","99","88"))
data$id10156_m.factor = factor(data$id10156_m,levels=c("1","0","99","88"))
data$id10157_m.factor = factor(data$id10157_m,levels=c("1","0","99","88"))
data$id10159_m.factor = factor(data$id10159_m,levels=c("1","0","99","88"))
data$id10165_m.factor = factor(data$id10165_m,levels=c("1","2","99","88"))
data$id10166_m.factor = factor(data$id10166_m,levels=c("1","0","99","88"))
data$id10168_m.factor = factor(data$id10168_m,levels=c("1","0","99","88"))
data$id10170_m.factor = factor(data$id10170_m,levels=c("1","0","99","88"))
data$id10171_m.factor = factor(data$id10171_m,levels=c("1","0","99","88"))
data$id10173_m.factor = factor(data$id10173_m,levels=c("1","0","99","88"))
data$id10174_m.factor = factor(data$id10174_m,levels=c("1","0","99","88"))
data$id10175_m.factor = factor(data$id10175_m,levels=c("1","0","99","88"))
data$id10181_m.factor = factor(data$id10181_m,levels=c("1","0","99","88"))
data$id10186_m.factor = factor(data$id10186_m,levels=c("1","0","99","88"))
data$id10187_m.factor = factor(data$id10187_m,levels=c("1","0","99","88"))
data$id10188_m.factor = factor(data$id10188_m,levels=c("1","0","99","88"))
data$id10189_m.factor = factor(data$id10189_m,levels=c("1","0","99","88"))
data$id10192_m.factor = factor(data$id10192_m,levels=c("1","0","99","88"))
data$id10193_m.factor = factor(data$id10193_m,levels=c("1","0","99","88"))
data$id10194_m.factor = factor(data$id10194_m,levels=c("1","0","99","88"))
data$id10195_m.factor = factor(data$id10195_m,levels=c("1","0","99","88"))
data$id10199_m.factor = factor(data$id10199_m,levels=c("1","2","3","99","88"))
data$id10200_m.factor = factor(data$id10200_m,levels=c("1","0","99","88"))
data$id10203_m.factor = factor(data$id10203_m,levels=c("1","2","99","88"))
data$id10204_m.factor = factor(data$id10204_m,levels=c("1","0","99","88"))
data$id10207_m.factor = factor(data$id10207_m,levels=c("1","0","99","88"))
data$id10208_m.factor = factor(data$id10208_m,levels=c("1","0","99","88"))
data$id10210_m.factor = factor(data$id10210_m,levels=c("1","0","99","88"))
data$id10212_m.factor = factor(data$id10212_m,levels=c("1","0","99","88"))
data$id10214_m.factor = factor(data$id10214_m,levels=c("1","0","99","88"))
data$id10215_m.factor = factor(data$id10215_m,levels=c("1","0","99","88"))
data$id10217_m.factor = factor(data$id10217_m,levels=c("1","0","99","88"))
data$id10218_m.factor = factor(data$id10218_m,levels=c("1","0","99","88"))
data$id10219_m.factor = factor(data$id10219_m,levels=c("1","0","99","88"))
data$id10222_m.factor = factor(data$id10222_m,levels=c("1","0","99","88"))
data$id10223_m.factor = factor(data$id10223_m,levels=c("1","0","99","88"))
data$id10225_m.factor = factor(data$id10225_m,levels=c("1","0","99","88"))
data$id10226_m.factor = factor(data$id10226_m,levels=c("1","0","99","88"))
data$id10224_m.factor = factor(data$id10224_m,levels=c("1","0","99","88"))
data$id10227_m.factor = factor(data$id10227_m,levels=c("1","0","99","88"))
data$id10228_m.factor = factor(data$id10228_m,levels=c("1","0","99","88"))
data$id10229_m.factor = factor(data$id10229_m,levels=c("1","0","99","88"))
data$id10230_m.factor = factor(data$id10230_m,levels=c("1","0","99","88"))
data$id10231_m.factor = factor(data$id10231_m,levels=c("1","0","99","88"))
data$id10233_m.factor = factor(data$id10233_m,levels=c("1","0","99","88"))
data$id10235_m___1.factor = factor(data$id10235_m___1,levels=c("0","1"))
data$id10235_m___2.factor = factor(data$id10235_m___2,levels=c("0","1"))
data$id10235_m___3.factor = factor(data$id10235_m___3,levels=c("0","1"))
data$id10235_m___4.factor = factor(data$id10235_m___4,levels=c("0","1"))
data$id10236_m.factor = factor(data$id10236_m,levels=c("1","0","99","88"))
data$id10237_m.factor = factor(data$id10237_m,levels=c("1","0","99","88"))
data$id10238_m.factor = factor(data$id10238_m,levels=c("1","0","99","88"))
data$id10241_m.factor = factor(data$id10241_m,levels=c("1","0","99","88"))
data$id10242_m.factor = factor(data$id10242_m,levels=c("1","0","99","88"))
data$id10243_m.factor = factor(data$id10243_m,levels=c("1","0","99","88"))
data$id10244_m.factor = factor(data$id10244_m,levels=c("1","0","99","88"))
data$id10245_m.factor = factor(data$id10245_m,levels=c("1","0","99","88"))
data$id10246_m.factor = factor(data$id10246_m,levels=c("1","0","99","88"))
data$id10247_m.factor = factor(data$id10247_m,levels=c("1","0","99","88"))
data$id10249_m.factor = factor(data$id10249_m,levels=c("1","0","99","88"))
data$id10251_m.factor = factor(data$id10251_m,levels=c("1","0","99","88"))
data$id10252_m.factor = factor(data$id10252_m,levels=c("1","0","99","88"))
data$id10253_m.factor = factor(data$id10253_m,levels=c("1","0","99","88"))
data$id10254_m.factor = factor(data$id10254_m,levels=c("1","0","99","88"))
data$id10255_m.factor = factor(data$id10255_m,levels=c("1","0","99","88"))
data$id10256_m.factor = factor(data$id10256_m,levels=c("1","0","99","88"))
data$id10257_m.factor = factor(data$id10257_m,levels=c("1","0","99","88"))
data$id10258_m.factor = factor(data$id10258_m,levels=c("1","0","99","88"))
data$id10259_m.factor = factor(data$id10259_m,levels=c("1","0","99","88"))
data$id10260_m___1.factor = factor(data$id10260_m___1,levels=c("0","1"))
data$id10260_m___2.factor = factor(data$id10260_m___2,levels=c("0","1"))
data$id10260_m___3.factor = factor(data$id10260_m___3,levels=c("0","1"))
data$id10260_m___4.factor = factor(data$id10260_m___4,levels=c("0","1"))
data$id10260_m___5.factor = factor(data$id10260_m___5,levels=c("0","1"))
data$id10260_m___6.factor = factor(data$id10260_m___6,levels=c("0","1"))
data$id10260_m___7.factor = factor(data$id10260_m___7,levels=c("0","1"))
data$id10260_m___8.factor = factor(data$id10260_m___8,levels=c("0","1"))
data$id10261_m.factor = factor(data$id10261_m,levels=c("1","0","99","88"))
data$id10263_m.factor = factor(data$id10263_m,levels=c("1","2","3","99","88"))
data$id10264_m.factor = factor(data$id10264_m,levels=c("1","0","99","88"))
data$id10265_m.factor = factor(data$id10265_m,levels=c("1","0","99","88"))
data$id10267_m.factor = factor(data$id10267_m,levels=c("1","0","99","88"))
data$id10268_m.factor = factor(data$id10268_m,levels=c("1","0","99","88"))
data$id10270_m.factor = factor(data$id10270_m,levels=c("1","0","99","88"))
data$id10486_m.factor = factor(data$id10486_m,levels=c("1","0","99","88"))
data$id10485_m.factor = factor(data$id10485_m,levels=c("1","0","99","88"))
data$id10294_m.factor = factor(data$id10294_m,levels=c("1","0","99","88"))
data$id10295_m.factor = factor(data$id10295_m,levels=c("1","0","99","88"))
data$id10296_m.factor = factor(data$id10296_m,levels=c("1","0","99","88"))
data$id10297_m.factor = factor(data$id10297_m,levels=c("1","0","99","88"))
data$id10298_m.factor = factor(data$id10298_m,levels=c("1","0","99","88"))
data$id10301_m.factor = factor(data$id10301_m,levels=c("1","0","99","88"))
data$id10299_m.factor = factor(data$id10299_m,levels=c("1","0","99","88"))
data$id10302_m.factor = factor(data$id10302_m,levels=c("1","0","99","88"))
data$id10300_m.factor = factor(data$id10300_m,levels=c("1","0","99","88"))
data$id10304_m.factor = factor(data$id10304_m,levels=c("1","0","99","88"))
data$id10305_m.factor = factor(data$id10305_m,levels=c("1","0","99","88"))
data$id10306_m.factor = factor(data$id10306_m,levels=c("1","0","99","88"))
data$id10307_m.factor = factor(data$id10307_m,levels=c("1","0","99","88"))
data$id10312_m.factor = factor(data$id10312_m,levels=c("1","0","99","88"))
data$id10313_m.factor = factor(data$id10313_m,levels=c("1","0","99","88"))
data$id10314_m.factor = factor(data$id10314_m,levels=c("1","0","99","88"))
data$id10315_m.factor = factor(data$id10315_m,levels=c("1","0","99","88"))
data$id10316_m.factor = factor(data$id10316_m,levels=c("1","0","99","88"))
data$id10317_m.factor = factor(data$id10317_m,levels=c("1","0","99","88"))
data$id10318_m.factor = factor(data$id10318_m,levels=c("1","0","99","88"))
data$id10320_m.factor = factor(data$id10320_m,levels=c("1","0","99","88"))
data$id10321_m.factor = factor(data$id10321_m,levels=c("1","0","99","88"))
data$id10322_m.factor = factor(data$id10322_m,levels=c("1","0","99","88"))
data$id10323_m.factor = factor(data$id10323_m,levels=c("1","0","99","88"))
data$id10324_m.factor = factor(data$id10324_m,levels=c("1","0","99","88"))
data$id10325_m.factor = factor(data$id10325_m,levels=c("1","0","99","88"))
data$id10326_m.factor = factor(data$id10326_m,levels=c("1","0","99","88"))
data$id10327_m.factor = factor(data$id10327_m,levels=c("1","0","99","88"))
data$id10328_m.factor = factor(data$id10328_m,levels=c("1","0","99","88"))
data$id10329_m.factor = factor(data$id10329_m,levels=c("1","0","99","88"))
data$id10330_m.factor = factor(data$id10330_m,levels=c("1","0","99","88"))
data$id10331_m.factor = factor(data$id10331_m,levels=c("1","0","99","88"))
data$id10333_m.factor = factor(data$id10333_m,levels=c("1","0","99","88"))
data$id10334_m.factor = factor(data$id10334_m,levels=c("1","0","99","88"))
data$id10335_m.factor = factor(data$id10335_m,levels=c("1","0","99","88"))
data$id10336_m.factor = factor(data$id10336_m,levels=c("1","0","99","88"))
data$id10337_m.factor = factor(data$id10337_m,levels=c("1","2","3","4","5","99","88"))
data$id10338_m.factor = factor(data$id10338_m,levels=c("1","0","99","88"))
data$id10339_m.factor = factor(data$id10339_m,levels=c("1","2","3","4","5","6","7","99","88"))
data$id10342_m.factor = factor(data$id10342_m,levels=c("1","0","99","88"))
data$id10343_m.factor = factor(data$id10343_m,levels=c("1","0","99","88"))
data$id10344_m.factor = factor(data$id10344_m,levels=c("1","0","99","88"))
data$id10347_m.factor = factor(data$id10347_m,levels=c("1","0","99","88"))
data$id10340_m.factor = factor(data$id10340_m,levels=c("1","0","99","88"))
data$id10411_m.factor = factor(data$id10411_m,levels=c("1","0","99","88"))
data$id10412_m.factor = factor(data$id10412_m,levels=c("1","0","99","88"))
data$id10413_m.factor = factor(data$id10413_m,levels=c("1","0","99","88"))
data$id10414_m___1.factor = factor(data$id10414_m___1,levels=c("0","1"))
data$id10414_m___2.factor = factor(data$id10414_m___2,levels=c("0","1"))
data$id10414_m___3.factor = factor(data$id10414_m___3,levels=c("0","1"))
data$id10414_m___4.factor = factor(data$id10414_m___4,levels=c("0","1"))
data$id10414_m___5.factor = factor(data$id10414_m___5,levels=c("0","1"))
data$id10414_m___99.factor = factor(data$id10414_m___99,levels=c("0","1"))
data$id10414_m___88.factor = factor(data$id10414_m___88,levels=c("0","1"))
data$id10418_m.factor = factor(data$id10418_m,levels=c("1","0","99","88"))
data$id10419_m.factor = factor(data$id10419_m,levels=c("1","0","99","88"))
data$id10420_m.factor = factor(data$id10420_m,levels=c("1","0","99","88"))
data$id10421_m.factor = factor(data$id10421_m,levels=c("1","0","99","88"))
data$id10422_m.factor = factor(data$id10422_m,levels=c("1","0","99","88"))
data$id10423_m.factor = factor(data$id10423_m,levels=c("1","0","99","88"))
data$id10424_m.factor = factor(data$id10424_m,levels=c("1","0","99","88"))
data$id10425_m.factor = factor(data$id10425_m,levels=c("1","0","99","88"))
data$id10426_m.factor = factor(data$id10426_m,levels=c("1","0","99","88"))
data$id10427_m.factor = factor(data$id10427_m,levels=c("1","0","99","88"))
data$id10432_m.factor = factor(data$id10432_m,levels=c("1","0","99","88"))
data$id10433_m___1.factor = factor(data$id10433_m___1,levels=c("0","1"))
data$id10433_m___2.factor = factor(data$id10433_m___2,levels=c("0","1"))
data$id10433_m___3.factor = factor(data$id10433_m___3,levels=c("0","1"))
data$id10433_m___4.factor = factor(data$id10433_m___4,levels=c("0","1"))
data$id10433_m___5.factor = factor(data$id10433_m___5,levels=c("0","1"))
data$id10433_m___6.factor = factor(data$id10433_m___6,levels=c("0","1"))
data$id10433_m___7.factor = factor(data$id10433_m___7,levels=c("0","1"))
data$id10433_m___8.factor = factor(data$id10433_m___8,levels=c("0","1"))
data$id10433_m___9.factor = factor(data$id10433_m___9,levels=c("0","1"))
data$id10433_m___10.factor = factor(data$id10433_m___10,levels=c("0","1"))
data$id10433_m___11.factor = factor(data$id10433_m___11,levels=c("0","1"))
data$id10433_m___99.factor = factor(data$id10433_m___99,levels=c("0","1"))
data$id10433_m___88.factor = factor(data$id10433_m___88,levels=c("0","1"))
data$id10435_m.factor = factor(data$id10435_m,levels=c("1","0","99","88"))
data$id10437_m.factor = factor(data$id10437_m,levels=c("1","0","99","88"))
data$id10438_m.factor = factor(data$id10438_m,levels=c("1","0","99","88"))
data$id10439_check_m.factor = factor(data$id10439_check_m,levels=c("1","0"))
data$id10440_check_m.factor = factor(data$id10440_check_m,levels=c("1","0"))
data$id10441_check_m.factor = factor(data$id10441_check_m,levels=c("1","0"))
data$id10450_m.factor = factor(data$id10450_m,levels=c("1","0","99","88"))
data$id10451_m.factor = factor(data$id10451_m,levels=c("1","0","99","88"))
data$id10452_m.factor = factor(data$id10452_m,levels=c("1","0","99","88"))
data$id10453_m.factor = factor(data$id10453_m,levels=c("1","0","99","88"))
data$id10454_m.factor = factor(data$id10454_m,levels=c("1","0","99","88"))
data$id10455_m.factor = factor(data$id10455_m,levels=c("1","0","99","88"))
data$id10456_m.factor = factor(data$id10456_m,levels=c("1","0","99","88"))
data$id10457_m.factor = factor(data$id10457_m,levels=c("1","0","99","88"))
data$id10458_m.factor = factor(data$id10458_m,levels=c("1","0","99","88"))
data$id10459_m.factor = factor(data$id10459_m,levels=c("1","0","99","88"))
data$id10477_m___1.factor = factor(data$id10477_m___1,levels=c("0","1"))
data$id10477_m___2.factor = factor(data$id10477_m___2,levels=c("0","1"))
data$id10477_m___3.factor = factor(data$id10477_m___3,levels=c("0","1"))
data$id10477_m___4.factor = factor(data$id10477_m___4,levels=c("0","1"))
data$id10477_m___5.factor = factor(data$id10477_m___5,levels=c("0","1"))
data$id10477_m___6.factor = factor(data$id10477_m___6,levels=c("0","1"))
data$id10477_m___7.factor = factor(data$id10477_m___7,levels=c("0","1"))
data$id10477_m___8.factor = factor(data$id10477_m___8,levels=c("0","1"))
data$id10477_m___9.factor = factor(data$id10477_m___9,levels=c("0","1"))
data$id10477_m___10.factor = factor(data$id10477_m___10,levels=c("0","1"))
data$id10477_m___11.factor = factor(data$id10477_m___11,levels=c("0","1"))
data$id10477_m___12.factor = factor(data$id10477_m___12,levels=c("0","1"))
data$id10477_m___99.factor = factor(data$id10477_m___99,levels=c("0","1"))
data$verbal_autopsy_mother_complete.factor = factor(data$verbal_autopsy_mother_complete,levels=c("0","1","2"))
data$id10007.factor = factor(data$id10007,levels=c("1","2","3"))
data$id10008.factor = factor(data$id10008,levels=c("1","2","3","4","5","6","7","8","88"))
data$id10009.factor = factor(data$id10009,levels=c("1","0","99","88"))
data$id10013.factor = factor(data$id10013,levels=c("1","0"))
data$id10487.factor = factor(data$id10487,levels=c("1","0","99","88"))
data$id10488.factor = factor(data$id10488,levels=c("1","0","99","88"))
data$babeadmit_va.factor = factor(data$babeadmit_va,levels=c("1","0"))
data$numbabeadmit_va.factor = factor(data$numbabeadmit_va,levels=c("1","2","3","4"))
data$babeadmitpathway_va___1.factor = factor(data$babeadmitpathway_va___1,levels=c("0","1"))
data$babeadmitpathway_va___2.factor = factor(data$babeadmitpathway_va___2,levels=c("0","1"))
data$babeadmitpathway_va___3.factor = factor(data$babeadmitpathway_va___3,levels=c("0","1"))
data$babeadmitpathway_va___98.factor = factor(data$babeadmitpathway_va___98,levels=c("0","1"))
data$babeadmitsymp_va___1.factor = factor(data$babeadmitsymp_va___1,levels=c("0","1"))
data$babeadmitsymp_va___2.factor = factor(data$babeadmitsymp_va___2,levels=c("0","1"))
data$babeadmitsymp_va___3.factor = factor(data$babeadmitsymp_va___3,levels=c("0","1"))
data$babeadmitsymp_va___4.factor = factor(data$babeadmitsymp_va___4,levels=c("0","1"))
data$babeadmitsymp_va___5.factor = factor(data$babeadmitsymp_va___5,levels=c("0","1"))
data$babeadmitsymp_va___6.factor = factor(data$babeadmitsymp_va___6,levels=c("0","1"))
data$babeadmitsymp_va___7.factor = factor(data$babeadmitsymp_va___7,levels=c("0","1"))
data$babeadmitsymp_va___8.factor = factor(data$babeadmitsymp_va___8,levels=c("0","1"))
data$babeadmitsymp_va___9.factor = factor(data$babeadmitsymp_va___9,levels=c("0","1"))
data$babeadmitsymp_va___10.factor = factor(data$babeadmitsymp_va___10,levels=c("0","1"))
data$babeadmitsymp_va___11.factor = factor(data$babeadmitsymp_va___11,levels=c("0","1"))
data$babeadmitsymp_va___12.factor = factor(data$babeadmitsymp_va___12,levels=c("0","1"))
data$babeadmitsymp_va___13.factor = factor(data$babeadmitsymp_va___13,levels=c("0","1"))
data$babeadmitsymp_va___14.factor = factor(data$babeadmitsymp_va___14,levels=c("0","1"))
data$babeadmitsymp_va___15.factor = factor(data$babeadmitsymp_va___15,levels=c("0","1"))
data$babeadmitsymp_va___16.factor = factor(data$babeadmitsymp_va___16,levels=c("0","1"))
data$babeadmitsymp_va___17.factor = factor(data$babeadmitsymp_va___17,levels=c("0","1"))
data$babeadmitsymp_va___18.factor = factor(data$babeadmitsymp_va___18,levels=c("0","1"))
data$babeadmitsymp_va___98.factor = factor(data$babeadmitsymp_va___98,levels=c("0","1"))
data$babeadmitsymp_va___99.factor = factor(data$babeadmitsymp_va___99,levels=c("0","1"))
data$transfusion_va.factor = factor(data$transfusion_va,levels=c("1","0"))
data$babeadmitcond_va___1.factor = factor(data$babeadmitcond_va___1,levels=c("0","1"))
data$babeadmitcond_va___2.factor = factor(data$babeadmitcond_va___2,levels=c("0","1"))
data$babeadmitcond_va___3.factor = factor(data$babeadmitcond_va___3,levels=c("0","1"))
data$babeadmitcond_va___4.factor = factor(data$babeadmitcond_va___4,levels=c("0","1"))
data$babeadmitcond_va___5.factor = factor(data$babeadmitcond_va___5,levels=c("0","1"))
data$babeadmitcond_va___6.factor = factor(data$babeadmitcond_va___6,levels=c("0","1"))
data$babeadmitcond_va___7.factor = factor(data$babeadmitcond_va___7,levels=c("0","1"))
data$babeadmitcond_va___8.factor = factor(data$babeadmitcond_va___8,levels=c("0","1"))
data$babeadmitcond_va___9.factor = factor(data$babeadmitcond_va___9,levels=c("0","1"))
data$babeadmitcond_va___10.factor = factor(data$babeadmitcond_va___10,levels=c("0","1"))
data$babeadmitcond_va___11.factor = factor(data$babeadmitcond_va___11,levels=c("0","1"))
data$babeadmitcond_va___98.factor = factor(data$babeadmitcond_va___98,levels=c("0","1"))
data$babeadmitcond_va___99.factor = factor(data$babeadmitcond_va___99,levels=c("0","1"))
data$matsymp_va.factor = factor(data$matsymp_va,levels=c("1","0"))
data$babefeed_va.factor = factor(data$babefeed_va,levels=c("1","0"))
data$babeseek_va.factor = factor(data$babeseek_va,levels=c("1","0"))
data$babeseekpathway_va___1.factor = factor(data$babeseekpathway_va___1,levels=c("0","1"))
data$babeseekpathway_va___2.factor = factor(data$babeseekpathway_va___2,levels=c("0","1"))
data$babeseekpathway_va___3.factor = factor(data$babeseekpathway_va___3,levels=c("0","1"))
data$babeseekpathway_va___98.factor = factor(data$babeseekpathway_va___98,levels=c("0","1"))
data$id10077.factor = factor(data$id10077,levels=c("1","0","99","88"))
data$id10079.factor = factor(data$id10079,levels=c("1","0","99","88"))
data$id10081.factor = factor(data$id10081,levels=c("1","2","3","4","5","6","7","99","88"))
data$id10082.factor = factor(data$id10082,levels=c("1","0","99","88"))
data$id10083.factor = factor(data$id10083,levels=c("1","0","99","88"))
data$id10084.factor = factor(data$id10084,levels=c("1","0","99","88"))
data$id10085.factor = factor(data$id10085,levels=c("1","0","99","88"))
data$id10086.factor = factor(data$id10086,levels=c("1","0","99","88"))
data$id10087.factor = factor(data$id10087,levels=c("1","0","99","88"))
data$id10088.factor = factor(data$id10088,levels=c("1","2","3","4","99","88"))
data$id10089.factor = factor(data$id10089,levels=c("1","0","99","88"))
data$id10090.factor = factor(data$id10090,levels=c("1","0","99","88"))
data$id10092.factor = factor(data$id10092,levels=c("1","0","99","88"))
data$id10093.factor = factor(data$id10093,levels=c("1","0","99","88"))
data$id10094.factor = factor(data$id10094,levels=c("1","0","99","88"))
data$id10095.factor = factor(data$id10095,levels=c("1","0","99","88"))
data$id10096.factor = factor(data$id10096,levels=c("1","0","99","88"))
data$id10097.factor = factor(data$id10097,levels=c("1","0","99","88"))
data$id10098.factor = factor(data$id10098,levels=c("1","0","99","88"))
data$id10100.factor = factor(data$id10100,levels=c("1","0","99","88"))
data$id10408.factor = factor(data$id10408,levels=c("1","0","99","88"))
data$id10123.factor = factor(data$id10123,levels=c("1","0","99","88"))
data$id10125.factor = factor(data$id10125,levels=c("1","0","99","88"))
data$id10128.factor = factor(data$id10128,levels=c("1","0","99","88"))
data$id10129.factor = factor(data$id10129,levels=c("1","0","99","88"))
data$id10131.factor = factor(data$id10131,levels=c("1","0","99","88"))
data$id10133.factor = factor(data$id10133,levels=c("1","0","99","88"))
data$id10134.factor = factor(data$id10134,levels=c("1","0","99","88"))
data$id10135.factor = factor(data$id10135,levels=c("1","0","99","88"))
data$id10136.factor = factor(data$id10136,levels=c("1","0","99","88"))
data$id10137.factor = factor(data$id10137,levels=c("1","0","99","88"))
data$id10142.factor = factor(data$id10142,levels=c("1","0","99","88"))
data$id10143.factor = factor(data$id10143,levels=c("1","0","99","88"))
data$id10144.factor = factor(data$id10144,levels=c("1","0","99","88"))
data$id10147.factor = factor(data$id10147,levels=c("1","0","99","88"))
data$id10147_measure.factor = factor(data$id10147_measure,levels=c("1","0"))
data$id10147_temp.factor = factor(data$id10147_temp,levels=c("1","0"))
data$id10149.factor = factor(data$id10149,levels=c("1","0","99","88"))
data$id10150.factor = factor(data$id10150,levels=c("1","2","3","99","88"))
data$id10151.factor = factor(data$id10151,levels=c("1","2","3","99","88"))
data$id10152.factor = factor(data$id10152,levels=c("1","0","99","88"))
data$id10153.factor = factor(data$id10153,levels=c("1","0","99","88"))
data$id10155.factor = factor(data$id10155,levels=c("1","0","99","88"))
data$id10156.factor = factor(data$id10156,levels=c("1","0","99","88"))
data$id10157.factor = factor(data$id10157,levels=c("1","0","99","88"))
data$id10158.factor = factor(data$id10158,levels=c("1","0","99","88"))
data$id10159.factor = factor(data$id10159,levels=c("1","0","99","88"))
data$id10165.factor = factor(data$id10165,levels=c("1","2","99","88"))
data$id10166.factor = factor(data$id10166,levels=c("1","0","99","88"))
data$id10168.factor = factor(data$id10168,levels=c("1","0","99","88"))
data$id10172.factor = factor(data$id10172,levels=c("1","0","99","88"))
data$id10173___1.factor = factor(data$id10173___1,levels=c("0","1"))
data$id10173___2.factor = factor(data$id10173___2,levels=c("0","1"))
data$id10173___3.factor = factor(data$id10173___3,levels=c("0","1"))
data$id10173___4.factor = factor(data$id10173___4,levels=c("0","1"))
data$id10173___99.factor = factor(data$id10173___99,levels=c("0","1"))
data$id10173___88.factor = factor(data$id10173___88,levels=c("0","1"))
data$id10181.factor = factor(data$id10181,levels=c("1","0","99","88"))
data$id10185.factor = factor(data$id10185,levels=c("1","0","99","88"))
data$id10186.factor = factor(data$id10186,levels=c("1","0","99","88"))
data$id10187.factor = factor(data$id10187,levels=c("1","0","99","88"))
data$id10188.factor = factor(data$id10188,levels=c("1","0","99","88"))
data$id10194.factor = factor(data$id10194,levels=c("1","0","99","88"))
data$id10188_projectile.factor = factor(data$id10188_projectile,levels=c("1","0","99","88"))
data$id10191.factor = factor(data$id10191,levels=c("1","0","99","88"))
data$id10188_billious.factor = factor(data$id10188_billious,levels=c("1","0","99","88"))
data$id10192.factor = factor(data$id10192,levels=c("1","0","99","88"))
data$id10193.factor = factor(data$id10193,levels=c("1","0","99","88"))
data$id10200.factor = factor(data$id10200,levels=c("1","0","99","88"))
data$id10203.factor = factor(data$id10203,levels=c("1","2","99","88"))
data$id10204.factor = factor(data$id10204,levels=c("1","0","99","88"))
data$id10214.factor = factor(data$id10214,levels=c("1","0","99","88"))
data$id10215.factor = factor(data$id10215,levels=c("1","0","99","88"))
data$id10217.factor = factor(data$id10217,levels=c("1","0","99","88"))
data$id10218.factor = factor(data$id10218,levels=c("1","0","99","88"))
data$id10219.factor = factor(data$id10219,levels=c("1","0","99","88"))
data$id10220.factor = factor(data$id10220,levels=c("1","0","99","88"))
data$id10222.factor = factor(data$id10222,levels=c("1","0","99","88"))
data$id10223.factor = factor(data$id10223,levels=c("1","0","99","88"))
data$id10225.factor = factor(data$id10225,levels=c("1","0","99","88"))
data$id10226.factor = factor(data$id10226,levels=c("1","0","99","88"))
data$id10224.factor = factor(data$id10224,levels=c("1","0","99","88"))
data$id10227.factor = factor(data$id10227,levels=c("1","0","99","88"))
data$id10229.factor = factor(data$id10229,levels=c("1","0","99","88"))
data$id10230.factor = factor(data$id10230,levels=c("1","0","99","88"))
data$id10231.factor = factor(data$id10231,levels=c("1","0","99","88"))
data$id10233.factor = factor(data$id10233,levels=c("1","0","99","88"))
data$id10235___1.factor = factor(data$id10235___1,levels=c("0","1"))
data$id10235___2.factor = factor(data$id10235___2,levels=c("0","1"))
data$id10235___3.factor = factor(data$id10235___3,levels=c("0","1"))
data$id10235___4.factor = factor(data$id10235___4,levels=c("0","1"))
data$id10236.factor = factor(data$id10236,levels=c("1","0","99","88"))
data$id10238.factor = factor(data$id10238,levels=c("1","0","99","88"))
data$id10239.factor = factor(data$id10239,levels=c("1","0","99","88"))
data$id10240.factor = factor(data$id10240,levels=c("1","0","99","88"))
data$id10241.factor = factor(data$id10241,levels=c("1","0","99","88"))
data$id10242.factor = factor(data$id10242,levels=c("1","0","99","88"))
data$id10243.factor = factor(data$id10243,levels=c("1","0","99","88"))
data$id10244.factor = factor(data$id10244,levels=c("1","0","99","88"))
data$id10245.factor = factor(data$id10245,levels=c("1","0","99","88"))
data$id10246.factor = factor(data$id10246,levels=c("1","0","99","88"))
data$id10247.factor = factor(data$id10247,levels=c("1","0","99","88"))
data$id10249.factor = factor(data$id10249,levels=c("1","0","99","88"))
data$id10251.factor = factor(data$id10251,levels=c("1","0","99","88"))
data$id10252.factor = factor(data$id10252,levels=c("1","0","99","88"))
data$id10253.factor = factor(data$id10253,levels=c("1","0","99","88"))
data$id10255.factor = factor(data$id10255,levels=c("1","0","99","88"))
data$id10256.factor = factor(data$id10256,levels=c("1","0","99","88"))
data$id10257.factor = factor(data$id10257,levels=c("1","0","99","88"))
data$id10258.factor = factor(data$id10258,levels=c("1","0","99","88"))
data$id10259.factor = factor(data$id10259,levels=c("1","0","99","88"))
data$id10260___1.factor = factor(data$id10260___1,levels=c("0","1"))
data$id10260___2.factor = factor(data$id10260___2,levels=c("0","1"))
data$id10260___3.factor = factor(data$id10260___3,levels=c("0","1"))
data$id10260___4.factor = factor(data$id10260___4,levels=c("0","1"))
data$id10260___5.factor = factor(data$id10260___5,levels=c("0","1"))
data$id10260___6.factor = factor(data$id10260___6,levels=c("0","1"))
data$id10260___7.factor = factor(data$id10260___7,levels=c("0","1"))
data$id10260___8.factor = factor(data$id10260___8,levels=c("0","1"))
data$id10261.factor = factor(data$id10261,levels=c("1","0","99","88"))
data$id10265.factor = factor(data$id10265,levels=c("1","0","99","88"))
data$id10268.factor = factor(data$id10268,levels=c("1","0","99","88"))
data$id10269.factor = factor(data$id10269,levels=c("1","0","99","88"))
data$id10271.factor = factor(data$id10271,levels=c("1","0","99","88"))
data$id10272.factor = factor(data$id10272,levels=c("1","0","99","88"))
data$id10273.factor = factor(data$id10273,levels=c("1","0","99","88"))
data$id10275.factor = factor(data$id10275,levels=c("1","0","99","88"))
data$id10276.factor = factor(data$id10276,levels=c("1","0","99","88"))
data$id10277.factor = factor(data$id10277,levels=c("1","0","99","88"))
data$id10278.factor = factor(data$id10278,levels=c("1","0","99","88"))
data$id10279.factor = factor(data$id10279,levels=c("1","0","99","88"))
data$id10281.factor = factor(data$id10281,levels=c("1","0","99","88"))
data$id10282.factor = factor(data$id10282,levels=c("1","0","99","88"))
data$id10283.factor = factor(data$id10283,levels=c("1","0","99","88"))
data$id10284.factor = factor(data$id10284,levels=c("1","0","99","88"))
data$id10286.factor = factor(data$id10286,levels=c("1","0","99","88"))
data$id10287.factor = factor(data$id10287,levels=c("1","0","99","88"))
data$id10288.factor = factor(data$id10288,levels=c("1","0","99","88"))
data$id10289.factor = factor(data$id10289,levels=c("1","0","99","88"))
data$id10354.factor = factor(data$id10354,levels=c("1","0","99","88"))
data$id10355.factor = factor(data$id10355,levels=c("1","2","99","88"))
data$id10435.factor = factor(data$id10435,levels=c("1","0","99","88"))
data$id10437.factor = factor(data$id10437,levels=c("1","0","99","88"))
data$id10438.factor = factor(data$id10438,levels=c("1","0","99","88"))
data$id10439_check.factor = factor(data$id10439_check,levels=c("1","0"))
data$id10450.factor = factor(data$id10450,levels=c("1","0","99","88"))
data$id10451.factor = factor(data$id10451,levels=c("1","0","99","88"))
data$id10452.factor = factor(data$id10452,levels=c("1","0","99","88"))
data$id10453.factor = factor(data$id10453,levels=c("1","0","99","88"))
data$id10454.factor = factor(data$id10454,levels=c("1","0","99","88"))
data$id10455.factor = factor(data$id10455,levels=c("1","0","99","88"))
data$id10456.factor = factor(data$id10456,levels=c("1","0","99","88"))
data$id10457.factor = factor(data$id10457,levels=c("1","0","99","88"))
data$id10458.factor = factor(data$id10458,levels=c("1","0","99","88"))
data$id10459.factor = factor(data$id10459,levels=c("1","0","99","88"))
data$id10478___1.factor = factor(data$id10478___1,levels=c("0","1"))
data$id10478___2.factor = factor(data$id10478___2,levels=c("0","1"))
data$id10478___3.factor = factor(data$id10478___3,levels=c("0","1"))
data$id10478___4.factor = factor(data$id10478___4,levels=c("0","1"))
data$id10478___5.factor = factor(data$id10478___5,levels=c("0","1"))
data$id10478___6.factor = factor(data$id10478___6,levels=c("0","1"))
data$id10478___7.factor = factor(data$id10478___7,levels=c("0","1"))
data$id10478___8.factor = factor(data$id10478___8,levels=c("0","1"))
data$id10478___9.factor = factor(data$id10478___9,levels=c("0","1"))
data$id10478___10.factor = factor(data$id10478___10,levels=c("0","1"))
data$id10478___11.factor = factor(data$id10478___11,levels=c("0","1"))
data$id10478___12.factor = factor(data$id10478___12,levels=c("0","1"))
data$id10478___13.factor = factor(data$id10478___13,levels=c("0","1"))
data$id10478___14.factor = factor(data$id10478___14,levels=c("0","1"))
data$id10478___15.factor = factor(data$id10478___15,levels=c("0","1"))
data$id10478___16.factor = factor(data$id10478___16,levels=c("0","1"))
data$id10478___99.factor = factor(data$id10478___99,levels=c("0","1"))
data$verbal_autopsy_newborn_complete.factor = factor(data$verbal_autopsy_newborn_complete,levels=c("0","1","2"))
data$journey_mapping_admission_and_discharge_complete.factor = factor(data$journey_mapping_admission_and_discharge_complete,levels=c("0","1","2"))
data$disch_72.factor = factor(data$disch_72,levels=c("1","0"))
data$exp_72.factor = factor(data$exp_72,levels=c("1","0"))
data$exp2_72.factor = factor(data$exp2_72,levels=c("1","0"))
data$quality_72.factor = factor(data$quality_72,levels=c("1","2","3","4","5","6"))
data$journey_mapping_72_hour_follow_up_complete.factor = factor(data$journey_mapping_72_hour_follow_up_complete,levels=c("0","1","2"))
data$calculated_fields_complete.factor = factor(data$calculated_fields_complete,levels=c("0","1","2"))

levels(data$redcap_repeat_instrument.factor)=c("6. Delivery Neonatal","8. Discharge Interview Neonatal","11. Six Week Follow Up Neonatal","Verbal Autopsy - Newborn")
levels(data$is_pilot_adm.factor)=c("Yes","No")
levels(data$site_adm.factor)=c("MRRH","JRRH")
levels(data$nurse_adm_v2.factor)=c("Olivia","Phionah","Jonan","Annet Happy","Annet Mary","Miria","Maureen","Clare","Kelemensia","Immaculate","Bosco","Enid Kibone","Other")
levels(data$reason_adm_v2.factor)=c("Yes","No")
levels(data$eligibleage_adm_v2.factor)=c("Yes","No")
levels(data$exclusion_adm_v2.factor)=c("No exclusion criteria apply","Not admitted within time cut-off","Labor before 28 weeks","From refugee camp","Language barrier","No method for contact by phone","Discharge before enrollment","Lives outside of catchment area","Admitted from prison","Refused consent")
levels(data$consentform_adm_v2.factor)=c("Yes","No")
levels(data$fetaldemise_adm.factor)=c("Yes","No")
levels(data$dobknown_adm_v2.factor)=c("Yes","No")
levels(data$admission_subject_details_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$time_tohosp_adm.factor)=c("less than 30 minutes","30 minutes - 1 hr","more than 1hrs and up to 2hrs","more than 2hrs and less than 4hrs","4 hours or more")
levels(data$transport_adm.factor)=c("Walk","Motorcycle","Public transport (bus, taxi)","Private transport (special hire, private vehicle)","Ambulance","Other")
levels(data$delay_adm___1.factor)=c("Unchecked","Checked")
levels(data$delay_adm___2.factor)=c("Unchecked","Checked")
levels(data$delay_adm___3.factor)=c("Unchecked","Checked")
levels(data$delay_adm___4.factor)=c("Unchecked","Checked")
levels(data$delay_adm___98.factor)=c("Unchecked","Checked")
levels(data$delay_adm___99.factor)=c("Unchecked","Checked")
levels(data$isreferral_adm.factor)=c("Yes","No")
levels(data$referralsrc_adm.factor)=c("Other Hospital","Health Centre/Clinic","VHT","Untrained Health Worker","Traditional Healer","Traditional Birth Attendant","Other")
levels(data$csection_adm.factor)=c("Yes","No")
levels(data$takevitals_adm.factor)=c("Yes","No")
levels(data$distress_adm.factor)=c("Yes","No","Unsure")
levels(data$admission_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$medhx_adm___1.factor)=c("Unchecked","Checked")
levels(data$medhx_adm___2.factor)=c("Unchecked","Checked")
levels(data$medhx_adm___3.factor)=c("Unchecked","Checked")
levels(data$medhx_adm___4.factor)=c("Unchecked","Checked")
levels(data$medhx_adm___5.factor)=c("Unchecked","Checked")
levels(data$medhx_adm___6.factor)=c("Unchecked","Checked")
levels(data$medhx_adm___7.factor)=c("Unchecked","Checked")
levels(data$medhx_adm___8.factor)=c("Unchecked","Checked")
levels(data$medhx_adm___9.factor)=c("Unchecked","Checked")
levels(data$medhx_adm___98.factor)=c("Unchecked","Checked")
levels(data$medhx_adm___99.factor)=c("Unchecked","Checked")
levels(data$duedate_adm.factor)=c("Yes","No")
levels(data$duedate2_adm.factor)=c("LNMP","Ultrasound","Woman herself")
levels(data$csect_adm.factor)=c("Yes","No")
levels(data$csect2_adm.factor)=c("< 18 months ago","18-36 months ago",">36 months ago")
levels(data$preghx_adm___1.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___2.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___3.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___4.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___5.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___6.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___7.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___8.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___9.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___10.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___11.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___12.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___13.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___14.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___15.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___16.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___17.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___18.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___98.factor)=c("Unchecked","Checked")
levels(data$preghx_adm___99.factor)=c("Unchecked","Checked")
levels(data$placenta_adm___1.factor)=c("Unchecked","Checked")
levels(data$placenta_adm___2.factor)=c("Unchecked","Checked")
levels(data$placenta_adm___97.factor)=c("Unchecked","Checked")
levels(data$placenta_adm___98.factor)=c("Unchecked","Checked")
levels(data$placenta_adm___99.factor)=c("Unchecked","Checked")
levels(data$medhbp_adm.factor)=c("Yes","No","Not sure")
levels(data$medarv_adm.factor)=c("Yes","No","Not sure")
levels(data$covidvax_adm.factor)=c("Yes - Fully vaccinated (completion DURING pregnancy)","Yes - Fully vaccinated (completion BEFORE pregnancy)","Yes - Currently partially vaccinated","No - Not vaccinated")
levels(data$prevadm_adm.factor)=c("Yes","No")
levels(data$lngth_prevadm_adm.factor)=c("1 - 7 days (in the past week)","7 - 28 days (from one week to one month ago)","28 days - 6 months (from one month to six months ago)","> 6 months ago (more than six months ago)")
levels(data$rs_prevadm_adm___1.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___2.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___3.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___4.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___5.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___6.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___7.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___8.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___9.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___10.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___11.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___12.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___13.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___14.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___98.factor)=c("Unchecked","Checked")
levels(data$rs_prevadm_adm___99.factor)=c("Unchecked","Checked")
levels(data$uti_abx_adm.factor)=c("Yes","No")
levels(data$numberanc_adm.factor)=c("0","1","2","3","4","5","6","7","8",">8")
levels(data$ancprovider_adm___1.factor)=c("Unchecked","Checked")
levels(data$ancprovider_adm___2.factor)=c("Unchecked","Checked")
levels(data$ancprovider_adm___3.factor)=c("Unchecked","Checked")
levels(data$ancprovider_adm___4.factor)=c("Unchecked","Checked")
levels(data$ancprovider_adm___5.factor)=c("Unchecked","Checked")
levels(data$ancprovider_adm___6.factor)=c("Unchecked","Checked")
levels(data$ancprovider_adm___7.factor)=c("Unchecked","Checked")
levels(data$pregnancy_history_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$marry_ses.factor)=c("Married monogamous","Married polygamous","Single","Separated/divorced","Widowed")
levels(data$livfather_ses.factor)=c("Yes","No")
levels(data$schoolyrs_ses.factor)=c("No school","< = P3","P4-P7","S1-S6","Post secondary (including post S4 technical school)","Dont know")
levels(data$nutri_adm.factor)=c("Yes","No","Not Sure")
levels(data$sesindex_flooring___1.factor)=c("Unchecked","Checked")
levels(data$sesindex_flooring___2.factor)=c("Unchecked","Checked")
levels(data$sesindex_flooring___3.factor)=c("Unchecked","Checked")
levels(data$sesindex_flooring___4.factor)=c("Unchecked","Checked")
levels(data$sesindex_flooring___98.factor)=c("Unchecked","Checked")
levels(data$sesindex_toilet___1.factor)=c("Unchecked","Checked")
levels(data$sesindex_toilet___2.factor)=c("Unchecked","Checked")
levels(data$sesindex_toilet___3.factor)=c("Unchecked","Checked")
levels(data$sesindex_toilet___4.factor)=c("Unchecked","Checked")
levels(data$sesindex_toilet___5.factor)=c("Unchecked","Checked")
levels(data$sesindex_toilet___6.factor)=c("Unchecked","Checked")
levels(data$sesindex_toilet___98.factor)=c("Unchecked","Checked")
levels(data$sesindex_toiletshared.factor)=c("Yes","No")
levels(data$sesindex_cooking___1.factor)=c("Unchecked","Checked")
levels(data$sesindex_cooking___2.factor)=c("Unchecked","Checked")
levels(data$sesindex_cooking___3.factor)=c("Unchecked","Checked")
levels(data$sesindex_cooking___4.factor)=c("Unchecked","Checked")
levels(data$sesindex_cooking___5.factor)=c("Unchecked","Checked")
levels(data$sesindex_cooking___6.factor)=c("Unchecked","Checked")
levels(data$sesindex_cooking___7.factor)=c("Unchecked","Checked")
levels(data$sesindex_cooking___8.factor)=c("Unchecked","Checked")
levels(data$sesindex_cooking___9.factor)=c("Unchecked","Checked")
levels(data$sesindex_safewater.factor)=c("Piped water","Borehole","Protected spring","Well/dam","Rainwater","River/lake","Bottled water","Other")
levels(data$sesindex_safewaterdistance.factor)=c("Yes","No")
levels(data$sesindex_assets___1.factor)=c("Unchecked","Checked")
levels(data$sesindex_assets___2.factor)=c("Unchecked","Checked")
levels(data$sesindex_assets___3.factor)=c("Unchecked","Checked")
levels(data$sesindex_assets___4.factor)=c("Unchecked","Checked")
levels(data$sesindex_assets___99.factor)=c("Unchecked","Checked")
levels(data$sesindex_assets2___1.factor)=c("Unchecked","Checked")
levels(data$sesindex_assets2___2.factor)=c("Unchecked","Checked")
levels(data$sesindex_assets2___3.factor)=c("Unchecked","Checked")
levels(data$sesindex_assets2___4.factor)=c("Unchecked","Checked")
levels(data$sesindex_assets2___5.factor)=c("Unchecked","Checked")
levels(data$sesindex_assets2___99.factor)=c("Unchecked","Checked")
levels(data$sesindex_room.factor)=c("One","Two","Three","More than three")
levels(data$child_death_ses.factor)=c("Yes","No")
levels(data$ses_and_demographics_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$prom_del.factor)=c("Yes","No","Not sure")
levels(data$rom_del.factor)=c("0 - 4 hours","4 - 8 hours","8 -12 hours","12 - 24 hours","> 24 hours")
levels(data$delmode_del.factor)=c("Vaginal","Assisted vaginal (vacuum or forceps)","Caesarean (with labour)","Caesarean (without labour)")
levels(data$episiotomy_del.factor)=c("Yes","No","Not sure")
levels(data$tear_del.factor)=c("Yes","No","Not sure")
levels(data$degreetear_del.factor)=c("1","2","3","4")
levels(data$induce_del.factor)=c("Yes","No","Not sure")
levels(data$inductype_del___1.factor)=c("Unchecked","Checked")
levels(data$inductype_del___2.factor)=c("Unchecked","Checked")
levels(data$inductype_del___3.factor)=c("Unchecked","Checked")
levels(data$inductype_del___4.factor)=c("Unchecked","Checked")
levels(data$inductype_del___5.factor)=c("Unchecked","Checked")
levels(data$inductype_del___98.factor)=c("Unchecked","Checked")
levels(data$pph_del.factor)=c("Yes","No","Not sure")
levels(data$transfx_del.factor)=c("Yes","No","Not sure")
levels(data$obstruct_del.factor)=c("Yes","No","Not sure")
levels(data$meconium_del.factor)=c("Yes","No","Not sure")
levels(data$vagexam_del.factor)=c("0","1","2","3","4","5","6","7","8","9",">9")
levels(data$placenta_del.factor)=c("Placenta previa","Placenta Abruption","Placenta accreta","Retained placenta","None")
levels(data$man_placenta_del.factor)=c("Yes","No")
levels(data$csecturgency_del.factor)=c("Immediate threat to life of woman or fetus","Maternal or fetal compromise which is not immediately life-threatening","Needing early delivery but no maternal or fetal compromise","At a time to suit the patient and maternity team","Not sure")
levels(data$csect_delay_del.factor)=c("No delay","30-60m","61m - 120m",">120m","Not sure")
levels(data$abx_del.factor)=c("Yes - within 1 hour of incision","Yes - but after incision","No","Not sure")
levels(data$csect_abx_del.factor)=c("IV Cefazolin","IV Ampicillin or Amoxicillin","IV cloxacilllin","IV penicillin","IV ceftriaxone","IV gentamicin","IV vancomycin","IV or PO metronidazole","IV or PO Clindamycin","Other (specify)","None")
levels(data$anticoag_del.factor)=c("Yes","No")
levels(data$delivery_maternal_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$infantstatus_del.factor)=c("Yes","No")
levels(data$sb20wk_del.factor)=c("Yes","No")
levels(data$sb_del.factor)=c("Fresh","Macerated","Not sure")
levels(data$sbcongenital_del.factor)=c("Yes","No","Not sure")
levels(data$sbsex_del.factor)=c("Male","Female","Not sure")
levels(data$sexbb_del.factor)=c("Male","Female","Other")
levels(data$cord_delay_del.factor)=c("Yes","No","Not sure")
levels(data$rescus_del.factor)=c("Yes","No","Not sure")
levels(data$resustype_del___1.factor)=c("Unchecked","Checked")
levels(data$resustype_del___2.factor)=c("Unchecked","Checked")
levels(data$resustype_del___3.factor)=c("Unchecked","Checked")
levels(data$resustype_del___4.factor)=c("Unchecked","Checked")
levels(data$resustype_del___5.factor)=c("Unchecked","Checked")
levels(data$resustype_del___6.factor)=c("Unchecked","Checked")
levels(data$resustype_del___7.factor)=c("Unchecked","Checked")
levels(data$resustype_del___8.factor)=c("Unchecked","Checked")
levels(data$resustype_del___98.factor)=c("Unchecked","Checked")
levels(data$resustype_del___99.factor)=c("Unchecked","Checked")
levels(data$delivery_neonatal_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$disnurse_mat.factor)=c("Olivia","Phionah","Jonan","Annet Happy","Annet Mary","Miria","Maureen","Clare","Kelemensia","Immaculate","Bosco","Enid Kibone","Other")
levels(data$adm_mat.factor)=c("Yes","No")
levels(data$dischstat_mat.factor)=c("Routine","Against medical advice","Died","Other")
levels(data$destination_mat.factor)=c("Own home","Home of relative","Home of parent (of woman)","Home of friend","Mother not yet sure","Other")
levels(data$support_mat___1.factor)=c("Unchecked","Checked")
levels(data$support_mat___2.factor)=c("Unchecked","Checked")
levels(data$support_mat___3.factor)=c("Unchecked","Checked")
levels(data$support_mat___4.factor)=c("Unchecked","Checked")
levels(data$support_mat___5.factor)=c("Unchecked","Checked")
levels(data$support_mat___99.factor)=c("Unchecked","Checked")
levels(data$bf_mat.factor)=c("Yes","No","Not Applicable (first baby)")
levels(data$symp_mat___1.factor)=c("Unchecked","Checked")
levels(data$symp_mat___2.factor)=c("Unchecked","Checked")
levels(data$symp_mat___3.factor)=c("Unchecked","Checked")
levels(data$symp_mat___4.factor)=c("Unchecked","Checked")
levels(data$symp_mat___5.factor)=c("Unchecked","Checked")
levels(data$symp_mat___6.factor)=c("Unchecked","Checked")
levels(data$symp_mat___7.factor)=c("Unchecked","Checked")
levels(data$symp_mat___8.factor)=c("Unchecked","Checked")
levels(data$symp_mat___9.factor)=c("Unchecked","Checked")
levels(data$symp_mat___10.factor)=c("Unchecked","Checked")
levels(data$symp_mat___99.factor)=c("Unchecked","Checked")
levels(data$abx_mat___1.factor)=c("Unchecked","Checked")
levels(data$abx_mat___2.factor)=c("Unchecked","Checked")
levels(data$abx_mat___3.factor)=c("Unchecked","Checked")
levels(data$abx_mat___99.factor)=c("Unchecked","Checked")
levels(data$discharge_interview_maternal_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$dispbb_neo.factor)=c("Discharged","Admitted","Died","Referred","Discharged, but readmitted after study nurse assessment","Other")
levels(data$admitsite_neo.factor)=c("Delivery hospital","Different hospital")
levels(data$admitdisposition_neo.factor)=c("Died","Discharged","Referred","Discharge against medical advice/fled")
levels(data$admitdiagnosis_neo___1.factor)=c("Unchecked","Checked")
levels(data$admitdiagnosis_neo___2.factor)=c("Unchecked","Checked")
levels(data$admitdiagnosis_neo___3.factor)=c("Unchecked","Checked")
levels(data$admitdiagnosis_neo___4.factor)=c("Unchecked","Checked")
levels(data$admitdiagnosis_neo___5.factor)=c("Unchecked","Checked")
levels(data$admitdiagnosis_neo___99.factor)=c("Unchecked","Checked")
levels(data$babedisch_neo.factor)=c("Yes","No")
levels(data$poop_neo.factor)=c("Yes","No","Dont know")
levels(data$pee_neo.factor)=c("Yes","No","Dont know")
levels(data$bf_neo.factor)=c("Yes","No")
levels(data$jaundice_neo.factor)=c("No","Yes","Yes (a little)")
levels(data$eyedischarge_neo.factor)=c("Yes","No")
levels(data$foot_o2src_neo.factor)=c("Using tablet","Separate device")
levels(data$rhand_o2src_neo.factor)=c("Using tablet","Separate device")
levels(data$abx_neo.factor)=c("Yes","No")
levels(data$discharge_interview_neonatal_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$physical_swf___1.factor)=c("Unchecked","Checked")
levels(data$respondent_swf.factor)=c("Woman herself","Other")
levels(data$otherrespondent_swf.factor)=c("Husband","Mother in law","Mother","Sibling","Father","Father in law","other relative/Friend","Other")
levels(data$home_swf.factor)=c("Yes","No")
levels(data$momalive_swf.factor)=c("Yes","No")
levels(data$momdeathplace_swf.factor)=c("Home","Hospital","On the way to hospital","Other")
levels(data$momadmit_swf.factor)=c("Yes","No")
levels(data$nummomadmit_swf.factor)=c("1","2","3",">3")
levels(data$momadmitpathway_swf___1.factor)=c("Unchecked","Checked")
levels(data$momadmitpathway_swf___2.factor)=c("Unchecked","Checked")
levels(data$momadmitpathway_swf___3.factor)=c("Unchecked","Checked")
levels(data$momadmitpathway_swf___98.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___1.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___2.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___3.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___4.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___5.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___6.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___7.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___8.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___9.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___10.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___11.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___12.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___13.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___14.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___15.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___16.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___17.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___18.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___19.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___20.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___21.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___98.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_swf___99.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___1.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___2.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___3.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___4.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___5.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___6.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___7.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___8.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___9.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___10.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___11.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___12.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___13.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___14.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___15.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___16.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___17.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___18.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___98.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_swf___99.factor)=c("Unchecked","Checked")
levels(data$momseek_swf.factor)=c("Yes","No")
levels(data$momseekpathway_swf___1.factor)=c("Unchecked","Checked")
levels(data$momseekpathway_swf___2.factor)=c("Unchecked","Checked")
levels(data$momseekpathway_swf___3.factor)=c("Unchecked","Checked")
levels(data$momseekpathway_swf___98.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___1.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___2.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___3.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___4.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___5.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___6.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___7.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___8.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___9.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___10.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___11.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___12.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___13.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___14.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___15.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___16.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___17.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___18.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___19.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___20.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___21.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___98.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_swf___99.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___1.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___2.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___3.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___4.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___5.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___6.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___7.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___8.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___9.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___10.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___11.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___12.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___13.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___14.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___15.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___16.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___17.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___18.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___98.factor)=c("Unchecked","Checked")
levels(data$momseekcond_swf___99.factor)=c("Unchecked","Checked")
levels(data$pnc_mom_swf.factor)=c("0","1","2","3",">3")
levels(data$six_week_follow_up_maternal_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$babephysical_swf___1.factor)=c("Unchecked","Checked")
levels(data$babealive_swf.factor)=c("Yes","No")
levels(data$babedeathplace_swf.factor)=c("Home","Hospital","on the way to hospital","Other")
levels(data$babeadmit_swf.factor)=c("Yes","No")
levels(data$numbabeadmit_swf.factor)=c("1","2","3",">3")
levels(data$babeadmitpathway_swf___1.factor)=c("Unchecked","Checked")
levels(data$babeadmitpathway_swf___2.factor)=c("Unchecked","Checked")
levels(data$babeadmitpathway_swf___3.factor)=c("Unchecked","Checked")
levels(data$babeadmitpathway_swf___98.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___1.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___2.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___3.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___4.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___5.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___6.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___7.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___8.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___9.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___10.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___11.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___12.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___13.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___14.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___15.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___16.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___17.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___18.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___98.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_swf___99.factor)=c("Unchecked","Checked")
levels(data$transfusion_swf.factor)=c("Yes","No")
levels(data$babeadmitcond_swf___1.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___2.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___3.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___4.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___5.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___6.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___7.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___8.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___9.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___10.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___11.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___98.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_swf___99.factor)=c("Unchecked","Checked")
levels(data$matsymp_babeoutcome.factor)=c("Yes","No")
levels(data$babefeed_outcome.factor)=c("Yes","No")
levels(data$babeseek_swf.factor)=c("Yes","No")
levels(data$babeseekpathway_swf___1.factor)=c("Unchecked","Checked")
levels(data$babeseekpathway_swf___2.factor)=c("Unchecked","Checked")
levels(data$babeseekpathway_swf___3.factor)=c("Unchecked","Checked")
levels(data$babeseekpathway_swf___98.factor)=c("Unchecked","Checked")
levels(data$pnc_baby_swf.factor)=c("0","1","2","3",">3")
levels(data$six_week_follow_up_neonatal_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$momchecks_swf.factor)=c("Yes","No")
levels(data$babechecks_swf.factor)=c("Yes","No")
levels(data$wellnesstimesthink_swf.factor)=c("0","1","2","3","4","More than 4")
levels(data$wellnesssource_swf___1.factor)=c("Unchecked","Checked")
levels(data$wellnesssource_swf___2.factor)=c("Unchecked","Checked")
levels(data$wellnesssource_swf___3.factor)=c("Unchecked","Checked")
levels(data$wellnesssource_swf___4.factor)=c("Unchecked","Checked")
levels(data$wellnesssource_swf___98.factor)=c("Unchecked","Checked")
levels(data$wellnesstimes_swf.factor)=c("0","1","2","3","4","More than 4")
levels(data$wellnesswhen_swf___1.factor)=c("Unchecked","Checked")
levels(data$wellnesswhen_swf___2.factor)=c("Unchecked","Checked")
levels(data$wellnesswhen_swf___3.factor)=c("Unchecked","Checked")
levels(data$wellnesswhen_swf___4.factor)=c("Unchecked","Checked")
levels(data$wellnesswhen_swf___5.factor)=c("Unchecked","Checked")
levels(data$wellnesswhen_swf___98.factor)=c("Unchecked","Checked")
levels(data$wellnesswhen_swf___99.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___1.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___2.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___3.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___4.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___5.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___6.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___7.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___8.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___9.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___10.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___98.factor)=c("Unchecked","Checked")
levels(data$wellnessreasonnot_swf___99.factor)=c("Unchecked","Checked")
levels(data$wellnessdiscuss_swf___1.factor)=c("Unchecked","Checked")
levels(data$wellnessdiscuss_swf___2.factor)=c("Unchecked","Checked")
levels(data$wellnessdiscuss_swf___3.factor)=c("Unchecked","Checked")
levels(data$wellnessdiscuss_swf___4.factor)=c("Unchecked","Checked")
levels(data$wellnessdiscuss_swf___5.factor)=c("Unchecked","Checked")
levels(data$wellnessdiscuss_swf___99.factor)=c("Unchecked","Checked")
levels(data$six_week_follow_up_wellness_checks_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$id10007_m.factor)=c("Female","Male","Ambiguous/Intersex")
levels(data$id10008_m.factor)=c("Parent","Child","Other family member","Friend","Spouse","Health worker","Public official","Another relationship","Refused to answer")
levels(data$id10009_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10013_m.factor)=c("Yes","No")
levels(data$id10487_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10488_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$momadmit_va.factor)=c("Yes","No")
levels(data$nummomadmit_va.factor)=c("1","2","3",">3")
levels(data$momadmitpathway_va___1.factor)=c("Unchecked","Checked")
levels(data$momadmitpathway_va___2.factor)=c("Unchecked","Checked")
levels(data$momadmitpathway_va___3.factor)=c("Unchecked","Checked")
levels(data$momadmitpathway_va___98.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___1.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___2.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___3.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___4.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___5.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___6.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___7.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___8.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___9.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___10.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___11.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___12.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___13.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___14.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___15.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___16.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___17.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___18.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___19.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___20.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___21.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___98.factor)=c("Unchecked","Checked")
levels(data$momadmitsymp_va___99.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___1.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___2.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___3.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___4.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___5.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___6.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___7.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___8.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___9.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___10.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___11.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___12.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___13.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___14.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___15.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___16.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___17.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___18.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___98.factor)=c("Unchecked","Checked")
levels(data$momadmitcond_va___99.factor)=c("Unchecked","Checked")
levels(data$momseek_va.factor)=c("Yes","No")
levels(data$momseekpathway_va___1.factor)=c("Unchecked","Checked")
levels(data$momseekpathway_va___2.factor)=c("Unchecked","Checked")
levels(data$momseekpathway_va___3.factor)=c("Unchecked","Checked")
levels(data$momseekpathway_va___98.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___1.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___2.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___3.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___4.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___5.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___6.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___7.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___8.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___9.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___10.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___11.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___12.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___13.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___14.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___15.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___16.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___17.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___18.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___19.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___20.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___21.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___98.factor)=c("Unchecked","Checked")
levels(data$momseeksymp_va___99.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___1.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___2.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___3.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___4.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___5.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___6.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___7.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___8.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___9.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___10.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___11.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___12.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___13.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___14.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___15.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___16.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___17.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___18.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___98.factor)=c("Unchecked","Checked")
levels(data$momseekcond_va___99.factor)=c("Unchecked","Checked")
levels(data$id10077_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10079_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10080_m.factor)=c("Pedestrian","Driver or passenger in car or light vehicle","Driver or passenger in bus or heavy vehicle","Driver or passenger on a motorcycle","Driver or passenger on a pedal cycle","Other","Doesnt know","Refused to answer")
levels(data$id10081_m.factor)=c("Pedestrian","Stationary object","Car or light vehicle","Bus or heavy vehicle","Motorcycle","Pedal cycle","Other","Doesnt know","Refused to answer")
levels(data$id10082_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10083_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10084_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10085_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10086_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10087_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10088_m.factor)=c("Dog","Snake","Insect or scorpion","Other","Doesnt know","Refused to answer")
levels(data$id10089_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10090_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10091_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10092_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10093_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10094_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10095_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10096_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10097_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10098_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10099_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10100_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10123_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10125_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10126_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10127_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10128_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10129_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10130_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10131_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10132_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10133_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10134_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10135_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10136_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10137_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10138_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10139_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10140_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10141_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10142_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10143_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10144_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10482_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10483_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10484_m.factor)=c("Positive","Negative","Unclear","Dont know","Refused to answer")
levels(data$id10147_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10149_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10150_m.factor)=c("Mild","Moderate","Severe","Doesnt know","Refused to answer")
levels(data$id10151_m.factor)=c("Continuous","On and off","Only at night","Doesnt know","Refused to answer")
levels(data$id10152_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10153_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10155_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10156_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10157_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10159_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10165_m.factor)=c("Continuous","On and off","Doesnt know","Refused to answer")
levels(data$id10166_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10168_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10170_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10171_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10173_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10174_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10175_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10181_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10186_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10187_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10188_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10189_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10192_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10193_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10194_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10195_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10199_m.factor)=c("Upper abdomen","Lower abdomen","Upper and lower abdomen","Doesnt know","Refused to answer")
levels(data$id10200_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10203_m.factor)=c("Rapidly","Slowly","Doesnt know","Refused to answer")
levels(data$id10204_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10207_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10208_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10210_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10212_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10214_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10215_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10217_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10218_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10219_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10222_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10223_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10225_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10226_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10224_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10227_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10228_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10229_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10230_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10231_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10233_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10235_m___1.factor)=c("Unchecked","Checked")
levels(data$id10235_m___2.factor)=c("Unchecked","Checked")
levels(data$id10235_m___3.factor)=c("Unchecked","Checked")
levels(data$id10235_m___4.factor)=c("Unchecked","Checked")
levels(data$id10236_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10237_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10238_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10241_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10242_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10243_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10244_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10245_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10246_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10247_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10249_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10251_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10252_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10253_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10254_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10255_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10256_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10257_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10258_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10259_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10260_m___1.factor)=c("Unchecked","Checked")
levels(data$id10260_m___2.factor)=c("Unchecked","Checked")
levels(data$id10260_m___3.factor)=c("Unchecked","Checked")
levels(data$id10260_m___4.factor)=c("Unchecked","Checked")
levels(data$id10260_m___5.factor)=c("Unchecked","Checked")
levels(data$id10260_m___6.factor)=c("Unchecked","Checked")
levels(data$id10260_m___7.factor)=c("Unchecked","Checked")
levels(data$id10260_m___8.factor)=c("Unchecked","Checked")
levels(data$id10261_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10263_m.factor)=c("Solids","Liquids","Both","Doesnt know","Refused to answer")
levels(data$id10264_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10265_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10267_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10268_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10270_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10486_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10485_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10294_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10295_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10296_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10297_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10298_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10301_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10299_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10302_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10300_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10304_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10305_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10306_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10307_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10312_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10313_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10314_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10315_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10316_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10317_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10318_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10320_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10321_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10322_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10323_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10324_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10325_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10326_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10327_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10328_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10329_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10330_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10331_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10333_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10334_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10335_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10336_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10337_m.factor)=c("Hospital","Other health facility","Home","On route to hospital or facility","Other","Doesnt know","Refused to answer")
levels(data$id10338_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10339_m.factor)=c("Doctor","Midwife","Nurse","Relative","Self (the mother)","Traditional birth attendant","Other","Doesnt know","Refused to answer")
levels(data$id10342_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10343_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10344_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10347_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10340_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10411_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10412_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10413_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10414_m___1.factor)=c("Unchecked","Checked")
levels(data$id10414_m___2.factor)=c("Unchecked","Checked")
levels(data$id10414_m___3.factor)=c("Unchecked","Checked")
levels(data$id10414_m___4.factor)=c("Unchecked","Checked")
levels(data$id10414_m___5.factor)=c("Unchecked","Checked")
levels(data$id10414_m___99.factor)=c("Unchecked","Checked")
levels(data$id10414_m___88.factor)=c("Unchecked","Checked")
levels(data$id10418_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10419_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10420_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10421_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10422_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10423_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10424_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10425_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10426_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10427_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10432_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10433_m___1.factor)=c("Unchecked","Checked")
levels(data$id10433_m___2.factor)=c("Unchecked","Checked")
levels(data$id10433_m___3.factor)=c("Unchecked","Checked")
levels(data$id10433_m___4.factor)=c("Unchecked","Checked")
levels(data$id10433_m___5.factor)=c("Unchecked","Checked")
levels(data$id10433_m___6.factor)=c("Unchecked","Checked")
levels(data$id10433_m___7.factor)=c("Unchecked","Checked")
levels(data$id10433_m___8.factor)=c("Unchecked","Checked")
levels(data$id10433_m___9.factor)=c("Unchecked","Checked")
levels(data$id10433_m___10.factor)=c("Unchecked","Checked")
levels(data$id10433_m___11.factor)=c("Unchecked","Checked")
levels(data$id10433_m___99.factor)=c("Unchecked","Checked")
levels(data$id10433_m___88.factor)=c("Unchecked","Checked")
levels(data$id10435_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10437_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10438_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10439_check_m.factor)=c("Yes","No")
levels(data$id10440_check_m.factor)=c("Yes","No")
levels(data$id10441_check_m.factor)=c("Yes","No")
levels(data$id10450_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10451_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10452_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10453_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10454_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10455_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10456_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10457_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10458_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10459_m.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10477_m___1.factor)=c("Unchecked","Checked")
levels(data$id10477_m___2.factor)=c("Unchecked","Checked")
levels(data$id10477_m___3.factor)=c("Unchecked","Checked")
levels(data$id10477_m___4.factor)=c("Unchecked","Checked")
levels(data$id10477_m___5.factor)=c("Unchecked","Checked")
levels(data$id10477_m___6.factor)=c("Unchecked","Checked")
levels(data$id10477_m___7.factor)=c("Unchecked","Checked")
levels(data$id10477_m___8.factor)=c("Unchecked","Checked")
levels(data$id10477_m___9.factor)=c("Unchecked","Checked")
levels(data$id10477_m___10.factor)=c("Unchecked","Checked")
levels(data$id10477_m___11.factor)=c("Unchecked","Checked")
levels(data$id10477_m___12.factor)=c("Unchecked","Checked")
levels(data$id10477_m___99.factor)=c("Unchecked","Checked")
levels(data$verbal_autopsy_mother_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$id10007.factor)=c("Female","Male","Ambiguous/Intersex")
levels(data$id10008.factor)=c("Parent","Child","Other family member","Friend","Spouse","Health worker","Public official","Another relationship","Refused to answer")
levels(data$id10009.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10013.factor)=c("Yes","No")
levels(data$id10487.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10488.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$babeadmit_va.factor)=c("Yes","No")
levels(data$numbabeadmit_va.factor)=c("1","2","3",">3")
levels(data$babeadmitpathway_va___1.factor)=c("Unchecked","Checked")
levels(data$babeadmitpathway_va___2.factor)=c("Unchecked","Checked")
levels(data$babeadmitpathway_va___3.factor)=c("Unchecked","Checked")
levels(data$babeadmitpathway_va___98.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___1.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___2.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___3.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___4.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___5.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___6.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___7.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___8.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___9.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___10.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___11.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___12.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___13.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___14.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___15.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___16.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___17.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___18.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___98.factor)=c("Unchecked","Checked")
levels(data$babeadmitsymp_va___99.factor)=c("Unchecked","Checked")
levels(data$transfusion_va.factor)=c("Yes","No")
levels(data$babeadmitcond_va___1.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___2.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___3.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___4.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___5.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___6.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___7.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___8.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___9.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___10.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___11.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___98.factor)=c("Unchecked","Checked")
levels(data$babeadmitcond_va___99.factor)=c("Unchecked","Checked")
levels(data$matsymp_va.factor)=c("Yes","No")
levels(data$babefeed_va.factor)=c("Yes","No")
levels(data$babeseek_va.factor)=c("Yes","No")
levels(data$babeseekpathway_va___1.factor)=c("Unchecked","Checked")
levels(data$babeseekpathway_va___2.factor)=c("Unchecked","Checked")
levels(data$babeseekpathway_va___3.factor)=c("Unchecked","Checked")
levels(data$babeseekpathway_va___98.factor)=c("Unchecked","Checked")
levels(data$id10077.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10079.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10081.factor)=c("Pedestrian","Stationary object","Car or light vehicle","Bus or heavy vehicle","Motorcycle","Pedal cycle","Other","Doesnt know","Refused to answer")
levels(data$id10082.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10083.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10084.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10085.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10086.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10087.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10088.factor)=c("Dog","Snake","Insect or scorpion","Other","Doesnt know","Refused to answer")
levels(data$id10089.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10090.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10092.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10093.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10094.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10095.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10096.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10097.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10098.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10100.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10408.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10123.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10125.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10128.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10129.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10131.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10133.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10134.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10135.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10136.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10137.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10142.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10143.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10144.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10147.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10147_measure.factor)=c("Yes","No")
levels(data$id10147_temp.factor)=c("Yes","No")
levels(data$id10149.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10150.factor)=c("Mild","Moderate","Severe","Doesnt know","Refused to answer")
levels(data$id10151.factor)=c("Continuous","On and off","Only at night","Doesnt know","Refused to answer")
levels(data$id10152.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10153.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10155.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10156.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10157.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10158.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10159.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10165.factor)=c("Continuous","On and off","Doesnt know","Refused to answer")
levels(data$id10166.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10168.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10172.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10173___1.factor)=c("Unchecked","Checked")
levels(data$id10173___2.factor)=c("Unchecked","Checked")
levels(data$id10173___3.factor)=c("Unchecked","Checked")
levels(data$id10173___4.factor)=c("Unchecked","Checked")
levels(data$id10173___99.factor)=c("Unchecked","Checked")
levels(data$id10173___88.factor)=c("Unchecked","Checked")
levels(data$id10181.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10185.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10186.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10187.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10188.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10194.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10188_projectile.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10191.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10188_billious.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10192.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10193.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10200.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10203.factor)=c("Rapidly","Slowly","Doesnt know","Refused to answer")
levels(data$id10204.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10214.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10215.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10217.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10218.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10219.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10220.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10222.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10223.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10225.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10226.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10224.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10227.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10229.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10230.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10231.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10233.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10235___1.factor)=c("Unchecked","Checked")
levels(data$id10235___2.factor)=c("Unchecked","Checked")
levels(data$id10235___3.factor)=c("Unchecked","Checked")
levels(data$id10235___4.factor)=c("Unchecked","Checked")
levels(data$id10236.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10238.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10239.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10240.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10241.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10242.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10243.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10244.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10245.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10246.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10247.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10249.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10251.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10252.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10253.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10255.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10256.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10257.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10258.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10259.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10260___1.factor)=c("Unchecked","Checked")
levels(data$id10260___2.factor)=c("Unchecked","Checked")
levels(data$id10260___3.factor)=c("Unchecked","Checked")
levels(data$id10260___4.factor)=c("Unchecked","Checked")
levels(data$id10260___5.factor)=c("Unchecked","Checked")
levels(data$id10260___6.factor)=c("Unchecked","Checked")
levels(data$id10260___7.factor)=c("Unchecked","Checked")
levels(data$id10260___8.factor)=c("Unchecked","Checked")
levels(data$id10261.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10265.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10268.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10269.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10271.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10272.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10273.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10275.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10276.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10277.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10278.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10279.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10281.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10282.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10283.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10284.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10286.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10287.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10288.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10289.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10354.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10355.factor)=c("First","Second or later","Doesnt know","Refused to answer")
levels(data$id10435.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10437.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10438.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10439_check.factor)=c("Yes","No")
levels(data$id10450.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10451.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10452.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10453.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10454.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10455.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10456.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10457.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10458.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10459.factor)=c("Yes","No","Doesnt know","Refused to answer")
levels(data$id10478___1.factor)=c("Unchecked","Checked")
levels(data$id10478___2.factor)=c("Unchecked","Checked")
levels(data$id10478___3.factor)=c("Unchecked","Checked")
levels(data$id10478___4.factor)=c("Unchecked","Checked")
levels(data$id10478___5.factor)=c("Unchecked","Checked")
levels(data$id10478___6.factor)=c("Unchecked","Checked")
levels(data$id10478___7.factor)=c("Unchecked","Checked")
levels(data$id10478___8.factor)=c("Unchecked","Checked")
levels(data$id10478___9.factor)=c("Unchecked","Checked")
levels(data$id10478___10.factor)=c("Unchecked","Checked")
levels(data$id10478___11.factor)=c("Unchecked","Checked")
levels(data$id10478___12.factor)=c("Unchecked","Checked")
levels(data$id10478___13.factor)=c("Unchecked","Checked")
levels(data$id10478___14.factor)=c("Unchecked","Checked")
levels(data$id10478___15.factor)=c("Unchecked","Checked")
levels(data$id10478___16.factor)=c("Unchecked","Checked")
levels(data$id10478___99.factor)=c("Unchecked","Checked")
levels(data$verbal_autopsy_newborn_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$journey_mapping_admission_and_discharge_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$disch_72.factor)=c("Yes","No")
levels(data$exp_72.factor)=c("Yes","No")
levels(data$exp2_72.factor)=c("Yes","No")
levels(data$quality_72.factor)=c("Very poor","Poor","Somewhat okay","Somewhat good","Good","Great")
levels(data$journey_mapping_72_hour_follow_up_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$calculated_fields_complete.factor)=c("Incomplete","Unverified","Complete")
