#-----------------------------------------------------------------------------#
# P U L M O N A R Y   R E H A B   C L I N I C A L   b u i l d   s c r i p t   #
#                                                                             #
# Author: Alex                                                                #
# Date created: 2022-08-09                                                    #
#-----------------------------------------------------------------------------#


library(dplyr)
# library(readstata13)
# library(xlsx)
# library(janitor)
# library(officer)
# library(flextable)
library(tidyverse)
library(survival)
library(survminer)
library(ggplot2)
library(survsup)
# library(epitools)
library(psych)
library(lme4)
'%!in%' <- function(x,y)!('%in%'(x,y))
library(car)
library(extrafont)
loadfonts()
fonts()
nlc <- function(x) {cat(paste("\n", x, "\n", sep = ""))}



dat <- read.csv("Z:/Group_work/Alex/Encrypted/Alex/PR/PR clinical 2021-22/Data/rawData/NACAP-PR-202103-202202-v106-Imperial.csv", header = TRUE, 
                    stringsAsFactors = TRUE, na.strings = c("NA", ""))


colnames(dat_old)

checkSame(dat, dat_old) %>% arrange(variable)

colnames(dat)



# First thing's first - rename column names
# R style guide says variables shouldn't be capitalised.
# NR = not recorded
# init = initial = at assessment
# dis = discharge

# Note - some of the column names have changed since the interim report.


head(dat)

dat <- dat %>% select(study_ID = RowID,
                      patient_ID = PatientID,
                      country = Country, 
                      region = Region,
                      trust_code = Tcode.Now,
                      trust_name = Trust.Now,
                      org_name = OrgName,
                      org_code = ORG.Service,     
                      nhs_number_valid = NHS.Number.Valid,
                      LSOA = ONSPD_AUG_2021_UK.lsoa11,
                      age = X.AgeAtAssessment,
                      gender = X1.3.Gender,
                      ethnicity = X1.5.Ethnicity,
                      ref_date = X2.1.Referral.Date,
                      ref_date_NR = X2.1.1.Not.recorded,
                      ref_location = X2.2.Referred.From,
                      assess_date = X2.3.Initial.PR.Assessment.Appointment,
                    #  smoke_status = X3.1.Smoking, 
                      FEV1_percpred = X3.1.FEV1...predicted.,
                      FEV1_NR = X3.1x.Not.recorded,
                      FEV1FVC = X3.2.FEV1.FVC.Ratio,
                      FEV1FVC_NR = X3.2x.Not.recorded,
                    #  BMI = X3.4.Patient.s.body.mass.index..BMI.,
                    #  BMI_NR = X3.4.1.Not.recorded,
                      MRC_score_init = X3.3.MRC.Score,
                    #  CVD_history_orig = X3.6.Cardiovascular.Disease,
                    #  musc_skel_history_orig = X3.7.Lower.Limb.Musculoskeletal.Disorder,
                    #  mental_history_orig = X3.8.Mental.Health,
                    #  mental_history_combined_illness = X3.8a.Mental.Health,
                    #  anxiety_bin = X3.8a.Mental.Health...Anxiety,
                    #  depression_bin = X3.8a.Mental.Health...Depression,
                    #  SMI_bin = X3.8a.Mental.Health...Severe.mental.illness,
                      test_type_init = X4.1.Initial.Assessment.Tests,
                      test_value_init = X4.1a.Value.in.metres,
                      prac_test_init = X4.1b.Practice.test.at.initial.assessment,
                      ESWT_at_init = X4.2.ESWT.at.initial.assessment,
                      ESWT_value_init = X4.2a.Value.in.seconds,
                      CRQ_init = X4.3.Chronic.Respiratory.Questionnaire..CRQ.,
                      CRQ_dyspnoea_init = X4.3a.Dyspnoea.score,
                      CRQ_fatigue_init = X4.3b.Fatigue.score,                                                                  
                      CRQ_emotion_init = X4.3c.Emotion.score,                                                                 
                      CRQ_mastery_init = X4.3d.Mastery.score,
                      CAT_init = X4.4.COPD.Assessment.Test..CAT.,
                      CAT_score_init = X4.4a.Total.CAT.score,
                      enrolled = X5.1.Enrolled.onto.PR.programme.,
                      start_date = X5.1a.Start.Date,
                      centre_based_V1 = X5.2.centre.based.PR.programme..V1.,
                    # Start of V1s
                    #  prog_type = X5.2a.Programme.Type,
                      scheduled_sess_centre_no_V1 = X5.2b.Centre.based.PR.Sessions.Scheduled..V1.,
                      rec_sess_centre_group_no_V1 = X5.2c.a..Group.centre.based.sessions.received..V1.,
                      rec_sess_centre_indiv_no_V1 = X5.2c.b..1.1.centre.based.sessions.received..V1.,
                      home_based_V1 = X5.3.Home.based.PR.programme..V1.,
                      scheduled_sess_home_no_V1 = X5.3a.Home.based.PR.Sessions.Scheduled..V1.,
                      rec_sess_home_in_person_no_V1 = X5.3b.a..Home.based.PR.Sessions...In.Person...V1.,
                      rec_sess_home_video_group_no_V1 = X5.3b.b..Home.based.PR.Sessions...Video.conferencing...group..V1.,
                      rec_sess_home_video_indiv_no_V1 = X5.3b.c..Home.based.PR.Sessions...Video.conferencing...1.1...V1.,
                      rec_sess_home_phone_no_V1 = X5.3b.d..Home.based.PR.Sessions...Phone.calls..V1.,
                      rec_sess_home_other_no_V1 = X5.3b.e..Home.based.PR.Sessions...Other.Digital.Communications..V1.,
                    
                    # Start of V2s
                    
                    centre_based_V2 = X5.2.Supervised.PR.programme...Centre,
                    home_based_V2 = X5.2.Supervised.PR.programme...Home,
                    home_based_in_person_V2 = X5.2a.Home.based.delivery.method...Person,
                    home_based_video_V2 = X5.2a.Home.based.delivery.method...Video,
                    home_based_phone_V2 = X5.2a.Home.based.delivery.method...Phone,
                    home_based_other_V2 = X5.2a.Home.based.delivery.method...Digital,
                    scheduled_sess_V2 = X5.3.Supervised.PR.sessions.scheduled..MERGED.,
                    rec_sess_group_V2 = X5.4a.Supervised.PR.group.sessions.received..V2.,
                    rec_sess_indiv_V2 = X5.4b.Supervised.PR.face.to.face.sessions.received..V2., 
                    
                      discharge_assess = X6.1.Discharge.assessment.performed.,
                      discharge_date = X6.1a.Discharge.assessment.date,
                      exercise_plan = X6.1b.Discharge.exercise.plan.provided,
                      MRC_score_dis = X7.1.MRC.Score.At.Discharge,
                      test_type_dis = X7.2.Walking.test.at.discharge.assessment.,
                      test_value_dis = X7.2a.Value.in.metres,
                      ESWT_at_dis = X7.3.Did.you.also.record.the.ESWT.at.discharge.,
                      ESWT_value_dis = X7.3a.Value.in.seconds,
                      CRQ_dis = X7.4.Chronic.Respiratory.Questionnaire..CRQ.,
                      CRQ_dyspnoea_dis = X7.4a.Dyspnoea.score,
                      CRQ_fatigue_dis = X7.4b.Fatigue.score,
                      CRQ_emotion_dis = X7.4c.Emotion.score,
                      CRQ_mastery_dis = X7.4d.Mastery.score,
                      CAT_dis = X7.5.COPD.Assessment.Test..CAT.,
                      CAT_score_dis = X7.5a.Total.CAT.score,
                      dataset = Dataset) 


# create the combined variables from V1 and V2

# first, recode the dataset variable.

# dat %>% filter(dataset == "V1A") %>% summary() 
# dat %>% filter(dataset == "V1") %>% summary() 


dat$dataset[dat$dataset == "V1A"] <- "V1"
dat$dataset <- factor(as.character(dat$dataset))
summary(dat$dataset)


summary(dat$centre_based_V1)
summary(dat$centre_based_V2)

dat %>% select(centre_based_V2, dataset, centre_based_V1) %>% unique() 

# let's just recode to missing any V2 variable that is from the V1 dataset.

V2_cols <- dat %>% select(ends_with("_V2")) %>% colnames()

dat$centre_based_V2[dat$dataset == "V1"] <- NA
dat$home_based_V2[dat$dataset == "V1"] <- NA
dat$home_based_video_V2[dat$dataset == "V1"] <- NA
dat$home_based_other_V2[dat$dataset == "V1"] <- NA
dat$home_based_phone_V2[dat$dataset == "V1"] <- NA
dat$home_based_in_person_V2[dat$dataset == "V1"] <- NA
# dat$scheduled_sess_V2[dat$dataset == "V1"] <- NA
dat$rec_sess_group_V2[dat$dataset == "V1"] <- NA
dat$rec_sess_indiv_V2[dat$dataset == "V1"] <- NA


summary(dat)

dat$centre_based_V2[dat$dataset == "V1" & dat$centre_based_V1 == "Yes"] <- 1 
dat$centre_based_V2[dat$dataset == "V1" & dat$centre_based_V1 == "No"] <- 0 

dat$home_based_V2[dat$dataset == "V1" & dat$home_based_V1 == "Yes"] <- 1
dat$home_based_V2[dat$dataset == "V1" & dat$home_based_V1 == "No"] <- 0


# Make sure that those who aren't enrolled are missing.

dat$centre_based_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_V2[dat$enrolled != "Yes"] <- NA



# home_based delivery is more complicated...

dat %>% filter(dataset == "V1") %>% select(home_based_in_person_V2, ends_with("_V1")) %>% head(100)


dat %>% filter(dataset == "V1") %>% filter(home_based_V1 == "Yes") %>% select(scheduled_sess_home_no_V1,
                                                                          rec_sess_home_video_group_no_V1,
                                                                          rec_sess_home_other_no_V1,
                                                                          rec_sess_home_phone_no_V1,
                                                                          rec_sess_home_in_person_no_V1,
                                                                          rec_sess_home_video_indiv_no_V1) %>% 
  summary()

# everyone has >0 scheduled home sessions

check <- dat %>% filter(dataset == "V1") %>% filter(home_based_V1 == "Yes") %>% select(scheduled_sess_home_no_V1,
                                                                              rec_sess_home_video_group_no_V1,
                                                                              rec_sess_home_other_no_V1,
                                                                              rec_sess_home_phone_no_V1,
                                                                              rec_sess_home_in_person_no_V1,
                                                                              rec_sess_home_video_indiv_no_V1) %>% 
  mutate(all_sessions_rec = rec_sess_home_video_group_no_V1 + rec_sess_home_other_no_V1 +
           rec_sess_home_phone_no_V1 + rec_sess_home_in_person_no_V1 + rec_sess_home_video_indiv_no_V1)


table(check$all_sessions_rec)

# 26 people didn't receive any sessions but were home-based, so cannot be classed for Q5.2 delivery method
summary(dat$home_based_in_person_V2)

dat %>% filter(dataset == "V1" & home_based_V1 == "Yes") %>% select(ends_with("V1")) %>% summary()

table(dat$enrolled, dat$dataset)


dat$home_based_in_person_V2[dat$dataset == "V1" & dat$home_based_V2 == 1 & dat$rec_sess_home_in_person_no_V1 > 0] <- 1
dat$home_based_in_person_V2[dat$dataset == "V1" & dat$home_based_V2 == 1 & dat$rec_sess_home_in_person_no_V1 == 0] <- 0

dat$home_based_other_V2[dat$dataset == "V1" & dat$home_based_V2 == 1 & dat$rec_sess_home_other_no_V1 > 0] <- 1
dat$home_based_other_V2[dat$dataset == "V1" & dat$home_based_V2 == 1 & dat$rec_sess_home_other_no_V1 == 0] <- 0

dat$home_based_phone_V2[dat$dataset == "V1" & dat$home_based_V2 == 1 & dat$rec_sess_home_phone_no_V1 > 0] <- 1
dat$home_based_phone_V2[dat$dataset == "V1" & dat$home_based_V2 == 1 & dat$rec_sess_home_phone_no_V1 == 0] <- 0

dat$home_based_video_V2[dat$dataset == "V1" & dat$home_based_V2 == 1 & 
                          (dat$rec_sess_home_video_group_no_V1 > 0 | dat$rec_sess_home_video_indiv_no_V1 > 0)] <- 1

dat$home_based_video_V2[dat$dataset == "V1" & dat$home_based_V2 == 1 & 
                          (dat$rec_sess_home_video_group_no_V1 == 0 | dat$rec_sess_home_video_indiv_no_V1 == 0)] <- 0


# unfortunately I still need to sort this out further to introduce some as missing

summary(dat)

dat$centre_based_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_video_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_other_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_phone_V2[dat$enrolled != "Yes"] <- NA
dat$home_based_in_person_V2[dat$enrolled != "Yes"] <- NA
dat$scheduled_sess_V2[dat$enrolled != "Yes"] <- NA
dat$rec_sess_group_V2[dat$enrolled != "Yes"] <- NA
dat$rec_sess_indiv_V2[dat$enrolled != "Yes"] <- NA


table(dat$home_based_V2, useNA = "ifany")

dat$home_based_video_V2[dat$home_based_V2 == "0"] <- NA
dat$home_based_other_V2[dat$home_based_V2 == "0"] <- NA
dat$home_based_phone_V2[dat$home_based_V2 == "0"] <- NA
dat$home_based_in_person_V2[dat$home_based_V2 == "0"] <- NA


table(dat$home_based_V2, dat$home_based_other_V2, useNA = "ifany")

dat %>% select(home_based_V2, home_based_other_V2) %>% table(useNA = "ifany")

dat %>% filter(enrolled == "Yes" & is.na(rec_sess_group_V2)) %>% summary()

summary(dat)

# for scheduled sessions, Tim's merge is fine.
# scheduled_sess_V2 = X5.3.Supervised.PR.sessions.scheduled..MERGED. 



# Aaaand the last bit is more straightforward. - received sessions for group sessions and 1:1 sessions.


dat$rec_sess_group_temp <- rowSums(select(dat,  rec_sess_centre_group_no_V1, rec_sess_home_video_group_no_V1),
                                   na.rm = TRUE)

dat$rec_sess_group_V2[dat$dataset == "V1"] <- dat$rec_sess_group_temp[dat$dataset == "V1"]

dat$rec_sess_indiv_temp <- rowSums(select(dat, rec_sess_centre_indiv_no_V1, 
                                               rec_sess_home_video_indiv_no_V1,
                                               rec_sess_home_in_person_no_V1, 
                                               rec_sess_home_phone_no_V1,
                                               rec_sess_home_other_no_V1), na.rm = TRUE)

dat$rec_sess_indiv_V2[dat$dataset == "V1"] <- dat$rec_sess_indiv_temp[dat$dataset == "V1"]

dat %>% filter(dataset == "V1") %>% select(starts_with("rec_sess")) %>% summary()


summary(dat$rec_sess_group_V2)
summary(dat$rec_sess_indiv_V2)

summary(dat$enrolled)

dat %>% filter(dataset == "V1") %>% select(starts_with("rec_sess")) %>% summary()

dat$rec_sess_group_V2[dat$enrolled != "Yes"] <- NA
dat$rec_sess_indiv_V2[dat$enrolled != "Yes"] <- NA

dat %>% filter(dataset == "V1") %>% select(starts_with("rec_sess")) %>% summary()

dat %>% filter(enrolled != "Yes") %>% select(starts_with("rec_sess")) %>% summary()
dat %>% select(starts_with("rec_sess")) %>% summary()

summary(dat)

dat %>% filter(is.na(scheduled_sess_V2)) %>% filter(!is.na(rec_sess_group_V2)) %>% summary()

dat %>% select(home_based_V2, home_based_in_person_V2) %>% table(useNA = "ifany")

# I think we have sorted it out.

# So, we remove all the variables we no longer need, and rename the V2 variables to remove the V references.

dat %>% select(ends_with("V2")) %>% colnames()

dat <- dat %>% rename(centre_based = centre_based_V2,
                      home_based = home_based_V2,
                      scheduled_sess = scheduled_sess_V2,
                      rec_sess_group = rec_sess_group_V2,
                      rec_sess_indiv = rec_sess_indiv_V2,
                      home_based_in_person = home_based_in_person_V2,
                      home_based_video = home_based_video_V2,
                      home_based_phone = home_based_phone_V2,
                      home_based_other = home_based_other_V2)

# Remove those still ending in V2, V1, or merge, or temp

dat <- dat %>% select(-ends_with("_V2"), -ends_with("_V1"), -ends_with("MERGED"), -ends_with("temp"))

# Also, we don't need the dataset column

dat <- dat %>% select(-dataset)


# And make sure that anyone who isn't enrolled is missing for these variables:

summary(dat)





dat %>% filter(enrolled != "Yes") %>% summary()
dat %>% summary()

# Phew!

# mainly fine, but have to sort out the question 5 stuff.


dat <- dat %>% mutate(study_ID = as.character(study_ID),
                      patient_ID = as.character(patient_ID),
                      LSOA = as.character(LSOA))
                      

# It's strange but these do all have different decimal point levels
head(dat$CAT_score_init, 30) # 0 dp
head(dat$CRQ_fatigue_init, 30) # 2 dp
head(dat$CRQ_dyspnoea_init, 30) # 1 dp
head(dat$CRQ_emotion_init, 30) # 2dp
head(dat$CRQ_mastery_init, 30) # 2dp




# Need to check the ones that involve levels, because that it where I could come unstuck if I just
# brainlessly run the cleaning code.

# Differences in:
# Trust code
# Org code
# Ethnicity (expected, probably some missed out of the interim
# All dates

# So all seems okay!
# This means I can just run the cleaning section without worrying.
# Only thing I have changed is the 'N/A' bits, and the age limits (upper limit changed to
# 'infinite')


# For the combining, we need to recode trust code and org code as character.


IMDeng <- read.csv("Z:/Group_work/PS_AA/General UK data/IMD/clean_IMD2019_England.csv",
                   header = TRUE, stringsAsFactors = FALSE)
IMDwales <- read.csv("Z:/Group_work/PS_AA/General UK data/IMD/clean_IMD2019_Wales.csv",
                   header = TRUE, stringsAsFactors = FALSE)
IMDscot <- read.csv("Z:/Group_work/PS_AA/General UK data/IMD/clean_IMD2016_Scotland.csv",
                   header = TRUE, stringsAsFactors = FALSE)


# Create the quintiles for the English IMD data

IMDeng$IMD_quintile <- NA

IMDeng$IMD_quintile[IMDeng$IMD_decile == 1] <- 1
IMDeng$IMD_quintile[IMDeng$IMD_decile == 2] <- 1
IMDeng$IMD_quintile[IMDeng$IMD_decile == 3] <- 2
IMDeng$IMD_quintile[IMDeng$IMD_decile == 4] <- 2
IMDeng$IMD_quintile[IMDeng$IMD_decile == 5] <- 3
IMDeng$IMD_quintile[IMDeng$IMD_decile == 6] <- 3
IMDeng$IMD_quintile[IMDeng$IMD_decile == 7] <- 4
IMDeng$IMD_quintile[IMDeng$IMD_decile == 8] <- 4
IMDeng$IMD_quintile[IMDeng$IMD_decile == 9] <- 5
IMDeng$IMD_quintile[IMDeng$IMD_decile == 10] <- 5


IMDeng <- IMDeng %>% select(LSOA = LSOA_code_2011, IMD_quintile_Eng = IMD_quintile)
IMDwales <- IMDwales %>% select(LSOA = LSOA_Code, IMD_quintile_Wal = WIMD_2019_Overall_Quintile)
IMDscot <- IMDscot %>% select(LSOA = LSOA_Code, IMD_quintile_Scot = IMD_quintile)



# Join them together:

dat <- left_join(dat, IMDeng, by = "LSOA")
dat <- left_join(dat, IMDwales, by = "LSOA")
dat <- left_join(dat, IMDscot, by = "LSOA")


dat <- dat %>% mutate(IMD_quintile_Eng = factor(IMD_quintile_Eng),
                      IMD_quintile_Wal = factor(IMD_quintile_Wal),
                      IMD_quintile_Scot = factor(IMD_quintile_Scot))


# create a variable to represent any IMD present

dat <- dat %>% mutate(anyIMD = factor(ifelse(is.na(IMD_quintile_Eng) & is.na(IMD_quintile_Wal) &
                                        is.na(IMD_quintile_Scot), "No IMD", "IMD present")))


# Extra variables:

summary(dat$anyIMD)

# IMD from any country:

dat$IMD_quintile_all <- dat$IMD_quintile_Eng
dat$IMD_quintile_all[is.na(dat$IMD_quintile_all)] <- dat$IMD_quintile_Wal[is.na(dat$IMD_quintile_all)]
dat$IMD_quintile_all[is.na(dat$IMD_quintile_all)] <- dat$IMD_quintile_Scot[is.na(dat$IMD_quintile_all)]




dat$country <- factor(dat$country, levels = c("England", "Wales"), ordered = FALSE)
levels(dat$country)

# relevel gender

dat$gender <- relevel(dat$gender, ref = "Male")


head(dat)



#Need to convert dates to dates

dat <- dat %>% mutate(ref_date = as.Date(ref_date, format = "%d/%m/%Y"), 
                      assess_date = as.Date(assess_date, format = "%d/%m/%Y"),
                      start_date = as.Date(start_date, format = "%d/%m/%Y"),
                      discharge_date = as.Date(discharge_date, format = "%d/%m/%Y"))


# Create the 'date to date' variables

dat <- dat %>% mutate(ref_to_start_days = as.numeric(start_date - ref_date),
                      assess_to_start_days = as.numeric(start_date - assess_date),
                      assess_to_discharge_days = as.numeric(discharge_date - assess_date))

# ref to start days needs to be for stable COPD

table(dat$ref_location)

dat <- dat %>% mutate(ref_to_start_days_stable_COPD = ifelse(
    ref_location == "Primary/Community - stable COPD" |
      ref_location == "Secondary Care - stable COPD", 
    ref_to_start_days, NA))

summary(dat$ref_to_start_days_stable_COPD)


# assess to start days probably also needs to be for stable COPD

dat <- dat %>% mutate(assess_to_start_days_stable_COPD = ifelse(
  ref_location == "Primary/Community - stable COPD" |
    ref_location == "Secondary Care - stable COPD", 
  assess_to_start_days, NA))

summary(dat$assess_to_start_days_stable_COPD)


# Make a variable that says whether or not referal date was recorded
                      
dat <- dat %>% mutate(ref_date_rec = as.character(ref_date_NR))
dat$ref_date_rec[is.na(dat$ref_date_rec)] <- "Known"
dat$ref_date_rec <- factor(dat$ref_date_rec)
summary(dat$ref_date_rec)

# We created a more useful variable so we can drop the other one:

dat$ref_date_NR <- NULL



# Make a variable that says whether or not referal date was recorded



dat <- dat %>% mutate(FEV1_percpred_rec = as.character(FEV1_NR),
                      FEV1FVC_rec = as.character(FEV1FVC_NR))

dat$FEV1_percpred_rec[is.na(dat$FEV1_percpred_rec)] <- "Recorded"
dat$FEV1FVC_rec[is.na(dat$FEV1FVC_rec)] <- "Recorded"

dat <- dat %>% mutate(FEV1_percpred_rec = factor(FEV1_percpred_rec),
                      FEV1FVC_rec = factor(FEV1FVC_rec))


dat$FEV1_NR <- NULL
dat$FEV1FVC_NR <- NULL

summary(dat$FEV1_percpred_rec)
summary(dat$FEV1FVC_rec)


# Start date within 90 days for stable COPD

dat <- dat %>% mutate(ninety_day_referral_to_start_for_stable_COPD = ifelse(
                      ref_to_start_days < 90, "<90 days", ">=90 days")) %>%
                        mutate(ninety_day_referral_to_start_for_stable_COPD = factor(ifelse(
                          ref_location == "Primary/Community - stable COPD" |
                            ref_location == "Secondary Care - stable COPD", 
                          ninety_day_referral_to_start_for_stable_COPD, NA)))



dat <- dat %>% mutate(thirty_day_referral_to_start_for_AECOPD = ifelse(
  ref_to_start_days < 30, "<30 days", ">=30 days")) %>%
  mutate(thirty_day_referral_to_start_for_AECOPD = as.factor(ifelse(
    ref_location == "Primary/Community - post treatment for AECOPD" |
      ref_location == "Secondary Care - post admission for AECOPD", 
    thirty_day_referral_to_start_for_AECOPD, NA))) # %>% 

summary(dat$ninety_day_referral_to_start_for_stable_COPD)
summary(dat$thirty_day_referral_to_start_for_AECOPD)


# Sort out tests

# Initial tests

dat %>% select(test_type_init, ESWT_at_init) %>% table(useNA = "ifany")

colnames(dat)

dat <- dat %>% mutate(all_3_test_types_init = ifelse(test_type_init == "6MWT" & ESWT_at_init == "No", "6MWT only",
                                              ifelse(test_type_init == "ISWT" & ESWT_at_init == "No", "ISWT only",
                                                     ifelse(test_type_init == "6MWT" & ESWT_at_init == "Yes", "6MWT and ESWT",
                                                            ifelse(test_type_init == "ISWT" & ESWT_at_init == "Yes", "ISWT and ESWT",
                                              ifelse(test_type_init == "None", "None", "Remote"))))))
table(dat$all_3_test_types_init)

dat$all_3_test_types_init <- factor(dat$all_3_test_types_init, levels = c("6MWT only", "6MWT and ESWT", "ISWT only", "ISWT and ESWT", 
                                                                    "None", "Remote"))

summary(dat$all_3_test_types_init)

# No one only get ESWT this time so don't need to do this:

# dat <- dat %>% mutate(who_only_gets_ESWT_init = ifelse(ESWT_at_init == "No", NA, 
#                                                    ifelse(ESWT_at_init == "Yes" & test_type_init == "None", "Only ESWT",
#                                                           "ESWT and other walking test")))
# table(dat$who_only_gets_ESWT_init)
# 
# dat <- dat %>% mutate(which_other_test_with_ESWT_init = ifelse(ESWT_at_init == "No", NA, 
#                                               ifelse(ESWT_at_init == "Yes" & test_type_init == "ISWT", "ISWT",
#                                               ifelse(ESWT_at_init == "Yes" & test_type_init == "6MWT", "6MWT", NA))))
 
# dat$who_only_gets_ESWT_init <- factor(dat$who_only_gets_ESWT_init)
# dat$which_other_test_with_ESWT_init <- factor(dat$which_other_test_with_ESWT_init)

# summary(dat$who_only_gets_ESWT_init)
# summary(dat$which_other_test_with_ESWT_init)

# table(dat$ESWT_at_init, dat$all_3_test_types_init, useNA = "ifany")

# Test value and practice test variable broken down by test type
dat <- dat %>% mutate(test_value_6MWT_init = ifelse(test_type_init == "6MWT", test_value_init, NA),
                      test_value_ISWT_init = ifelse(test_type_init == "ISWT", test_value_init, NA))

dat <- dat %>% mutate(prac_test_6MWT_init = factor(ifelse(test_type_init == "6MWT",
                                                          as.character(prac_test_init), NA)),
                      prac_test_ISWT_init = factor(ifelse(test_type_init == "ISWT",
                                                          as.character(prac_test_init), NA)))
 
summary(dat$test_value_6MWT_init)
summary(dat$test_value_ISWT_init)

summary(dat$prac_test_6MWT_init)

# Discharge
dat <- dat %>% mutate(all_3_test_types_dis = ifelse(test_type_dis == "6MWT" & ESWT_at_dis == "No", "6MWT only",
                                                     ifelse(test_type_dis == "ISWT" & ESWT_at_dis == "No", "ISWT only",
                                                     ifelse(test_type_dis == "6MWT" & ESWT_at_dis == "Yes", "6MWT and ESWT",
                                                     ifelse(test_type_dis == "ISWT" & ESWT_at_dis == "Yes", "ISWT and ESWT",
                                                     ifelse(test_type_dis == "None", "None", "Remote"))))))
       
# # # # 

dat$all_3_test_types_dis <- factor(dat$all_3_test_types_dis, levels = c("6MWT only", "6MWT and ESWT", "ISWT only", "ISWT and ESWT", 
                                                                    "None", "Remote"))
table(dat$ESWT_at_dis)

table(dat$all_3_test_types_dis, useNA = "ifany")

# dat <- dat %>% mutate(who_is_ESWT_given_to_dis = ifelse(ESWT_at_dis == "Yes" & test_type_dis == "ISWT", "ISWT",
#                                                          ifelse(ESWT_at_dis == "Yes" & test_type_dis == "6MWT", "6MWT",
#                                                                 ifelse(ESWT_at_dis == "Yes" & test_type_dis == "None",
#                                                                        "ESWT only", NA))))

# Test value and practice test variable broken down by test type


dat <- dat %>% mutate(test_value_6MWT_dis = ifelse(test_type_dis == "6MWT", test_value_dis, NA),
                      test_value_ISWT_dis = ifelse(test_type_dis == "ISWT", test_value_dis, NA))







# No practice test info for discharge


# Where are patients enrolled?

table(dat$centre_based, dat$home_based, useNA = "ifany")

colnames(dat)

summary(dat$enrolled)

summary(dat$centre_based)
summary(dat$home_based)

summary(dat)

dat <- dat %>% mutate(PR_location = NA)
dat$PR_location[dat$centre_based == 1 & dat$home_based == 0] <- "Centre-based"
dat$PR_location[dat$centre_based == 0 & dat$home_based == 1] <- "Home-based"
dat$PR_location[dat$centre_based == 1 & dat$home_based == 1] <- "Both"


dat$PR_location <- factor(dat$PR_location, levels = c("Centre-based", "Home-based", "Both"))


# discharge assess bin

summary(dat$discharge_assess)

dat <- dat %>% mutate(discharge_assess_bin = NA)
dat$discharge_assess_bin[dat$discharge_assess == "Yes"] <- "Yes"
dat$discharge_assess_bin[dat$discharge_assess == "No - DNA" | 
                         dat$discharge_assess == "No - drop-out - health reasons" |
                         dat$discharge_assess == "No - drop-out - patient choice" ] <- "No"

# this is temporarily a character variable in order to make it easier to make the variables
# that are combined with programme type, but at the end I will convert it to a factor

table(dat$discharge_assess, dat$discharge_assess_bin)

# Change the discharge assessment variable...


# Not sure we need this variable...
# 
# 
# dat <- dat %>% rename(discharge_assess_no_reason = discharge_assess)
# dat$discharge_assess_no_reason[dat$discharge_assess_no_reason == "Yes"] <- NA
# dat <- dat %>% mutate(discharge_assess_no_reason = fct_drop(discharge_assess_no_reason))
# 
# table(dat$discharge_assess_no_reason, dat$discharge_assess_bin, useNA = "ifany")

# colnames(dat)
# 
# summary(dat$discharge_assess_bin)
# summary(dat$enrolled)

# Create the variables of discharge assessment by programme type




dat <- dat %>% mutate(discharge_assess_bin_by_centre = ifelse(PR_location == "Centre-based",
                                                            discharge_assess_bin, NA))
table(dat$discharge_assess_bin_by_centre, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(discharge_assess_bin_by_home = ifelse(PR_location == "Home-based",
                                                               discharge_assess_bin, NA))
table(dat$discharge_assess_bin_by_home, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(discharge_assess_bin_by_both = ifelse(PR_location == "Both",
                                                               discharge_assess_bin, NA))
table(dat$discharge_assess_bin_by_both, dat$PR_location, useNA = "ifany")


head(dat)

# Also create the variable exercise plan by programme type
# To do this, I need to convert exercise plan to a characer variable first

dat <- dat %>% mutate(exercise_plan = as.character(exercise_plan))




dat <- dat %>% mutate(exercise_plan_by_centre = ifelse(PR_location == "Centre-based",
                                                              exercise_plan, NA))
table(dat$exercise_plan_by_centre, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(exercise_plan_by_home = ifelse(PR_location == "Home-based",
                                                            exercise_plan, NA))
table(dat$exercise_plan_by_home, dat$PR_location, useNA = "ifany")


dat <- dat %>% mutate(exercise_plan_by_both = ifelse(PR_location == "Both",
                                                            exercise_plan, NA))
table(dat$exercise_plan_by_both, dat$PR_location, useNA = "ifany")


dat$PR_location <- factor(dat$PR_location)
summary(dat$PR_location)

dat <- dat %>% mutate(exercise_plan = factor(exercise_plan, levels = c("No", "Yes")))

  

dat <- dat %>% mutate(MRC_score_both = "Both known")
dat$MRC_score_both[dat$MRC_score_init == "Not recorded" | 
                     dat$MRC_score_dis == "Not recorded"] <- "1 or more not known"
dat$MRC_score_both[is.na(dat$MRC_score_init) | is.na(dat$MRC_score_dis)] <- NA


table(dat$MRC_score_both, useNA = "ifany")
table(dat$MRC_score_dis, useNA = "ifany")
table(dat$MRC_score_init, useNA = "ifany")

dat %>% select(MRC_score_init, MRC_score_dis) %>% table()

# Done.

# Now, to create the variable that says whether MRC score has changed.
# to do this safely, need to explicitly give the levels for the MRC score.

levels(dat$MRC_score_init) <- c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Not recorded")
levels(dat$MRC_score_dis) <- c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Not recorded")

dat <- dat %>% mutate(MRC_score_init_num = as.numeric(MRC_score_init))
dat$MRC_score_init_num[dat$MRC_score_init_num == 6] <- NA

dat <- dat %>% mutate(MRC_score_dis_num = as.numeric(MRC_score_dis))
dat$MRC_score_dis_num[dat$MRC_score_dis_num == 6] <- NA

# This could be useful
dat <- dat %>% mutate(MRC_change_value = MRC_score_dis_num - MRC_score_init_num)

dat <- dat %>% mutate(MRC_change_factor = NA)
dat$MRC_change_factor[dat$MRC_change_value < 0] <- "Improved"
dat$MRC_change_factor[dat$MRC_change_value == 0] <- "Same"
dat$MRC_change_factor[dat$MRC_change_value > 0] <- "Worse"
dat$MRC_change_factor <- factor(dat$MRC_change_factor)

summary(dat$MRC_score_init_num)
summary(dat$MRC_score_dis_num)
summary(dat$MRC_change_value)

# Create a value for difference in walking tests and helath status questionnaires

dat <- dat %>% mutate(test_value_ISWT_diff = test_value_ISWT_dis - test_value_ISWT_init)
dat <- dat %>% mutate(test_value_6MWT_diff = test_value_6MWT_dis - test_value_6MWT_init)
dat <- dat %>% mutate(test_value_ESWT_diff = ESWT_value_dis - ESWT_value_init)
dat <- dat %>% mutate(CAT_score_diff = CAT_score_dis - CAT_score_init)
dat <- dat %>% mutate(CRQ_dyspnoea_diff = CRQ_dyspnoea_dis - CRQ_dyspnoea_init)
dat <- dat %>% mutate(CRQ_fatigue_diff = CRQ_fatigue_dis - CRQ_fatigue_init)
dat <- dat %>% mutate(CRQ_emotion_diff = CRQ_emotion_dis - CRQ_emotion_init)
dat <- dat %>% mutate(CRQ_mastery_diff = CRQ_mastery_dis - CRQ_mastery_init)


# Create MCID binary values

# MCID is now 35m for ISWT (was 48m)
dat <- dat %>% mutate(MCID_ISWT = NA)
dat$MCID_ISWT[dat$test_value_ISWT_diff < 35] <- "MCID not met"  # changed from 48m
dat$MCID_ISWT[dat$test_value_ISWT_diff >= 35] <- "MCID met"     # changed from 48m
dat$MCID_ISWT <- factor(dat$MCID_ISWT)
summary(dat$MCID_ISWT)

# MCID is 30 for 6MWT
dat <- dat %>% mutate(MCID_6MWT = NA)
dat$MCID_6MWT[dat$test_value_6MWT_diff < 30] <- "MCID not met"
dat$MCID_6MWT[dat$test_value_6MWT_diff >= 30] <- "MCID met"
dat$MCID_6MWT <- factor(dat$MCID_6MWT)
summary(dat$MCID_6MWT)

dat <- dat %>% mutate(MCID_N_ISWT_6MWT = NA)
dat$MCID_N_ISWT_6MWT[!is.na(dat$MCID_6MWT)] <- 1
dat$MCID_N_ISWT_6MWT[!is.na(dat$MCID_ISWT)] <- 1


# MCID for CAT is -2
dat <- dat %>% mutate(MCID_CAT = NA)
dat$MCID_CAT[dat$CAT_score_diff <= -2] <- "MCID met"
dat$MCID_CAT[dat$CAT_score_diff > -2] <- "MCID not met"
dat$MCID_CAT <- factor(dat$MCID_CAT)
summary(dat$MCID_CAT)

# MCID for each CRQ is 0.5
dat <- dat %>% mutate(MCID_CRQ_dyspnoea = NA)
dat$MCID_CRQ_dyspnoea[dat$CRQ_dyspnoea_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_dyspnoea[dat$CRQ_dyspnoea_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_dyspnoea <- factor(dat$MCID_CRQ_dyspnoea)
summary(dat$MCID_CRQ_dyspnoea)

dat <- dat %>% mutate(MCID_CRQ_fatigue = NA)
dat$MCID_CRQ_fatigue[dat$CRQ_fatigue_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_fatigue[dat$CRQ_fatigue_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_fatigue <- factor(dat$MCID_CRQ_fatigue)
summary(dat$MCID_CRQ_fatigue)

dat <- dat %>% mutate(MCID_CRQ_emotion = NA)
dat$MCID_CRQ_emotion[dat$CRQ_emotion_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_emotion[dat$CRQ_emotion_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_emotion <- factor(dat$MCID_CRQ_emotion)
summary(dat$MCID_CRQ_emotion)

dat <- dat %>% mutate(MCID_CRQ_mastery = NA)
dat$MCID_CRQ_mastery[dat$CRQ_mastery_diff < 0.5] <- "MCID not met"
dat$MCID_CRQ_mastery[dat$CRQ_mastery_diff >= 0.5] <- "MCID met"
dat$MCID_CRQ_mastery <- factor(dat$MCID_CRQ_mastery)
summary(dat$MCID_CRQ_mastery)


dat <- dat %>% mutate(MCID_N_CAT_CRQ = NA)
dat$MCID_N_CAT_CRQ[!is.na(dat$MCID_CAT)] <- 1
dat$MCID_N_CAT_CRQ[!is.na(dat$MCID_CRQ_fatigue)] <- 1



dat <- dat %>% mutate(CAT_CRQ_dis_N = NA)
dat$CAT_CRQ_dis_N[!is.na(dat$CAT_dis)] <- 1
dat$CAT_CRQ_dis_N[!is.na(dat$CRQ_dis)] <- 1


dat <- dat %>% mutate(CAT_CRQ_score_diff_N = NA)
dat$CAT_CRQ_score_diff_N[!is.na(dat$CAT_score_diff)] <- 1
dat$CAT_CRQ_score_diff_N[!is.na(dat$CRQ_score_diff)] <- 1


# recode as factors the variables we left before

dat$discharge_assess_bin <- factor(dat$discharge_assess_bin)

# Let's create some benchmarking variables

dat <- dat %>% mutate(BM_start_90 = ifelse(ref_to_start_days_stable_COPD < 90, 1,
                                    ifelse(ref_to_start_days_stable_COPD >= 90, 0, NA)))



# The bit of code below is just to reassure myself that the median for practice test really is 8% with an upper
# quartile of 82%.

# dat %>% group_by(org_code) %>% summarise(practest = sum(BM_prac_test, na.rm = TRUE)/sum(!is.na(BM_prac_test))*100) %>% 
#   arrange(practest) %>% print(., n = 300) 

dat <- dat %>% mutate(BM_prac_test = ifelse(prac_test_init == "No", 0,
                                     ifelse(prac_test_init == "Yes", 1, NA))) 

dat <- dat %>% mutate(BM_discharge_assess = ifelse(discharge_assess_bin == "No", 0,
                                            ifelse(discharge_assess_bin == "Yes", 1, NA)))


dat <- dat %>% mutate(BM_exercise_plan = ifelse(exercise_plan == "No", 0,
                                         ifelse(exercise_plan == "Yes", 1, NA)))


dat <- dat %>% mutate(BM_MCID_exercise = 0)
dat$BM_MCID_exercise[is.na(dat$MCID_6MWT) & is.na(dat$MCID_ISWT)] <- NA
dat$BM_MCID_exercise[dat$MCID_6MWT == "MCID met"] <- 1
dat$BM_MCID_exercise[dat$MCID_ISWT == "MCID met"] <- 1



# table(dat$BM_MCID_exercise, dat$MCID_6MWT, useNA = "ifany")
# table(dat$BM_MCID_exercise, dat$MCID_ISWT, useNA = "ifany")
# table(dat$MCID_ISWT, dat$MCID_6MWT, useNA = "ifany")
# table(dat$BM_MCID_exercise, useNA = "ifany")


dat <- dat %>% mutate(BM_MCID_CAT_CRQ = 0)
dat$BM_MCID_CAT_CRQ[is.na(dat$MCID_CAT) & is.na(dat$MCID_CRQ_dyspnoea)] <- NA

dat$BM_MCID_CAT_CRQ[dat$MCID_CAT == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_dyspnoea == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_emotion == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_fatigue == "MCID met"] <- 1
dat$BM_MCID_CAT_CRQ[dat$MCID_CRQ_mastery == "MCID met"] <- 1


table(dat$BM_MCID_CAT_CRQ, dat$MCID_CAT)
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_fatigue, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_emotion, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_dyspnoea, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, dat$MCID_CRQ_mastery, useNA = "ifany")
table(dat$BM_MCID_CAT_CRQ, useNA = "ifany")


# We need this for the analysis as well... But I will do it as a factor
dat <- dat %>% mutate(MCID_exercise_cat = BM_MCID_exercise)
dat$MCID_exercise_cat[dat$MCID_exercise_cat == 0] <- "MCID not met"
dat$MCID_exercise_cat[dat$MCID_exercise_cat == 1] <- "MCID met"
dat$MCID_exercise_cat <- factor(dat$MCID_exercise_cat)
dat$MCID_exercise_cat <- relevel(dat$MCID_exercise_cat, ref = "MCID not met")
summary(dat$MCID_exercise_cat)


# We need this for the analysis as well... But I will do it as a factor
dat <- dat %>% mutate(MCID_CAT_CRQ_cat = BM_MCID_CAT_CRQ)
dat$MCID_CAT_CRQ_cat[dat$MCID_CAT_CRQ_cat == 0] <- "MCID not met"
dat$MCID_CAT_CRQ_cat[dat$MCID_CAT_CRQ_cat == 1] <- "MCID met"
dat$MCID_CAT_CRQ_cat <- factor(dat$MCID_CAT_CRQ_cat)
dat$MCID_CAT_CRQ_cat <- relevel(dat$MCID_CAT_CRQ_cat, ref = "MCID not met")
summary(dat$MCID_CAT_CRQ_cat)



dat <- dat %>% mutate(discharge_assess_bin_by_MRC1_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                          ifelse(MRC_score_init != "Grade 1", NA,
                                                                 as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC2_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 2", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC3_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 3", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC4_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 4", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC5_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Grade 5", NA,
                                                                               as.character(discharge_assess_bin)))),
                      discharge_assess_bin_by_MRC_NR_init = factor(ifelse(is.na(discharge_assess_bin), NA,
                                                                        ifelse(MRC_score_init != "Not recorded", NA,
                                                                               as.character(discharge_assess_bin)))))


dat %>% select(discharge_assess_bin, MRC_score_init) %>% table()

summary(dat$discharge_assess_bin_by_MRC1_init)
summary(dat$discharge_assess_bin_by_MRC2_init)
summary(dat$discharge_assess_bin_by_MRC3_init)
summary(dat$discharge_assess_bin_by_MRC4_init)
summary(dat$discharge_assess_bin_by_MRC5_init)
summary(dat$discharge_assess_bin_by_MRC_NR_init)


# I've checked that all these variables are okay, and correspond to the number of missing values.

summary(dat$ref_location)


# Can split into centre/home-based/both, if required:

# mediSumRound(dat, "scheduled_sess", 0), # good
# # need rec_sess_group and rec_sess indiv.
# # No way to tell where these sessions were based
# mediSumRound(dat, "rec_sess_group", 0),
# mediSumRound(dat, "rec_sess_indiv", 0),

dat$scheduled_sess_by_centre <- dat$scheduled_sess
dat$scheduled_sess_by_centre[dat$PR_location != "Centre-based"] <- NA
dat$rec_sess_group_by_centre <- dat$rec_sess_group
dat$rec_sess_group_by_centre[dat$PR_location != "Centre-based"] <- NA
dat$rec_sess_indiv_by_centre <- dat$rec_sess_indiv
dat$rec_sess_indiv_by_centre[dat$PR_location != "Centre-based"] <- NA

dat$scheduled_sess_by_home <- dat$scheduled_sess
dat$scheduled_sess_by_home[dat$PR_location != "Home-based"] <- NA
dat$rec_sess_group_by_home <- dat$rec_sess_group
dat$rec_sess_group_by_home[dat$PR_location != "Home-based"] <- NA
dat$rec_sess_indiv_by_home <- dat$rec_sess_indiv
dat$rec_sess_indiv_by_home[dat$PR_location != "Home-based"] <- NA

dat$scheduled_sess_by_both <- dat$scheduled_sess
dat$scheduled_sess_by_both[dat$PR_location != "Both"] <- NA
dat$rec_sess_group_by_both <- dat$rec_sess_group
dat$rec_sess_group_by_both[dat$PR_location != "Both"] <- NA
dat$rec_sess_indiv_by_both <- dat$rec_sess_indiv
dat$rec_sess_indiv_by_both[dat$PR_location != "Both"] <- NA


summary(dat$scheduled_sess_by_centre)
summary(dat$rec_sess_group_by_centre)
summary(dat$rec_sess_indiv_by_centre)

summary(dat$scheduled_sess_by_home)
summary(dat$rec_sess_group_by_home)
summary(dat$rec_sess_indiv_by_home)

summary(dat$scheduled_sess_by_both)
summary(dat$rec_sess_group_by_both)
summary(dat$rec_sess_indiv_by_both)

summary(dat$PR_location)







# Do some data cleaning

nlc("Total number of admissions in dataset:")

nrow(dat)

summary(dat$assess_date)
summary(dat$discharge_date)

# We are changing the time period to a 3-month time period, from 1st March to 31st May.



nlc("How many people are in the dataset who were assessed before 1st March 2021?")

dat %>% filter(assess_date < "2021-03-01") %>% nrow()

nlc("How many people are assessed after the 28th February 2022?")

dat %>% filter(assess_date > "2022-02-28") %>% nrow()




nrow(dat)

nlc("Now we do some data cleaning. Is there anyone who receives their assessment date before their referral date?")

dat %>% filter(assess_date < ref_date) %>% nrow()
dat <- dat %>% filter(assess_date >= ref_date | is.na(assess_date) | is.na(ref_date)) 

nlc("does anyone receive their start date before their assessment date?")

dat %>% filter(start_date < assess_date) %>% nrow()
dat <- dat %>% filter(start_date >= assess_date | is.na(assess_date) | is.na(start_date)) 

nlc("Is anyone discharged before their start date?")
dat %>% filter(discharge_date < start_date) %>% nrow()
dat <- dat %>% filter(discharge_date >= start_date | is.na(discharge_date) | is.na(start_date)) 


nlc("Is anyone marked as missing something who has that same thing encoded?")

# referral date
dat %>% filter(is.na(ref_date) & ref_date_rec == "Known") %>% nrow()
dat %>% filter(!is.na(ref_date) & ref_date_rec == "Not known") %>% nrow()

dat %>% filter(is.na(ref_date) & ref_date_rec == "Not known") %>% nrow()
dat %>% filter(!is.na(ref_date) & ref_date_rec == "Known") %>% nrow()

dat <- dat %>% filter(!(is.na(ref_date) & ref_date_rec == "Known"))
dat <- dat %>% filter(!(!is.na(ref_date) & ref_date_rec == "Not known"))

# This is all fine.

# FEV1 perc pred
dat %>% filter(is.na(FEV1_percpred) & FEV1_percpred_rec == "Recorded") %>% nrow()
dat %>% filter(!is.na(FEV1_percpred) & FEV1_percpred_rec == "Not recorded") %>% nrow()

dat %>% filter(is.na(FEV1_percpred) & FEV1_percpred_rec == "Not recorded") %>% nrow()
dat %>% filter(!is.na(FEV1_percpred) & FEV1_percpred_rec == "Recorded") %>% nrow()

dat <- dat %>% filter(!(is.na(FEV1_percpred) & FEV1_percpred_rec == "Recorded"))
dat <- dat %>% filter(!(!is.na(FEV1_percpred) & FEV1_percpred_rec == "Not recorded"))

# FEV1FVC
dat %>% filter(is.na(FEV1FVC) & FEV1FVC_rec == "Recorded") %>% nrow()
dat %>% filter(is.na(FEV1FVC) & FEV1FVC_rec == "Not recorded") %>% nrow()
dat %>% filter(!is.na(FEV1FVC) & FEV1FVC_rec == "Recorded") %>% nrow()
dat %>% filter(!is.na(FEV1FVC) & FEV1FVC_rec == "Not recorded") %>% nrow()

dat <- dat %>% filter(!(is.na(FEV1FVC) & FEV1FVC_rec == "Recorded"))
dat <- dat %>% filter(!(!is.na(FEV1FVC) & FEV1FVC_rec == "Not recorded"))

nrow(dat)



# No conflicting records.

nlc("How many people are missing IMD quintiles for any country?")
dat %>% filter(is.na(IMD_quintile_Eng) & is.na(IMD_quintile_Scot) & is.na(IMD_quintile_Wal)) %>% nrow()

nlc("This doesn't matter though.")

nlc("Finally, we filter out those with invalid NHS numbers:")
dat %>% filter(nhs_number_valid == 0) %>% nrow()

nlc("Removed to leave this many people in our dataset:")
dat <- dat %>% filter(nhs_number_valid == 1)
nrow(dat)


dat %>% select(country, trust_code, org_code, LSOA, age, gender, ethnicity, ref_date, 
               ref_location, assess_date) %>% nrow()

nlc("assess whether there are duplicate records. Done based on:
country, trust_code, patient_ID, org_code, LSOA, age, gender, ethnicity, ref_date, 
ref_location, assess_date. This many duplicates:")

dat %>% select(country, trust_code, patient_ID, org_code, LSOA, age, gender, ethnicity, ref_date, 
               ref_location, assess_date) %>% duplicated() %>% sum() %>% nlc()


nlc("After duplicates removed, we have this many people:")
dat <- dat[!duplicated(select(dat, country, trust_code, patient_ID, org_code, LSOA, age, gender, ethnicity, ref_date, 
                               ref_location, assess_date)), ]

nlc(nrow(dat))



# We have a few columns that are character when they should be factor, so we convert them to factor,
# but we leave 'study_ID', 'patient_ID' and 'LSOA' as character.

dat %>% select_if(is.character) %>% colnames()

dat <- dat %>% mutate_if(is.character, factor) %>% 
           mutate_at(c("study_ID", "patient_ID", "LSOA"), as.character)

sink()

# saveRDS(dat, "Z:/Group_work/Alex/Encrypted/Alex/PR/PR clinical 2021-22/Data/tidyData/PR_clinical_2021-22_cleaned.RDS")




