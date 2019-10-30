#### Risk factors for OM ####
# load libraries
library(tidyverse)
library(readxl)
library(survival)

# load neonate predictions
load("neonate_predictions.Rda")

# import demographics
n = 'numeric'
s = 'skip'
d = 'date'
t = 'text'

demographics_1 = read_xlsx("Demographic info.xlsx", sheet=1, na=c(NA, "", "NFA", "NA"), 
                           col_types = c('numeric', 'skip', 'skip', 'skip', 'date', 'date', 'skip', 'skip', 'skip', 'skip', 
                                         'skip', 'date', 'skip', 'skip', 'date', 'skip', 'skip', 'date', 'skip', 'skip', 
                                         'skip', 'date', 'skip', 'skip', 'skip', 'date', 'skip', 'skip', 'skip', 'skip', 
                                         'skip', 'skip'))

colnames(demographics_1) = c("id",  "dob", "date.of.test.0", "date.of.test.6", "date.of.test.12", "date.of.test.18",  
                             "12.date.ques.complete", "24.date.ques.complete")

demographics_2 = read_xlsx("Demographic info.xlsx", sheet=3, na=c(NA, "", "NFA", "NA"), 
                           col_types = c(n, s, s, s, d, t, n, t, n, n, n, t, s, s, s, 
                                         t, d, s, t, t, t, t, t, n, t, t, t, t, t, t, 
                                         n, t, n, t, n, n, s, n, s, n, s, n, s, t, n, n, n))

colnames(demographics_2) = c("id", "dob", "gender", "ga", "type.of.birth", "birth.weight", "head.circum", 'body.length',
                             'ethnicity', "risk.factors.SNHL", "date.time.birth", "type.of.feed.0", "establish.feed.0", 
                             "using.pacifier.0", "type.of.feed.6", "solids.6", "age.solids.introduced.mths", "using.pacifier.6", 
                             "type.of.feed.12", 'using.pacifier.12', 'type.of.feed.18', "using.pacifier.18", "cold.or.flu.6", 
                             "when.cold.6.mths", "cold.or.flu.12", "when.cold.12", "cold.or.flu.18", "when.cold.18", "age.0.hrs", 
                             "age.6.mth.weeks", "age.12.mth.weeks", "age.18.mth.weeks", "daycare.18.mth", "how.many.days.18mth", 
                             "num.in.group.18mth", "num.of.ear.infections.since.turning.1.18mth")

# import tymp data  
tymp_6 = read_xlsx("6 months.xlsx", sheet = 1, na=c(NA, "", "NFA", "NA", "CNT"),
                   col_types = c(n, s, s, s, s, s, t, s, s, s, s, s, s, s, s, s, s))
tymp_12 = read_xlsx("12 months.xlsx", sheet = 1, na=c(NA, "", "NFA", "NA", "CNT"),
                    col_types = c(n, s, s, s, t, s, s, s, s, s, s, s, s))
tymp_18 = read_xlsx("18 months.xlsx", sheet = 1, na=c(NA, "", "NFA", "NA", "CNT"),
                    col_types = c(n, s, s, s, t, s, s, s, s, s, s, s, s))
colnames(tymp_6) = c("id", "tymp")
colnames(tymp_12) = c("id", "tymp")
colnames(tymp_18) = c("id", "tymp")

summary(as.factor(tymp_6$tymp))
summary(as.factor(tymp_12$tymp))
summary(as.factor(tymp_18$tymp))

tymp_6$tymp[tymp_6$tymp == 'Pass'] = "pass"
tymp_6$tymp[tymp_6$tymp == "ps"] = "pass"

tymp_12$tymp[tymp_12$tymp == 'High compliance'] = "pass"
tymp_12$tymp[tymp_12$tymp == "negative"] = "pass"
tymp_12$tymp[tymp_12$tymp == "Negative peak"] = "pass"

tymp_18$tymp[tymp_18$tymp == 'High compliance'] = "pass"
tymp_18$tymp[tymp_18$tymp == "negative"] = "pass"
tymp_18$tymp[tymp_18$tymp == "Negative peak"] = "pass"

tymp_6$tymp = as.factor(tymp_6$tymp)
tymp_12$tymp = as.factor(tymp_12$tymp)
tymp_18$tymp = as.factor(tymp_18$tymp)

summary(tymp_6$tymp)
summary(tymp_12$tymp)
summary(tymp_18$tymp)

tymp_6 = na.omit(tymp_6)
tymp_12 = na.omit(tymp_12)
tymp_18 = na.omit(tymp_18)

# if om in one or both ears counts as 1 episode of om
max_tymp_6 = tymp_6 %>% 
  group_by(id) %>% 
  summarise(tymp = max(as.numeric(tymp)))

max_tymp_12 = tymp_12 %>% 
  group_by(id) %>% 
  summarise(tymp = max(as.numeric(tymp)))

max_tymp_18 = tymp_18 %>% 
  group_by(id) %>% 
  summarise(tymp = max(as.numeric(tymp)))

max_tymp_6$tymp = max_tymp_6$tymp - 1
max_tymp_12$tymp = max_tymp_12$tymp - 1
max_tymp_18$tymp = max_tymp_18$tymp - 1

# add together number of episodes from 6-18 months
id = as.data.frame(1:863)
colnames(id) = "id"

tymp = left_join(id, max_tymp_6)
colnames(tymp) = c("id", "tymp_6")

tymp = left_join(tymp, max_tymp_12)
colnames(tymp) = c("id", "tymp_6", "tymp_12")

tymp = left_join(tymp, max_tymp_18)
colnames(tymp) = c("id", "tymp_6", "tymp_12", "tymp_18")

tymp <- tymp[!(is.na(tymp$tymp_6)) | !(is.na(tymp$tymp_12)) | !(is.na(tymp$tymp_12)),]

tymp = tymp %>% 
  mutate(total_ome = rowSums(.[2:4], na.rm = T))

tymp = left_join(id, tymp)

# questionnaires
# need to make variables amt time breast fed, amt time formula etc

quest_12 = read_xlsx("Questionnaires.xlsx", sheet=1, na=c(NA, "", "NFA", "NA"),
                     col_types = c(n,  s,  s,  s,  s,  s,  n,  n,  t, s, s, s, s, s, t, 
                                 n, n, t, n, n, n, s, s, t, t, n, n, n, s, s, 
                                 s, s, s, s, n, s, s, s, s, s, n, n, s, s, n,
                                 t, s, t, t, t, t, t, t, t, s))

colnames(quest_12) = c('id', 'num_om_0_to_1', 'age_first_om_q12', 'saw_gp_q12', 'use_dummy_q12', 'start_dummy_q12', 'stop_dummy_q12',
                       'feed_q12', 'age_breast_start_q12', 'age_breast_stop_q12', 'age_formula_start_q12', 'day_care_q12', 'type_care_q12', 
                       'num_kids_in_group_q12', 'num_days_per_week_q12', 'age_start_care_q12', "num_urti_q12", 'num_people_at_home_q12', 
                       'num_siblings_at_home_q12', "num_sibs_under_5_q12", 'parent_hx_om_q12', 'mum_edu_q12', 'dad_edu_q12', 'income_q12', 
                       'mum_smoke_q12', 'num_cig_mum_q12', 'dad_smoke_q12', 'num_cig_dad_q12')

quest_24 = read_xlsx("Questionnaires.xlsx", sheet=2, na=c(NA, "", "NFA", "NA"),
                     col_types = c(n,  s,  s,  s,  s,  s,  n,  n,  t, s, s, s, s, s, t, 
                     n, n, t, n, n, n, s, s, t, t, n, n, n, s, s, 
                     s, s, s, s, n, s, s, s, s, s, n, n, s, s, n,
                     t, s, t, t, t, t, t, t, t, s, s))

colnames(quest_24) = c('id', 'num_om_1_to_2', 'age_first_om_q24', 'saw_gp_q24', 'use_dummy_q24', 'start_dummy_q24', 'stop_dummy_q24',
                       'feed_q24', 'age_breast_start_q24', 'age_breast_stop_q24', 'age_formula_start_q24', 'day_care_q24', 'type_care_q24', 
                       'num_kids_in_group_q24', 'num_days_per_week_q24', 'age_start_care_q24', 'num_urti_q24', 'num_people_at_home_q24', 
                       'num_siblings_at_home_q24', "num_sibs_under_5_q24", 'parent_hx_om_q24', 'mum_edu_q24', 'dad_edu_q24', 'income_q24', 
                       'mum_smoke_q24', 'num_cig_mum_q24', 'dad_smoke_q24', 'num_cig_dad_q24')


num_om_12 = cbind.data.frame(quest_12$id, quest_12$num_om_0_to_1)
num_om_24 = cbind.data.frame(quest_12$id, quest_24$num_om_1_to_2)
colnames(num_om_12) = c('id', 'num_om_0_to_1')
colnames(num_om_24) = c('id', 'num_om_1_to_2')

num_om = left_join(num_om_12, num_om_24)      

num_om <- num_om[!(is.na(num_om$num_om_0_to_1)) | !(is.na(num_om$num_om_1_to_2)),]

num_om = num_om %>% 
  mutate(total_om = rowSums(.[2:3], na.rm = T))

num_om = left_join(id, num_om)
num_om = select(num_om, c(id, total_om))

tymp = select(tymp, id, total_ome, tymp_6, tymp_12, tymp_18)

total_tymp_om = left_join(tymp, num_om)

total_tymp_om <- total_tymp_om[!(is.na(total_tymp_om$total_ome)) | !(is.na(total_tymp_om$total_om)),]

total_tymp_om = total_tymp_om %>% 
  mutate(total_ome_aom = rowSums(.[c(2,6)], na.rm = T))

# total_y is total number of fail tymps + number of episodes of aom
total_tymp_om = left_join(id, total_tymp_om)

# join wai preds
om_preds = left_join(total_tymp_om, max_preds)
colnames(om_preds) = c('id', 'total_ome', 'tymp_6', 'tymp_12', 'tymp_18', 'total_aom', 'total_ome_aom', 'wai_pred_neonate')

# join demographics and questionnaires
full_df = left_join(om_preds, demographics_1)
full_df = left_join(full_df, demographics_2)

full_df = left_join(full_df, quest_12)
full_df = left_join(full_df, quest_24)

# remove any not followed up at all (not seen 6, 12, 18, and no questionnaires)
followed_up = full_df[!(is.na(full_df$total_ome)) | !(is.na(full_df$total_aom)),]

# use tymp_6, tymp_12, tymp_18 for number seen for follow up 6, 12, 18 months
# the date of test 6, 12, 18 are incorrect, because admin were using these to make bookings - doesn't mean they were seen
# number of ome/aom episodes
summary(as.factor(followed_up$total_ome_aom))
summary(followed_up$total_ome_aom)

# followed up at least once (6, 12, 18)
# total
summary(as.factor(followed_up$total_ome))
# each age group
summary(as.factor(followed_up$tymp_6))
summary(as.factor(followed_up$tymp_12))
summary(as.factor(followed_up$tymp_18))

# completed at least one questionnaire
summary(as.factor(followed_up$total_aom))
# completed 12 and 24 mth
summary(followed_up$`12.date.ques.complete`) # 134 NA
summary(followed_up$`24.date.ques.complete`) # 271 NA

# now create features, eg, number of months breastfed, num formula etc

# then deal with missing data somehow
# Harrell p 402: "we assume that the censoring is non-informative about the event; 
# that is, the censoring is caused by something that is independent of the impending failure."

# could do rf for early oneset - because so many missing >12 months ???

# create a variable - last_seen - 6, 12, 18, or 24
followed_up$exit_study = ifelse(!is.na(followed_up$`24.date.ques.complete`), '24',
                                ifelse(!is.na(followed_up$tymp_18), '18',
                                       ifelse(!is.na(followed_up$tymp_12), '12',
                                              ifelse(!is.na(followed_up$`12.date.ques.complete`), '12',
                                                     ifelse(!is.na(followed_up$tymp_6), '6', NA)))))
         
summary(as.factor(followed_up$exit_study)) # 124 stayed right to the end                         

# create features and discard unneeded variables
# parent smokes
followed_up$parent_smoke = ifelse(followed_up$mum_smoke_q24 %in% c('yes', 'Yes') | followed_up$dad_smoke_q24 %in% c('yes', 'Yes')
                                   | followed_up$mum_smoke_q12 %in% c('yes', 'Yes') | followed_up$dad_smoke_q12 %in% c('yes', 'Yes'),
                                  'yes', ifelse(is.na(followed_up$mum_smoke_q24) %in% c('no', 'No') | followed_up$dad_smoke_q24 %in% c('nO', 'no', 'No')
                                                | followed_up$mum_smoke_q12 %in% c('`No', 'No', 'no') | followed_up$dad_smoke_q12 %in% c('no', 'No'), 
                                                'no', 'unknown'))
followed_up$parent_smoke = as.factor(followed_up$parent_smoke)

# type of feed at birth (6 na, impute as breast)
followed_up$feed_birth = ifelse(followed_up$type.of.feed.0 %in% c('B', 'breast', 'Breast'), 'breast',
                                ifelse(followed_up$type.of.feed.0 %in% c('both', 'Both'), 'mixed', 
                                       ifelse(followed_up$type.of.feed.0 %in% c('formula', 'Formula'), 'formula', 'breast')))
followed_up$feed_birth = as.factor(followed_up$feed_birth)

# length of breast feeding

# age entered daycare

# survival analysis
surv_ome = select(followed_up, c(id, tymp_6, tymp_12, tymp_18, gender, type.of.feed.0))
# only include infants followed up
surv_ome <- surv_ome[!(is.na(surv_ome$tymp_6)) | !(is.na(surv_ome$tymp_12)) | !(is.na(surv_ome$tymp_18)),]

# these data are interval censored https://www.r-project.org/conferences/useR-2010/tutorials/Fay_1.pdf
# Need to have 2 models because different intervals
# ome model 6, 12, 18 - censored = 0
# aom model 12, 24 - censored = 0, and have a covariate with number of episodes

# I have some interval censored - solution is to put 0 if right or interval censored, if not censored and normal remove that row
# https://stats.stackexchange.com/questions/383713/survival-analysis-with-recurrent-events-with-subjects-that-move-in-and-out-of-ri
# set censored to 0 and then rm na after in long form
# set to 0 (censored) if seen at 12 months, but NA at 6mths (interval censoring)
# 1. OME model
# add censoring, eg if 6,12,18 all na, 6 should be censored 
# if 12!=NA and 18=NA, 18 should be censored (seen at 12 months but not at 18)
# make censoring 2 at first, because 0 was normal, then set 0 to NA, then 2 to 0

# set 6 to censored if missing 6 and 12
surv_ome$tymp_6 = ifelse(is.na(surv_ome$tymp_6) & is.na(surv_ome$tymp_12), 2, surv_ome$tymp_6)

# set 6 to censored if attended 12 but not 6
surv_ome$tymp_6 = ifelse(is.na(surv_ome$tymp_6) & surv_ome$tymp_12 %in% c(0,1), 2, surv_ome$tymp_6)

# set 12 to censored if attended 6 (0 or 1) but not 12 or 18
surv_ome$tymp_12 = ifelse(surv_ome$tymp_6 %in% c(0,1) & is.na(surv_ome$tymp_12) & is.na(surv_ome$tymp_18), 2, surv_ome$tymp_12)

# set 12 to censored if attended 18 but 12 is missing and attended 6
surv_ome$tymp_12 = ifelse(surv_ome$tymp_18 %in% c(0,1) & is.na(surv_ome$tymp_12) & surv_ome$tymp_6 %in% c(0,1), 2, surv_ome$tymp_12)

# set 18 to censored if attended 12 but not 18 (censored at 18)
surv_ome$tymp_18 = ifelse(surv_ome$tymp_12 %in% c(0,1) & is.na(surv_ome$tymp_18), 2, surv_ome$tymp_18)

# make long
surv_ome_long = pivot_longer(surv_ome, cols = starts_with('tymp'))
surv_ome_long$start=0
surv_ome_long = select(surv_ome_long, c(id, start, name, value, gender, type.of.feed.0))
colnames(surv_ome_long) = c('id', 'start', 'stop', 'event', 'gender', 'type.of.feed.0')

surv_ome_long$stop = ifelse(surv_ome_long$stop == 'tymp_6', '6',
                        ifelse(surv_ome_long$stop == 'tymp_12', '12', 
                               ifelse(surv_ome_long$stop == 'tymp_18', '18', NA)))

surv_ome_long$start = ifelse(surv_ome_long$stop == '6', '0',
                         ifelse(surv_ome_long$stop == '12', '6', 
                                ifelse(surv_ome_long$stop == '18', '12', NA)))

# now set 0 to NA
surv_ome_long$event = ifelse(surv_ome_long$event == 0, NA, surv_ome_long$event)

# then 2 to 0 (0 is censored)
surv_ome_long$event = ifelse(surv_ome_long$event == 2, 0, surv_ome_long$event)

# then na.rm
surv_ome_long = surv_ome_long[complete.cases(surv_ome_long$event),]

# 2 AOM model
surv_aom = select(followed_up, c(id, num_om_0_to_1, num_om_1_to_2, gender, type.of.feed.0))
# only include infants followed up
surv_aom <- surv_aom[!(is.na(surv_aom$num_om_0_to_1)) | !(is.na(surv_aom$num_om_1_to_2)),]
colnames(surv_aom) = c('id', 'year_1', 'year_2', 'gender', 'type.of.feed.0')

# set censoring to 'c', then 0 to na, then c to 0
# if 24mth not missing, and 12 is missing, 12 is censored
surv_aom$year_1 = ifelse(!is.na(surv_aom$year_2) & is.na(surv_aom$year_1), 'c', surv_aom$year_1)
# if both normal, 24 is censored
surv_aom$year_2 = ifelse(surv_aom$year_1==0 & surv_aom$year_2==0, 'c', surv_aom$year_2)
# if both missing, 12 is censored
surv_aom$year_1 = ifelse(is.na(surv_aom$year_1) & is.na(surv_aom$year_2), 'c', surv_aom$year_1)
# if 12mth is normal and 24mth missing, 12 mth is censored
surv_aom$year_1 = ifelse(is.na(surv_aom$year_2) & !is.na(surv_aom$year_1), 'c', surv_aom$year_1)
# if 24mth is normal, is censored
surv_aom$year_2 = ifelse(surv_aom$year_2==0, 'c', surv_aom$year_2)

surv_aom_long = pivot_longer(surv_aom, cols = starts_with('year'))
surv_aom_long$start=0

surv_aom_long = select(surv_aom_long, c(id, start, name, value, gender, type.of.feed.0))
colnames(surv_aom_long) = c('id', 'start', 'stop', 'event', 'gender', 'type.of.feed.0')
surv_aom_long$num_infect = surv_aom_long$event
surv_aom_long$num_infect = ifelse(surv_aom_long$num_infect=='c', 0, surv_aom_long$num_infect) # will need to impute missing

# now set >0 to 1, 0=NA, c=0 
# then remove NA rows (no event or censoring)
surv_aom_long$event = ifelse(surv_aom_long$event==0, NA, 
                             ifelse(surv_aom_long$event =='c', 0,
                                    ifelse(surv_aom_long$event >0, 1, NA)))

surv_aom_long$stop = ifelse(surv_aom_long$stop=='year_1', '12',
                            ifelse(surv_aom_long$stop=='year_2', '24', NA))

surv_aom_long$start = ifelse(surv_aom_long$stop == '12', '0',
                             ifelse(surv_aom_long$stop=='24', '12', NA))

surv_aom_long = surv_aom_long[complete.cases(surv_aom_long$event), ]
surv_aom_long$event = as.numeric(surv_aom_long$event)









