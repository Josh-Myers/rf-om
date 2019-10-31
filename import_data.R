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
                                   s, s, s, s, n, s, s, s, s, s, n, n, t, s, n,
                                   t, s, t, t, t, t, t, t, t, s))

colnames(quest_12) = c('id', 'num_om_0_to_1', 'age_first_om_q12', 'saw_gp_q12', 'use_dummy_q12', 'start_dummy_q12', 'stop_dummy_q12',
                       'feed_q12', 'age_breast_start_q12', 'age_breast_stop_q12', 'age_formula_start_q12', 'day_care_q12', 'type_care_q12', 
                       'num_kids_in_group_q12', 'num_days_per_week_q12', 'age_start_care_q12', "num_urti_q12", 'num_people_at_home_q12', 
                       'num_siblings_at_home_q12', 'sib_om_hx_12', "num_sibs_under_5_q12", 'parent_hx_om_q12', 'mum_edu_q12', 'dad_edu_q12', 'income_q12', 
                       'mum_smoke_q12', 'num_cig_mum_q12', 'dad_smoke_q12', 'num_cig_dad_q12')

quest_24 = read_xlsx("Questionnaires.xlsx", sheet=2, na=c(NA, "", "NFA", "NA"),
                     col_types = c(n,  s,  s,  s,  s,  s,  n,  n,  t, s, s, s, s, s, t, 
                                   n, n, t, n, n, n, s, s, t, t, n, n, n, s, s, 
                                   s, s, s, s, n, s, s, s, s, s, n, n, t, s, n,
                                   t, s, t, t, t, t, t, t, t, s, s))

colnames(quest_24) = c('id', 'num_om_1_to_2', 'age_first_om_q24', 'saw_gp_q24', 'use_dummy_q24', 'start_dummy_q24', 'stop_dummy_q24',
                       'feed_q24', 'age_breast_start_q24', 'age_breast_stop_q24', 'age_formula_start_q24', 'day_care_q24', 'type_care_q24', 
                       'num_kids_in_group_q24', 'num_days_per_week_q24', 'age_start_care_q24', 'num_urti_q24', 'num_people_at_home_q24', 
                       'num_siblings_at_home_q24', 'sib_om_hx_24', "num_sibs_under_5_q24", 'parent_hx_om_q24', 'mum_edu_q24', 'dad_edu_q24', 'income_q24', 
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

# survival analysis
# 1. OME
ome_df = select(followed_up, id, tymp_6, tymp_12, tymp_18, wai_pred_neonate, gender, type.of.birth, birth.weight, body.length,
                  head.circum)

ome_df$parent_smoke = ifelse(followed_up$mum_smoke_q24 %in% c('yes', 'Yes') | followed_up$dad_smoke_q24 %in% c('yes', 'Yes')
                               | followed_up$mum_smoke_q12 %in% c('yes', 'Yes') | followed_up$dad_smoke_q12 %in% c('yes', 'Yes'),
                               'yes', ifelse(is.na(followed_up$mum_smoke_q24) %in% c('no', 'No') | followed_up$dad_smoke_q24 %in% c('nO', 'no', 'No')
                                             | followed_up$mum_smoke_q12 %in% c('`No', 'No', 'no') | followed_up$dad_smoke_q12 %in% c('no', 'No'), 
                                             'no', 'unknown'))
ome_df$parent_smoke = as.factor(ome_df$parent_smoke)

ome_df$feed_birth = ifelse(followed_up$type.of.feed.0 %in% c('B', 'breast', 'Breast'), 'breast',
                             ifelse(followed_up$type.of.feed.0 %in% c('both', 'Both'), 'mixed', 
                                    ifelse(followed_up$type.of.feed.0 %in% c('formula', 'Formula'), 'formula', 'breast')))
ome_df$feed_birth = as.factor(ome_df$feed_birth)

ome_df$dummy_birth = ifelse(followed_up$using.pacifier.0 %in% c('Yes'), 'Yes', 'No') # 7 na impute as no

# the feed, urti, dummy 6, 12, 18 vars are time varying and need to be consolidated after changing to long form
ome_df$feed_6 = ifelse(followed_up$type.of.feed.6 %in% c('breast', 'Breast'), 'Breast',
                       ifelse(followed_up$type.of.feed.6 %in% c('formula', 'bottle'), 'Formula',
                              ifelse(followed_up$type.of.feed.6 %in% c('both', "Both", "Cow's Milk", 'mixed'), 'Mixed/Other', NA))) # other is cow milk n=1

ome_df$feed_12 = ifelse(followed_up$type.of.feed.12 %in% c('breast', 'Breast'), 'Breast',
                        ifelse(followed_up$type.of.feed.12 %in% c('formula', 'Formula'), 'Formula',
                               ifelse(followed_up$type.of.feed.12 %in% c('both', 'Both', 'Mixed', 'cow', "Cow's milk", "cow's milk",
                                                                         'mixed', 'Neither', 'nil', 'Nil', 'other', 'solid', 'Solid'), "Mixed/Other", NA)))

ome_df$feed_18 = ifelse(followed_up$type.of.feed.18 %in% c('breast', "Breast"), 'Breast',
                        ifelse(followed_up$type.of.feed.18 %in% c('formula', 'Formula'), 'Formula',
                               ifelse(followed_up$type.of.feed.18 %in% c('both', "breast & cow's milk", 'cow', "Cow's milk", 'mixed',
                                                                         'Na', 'nil', 'other', 'Solid'), 'Mixed/Other', NA)))

ome_df$urti_6 = ifelse(followed_up$cold.or.flu.6 %in% c('yes', "Yes"), "Yes",
                       ifelse(followed_up$cold.or.flu.6 %in% c('Na', 'no', 'No'), 'No', NA))

ome_df$urti_12 = ifelse(followed_up$cold.or.flu.12 %in% c('yes', 'Yes'), "Yes",
                        ifelse(followed_up$cold.or.flu.12 %in% c('no', 'No'), 'No', NA))

ome_df$urti_18 = ifelse(followed_up$cold.or.flu.18 %in% c('yes', "Yes"), "Yes",
                        ifelse(followed_up$cold.or.flu.18 %in% c('no', "No"), "No", NA))

ome_df$dummy_6 = ifelse(followed_up$using.pacifier.6 %in% c('Yes', 'yes'), "Yes",
                        ifelse(followed_up$using.pacifier.6 %in% c('no', 'No'), "No", NA))

ome_df$dummy_12 = ifelse(followed_up$using.pacifier.12 %in% c('Yes', 'yes'), "Yes", 
                         ifelse(followed_up$using.pacifier.12 %in% c('no', "No"), 'No', NA))

ome_df$dummy_18 = ifelse(followed_up$using.pacifier.18 %in% c('yes', 'Yes'), "Yes",
                         ifelse(followed_up$using.pacifier.18 %in% c('Na', 'no', "No"), 'No', NA))

# daycare
# create daycare yes/no to use in start daycare (months) variable 
followed_up$daycare = ifelse(followed_up$day_care_q12 %in% c('yes', "Yes"), "Yes",
                         ifelse(followed_up$day_care_q12 %in% c('no', "No"), "No",
                                ifelse(followed_up$day_care_q24 %in% c('yes', 'Yes'), 'Yes',
                                       ifelse(followed_up$day_care_q24 %in% c('no', "No"), 'No', NA))))

ome_df$start_daycare = ifelse(!is.na(followed_up$age_start_care_q12), followed_up$age_start_care_q12,
                              ifelse(!is.na(followed_up$age_start_care_q24), followed_up$age_start_care_q24,
                                     ifelse(followed_up$daycare %in% 'No', 24, NA))) # if no haven't started daycare by 24 months

ome_df$daycare_num_kids = ifelse(!is.na(followed_up$num_kids_in_group_q12), followed_up$num_kids_in_group_q12,
                                 ifelse(!is.na(followed_up$num_kids_in_group_q24), followed_up$num_kids_in_group_q24,
                                        ifelse(followed_up$daycare %in% 'No', 0, NA))) # will need to impute some of these

ome_df$daycare_num_days = ifelse(!is.na(followed_up$num_days_per_week_q12), followed_up$num_days_per_week_q12,
                                 ifelse(!is.na(followed_up$num_days_per_week_q24), followed_up$num_days_per_week_q24,
                                        ifelse(followed_up$daycare %in% 'No', 0, NA)))

ome_df$num_ppl_home = ifelse(!is.na(followed_up$num_people_at_home_q12), followed_up$num_people_at_home_q12,
                             ifelse(!is.na(followed_up$num_people_at_home_q24), followed_up$num_people_at_home_q24, NA))

ome_df$num_sibs = ifelse(!is.na(followed_up$num_siblings_at_home_q12), followed_up$num_siblings_at_home_q12,
                         ifelse(!is.na(followed_up$num_siblings_at_home_q24), followed_up$num_siblings_at_home_q24, NA))

ome_df$num_sibs_under5 = ifelse(!is.na(followed_up$num_sibs_under_5_q12), followed_up$num_sibs_under_5_q12,
                                ifelse(!is.na(followed_up$num_sibs_under_5_q24), followed_up$num_sibs_under_5_q24, NA))
  
ome_df$income = ifelse(!is.na(followed_up$income_q12), followed_up$income_q12,
                       ifelse(!is.na(followed_up$income_q24), followed_up$income_q24, NA))

ome_df$fam_hx_om = ifelse(!is.na(followed_up$parent_hx_om_q12), followed_up$parent_hx_om_q12,
                       ifelse(!is.na(followed_up$parent_hx_om_q24), followed_up$parent_hx_om_q24,
                              ifelse(!is.na(followed_up$sib_om_hx_12), followed_up$sib_om_hx_12,
                                     ifelse(!is.na(followed_up$sib_om_hx_24), followed_up$sib_om_hx_24, NA))))

ome_df$mum_edu = ifelse(!is.na(followed_up$mum_edu_q12), followed_up$mum_edu_q12,
                        ifelse(!is.na(followed_up$mum_edu_q24), followed_up$mum_edu_q24, NA))

ome_df$dad_edu = ifelse(!is.na(followed_up$dad_edu_q12), followed_up$dad_edu_q12,
                        ifelse(!is.na(followed_up$dad_edu_q24), followed_up$dad_edu_q24, NA))

ome_df$age_first_om = ifelse(!is.na(followed_up$age_first_om_q12), followed_up$age_first_om_q12,
                             ifelse(!is.na(followed_up$age_first_om_q24), followed_up$age_first_om_q24, NA))

# # length of breast feeding (make time varying instead)
# model_df$length_breast = ifelse(followed_up$type.of.feed.18 %in% c('both', 'breast', 'Breast', "breast & cow's milk"), '>18',
#                                 ifelse(followed_up$type.of.feed.12 %in% c('both', 'Both', 'breast', "Breast"), ">12", 
#                                        ifelse(followed_up$type.of.feed.6 %in% c('both', 'Both', 'mixed'), '>6',
#                                               ifelse(followed_up$type.of.feed.0 %in% c('B', 'both', 'Both', 'breast', 'Breast'), '>0',
#                                                      NA) )))
# 
# # or age formula introduced
# model_df$formula_intro = ifelse(!is.na(followed_up$age_formula_start_q12), followed_up$age_formula_start_q12,
#                                 ifelse(!is.na(followed_up$age_formula_start_q24), followed_up$age_formula_start_q24,
#                                        ifelse(followed_up$type.of.feed.0 %in% c('both', 'Both', 'formula', "Formula"), 'birth',
#                                               ifelse(followed_up$type.of.feed.6 %in% c('both', 'Both', 'bottle', 'formula', 'mixed'), '<6',
#                                                      ifelse(followed_up$type.of.feed.12 %in% c('both', 'Both', 'formula', 'Formula', 'mixed'), '<12',
#                                                             ifelse(followed_up$type.of.feed.18 %in% c('both', 'formula', 'Formula', 'mixed'), '<18',
#                                                                    'breast')))))) # all with no formla info had breast info, therfore, likely to be exclusively breastfed

# only include infants followed up
ome_df <- ome_df[!(is.na(ome_df$tymp_6)) | !(is.na(ome_df$tymp_12)) | !(is.na(ome_df$tymp_18)),]

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
ome_df$tymp_6 = ifelse(is.na(ome_df$tymp_6) & is.na(ome_df$tymp_12), 2, ome_df$tymp_6)

# set 6 to censored if attended 12 but not 6
ome_df$tymp_6 = ifelse(is.na(ome_df$tymp_6) & ome_df$tymp_12 %in% c(0,1), 2, ome_df$tymp_6)

# set 12 to censored if attended 6 (0 or 1) but not 12 or 18
ome_df$tymp_12 = ifelse(ome_df$tymp_6 %in% c(0,1) & is.na(ome_df$tymp_12) & is.na(ome_df$tymp_18), 2, ome_df$tymp_12)

# set 12 to censored if attended 18 but 12 is missing and attended 6
ome_df$tymp_12 = ifelse(ome_df$tymp_18 %in% c(0,1) & is.na(ome_df$tymp_12) & ome_df$tymp_6 %in% c(0,1), 2, ome_df$tymp_12)

# set 18 to censored if attended 12 but not 18 (censored at 18)
ome_df$tymp_18 = ifelse(ome_df$tymp_12 %in% c(0,1) & is.na(ome_df$tymp_18), 2, ome_df$tymp_18)

# make long
ome_df_long = pivot_longer(ome_df, cols = starts_with('tymp'))
ome_df_long$start=0
ome_df_long = select(ome_df_long, c(id, start, name, value, gender, type.of.feed.0))
colnames(ome_df_long) = c('id', 'start', 'stop', 'event', 'gender', 'type.of.feed.0')

ome_df_long$stop = ifelse(ome_df_long$stop == 'tymp_6', '6',
                        ifelse(ome_df_long$stop == 'tymp_12', '12', 
                               ifelse(ome_df_long$stop == 'tymp_18', '18', NA)))

ome_df_long$start = ifelse(ome_df_long$stop == '6', '0',
                         ifelse(ome_df_long$stop == '12', '6', 
                                ifelse(ome_df_long$stop == '18', '12', NA)))

# now set 0 to NA
ome_df_long$event = ifelse(ome_df_long$event == 0, NA, ome_df_long$event)

# then 2 to 0 (0 is censored)
ome_df_long$event = ifelse(ome_df_long$event == 2, 0, ome_df_long$event)

# then na.rm
ome_df_long = ome_df_long[complete.cases(ome_df_long$event),]



# 2 AOM model
# need to recreate df because some vars will be different than ome df - different time varying variables

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









