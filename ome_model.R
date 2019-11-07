# OME survival model
library(tidyverse)
library(survival)
library(mice)

load('om_data.Rda')

ome_df = select(followed_up, id, tymp_6, tymp_12, tymp_18, wai_pred_neonate, gender, type.of.birth, birth.weight, body.length,head.circum)

ome_df$parent_smoke = ifelse(followed_up$mum_smoke_q24 %in% c('yes', 'Yes') | followed_up$dad_smoke_q24 %in% c('yes', 'Yes')
                             | followed_up$mum_smoke_q12 %in% c('yes', 'Yes') | followed_up$dad_smoke_q12 %in% c('yes', 'Yes'),
                             'yes', ifelse(is.na(followed_up$mum_smoke_q24) %in% c('no', 'No') | followed_up$dad_smoke_q24 %in% c('nO', 'no', 'No')
                                           | followed_up$mum_smoke_q12 %in% c('`No', 'No', 'no') | followed_up$dad_smoke_q12 %in% c('no', 'No'), 
                                           'no', NA))
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
                                                                         'Na', 'nil', 'other', 'Solid'), 'Mixed/Other',
                                      ifelse(followed_up$feed_q24 %in% c('breast', 'Breast', 'Breast fed'), 'Breast',
                                             ifelse(followed_up$feed_q24 %in% c('formula', 'Formula'), 'Formula',
                                                    ifelse(followed_up$feed_q24 %in% c('Both', 'Breast and Formula', 'cow', "Cow's",
                                                                                       "cow's milk", 'mixed', 'Neither', 'Niether',
                                                                                       'none', 'None', 'Other'), 'Mixed/Other', NA))))))

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
                         ifelse(followed_up$using.pacifier.18 %in% c('Na', 'no', "No"), 'No',
                                ifelse(followed_up$use_dummy_q24 %in% c('yes', 'Yes'), 'Yes',
                                       ifelse(followed_up$use_dummy_q24 %in% c('no', "No", "Noq"), 'No', NA))))

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
                                        ifelse(followed_up$daycare %in% 'No', 0, NA))) 

ome_df$daycare_num_days = ifelse(!is.na(followed_up$num_days_per_week_q12), followed_up$num_days_per_week_q12,
                                 ifelse(!is.na(followed_up$num_days_per_week_q24), followed_up$num_days_per_week_q24,
                                        ifelse(followed_up$daycare %in% 'No', 0, NA)))

ome_df$num_ppl_home = ifelse(!is.na(followed_up$num_people_at_home_q12), followed_up$num_people_at_home_q12,
                             ifelse(!is.na(followed_up$num_people_at_home_q24), followed_up$num_people_at_home_q24, NA))

ome_df$num_sibs = ifelse(!is.na(followed_up$num_siblings_at_home_q12), followed_up$num_siblings_at_home_q12,
                         ifelse(!is.na(followed_up$num_siblings_at_home_q24), followed_up$num_siblings_at_home_q24, NA))

ome_df$num_sibs_under5 = ifelse(!is.na(followed_up$num_sibs_under_5_q12), followed_up$num_sibs_under_5_q12,
                                ifelse(!is.na(followed_up$num_sibs_under_5_q24), followed_up$num_sibs_under_5_q24,
                                       ifelse(followed_up$num_siblings_at_home_q12 == 0, 0,
                                              ifelse(followed_up$num_siblings_at_home_q24 == 0, 0, NA))))

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
                             ifelse(!is.na(followed_up$age_first_om_q24), followed_up$age_first_om_q24, 
                                    ifelse(!is.na(followed_up$num_om_0_to_1), '<12',
                                           ifelse(!is.na(followed_up$num.of.ear.infections.since.turning.1.18mth), '<12',
                                                  ifelse(!is.na(followed_up$num_om_1_to_2), '<24', NA)))))

ome_df$age_first_om = ifelse(ome_df$age_first_om %in% c('0', '1', '1.5', '2', '3'), '0-3 months', 
                             ifelse(ome_df$age_first_om %in% c('4', '5', '6'), '4-6 months',
                                    ifelse(ome_df$age_first_om %in% c('7', '8', '9', '10', '11', '12', '<12'), '7-12 months',
                                           ifelse(ome_df$age_first_om %in% c('13', '14', '16', '18', '20', '22', '23', '24', '25', '<24'), 
                                                  '13-24 months', NA))))
ome_df$age_first_om = factor(ome_df$age_first_om, levels = c('0-3 months', '4-6 months', '7-12 months', '13-24 months'), ordered=T)
summary(ome_df)

# clean variables and put in correct format
ome_df$daycare_num_kids[ome_df$daycare_num_kids >30] = NA
ome_df$gender = as.factor(ome_df$gender)
ome_df$type.of.birth = ifelse(ome_df$type.of.birth %in% c('SVB', 'VB', 'Vacuum', NA), 'Vaginal',
                              ifelse(ome_df$type.of.birth %in% c('CS', 'E LSCS', "ELSCS", "LSCS"), 'C-section', NA))
ome_df$dummy_birth = as.factor(ome_df$dummy_birth)
ome_df$daycare_num_days = as.integer(ome_df$daycare_num_days)
ome_df$daycare_num_kids = as.integer(ome_df$daycare_num_kids)
ome_df$num_ppl_home = as.integer(ome_df$num_ppl_home)
ome_df$num_sibs = as.integer(ome_df$num_sibs)
ome_df$num_sibs_under5 = as.integer(ome_df$num_sibs_under5)
ome_df$income = ifelse(ome_df$income %in% c('<50', '< 50'), "<50",
                       ifelse(ome_df$income %in% c('> 150', '>150'), ">150",
                              ifelse(ome_df$income %in% c('100-150', '101 - 150', '101 -150', '101-150'), "101-150",
                                     ifelse(ome_df$income %in% c('50 - 100', '50 -100', '50 to 100', '50-100'), '50-100', NA))))
ome_df$income = factor(ome_df$income, levels=c('<50', '50-100', '101-150', '>150'), ordered=T)
ome_df$fam_hx_om = ifelse(ome_df$fam_hx_om %in% c('1', 'yes', 'Yes', 'Ys'), "Yes",
                          ifelse(ome_df$fam_hx_om %in% c('nno', 'no', 'No', 'NO', 'Unsure'), 'No', NA))
ome_df$mum_edu = ifelse(ome_df$mum_edu %in% c('high school', 'High school', 'High School'), 'High school',
                        ifelse(ome_df$mum_edu %in% c('tertiary', 'Tertiary'), 'Tertiary',
                               ifelse(ome_df$mum_edu %in% c('vocational', "Vocational"), 'Vocational', NA)))
ome_df$dad_edu = ifelse(ome_df$dad_edu %in% c('high school', 'High school', 'High School'), 'High school',
                        ifelse(ome_df$dad_edu %in% c('tertiary', 'Tertiary'), 'Tertiary',
                               ifelse(ome_df$dad_edu %in% c('vocational', "Vocational"), 'Vocational', NA)))
summary(as.factor(ome_df$dad_edu))

# only include infants followed up
ome_df <- ome_df[!(is.na(ome_df$tymp_6)) | !(is.na(ome_df$tymp_12)) | !(is.na(ome_df$tymp_18)),]
ome_df = select(ome_df, -c(birth.weight, body.length, head.circum))

# correct data types
ome_df$gender = ifelse(ome_df$gender %in% c('m', 'M'), 'M',
                            ifelse(ome_df$gender %in% 'F', 'F', NA))
ome_df$gender = as.factor(ome_df$gender)
ome_df$type.of.birth = as.factor(ome_df$type.of.birth)
ome_df$fam_hx_om = as.factor(ome_df$fam_hx_om)
ome_df$mum_edu = factor(ome_df$mum_edu, levels = c('High school', 'Vocational', 'Tertiary'), ordered = T)
ome_df$dad_edu = factor(ome_df$dad_edu, levels = c('High school', 'Vocational', 'Tertiary'), ordered = T)
ome_df$dummy_birth = as.factor(ome_df$dummy_birth)
ome_df$feed_6 = as.factor(ome_df$feed_6)
ome_df$feed_12 = as.factor(ome_df$feed_12)
ome_df$feed_18 = as.factor(ome_df$feed_18)
ome_df$urti_6 = as.factor(ome_df$urti_6)
ome_df$urti_12 = as.factor(ome_df$urti_12)
ome_df$urti_18 = as.factor(ome_df$urti_18)
ome_df$dummy_6 = as.factor(ome_df$dummy_6)
ome_df$dummy_12 = as.factor(ome_df$dummy_12)
ome_df$dummy_18 = as.factor(ome_df$dummy_18)

# na
ome_na = ome_df %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))
ome_na
ome_na / 370


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
ome_df_long = rename(ome_df_long, event=value, stop=name)
ome_df_long = select(ome_df_long, id, start, stop, event, everything())

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

# now set time varying vars - feed, urti, dummy
ome_df_long$feed = ifelse(ome_df_long$stop=='6', as.character(ome_df_long$feed_6),
                          ifelse(ome_df_long$stop=='12', as.character(ome_df_long$feed_12),
                                 ifelse(ome_df_long$stop=='18', as.character(ome_df_long$feed_18), NA)))

ome_df_long$urti = ifelse(ome_df_long$stop=='6', as.character(ome_df_long$urti_6),
                          ifelse(ome_df_long$stop=='12', as.character(ome_df_long$urti_12),
                                 ifelse(ome_df_long$stop=='18', as.character(ome_df_long$urti_18), NA)))

ome_df_long$dummy = ifelse(ome_df_long$stop=='6', as.character(ome_df_long$dummy_6),
                           ifelse(ome_df_long$stop=='12', as.character(ome_df_long$dummy_12),
                                  ifelse(ome_df_long=='18', as.character(ome_df_long$dummy_18), NA)))

ome_df_long = select(ome_df_long, -c(feed_6, feed_12, feed_18, urti_6, urti_12, urti_18, dummy_6, dummy_12, dummy_18))

ome_df_long$start = as.numeric(ome_df_long$start)
ome_df_long$stop = as.numeric(ome_df_long$stop)
ome_df_long$feed = as.factor(ome_df_long$feed)
ome_df_long$urti = as.factor(ome_df_long$urti)
ome_df_long$dummy = as.factor(ome_df_long$dummy)

# impute NA
ome_na = ome_df_long %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))

ome_na / 545

# impute missing
imputed = mice(ome_df_long, m=1)
ome_df_imp <- complete(imputed)
sapply(ome_df_long, function(x) sum(is.na(x)))
sapply(ome_df_imp, function(x) sum(is.na(x)))

# fit model
# make income just low or not
#ome_df_imp$low_income = ifelse(ome_df_imp$income %in% '<50', 'Yes', "No")
y = Surv(time = ome_df_imp$start, time2 = ome_df_imp$stop, event = ome_df_imp$event)

ome_f = coxph(y ~ wai_pred_neonate + gender + type.of.birth + parent_smoke + feed_birth + dummy_birth + start_daycare + daycare_num_kids + 
                daycare_num_days + num_ppl_home + num_sibs + num_sibs_under5 + income + fam_hx_om + mum_edu + dad_edu + age_first_om + 
                feed + urti + dummy + cluster(id), data = ome_df_imp) 
# cluster(id) in the model formula requests robust standard errors for the parameter estimates (survival analysis book p. 661)

summary(ome_f)

# now need to make sure I have done the time varying variables correctly
# and check goodness of fit - cox assumptions etc
# it would be cool to plot the effects of significant variables - like can do with rms
# also need to better impute variables - can choose vars to impute with etc








