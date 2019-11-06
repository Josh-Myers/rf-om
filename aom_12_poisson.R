# AOM poission model
library(mice)
library(jtools)
library(sandwich)
library(lmtest)
library(car)
library(tidyverse)

set.seed(42)
load('om_data.Rda')

aom_df = dplyr::select(followed_up, id, num_om_0_to_1, num_om_1_to_2, wai_pred_neonate, gender, type.of.birth, birth.weight, 
                body.length,head.circum)

aom_df$parent_smoke = ifelse(followed_up$mum_smoke_q12 %in% c('yes', 'Yes') | followed_up$dad_smoke_q12 %in% c('yes', 'Yes')
                             | followed_up$mum_smoke_q24 %in% c('yes', 'Yes') | followed_up$dad_smoke_q24 %in% c('yes', 'Yes'),
                             'yes', ifelse(is.na(followed_up$mum_smoke_q12) %in% c('no', 'No') | followed_up$dad_smoke_q12 %in% c('nO', 'no', 'No')
                                           | followed_up$mum_smoke_q24 %in% c('`No', 'No', 'no') | followed_up$dad_smoke_q24 %in% c('no', 'No'), 
                                           'no', NA))
aom_df$parent_smoke = as.factor(aom_df$parent_smoke)

aom_df$feed_birth = ifelse(followed_up$type.of.feed.0 %in% c('B', 'breast', 'Breast'), 'breast',
                           ifelse(followed_up$type.of.feed.0 %in% c('both', 'Both'), 'mixed', 
                                  ifelse(followed_up$type.of.feed.0 %in% c('formula', 'Formula'), 'formula', 'breast')))
aom_df$feed_birth = as.factor(aom_df$feed_birth)

aom_df$dummy_birth = ifelse(followed_up$using.pacifier.0 %in% c('Yes'), 'Yes', 'No') # 7 na impute as no

# the feed, urti, dummy 6, 12, 18 vars are time varying and need to be consolidated after changing to long form
aom_df$feed_12 = ifelse(followed_up$feed_q12 %in% c('breast', 'Breast'), 'Breast',
                        ifelse(followed_up$feed_q12 %in% c('formula', 'formla', 'Formula'), 'Formula',
                               ifelse(followed_up$feed_q12 %in% c('both', 'Both', 'cow', "Cow's milk", 'cows', 'Cows milk', 
                                                                  'Formula and cows milk', 'mixed', 'Neither', 'No', 'Other', 'solids'), 'Mixed/Other',
                                      ifelse(followed_up$type.of.feed.12 %in% c('breast', 'Breast'), 'Breast',
                                             ifelse(followed_up$type.of.feed.12 %in% c('formula', 'Formula'), 'Formula',
                                                    ifelse(followed_up$type.of.feed.12 %in% c('both', 'Both', 'Mixed', 'cow', "Cow's milk", "cow's milk",
                                                                                              'mixed', 'Neither', 'nil', 'Nil', 'other', 'solid', 'Solid'), 
                                                           "Mixed/Other", NA))))))

# aom_df$feed_24 = ifelse(followed_up$feed_q24 %in% c('breast', 'Breast', 'Breast fed'), 'Breast',
#                         ifelse(followed_up$feed_q24 %in% c('formula', 'Formula'), 'Formula',
#                                ifelse(followed_up$feed_q24 %in% c('Both', 'cow', "Cow's", "cow's milk", 'Breast and Formula',
#                                                                   'mixed', 'Neither', 'Niether', 'none', 'None', 'Other'), 'Mixed/Other',
#                                       ifelse(followed_up$type.of.feed.18 %in% c('breast', 'Breast'), 'Breast',
#                                              ifelse(followed_up$type.of.feed.18 %in% c('formula', 'Formula'), 'Formula',
#                                                     ifelse(followed_up$type.of.feed.18 %in% c('both', "breast & cow's milk", 'cow', "Cow's milk", 
#                                                                                               'mixed', 'Na', 'nil', 'other', 'Solid'), 
#                                                            "Mixed/Other", NA))))))


aom_df$num_urti_12 = ifelse(!is.na(followed_up$num_urti_q12), followed_up$num_urti_q12, NA)
#aom_df$num_urti_24 = ifelse(!is.na(followed_up$num_urti_q24), followed_up$num_urti_q24, NA)

aom_df$dummy_12 = ifelse(!is.na(followed_up$use_dummy_q12), followed_up$use_dummy_q12, NA)
#aom_df$dummy_24 = ifelse(!is.na(followed_up$use_dummy_q24), followed_up$use_dummy_q24, NA)

# daycare
# create daycare yes/no to use in start daycare (months) variable 
followed_up$daycare = ifelse(followed_up$day_care_q12 %in% c('yes', "Yes"), "Yes",
                             ifelse(followed_up$day_care_q12 %in% c('no', "No"), "No",
                                    ifelse(followed_up$day_care_q24 %in% c('yes', 'Yes'), 'Yes',
                                           ifelse(followed_up$day_care_q24 %in% c('no', "No"), 'No', NA))))

aom_df$start_daycare = ifelse(!is.na(followed_up$age_start_care_q12), followed_up$age_start_care_q12,
                              ifelse(!is.na(followed_up$age_start_care_q24), followed_up$age_start_care_q24,
                                     ifelse(followed_up$daycare %in% 'No', 24, NA))) # if no haven't started daycare by 24 months

aom_df$start_daycare = ifelse(aom_df$start_daycare %in% c(0.5, 1, 1.5, 2, 4, 4.5, 5, 5.5), '<6 months',
                              ifelse(aom_df$start_daycare %in% c(6, 7, 7.5, 8, 8.5, 9, 9.5), '6-9 months',
                                     ifelse(aom_df$start_daycare %in% c(10, 10.5, 11, 11.5, 12), '10-12 months',
                                            ifelse(aom_df$start_daycare %in% c(12, 13, 14, 15, 18, 19, 20, 21, 22, 24), '>12 months', NA))))
aom_df$start_daycare = factor(aom_df$start_daycare, levels = c('<6 months', '6-9 months', '10-12 months', '>12 months'), ordered = T)
aom_df$start_daycare = ifelse(aom_df$start_daycare %in% c('<6 months', '6-9 months', '10-12 months'), 'Yes', 'No')

aom_df$daycare_num_kids = ifelse(!is.na(followed_up$num_kids_in_group_q12), followed_up$num_kids_in_group_q12,
                                 ifelse(!is.na(followed_up$num_kids_in_group_q24), followed_up$num_kids_in_group_q24,
                                        ifelse(followed_up$daycare %in% 'No', 0, NA))) # will need to impute some of these

aom_df$daycare_num_days = ifelse(!is.na(followed_up$num_days_per_week_q12), followed_up$num_days_per_week_q12,
                                 ifelse(!is.na(followed_up$num_days_per_week_q24), followed_up$num_days_per_week_q24,
                                        ifelse(followed_up$daycare %in% 'No', 0, NA)))

aom_df$num_ppl_home = ifelse(!is.na(followed_up$num_people_at_home_q12), followed_up$num_people_at_home_q12,
                             ifelse(!is.na(followed_up$num_people_at_home_q24), followed_up$num_people_at_home_q24, NA))

aom_df$num_sibs = ifelse(!is.na(followed_up$num_siblings_at_home_q12), followed_up$num_siblings_at_home_q12,
                         ifelse(!is.na(followed_up$num_siblings_at_home_q24), followed_up$num_siblings_at_home_q24, NA))

aom_df$num_sibs_under5 = ifelse(!is.na(followed_up$num_sibs_under_5_q12), followed_up$num_sibs_under_5_q12,
                                ifelse(!is.na(followed_up$num_sibs_under_5_q24), followed_up$num_sibs_under_5_q24, 
                                       ifelse(followed_up$num_siblings_at_home_q12 == 0, 0,
                                              ifelse(followed_up$num_siblings_at_home_q24 == 0, 0, NA))))

aom_df$income = ifelse(!is.na(followed_up$income_q12), followed_up$income_q12,
                       ifelse(!is.na(followed_up$income_q24), followed_up$income_q24, NA))

aom_df$fam_hx_om = ifelse(!is.na(followed_up$parent_hx_om_q12), followed_up$parent_hx_om_q12,
                          ifelse(!is.na(followed_up$parent_hx_om_q24), followed_up$parent_hx_om_q24,
                                 ifelse(!is.na(followed_up$sib_om_hx_12), followed_up$sib_om_hx_12,
                                        ifelse(!is.na(followed_up$sib_om_hx_24), followed_up$sib_om_hx_24, NA))))

aom_df$mum_edu = ifelse(!is.na(followed_up$mum_edu_q12), followed_up$mum_edu_q12,
                        ifelse(!is.na(followed_up$mum_edu_q24), followed_up$mum_edu_q24, NA))

aom_df$dad_edu = ifelse(!is.na(followed_up$dad_edu_q12), followed_up$dad_edu_q12,
                        ifelse(!is.na(followed_up$dad_edu_q24), followed_up$dad_edu_q24, NA))

aom_df$age_first_om = ifelse(!is.na(followed_up$age_first_om_q12), followed_up$age_first_om_q12,
                             ifelse(!is.na(followed_up$age_first_om_q24), followed_up$age_first_om_q24, 
                                    ifelse(!is.na(followed_up$num_om_0_to_1), '<12',
                                           ifelse(!is.na(followed_up$num.of.ear.infections.since.turning.1.18mth), '<12',
                                                  ifelse(!is.na(followed_up$num_om_1_to_2), '<24', NA)))))

aom_df$age_first_om = ifelse(aom_df$age_first_om %in% c('0', '1', '1.5', '2', '3'), '0-3 months', 
                             ifelse(aom_df$age_first_om %in% c('4', '5', '6'), '4-6 months',
                                    ifelse(aom_df$age_first_om %in% c('7', '8', '9', '10', '11'), '7-11 months',
                                           ifelse(aom_df$age_first_om %in% c('12', '<12', '13', '14', '16', '18', '20', '22', '23', '24', '25', '<24'), 
                                                  '>11 months', NA))))
aom_df$age_first_om = factor(aom_df$age_first_om, levels = c('0-3 months', '4-6 months', '7-11 months', '>11 months'), ordered=T)

# clean variables and put in correct format
aom_df$daycare_num_kids[aom_df$daycare_num_kids >30] = NA
aom_df$gender = ifelse(aom_df$gender %in% c('m', "M"), "M",
                       ifelse(aom_df$gender %in% 'F', 'F', NA))
aom_df$gender = as.factor(aom_df$gender)
aom_df$type.of.birth = ifelse(aom_df$type.of.birth %in% c('SVB', 'VB', 'Vacuum', NA), 'Vaginal',
                              ifelse(aom_df$type.of.birth %in% c('CS', 'E LSCS', "ELSCS", "LSCS"), 'C-section', NA))
aom_df$dummy_birth = as.factor(aom_df$dummy_birth)
aom_df$daycare_num_days = as.integer(aom_df$daycare_num_days)
aom_df$daycare_num_kids = as.integer(aom_df$daycare_num_kids)
aom_df$num_ppl_home = as.integer(aom_df$num_ppl_home)
aom_df$num_sibs = as.integer(aom_df$num_sibs)
aom_df$num_sibs_under5 = as.integer(aom_df$num_sibs_under5)
aom_df$income = ifelse(aom_df$income %in% c('<50', '< 50'), "<50",
                       ifelse(aom_df$income %in% c('> 150', '>150'), ">150",
                              ifelse(aom_df$income %in% c('100-150', '101 - 150', '101 -150', '101-150'), "101-150",
                                     ifelse(aom_df$income %in% c('50 - 100', '50 -100', '50 to 100', '50-100'), '50-100', NA))))
aom_df$income = factor(aom_df$income, levels=c('<50', '50-100', '101-150', '>150'), ordered=T)
aom_df$fam_hx_om = ifelse(aom_df$fam_hx_om %in% c('1', 'yes', 'Yes', 'Ys'), "Yes",
                          ifelse(aom_df$fam_hx_om %in% c('nno', 'no', 'No', 'NO', 'Unsure'), 'No', NA))
aom_df$mum_edu = ifelse(aom_df$mum_edu %in% c('high school', 'High school', 'High School'), 'High school',
                        ifelse(aom_df$mum_edu %in% c('tertiary', 'Tertiary'), 'Tertiary',
                               ifelse(aom_df$mum_edu %in% c('vocational', "Vocational"), 'Vocational', NA)))
aom_df$dad_edu = ifelse(aom_df$dad_edu %in% c('high school', 'High school', 'High School'), 'High school',
                        ifelse(aom_df$dad_edu %in% c('tertiary', 'Tertiary'), 'Tertiary',
                               ifelse(aom_df$dad_edu %in% c('vocational', "Vocational"), 'Vocational', NA)))

aom_df = dplyr::select(aom_df, -num_om_1_to_2)
aom_df = rename(aom_df, num_om = num_om_0_to_1)
aom_df$type.of.birth = as.factor(aom_df$type.of.birth)
aom_df$birth.weight = aom_df$birth.weight / 1000
aom_df$fam_hx_om = as.factor(aom_df$fam_hx_om)
aom_df$mum_edu = factor(aom_df$mum_edu, levels = c('High school', 'Vocational', 'Tertiary'), ordered = T)
aom_df$dad_edu = factor(aom_df$dad_edu, levels = c('High school', 'Vocational', 'Tertiary'), ordered = T)
aom_df$dummy_12 = ifelse(aom_df$dummy_12 %in% c('yes', "Yes",'Ywa'), "Yes",
                         ifelse(aom_df$dummy_12 %in% c('no', 'No', 'Noq'), 'No', NA))
aom_df$dummy_12 = as.factor(aom_df$dummy_12)
aom_df$feed_12 = as.factor(aom_df$feed_12)
aom_df$start_daycare = as.factor(aom_df$start_daycare)

# only include infants followed up
aom_df <- aom_df[!(is.na(aom_df$num_om)), ]
summary(aom_df)

# impute missing
imputed = mice(aom_df, m=5)

aom_df_imp <- complete(imputed)
num_na = sapply(aom_df, function(x) sum(is.na(x)))
prop_na = num_na / 329
prop_na
sapply(aom_df_imp, function(x) sum(is.na(x)))

# fit model
aom_f = glm(num_om ~ wai_pred_neonate + gender + type.of.birth + parent_smoke + feed_birth + dummy_birth + feed_12 + num_urti_12 + 
              dummy_12 + start_daycare + daycare_num_kids + daycare_num_days + num_ppl_home + num_sibs + num_sibs_under5 + income + 
              fam_hx_om + mum_edu + dad_edu, data = aom_df_imp, family = poisson) 

summary(aom_f)
# overdispersed - residual deviance > df (444.78 > 303)

# therefore, use quasi-poisson because of overdispersion: https://stats.stackexchange.com/questions/201903/how-to-deal-with-overdispersion-in-poisson-regression-quasi-likelihood-negativ
aom_q = glm(num_om ~ wai_pred_neonate + gender + type.of.birth + parent_smoke + feed_birth + dummy_birth + feed_12 + num_urti_12 + 
              dummy_12 + start_daycare + daycare_num_kids + daycare_num_days + num_ppl_home + num_sibs + num_sibs_under5 + income + 
              fam_hx_om + mum_edu + dad_edu, data = aom_df_imp, family = quasipoisson) 

#saveRDS(aom_q, 'quasimodel_aom.rds')
summary(aom_q)
Anova(aom_q)

effect_plot(aom_q, pred = start_daycare, interval = T, plot.points = F)
effect_plot(aom_q, pred = num_urti_12, interval = T, plot.points = F)
effect_plot(aom_q, pred = fam_hx_om, interval = T, plot.points = F)


