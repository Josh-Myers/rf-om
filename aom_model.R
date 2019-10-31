# AOM survival model
# OME survival model
load('om_data.Rda')

aom_df = select(followed_up, id, num_om_0_to_1, num_om_1_to_2, tymp_18, wai_pred_neonate, gender, type.of.birth, birth.weight, 
                body.length,head.circum)

aom_df$parent_smoke = ifelse(followed_up$mum_smoke_q24 %in% c('yes', 'Yes') | followed_up$dad_smoke_q24 %in% c('yes', 'Yes')
                             | followed_up$mum_smoke_q12 %in% c('yes', 'Yes') | followed_up$dad_smoke_q12 %in% c('yes', 'Yes'),
                             'yes', ifelse(is.na(followed_up$mum_smoke_q24) %in% c('no', 'No') | followed_up$dad_smoke_q24 %in% c('nO', 'no', 'No')
                                           | followed_up$mum_smoke_q12 %in% c('`No', 'No', 'no') | followed_up$dad_smoke_q12 %in% c('no', 'No'), 
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

aom_df$feed_24 = ifelse(followed_up$feed_q24 %in% c('breast', 'Breast', 'Breast fed'), 'Breast',
                        ifelse(followed_up$feed_q24 %in% c('formula', 'Formula'), 'Formula',
                               ifelse(followed_up$feed_q24 %in% c('Both', 'cow', "Cow's", "cow's milk", 'Breast and Formula',
                                                                  'mixed', 'Neither', 'Niether', 'none', 'None', 'Other'), 'Mixed/Other',
                                      ifelse(followed_up$type.of.feed.18 %in% c('breast', 'Breast'), 'Breast',
                                             ifelse(followed_up$type.of.feed.18 %in% c('formula', 'Formula'), 'Formula',
                                                    ifelse(followed_up$type.of.feed.18 %in% c('both', "breast & cow's milk", 'cow', "Cow's milk", 
                                                                                              'mixed', 'Na', 'nil', 'other', 'Solid'), 
                                                           "Mixed/Other", NA))))))


aom_df$num_urti_12 = ifelse(!is.na(followed_up$num_urti_q12), followed_up$num_urti_q12, NA)
aom_df$num_urti_24 = ifelse(!is.na(followed_up$num_urti_q24), followed_up$num_urti_q24, NA)

aom_df$dummy_12 = ifelse(!is.na(followed_up$use_dummy_q12), followed_up$use_dummy_q12, NA)
aom_df$dummy_24 = ifelse(!is.na(followed_up$use_dummy_q24), followed_up$use_dummy_q24, NA)

# daycare
# create daycare yes/no to use in start daycare (months) variable 
followed_up$daycare = ifelse(followed_up$day_care_q12 %in% c('yes', "Yes"), "Yes",
                             ifelse(followed_up$day_care_q12 %in% c('no', "No"), "No",
                                    ifelse(followed_up$day_care_q24 %in% c('yes', 'Yes'), 'Yes',
                                           ifelse(followed_up$day_care_q24 %in% c('no', "No"), 'No', NA))))

aom_df$start_daycare = ifelse(!is.na(followed_up$age_start_care_q12), followed_up$age_start_care_q12,
                              ifelse(!is.na(followed_up$age_start_care_q24), followed_up$age_start_care_q24,
                                     ifelse(followed_up$daycare %in% 'No', 24, NA))) # if no haven't started daycare by 24 months

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
                                ifelse(!is.na(followed_up$num_sibs_under_5_q24), followed_up$num_sibs_under_5_q24, NA))

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
                             ifelse(!is.na(followed_up$age_first_om_q24), followed_up$age_first_om_q24, NA))
summary(aom_df)

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
summary(as.factor(aom_df$dad_edu))



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


