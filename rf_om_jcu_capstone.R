# MA5820 Capstone project
# Joshua Myers 
library(tidyverse)
library(mice)
library(cowplot)
library(car)
library(jtools)

# load and explore data
set.seed(42)
aom_data = read.csv('JoshuaMyersMA5820Data.csv', row.names=NULL)[-1] # remove index col
summary(aom_data)
# there are 329 observations of 11 variables
# num_om is number of ear infections in first 12 months of life - this outcome 
# gender - 1 missing
# feed birth - 0 missing
# dummy birth - 0 missing
# feed_12 - 25 missing
# dummary_12 - 22 missing
# number of urti first year of life - 35 missing
# attends daycase - 0 missing
# has siblings - 28 missing
# family history of om (parent of sibling) - 19 missing
# does either parent smoke - 21 missing

# mean and variance of response
mean(aom_data$num_om)
var(aom_data$num_om)

# impute missing
num_na = sapply(aom_data, function(x) sum(is.na(x)))
prop_na = num_na / 329
prop_na # proportion missing
imputed = mice(aom_data, m=1)
aom_data <- mice::complete(imputed)

summary(imputed$imp$gender)
summary(imputed$imp$feed_12)
summary(imputed$imp$dummy_12)
summary(imputed$imp$num_urti_12)
summary(imputed$imp$sibs)
summary(imputed$imp$fam_hx_om)
summary(imputed$imp$parent_smoke)

# plots of variables by outcome
# set theme
theme_set(theme_bw()) 

gender_p = aom_data %>% 
  ggplot(aes(x=num_om, fill=gender)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = "Set2", labels = c("Female", "Male")) +
  theme(legend.title=element_blank(), plot.title = element_text(face="bold"), legend.justification = "top") +
  xlab('Number OM') +
  ylab('Proportion') +
  ggtitle('Gender') 
  
feed_birth_p = aom_data %>% 
  ggplot(aes(x=num_om, fill=feed_birth)) +
  geom_bar(position='fill') +
  scale_fill_brewer(palette = "Set2", labels = c("Breast", "Formula", "Mixed")) +
  theme(legend.title=element_blank(), plot.title = element_text(face="bold"), legend.justification = "top") +
  xlab('Number OM') +
  ylab('Proportion') +
  ggtitle('Feed at Birth')

dummy_birth_p = aom_data %>% 
  ggplot(aes(x=num_om, fill=dummy_birth)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.title=element_blank(), plot.title = element_text(face="bold"), legend.justification = "top") +
  xlab('Number OM') +
  ylab('Proportion') +
  ggtitle('Dummy at Birth')

feed_12_p = aom_data %>% 
  ggplot(aes(x=num_om, fill=feed_12)) +
  geom_bar(position='fill') +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.title=element_blank(), plot.title = element_text(face="bold"), legend.justification = "top") +
  xlab('Number OM') +
  ylab('Proportion') +
  ggtitle('Feed at 12 Months')

dummy_12_p = aom_data %>% 
  ggplot(aes(x=num_om, fill=dummy_12)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.title=element_blank(), plot.title = element_text(face="bold"), legend.justification = "top") +
  xlab('Number OM') +
  ylab('Proportion') +
  ggtitle('Dummy at 12 Months')

daycare_p = aom_data %>% 
  ggplot(aes(x=num_om, fill=start_daycare)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.title=element_blank(), plot.title = element_text(face="bold"), legend.justification = "top") +
  xlab('Number OM') +
  ylab('Proportion') +
  ggtitle('Attends Daycare')

fam_hx_om_p = aom_data %>% 
  ggplot(aes(x=num_om, fill=fam_hx_om)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.title=element_blank(), plot.title = element_text(face="bold"), legend.justification = "top") +
  xlab('Number OM') +
  ylab('Proportion') +
  ggtitle('Family History of OM')

sibs_p = aom_data %>% 
  ggplot(aes(x=num_om, fill=sibs)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.title=element_blank(), plot.title = element_text(face="bold"), legend.justification = "top") +
  xlab('Number OM') +
  ylab('Proportion') +
  ggtitle('Has Siblings')

smoke_p = aom_data %>% 
  ggplot(aes(x=num_om, fill=parent_smoke)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.title=element_blank(), plot.title = element_text(face="bold"), legend.justification = "top") +
  xlab('Number OM') +
  ylab('Proportion') +
  ggtitle('Parent Smokes')

num_urti_p = aom_data %>% 
  ggplot(aes(x=num_om, y=num_urti_12)) +
  geom_jitter(alpha=0.2) +
  theme(legend.title=element_blank(), plot.title = element_text(face="bold")) +
  scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10), labels = c(0, 2, 4, 6, 8, 10)) +
  xlab('Number OM') +
  ylab('Number URTI') +
  ggtitle('Number of URTIs')

var_plots = plot_grid(gender_p, feed_birth_p, feed_12_p, dummy_birth_p, dummy_12_p, daycare_p, fam_hx_om_p, sibs_p, smoke_p, num_urti_p,
                  nrow=4, labels = LETTERS[1:10], axis='lblr', align = 'v')
save_plot('var_plots.png', var_plots, nrow=4, ncol = 3, base_height = 2.4)

# fit model
aom_f = glm(num_om ~ gender + feed_birth + dummy_birth + feed_12 + dummy_12 + num_urti_12 + start_daycare + sibs + 
              fam_hx_om + parent_smoke, data = aom_data, family = poisson) 

summary(aom_f)
# overdispersed: residual deviance > df (455.98 > 316)

# therefore, use quasi-poisson because of overdispersion: https://stats.stackexchange.com/questions/201903/how-to-deal-with-overdispersion-in-poisson-regression-quasi-likelihood-negativ
aom_q = glm(num_om ~ gender + feed_birth + dummy_birth + feed_12 + dummy_12 + num_urti_12 + start_daycare + sibs + 
              fam_hx_om + parent_smoke, data = aom_data, family = quasipoisson) 

Anova(aom_q)
daycare_f_p = effect_plot(aom_q, pred = start_daycare, interval = T, plot.points = F, line.thickness = 0.9,
            y.label = 'Number OM', x.label = "Attends Daycare")
urti_f_p = effect_plot(aom_q, pred = num_urti_12, interval = T, plot.points = F, line.thickness = 0.9,
            y.label = 'Number OM', x.label = "Number of URTIs")
fam_hx_f_p = effect_plot(aom_q, pred = fam_hx_om, interval = T, plot.points = F, line.thickness = 0.9,
            y.label = 'Number OM', x.label = "Family History OM")

fit_plots = plot_grid(daycare_f_p, fam_hx_f_p,urti_f_p, nrow = 1, labels = LETTERS[1:3])
save_plot('model_plots.png', fit_plots, nrow = 1, ncol = 3, base_height = 2.4, base_asp = 1)

