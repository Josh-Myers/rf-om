#### Make predictions for newborn wai results ####

library(rms)
library(tidyverse)

# load model
load("neonateModel.rda")
model = f.final

# wai data
abs <- read_excel("Newborn.xlsx", sheet = 4, na = "NA")
mag <- read_excel("Newborn.xlsx", sheet = 5, na = "NA")
pha <- read_excel("Newborn.xlsx", sheet = 6, na = "NA")

abs = abs[,c(1:3, 5, 9:115)]
mag = mag[,c(9:115)]
pha = pha[,c(9:115)]

wai.24 = cbind.data.frame(abs, mag, pha)
wai.24$Test.Retest[wai.24$Test.Retest == "Retest"] = "retest"
wai.24 = filter(wai.24, Test.Retest == "test")

# 1 octave averaging
abs250	<- transmute(wai.24, abs250 = (abs226	+ abs257.33	+ abs280.62	+ abs297.3	+ abs324.21	+ abs343.49)/6) # 226 - 353.55
abs500	<- transmute(wai.24, abs500 = (abs363.91	+ abs385.55	+ abs408.48	+ abs432.77	+ abs458.5	+ abs471.94	+ abs500	+ 
                                             abs514.65	+ abs545.25	+ abs561.23	+ abs577.68	+ abs594.6	+ 
                                             abs629.96	+ abs648.42	+ abs667.42	+ abs686.98	+ abs707.11)/17) # 353.56 - 707.11
abs1000	<- transmute(wai.24, abs1000 = (abs727.83	+ abs749.15	+ abs771.11	+ abs793.7	+ abs816.96	+ abs840.9	+ abs865.54	+ 
                                               abs890.9	+ abs917	+ abs943.87	+ abs971.53	+ abs1000	+ abs1029.3	+ abs1059.46	+ 
                                               abs1090.51	+ abs1122.46	+ abs1155.35	+ abs1189.21	+ abs1224.05	+ abs1259.92	+ 
                                               abs1296.84	+ abs1334.84	+ abs1373.95	+ abs1414.21)/24) # 707.12 - 1414.21
abs2000	<- transmute(wai.24, abs2000 = (abs1455.65	+ abs1498.31	+ abs1542.21	+ abs1587.4	+ abs1633.92	+ abs1681.79	+ 
                                               abs1731.07	+ abs1781.8	+ abs1834.01	+ abs1887.75	+ abs1943.06	+ abs2000	+ abs2058.6	+ 
                                               abs2118.93	+ abs2181.02	+ abs2244.92	+ abs2310.71	+ abs2378.41	+ abs2448.11	+ 
                                               abs2519.84	+ abs2593.68	+ abs2669.68	+ abs2747.91	+ abs2828.43)/24) # 1414.22 - 2828.43
abs4000	<- transmute(wai.24,abs4000 = (abs2911.31	+ abs2996.61	+ abs3084.42	+ abs3174.8	+ abs3267.83	+ abs3363.59	+ 
                                              abs3462.15	+ abs3563.59	+ abs3668.02	+ abs3775.5	+ abs3886.13	+ abs4000	+ abs4117.21	+ 
                                              abs4237.85	+ abs4362.03	+ abs4489.85	+ abs4621.41	+ abs4756.83	+ abs4896.21	+ 
                                              abs5039.68	+ abs5187.36	+ abs5339.36	+ abs5495.81	+ abs5656.85)/24) # 2828.44 - 5656.85
abs8000 <- transmute(wai.24, abs8000 = (abs5822.61	+ abs5993.23	+ abs6168.84	+ abs6349.6	+ abs6535.66	+ 
                                               abs6727.17	+ abs6924.29	+ abs7127.19	+ abs7336.03	+ abs7550.99	+ abs7772.26	+ abs8000)/12) # 5656.86 - 8000
abs.1 <- cbind(abs250,	abs500,	abs1000,	abs2000,	abs4000,	abs8000)

mag250	<- transmute(wai.24, mag250 = (mag226	+ mag257.33	+ mag280.62	+ mag297.3	+ mag324.21	+ mag343.49)/6) # 226 - 353.55
mag500	<- transmute(wai.24, mag500 = (mag363.91	+ mag385.55	+ mag408.48	+ mag432.77	+ mag458.5	+ mag471.94	+ mag500	+ 
                                             mag514.65	+ mag545.25	+ mag561.23	+ mag577.68	+ mag594.6	+ mag629.96	+ mag648.42	+ 
                                             mag667.42	+ mag686.98	+ mag707.11)/17) # 353.56 - 707.11
mag1000	<- transmute(wai.24, mag1000 = (mag727.83	+ mag749.15	+ mag771.11	+ mag793.7	+ mag816.96	+ mag840.9	+ mag865.54	+ 
                                               mag890.9	+ mag917	+ mag943.87	+ mag971.53	+ mag1000	+ mag1029.3	+ mag1059.46	+ 
                                               mag1090.51	+ mag1122.46	+ mag1155.35	+ mag1189.21	+ mag1224.05	+ mag1259.92	+ 
                                               mag1296.84	+ mag1334.84	+ mag1373.95	+ mag1414.21)/24) # 707.12 - 1414.21
mag2000	<- transmute(wai.24, mag2000 = (mag1455.65	+ mag1498.31	+ mag1542.21	+ mag1587.4	+ mag1633.92	+ mag1681.79	+ 
                                               mag1731.07	+ mag1781.8	+ mag1834.01	+ mag1887.75	+ mag1943.06	+ mag2000	+ mag2058.6	+ 
                                               mag2118.93	+ mag2181.02	+ mag2244.92	+ mag2310.71	+ mag2378.41	+ mag2448.11	+ mag2519.84	+ 
                                               mag2593.68	+ mag2669.68	+ mag2747.91	+ mag2828.43)/24) # 1414.22 - 2828.43
mag4000	<- transmute(wai.24, mag4000 = (mag2911.31	+ mag2996.61	+ mag3084.42	+ mag3174.8	+ mag3267.83	+ 
                                               mag3363.59	+ mag3462.15	+ mag3563.59	+ mag3668.02	+ mag3775.5	+ mag3886.13	+ mag4000	+ 
                                               mag4117.21	+ mag4237.85	+ mag4362.03	+ mag4489.85	+ mag4621.41	+ mag4756.83	+ 
                                               mag4896.21	+ mag5039.68	+ mag5187.36	+ mag5339.36	+ mag5495.81	+ mag5656.85)/24) # 2828.44 - 5656.85
mag8000 <- transmute(wai.24, mag8000 = (mag5822.61	+ mag5993.23	+ mag6168.84	+ mag6349.6	+ mag6535.66	+ mag6727.17	+ 
                                               mag6924.29	+ mag7127.19	+ mag7336.03	+ mag7550.99	+ mag7772.26	+ mag8000)/12) # 5656.86 - 8000
mag.1 <- cbind(mag250,	mag500,	mag1000,	mag2000,	mag4000,	mag8000)

pha250 <- transmute(wai.24, pha250 = (pha226	+ pha257.33	+ pha280.62	+ pha297.3	+ pha324.21+ pha343.49)/6)	# 226 - 353.55
pha500	<- transmute(wai.24, pha500 = (pha363.91	+ pha385.55	+ pha408.48	+ pha432.77	+ pha458.5	+ pha471.94	+ pha500	+ 
                                             pha514.65	+ pha545.25	+ pha561.23	+ pha577.68	+ pha594.6	+ pha629.96	+ pha648.42	+ 
                                             pha667.42	+ pha686.98	+ pha707.11)/17) # 353.56 - 707.11
pha1000	<- transmute(wai.24, pha1000 = (pha727.83	+ pha749.15	+ pha771.11	+ pha793.7	+ pha816.96	+ pha840.9	+ pha865.54	+ 
                                               pha890.9	+ pha917	+ pha943.87	+ pha971.53	+ pha1000	+ pha1029.3	+ pha1059.46	+ 
                                               pha1090.51	+ pha1122.46	+ pha1155.35	+ pha1189.21	+ pha1224.05	+ pha1259.92	+ 
                                               pha1296.84	+ pha1334.84	+ pha1373.95	+ pha1414.21)/24) # 707.12 - 1414.21
pha2000	<- transmute(wai.24, pha2000 = (pha1455.65	+ pha1498.31	+ pha1542.21	+ pha1587.4	+ pha1633.92	+ pha1681.79	+ 
                                               pha1731.07	+ pha1781.8	+ pha1834.01	+ pha1887.75	+ pha1943.06	+ pha2000	+ pha2058.6	+ 
                                               pha2118.93	+ pha2181.02	+ pha2244.92	+ pha2310.71	+ pha2378.41	+ pha2448.11	+ pha2519.84	+ 
                                               pha2593.68	+ pha2669.68	+ pha2747.91	+ pha2828.43)/24) # 1414.22 - 2828.43
pha4000	<- transmute(wai.24, pha4000 = (pha2911.31	+ pha2996.61	+ pha3084.42	+ pha3174.8	+ pha3267.83	+ pha3363.59	+ 
                                               pha3462.15	+ pha3563.59	+ pha3668.02	+ pha3775.5	+ pha3886.13	+ pha4000	+ pha4117.21	+ 
                                               pha4237.85	+ pha4362.03	+ pha4489.85	+ pha4621.41	+ pha4756.83	+ pha4896.21	+ pha5039.68	+ 
                                               pha5187.36	+ pha5339.36	+ pha5495.81	+ pha5656.85)/24) # 2828.44 - 5656.85
pha8000 <- transmute(wai.24, pha8000 = (pha5822.61	+ pha5993.23	+ pha6168.84	+ pha6349.6	+ pha6535.66	+ pha6727.17	+ 
                                               pha6924.29	+ pha7127.19	+ pha7336.03	+ pha7550.99	+ pha7772.26	+ pha8000)/12) # 5656.86 - 8000
pha.1 <- cbind(pha250,	pha500,	pha1000,	pha2000,	pha4000,	pha8000)

wai.1 <- cbind(abs.1, mag.1, pha.1)
keep(wai.1, model, sure = TRUE)

# calculate predictions
preds <- predict(model, wai.1, type="fitted")
preds = as.data.frame(preds)

id = wai.24$id.res
ear = wai.24$ear

# keep prediction for ear with highest probability
preds = cbind.data.frame(id, ear, preds)
max_preds = preds %>% 
  group_by(id) %>% 
  summarise(pred = max(preds))

save(max_preds ,file="neonate_predictions.Rda")


