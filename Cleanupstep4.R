rm(list = ls());gc()
library(data.table)
library(tidyverse)

# assess the treatment details
data <- fread('/Users/maximilianlindholz/Final/baseinfo3.csv')
medis <- fread('/Users/maximilianlindholz/Final/medicationclean.csv')

# check time during stay
medis <- subset(medis, medis$c_application_start >=medis$aufn & medis$c_application_start<=medis$entl)

# data (unncessaryly long code, could have used -c...)
data <- data %>% select('c_pseudonym', 'geschlecht', 'Behandlungsdauer', 'aufn', 'entl', 'age', 
                        'chf', 'carit', 'valv', 'pcd', 'pvd', 'hypunc', 'hypc', 'para', 'ond', 'cpd',
                        'diabunc', 'diabc', 'hypothy', 'rf', 'ld', 'pud', 'aids', 'lymph', 'metacanc',
                        'solidtum', 'rheumd', 'coag', 'obes', 'wloss', 'fed', 'blane', 'dane', 'alcohol',
                        'drug', 'psycho', 'depre', 'Elixhauser',   'Gewicht', 'dialyse', 'ecmo', 'highflow',
                        'intubated', 'maskedventilation', 'tracheostomie', 'index', 'n', 'max', 'min',
                        'frühmobi', 'perday', 'prone', 'admissionapache', 'admissionsofa', 'meanAbsoluteRass',
                        'Fachrichtung', 'CovidStationstyp','averagelengthofPT')

# # admission and discharge
check <- data %>% select('c_pseudonym','aufn','entl')
test <- fread('/Users/maximilianlindholz/MobiCovid/sap_movements.csv')
test <- merge(test, check, by = 'c_pseudonym')
test <- test %>% select('c_pseudonym', 'c_bewty','c_bwart', 'c_orgfa', 'c_bwedt','c_orgpf',  'c_zimmr','c_wplid')
library(lubridate)
test$c_bwedt<-ymd(test$c_bwedt)
# some data is 9999-999 => those are duplicates as i found in data scoping
test <- subset(test, test$c_bwedt < as.Date("2022-01-01"))

# admission discharge bewty 2 and 1
test22 <- subset(test, test$c_bewty==2 | test$c_bewty==1)
test22 <- merge(test22, check, by = 'c_pseudonym')

# admission
aufnahme <- subset(test22, test22$c_bewty ==1) %>% select('c_pseudonym', 'c_bewty', 'c_bwart', 'c_bwedt')
# sometimes duplicate due to insurance (BU) 
aufnahme <- aufnahme %>% group_by(c_pseudonym) %>% arrange(c_bwedt) %>% slice(1L)

# discharge or death
entlassung <- subset(test22, test22$c_bewty==2) %>% select('c_pseudonym', 'c_bewty', 'c_bwart', 'c_bwedt')
tod <- subset(entlassung, entlassung$c_bwart %in% c('T', 'EH'))
tod <- unique(tod$c_pseudonym)
entlassung <- entlassung %>% group_by(c_pseudonym) %>% arrange(desc(c_bwedt)) %>% slice(1)

# calc LOS in hospital
aufenthaltsdaten <- merge(aufnahme, entlassung, by ='c_pseudonym')
aufenthaltsdaten$HospLOS <- difftime(aufenthaltsdaten$c_bwedt.y, aufenthaltsdaten$c_bwedt.x, units = "days")
aufenthaltsdaten$HospLOS<-as.integer(aufenthaltsdaten$HospLOS)

# second step of data cleaning (Exclusion Criteria) 9863 cases at start and cleaning of factors
# merge First exclusion step, aufenthaltsdata was not complete (admission or discharge missing/faulty)
data <- merge(data, aufenthaltsdaten, by = 'c_pseudonym')
data$tod <-0
data$tod[data$c_pseudonym %in% tod]<-1
#9852

# sex
data$geschlecht[data$geschlecht == "W"]<-"F"
data <- subset(data, data$geschlecht !="NA")
data <- subset(data, data$geschlecht != 'U')
# 9816

data <- subset(data, !is.na(data$meanAbsoluteRass)&!is.na(data$admissionsofa)&!is.na(data$admissionapache))
# 7888

data <- data %>% select(-c('c_bwedt.y', 'c_bewty.x', 'c_bwart.x',  'c_bwedt.x', 'c_bewty.y','c_bwart.y'))

# write table
write.table(data, '/Users/maximilianlindholz/Final/baseinfo4.csv', sep = '|', row.names = FALSE)



