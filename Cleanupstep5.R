rm(list = ls());gc()
library(data.table)
library(tidyverse)

data <- fread('/Users/maximilianlindholz/Final/baseinfo4.csv')
full <- fread('/Users/maximilianlindholz/Final/pt.csv')

# key
key5 <- fread('/Users/maximilianlindholz/Final/key5.csv')
key6 <- fread('/Users/maximilianlindholz/Final/key6.csv')

# medis here for rate
medis <- fread('/Users/maximilianlindholz/Final/medicationclean.csv')
medis <- subset(medis, medis$c_rate>0)
norepi <- subset(medis, medis$Category=="Norepinephrine")
norepi <- norepi %>% dplyr::select('c_pseudonym', 'c_amount_total','c_amount_unit','day', 'c_application_start','c_application_end','c_rate', 'c_rate_unit','c_application_location')
iv <- subset(norepi, norepi$c_application_location %in% c("i.v. kontinuierlich", "i.v."  ,              "i.v. kont." ,         "i.v. Kurzinf."))
iv <- subset(iv, iv$c_rate_unit != "" )
weight <- data %>% dplyr::select('c_pseudonym', 'Gewicht')
iv <- merge(iv, weight, by = 'c_pseudonym')
iv$length <- difftime(iv$c_application_end, iv$c_application_start, units="mins")

# QC: weight and rate unclear => These cases i cant controll for weight in the rate because i have neither weight to calculate it manually nor rate(already weight adjusted, this will cancel out to around
# 82 cases during PT, that i will remove there because i cant compare it to the rest)
unclear <- subset(iv, is.na(iv$Gewicht) & c_rate_unit %in% c("mg/h","mg/d","mg/min"))

# with regard to weight nach gewicht normiert
iv$c_rate[iv$c_rate_unit %in% c("mg/h","mg/d","mg/min")]<- iv$c_rate[iv$c_rate_unit %in% c("mg/h","mg/d","mg/min")]/iv$Gewicht[iv$c_rate_unit %in% c("mg/h","mg/d","mg/min")]
iv$c_rate_unit[iv$c_rate_unit == "mg/h"]<- "mg/kg/h"
iv$c_rate_unit[iv$c_rate_unit == "mg/d"]<- "mg/kg/d"
iv$c_rate_unit[iv$c_rate_unit == "mg/min"]<- "mg/kg/min"

# same unit
iv$c_rate[iv$c_rate_unit == "mg/kg/min"]<-iv$c_rate[iv$c_rate_unit == "mg/kg/min"]*1000
iv$c_rate[iv$c_rate_unit == "mg/kg/h"]<-(iv$c_rate[iv$c_rate_unit == "mg/kg/h"]/60)*1000
iv$c_rate[iv$c_rate_unit == "mg/kg/d"]<-(iv$c_rate[iv$c_rate_unit == "mg/kg/d"]/1440)*1000
iv$c_rate_unit <- "µg/kg/min"

# data.table solution for expand rate to multiple days if given over multiple days (for later merge)
dt <- data.table(iv)
test <- dt[, list(c_pseudonym,length, c_rate,c_application_start,c_application_end, date = seq(c_application_start, c_application_end, by = "day")), by = 1:nrow(dt)]
test$date<-as.Date(test$date)

# index to remove duplicates later, time to posict
full$index <- 1:63473
# calc for flow chart I rest below
# calc <- full
full$exacttime <- chartr('.', '-', full$exacttime)
full$exacttime <- as.POSIXct(full$exacttime, format = "%d-%m-%y %H:%M")
full <- merge(full, test, by.x = c('c_pseudonym','c_date_time_to'), by.y = c('c_pseudonym','date'), all.x = TRUE)
full$c_rate[is.na(full$c_rate)]<-0


# only take cases where katecholamines were administered during pt
full <- subset(full, !is.na(full$c_application_end))
full2 <- subset(full, full$exacttime> full$c_application_start & full$exacttime<full$c_application_end) 

# if given multiple perfusors take max (only few cases)
full3 <- full2 %>% group_by(index) %>% summarise(c_rate =sum(c_rate))
backstep <- full2 %>% select('c_pseudonym',  'IMS', 'c_val', 'length', 'exacttime','index','c_date_time_to')
backstep <- backstep %>% distinct(index, .keep_all = TRUE)
full4 <- merge(backstep, full3, by = 'index')

# remove cases with 0 = not weight adjustable (created through NA calc)
#length(full4$c_rate[full4$c_rate==0])
# 76
full4 <- subset(full4, full4$c_rate >0)

# Select relevant columns and define unit as calculated above
long <- full4 %>% dplyr::select('c_pseudonym','exacttime', 'IMS','c_val','c_rate', 'length','c_date_time_to')
long$rateunit <- "µg/kg/min"

# first apache
# apache cobra5
scores5 <- fread('/Users/maximilianlindholz/Final/vierterexpocobra5.csv')
rass <- subset(scores5, scores5$c_name == 'Rass')
rass <- rass %>% dplyr::select('c_dat_id', 'c_vstring', 'c_datum_fuer_wann')
rass <- merge(rass, key5, by.x='c_dat_id', by.y = 'co5_dat_id')
scores6 <- fread('/Users/maximilianlindholz/MobiCovid/CO6_Data_Long-SOFA, GCS, BPS, RASS, APACHE2.csv')
scores6 <- scores6 %>% dplyr::select('co6_patient_id','c_var_id', 'c_date_time_to','c_val')
rass6 <- subset(scores6, scores6$c_var_id == 106195)
rass6 <- merge(rass6, key6, by = 'co6_patient_id')
rass5 <- rass %>% dplyr::select('c_pseudonym', 'c_vstring', 'c_datum_fuer_wann')
rass6 <- rass6 %>% dplyr::select('c_pseudonym', 'c_val', 'c_date_time_to')
rass5$c_vstring<-as.integer(rass5$c_vstring)
names(rass5)[2]<-'c_val'
names(rass6)[3]<-'c_datum_fuer_wann'
rass <- rbind(rass5, rass6)
rass$date <- as.Date(rass$c_datum_fuer_wann)
rass$c_val<- abs(rass$c_val)

# merge rass and long
long$c_date_time_to<-as.Date(long$c_date_time_to)
long$index <- 1:3462
test4 <- merge(long, rass, by.x = c('c_pseudonym','c_date_time_to'), by.y = c('c_pseudonym','date'))
test4 <- subset(test4, test4$c_datum_fuer_wann <= test4$exacttime)
test5 <- test4 %>% arrange(desc(c_val.y)) %>%filter(duplicated(index) == FALSE)
test5 <- test5 %>% select(-c('index', 'c_datum_fuer_wann'))
names(test5)[9]<-'RASS'

# norepinephrine yes/no
check <- data %>% dplyr::select('c_pseudonym','aufn','entl')
norepiyn <- fread('/Users/maximilianlindholz/Final/medicationclean.csv')
norepiyn <- subset(norepiyn, norepiyn$Category=="Norepinephrine")
norepiyn <- subset(norepiyn, norepiyn$c_application_location %in% c("i.v. kontinuierlich", "i.v."  ,              "i.v. kont." ,         "i.v. Kurzinf."))
norepiyn <- subset(norepiyn, as.Date(norepiyn$c_application_start) >= aufn & as.Date(norepiyn$c_application_start)<=entl)
yn <- unique(norepiyn$c_pseudonym)

data$norepinephrine <-0
data$norepinephrine[data$c_pseudonym%in%yn]<-1

# flow chart II
# calc <- merge(calc, data, by = "c_pseudonym")
# calc <- subset(calc, calc$norepinephrine ==1)
# 35555 => non norepi group 59538- = 23983
# calc <- subset(calc, calc$c_date_time_to <= calc$entl)


write.table(test5, '/Users/maximilianlindholz/Final/during.csv', sep = '|',row.names = F)
write.table(data, '/Users/maximilianlindholz/Final/baseinfo5.csv', sep = '|',row.names = F)

