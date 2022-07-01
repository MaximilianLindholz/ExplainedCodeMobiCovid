rm(list = ls());gc()
library(data.table)
library(tidyverse)

data <- fread('/Users/maximilianlindholz/Final/baseinfo4.csv')
full <- fread('/Users/maximilianlindholz/Final/pt.csv')

# frühmobi
frühmobi <- merge(full, data, by = 'c_pseudonym')
frühmobi$c_date_time_to <- as.Date(frühmobi$c_date_time_to)
frühmobi$ersteAufnahme <- as.Date(frühmobi$ersteAufnahme)
frühmobi <- subset(frühmobi, frühmobi$c_date_time_to < frühmobi$aufn +3 & frühmobi$c_date_time_to>=frühmobi$aufn )
frühmobi <- unique(frühmobi$c_pseudonym)
data$frühmobi <-0
data$frühmobi[data$c_pseudonym %in% frühmobi]<-1

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

# write clean NA
write.table(iv, '/Users/maximilianlindholz/Final/norepiclean.csv', sep = '|',row.names = F)

# data.table solution for expand rate to multiple days if given over multiple days (for later merge)
dt <- data.table(iv)
test <- dt[, list(c_pseudonym,length, c_rate,c_application_start,c_application_end, date = seq(c_application_start, c_application_end, by = "day")), by = 1:nrow(dt)]
test$date<-as.Date(test$date)

# index to remove duplicates later, time to posict
full$index <- 1:81529
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
rass6 <- merge(rass6, key6, by = 'co6_patient_id', allow.cartesian = T)
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
long$index <- 1:4937
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

# covid yes/no
data$CovidStationstyp<-as.factor(data$CovidStationstyp)
covid <- fread('/Users/maximilianlindholz/Final/Covid.csv')
fnmmer6 <- fread('/Users/maximilianlindholz/MobiCovid/co6_cohort.csv')
covid6 <- merge(covid, fnmmer6, by ='Fallnummer' )
covid6 <- merge(covid6, key6, by = 'co6_patient_id')
fnmmer5 <- fread('/Users/maximilianlindholz/MobiCovid/Cobra5/co5_cohort.csv')
covid5 <- merge(covid, fnmmer5, by = 'Fallnummer')
covid5 <- merge(covid5, key5, by = 'co5_dat_id')
covid5 <- covid5 %>% select('c_pseudonym')
covid6 <- covid6 %>% select('c_pseudonym')
covid <- rbind(covid5, covid6)
covid <- covid %>% distinct()
data$covid <- 0
data$covid[data$c_pseudonym %in% covid$c_pseudonym]<-1

# duplicate check from cleanupstep 1, always take the duplicate with most pt entrys documented
index <- fread('/Users/maximilianlindholz/Final/index.csv')
index <- index %>% select('c_pseudonym', 'index.y')
index <- index %>% distinct()
dups <-index[duplicated(index$index.y)|duplicated(index$index.y, fromLast=TRUE),]
dups <- unique(dups$c_pseudonym)
data$dup <- 0
data$dup[data$c_pseudonym%in%dups]<-1
datadub <- subset(data, data$dup == 1)
datadub <- datadub %>% select('c_pseudonym', 'norepinephrine', 'aufn', 'n','Behandlungsdauer')
datadub <- merge(datadub, index, by = 'c_pseudonym')
datadubmaxx <- datadub

# datadubmaxx$co5<-0
# datadubmaxx$co5[datadubmaxx$c_pseudonym %in% unique(key5$c_pseudonym)]<-1
# datadubmaxx$co6<-0
# datadubmaxx$co6[datadubmaxx$c_pseudonym %in% unique(key6$c_pseudonym)]<-1
# 
# datadubmaxx <- datadub %>% group_by(index.y) %>% slice(which.max(n))
take <- unique(datadubmaxx$c_pseudonym)
toss <- data.table(unique(datadub$c_pseudonym))
toss <- subset(toss, !(toss$V1 %in%take))
take <- data.table(take)

# cov back check because double doc was predom. in covid

susb <- subset(data, data$c_pseudonym %in% c('boiesd_mobicovid_2140','boiesd_mobicovid_2038','boiesd_mobicovid_3017','boiesd_mobicovid_3309'))


# Exclusionsteps start: 19931
sum(data$n[!is.na(data$n)]) #73031
data <- subset(data, data$Behandlungsdauer>2) # 10085
data <- subset(data, data$age>17) # 10010
sum(data$n[!is.na(data$n)]) #69444

data<- subset(data, !is.na(data$medianAbsoluteRass))
data<- subset(data, !is.na(data$admissionsofa))
data<- subset(data, !is.na(data$admissionapache)) # 8823
data<- subset(data, data$geschlecht %in% c("M","F", "W")) #8790
data<- subset(data, !is.na(data$age))
data<- subset(data, data$age <101) # 8787

data$perday <- data$n/data$Behandlungsdauer
data$perday[is.na(data$perday)]<-0
sum(data$n[!is.na(data$n)]) #65307

# sex
data$geschlecht[data$geschlecht == "W"]<-"F"

length(data$norepinephrine[data$norepinephrine==1])# 4584
sum(data$n[!is.na(data$n)&data$norepinephrine==1]) #30545
length(data$norepinephrine[data$norepinephrine==0]) #4734
sum(data$n[!is.na(data$n)&data$norepinephrine==0]) #19869
test5 <- subset(test5, test5$c_pseudonym %in% unique(data$c_pseudonym)) #3230
length(unique(test5$c_pseudonym)) #763


data <- subset(data, !(data$c_pseudonym %in% dups))

#write part
write.table(test5, '/Users/maximilianlindholz/Final/during.csv', sep = '|',row.names = F)
write.table(data, '/Users/maximilianlindholz/Final/baseinfo5.csv', sep = '|',row.names = F)


# for clara
claratable <- data
claratable[is.na(claratable)]<- 999
claratable <- claratable %>% select(-c('index','c_bewty.x', 'c_bwart.x',  'c_bwedt.x', 'c_bewty.y', 'c_bwart.y',  'c_bwedt.y','dup'))
write.table(data, '/Users/maximilianlindholz/Final/clara999.csv', sep = '|',row.names = F)

