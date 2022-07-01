rm(list = ls());gc()
library(data.table)
library(tidyverse)

data <- fread('/Users/maximilianlindholz/Final/baseinfo2.csv')
pt <- fread('/Users/maximilianlindholz/Final/pt.csv')

# key
key5 <- fread('/Users/maximilianlindholz/Final/key5.csv')
key6 <- fread('/Users/maximilianlindholz/Final/key6.csv')

# first apache
# apache cobra5
scores5 <- fread('/Users/maximilianlindholz/MobiCovid/Scores.csv', select = c('c_dat_id', 'c_var_id', 'c_gesamtscore', 'c_datum_fure_wann'))
apache <- subset(scores5, scores5$c_var_id == 20512776)
apache$c_datum_fure_wann <- as.Date(apache$c_datum_fure_wann)
apache <- subset(apache, apache$c_datum_fure_wann > as.Date('2018-01-01'))
apache5 <- merge(apache, key5, by.x='c_dat_id', by.y = 'co5_dat_id')

scores6 <- fread('/Users/maximilianlindholz/MobiCovid/CO6_Data_Long-SOFA, GCS, BPS, RASS, APACHE2.csv')
scores6 <- scores6 %>% select('co6_patient_id','c_var_id', 'c_date_time_to','c_val')
apache6 <- subset(scores6, scores6$c_var_id == 106868)
apache6 <- merge(apache6, key6, by = 'co6_patient_id')

apache5 <- apache5 %>% select('c_pseudonym', 'c_gesamtscore', 'c_datum_fure_wann')
apache6 <- apache6 %>% select('c_pseudonym', 'c_val', 'c_date_time_to')
names(apache6)[2]<-'c_gesamtscore'
names(apache6)[3]<-'c_datum_fure_wann'
apache6$c_datum_fure_wann <- as.Date(apache6$c_datum_fure_wann)
apache <- rbind(apache5, apache6)

# create check variable
check <- data %>% select('c_pseudonym', 'aufn','entl')
apache <- merge(apache, check, by = 'c_pseudonym')
apache <- subset(apache, apache$c_datum_fure_wann >= apache$aufn & apache$c_datum_fure_wann<= apache$entl)

# get first/admission apache by id, assuming that everyone does the mandatory admission exam (hard to define it in another way)
apache <- apache %>% arrange(c_datum_fure_wann)%>% group_by(c_pseudonym) %>% summarise_at('c_gesamtscore', first)
apache <- subset(apache, apache$c_gesamtscore < 70) # clearly wrong entrys with  e.g. 80000, apache only goes to 69
names(apache)[2]<-'admissionapache'
data <- merge(data, apache, by = 'c_pseudonym', all.x = TRUE)


# sofa score // cartesian true, because i will filter later on with the index file from cleanup1
sofa6 <- subset(scores6,scores6$c_var_id == 101490)
sofa6 <- merge(sofa6, key6, by = 'co6_patient_id', allow.cartesian = T)
sofa6 <- sofa6 %>% select('c_pseudonym', 'c_date_time_to', 'c_val')

sofa <- subset(scores5, scores5$c_var_id == 20512778)
sofa$c_datum_fure_wann <- as.Date(sofa$c_datum_fure_wann)
sofa <- merge(sofa, key5,by.x='c_dat_id', by.y = 'co5_dat_id')
sofa <- sofa %>% select(-c('c_dat_id', 'c_var_id'))
names(sofa6)[3]<-'c_gesamtscore'
names(sofa6)[2]<-'c_datum_fure_wann'
sofa6$c_datum_fure_wann<-as.Date(sofa6$c_datum_fure_wann)
sofa <- rbind(sofa, sofa6)
sofa <- merge(sofa, check, by = 'c_pseudonym')
sofa <- subset(sofa, sofa$c_datum_fure_wann >= sofa$aufn & sofa$c_datum_fure_wann<= sofa$entl)

# get first apache by id (same assumption as above)
sofa <- sofa %>% arrange(c_datum_fure_wann)%>% group_by(c_pseudonym) %>% summarise_at('c_gesamtscore', first)
sofa <- subset(sofa, sofa$c_gesamtscore < 25)
names(sofa)[2]<-'admissionsofa'
data <- merge(data, sofa, by = 'c_pseudonym', all.x = TRUE)

# rass part, mean rass of day, mean of days, for long analysis i will re read the RASS scores and assign them in Longcleanup
rass6 <- subset(scores6, scores6$c_var_id == 106195)
rass6 <- rass6 %>% select('co6_patient_id','c_date_time_to', 'c_val')
rass6 <- merge(rass6, key6, by.x='co6_patient_id', by.y = 'co6_patient_id', allow.cartesian = T)
rass6$c_val[rass6$c_val<0] <- rass6$c_val[rass6$c_val<0]*-1
rass6 <- rass6 %>% select ('c_date_time_to', 'c_val', 'c_pseudonym')
rass5 <- fread('/Users/maximilianlindholz/Final/vierterexpocobra5.csv')
rass5 <- subset(rass5, rass5$c_name == 'Rass')
rass5 <- rass5%>% select('c_datum_fuer_wann','c_vstring','c_dat_id')
rass5 <- na.omit(rass5)
rass5 <- merge(rass5, key5, by.x='c_dat_id', by.y = 'co5_dat_id')
rass5 <- rass5 %>% select(-c('c_dat_id'))
names(rass5)[2]<-'c_val'
names(rass5)[1]<-'c_date_time_to'
rass <- rbind(rass5,rass6)
rass$c_date_time_to<- as.Date(rass$c_date_time_to)
rass$c_val<- as.integer(rass$c_val)
rass <- merge(rass, check, by = 'c_pseudonym')
rass <- subset(rass, rass$c_date_time_to >= rass$aufn & rass$c_date_time_to<= rass$entl)
rass$c_val <- abs(rass$c_val)
rass <- rass %>% group_by(c_pseudonym,c_date_time_to) %>% summarise(c_val =median(c_val))
rass <- rass %>% group_by(c_pseudonym) %>% summarise(c_val =median(c_val))
data <- merge(data, rass, by = 'c_pseudonym', all.x = TRUE)
names(data)[76]<-'medianAbsoluteRass'

#fachrichtung as a factor not unique column
newfactor <- rep(NA, times=nrow(data))
a <-rowSums(data[,56:59])
newfactor[a > 1] <- "multi"
newfactor[a == 1 & data$InterdisciplinaryOperative==1] <- "InterdisciplinaryOperative"
newfactor[a == 1 & data$Neurocritical==1] <- "Neurocritical"
newfactor[a == 1 & data$InternalMedicine==1] <- "InternalMedicine"
newfactor[a == 1 & data$CardiacSurgery==1] <- "CardiacSurgery"
data <- cbind(data, newfactor)
data <- data %>% select(-c('InterdisciplinaryOperative', 'Neurocritical', 'InternalMedicine', 'CardiacSurgery'))
names(data)[73]<-'Fachrichtung'

# covid stationstyp as factor not unique column
newfactor2 <- rep(NA, times=nrow(data))
a <-rowSums(data[,56:58])
newfactor2[a > 1] <- "multi"
newfactor2[a == 1 & data$NonCovid==1] <- "NonCovid"
newfactor2[a == 1 & data$Established==1] <- "Established"
newfactor2[a == 1 & data$Surge==1] <- "Surge"
data <- cbind(data, newfactor2)
data <- data %>% select(-c("NonCovid","Established","Surge"))
names(data)[71]<-'CovidStationstyp'

# merge average physiotime, a lot of missing data
physiotime <- fread('/Users/maximilianlindholz/Final/physiotimes.csv')
physiotime <- physiotime %>% group_by(c_pseudonym) %>% summarise(c_val =mean(c_val))
physiotime <- na.omit(physiotime)
names(physiotime)[2]<- 'averagelengthofPT'
data <- merge(data, physiotime, by = 'c_pseudonym', all.x = TRUE)

write.table(data, '/Users/maximilianlindholz/Final/baseinfo3.csv', sep = '|', row.names = FALSE)
