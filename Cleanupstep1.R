rm(list = ls());gc()
library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(comorbidity)
library(bit64)
base <- fread('/Users/maximilianlindholz/Final/basenew.csv')

# calculate time of treatment
base$Behandlungsdauer <- difftime(base$entl,base$aufn, units = c("days"))
base$Behandlungsdauer <- as.double(base$Behandlungsdauer)
key6 <- base %>% select('c_pseudonym', 'co6_patient_id')
key6 <- key6 %>% distinct()
key5 <- base %>% select('c_pseudonym', 'co5_dat_id')
key5 <- key5 %>% distinct()

# age from sap
sappat <- read.csv2('/Users/maximilianlindholz/MobiCovid/sap_patient.csv', sep = '|')
sappat <- sappat[,c(1,5)]
sappat$c_gbdat <- ymd(sappat$c_gbdat)
sappat <- distinct(sappat)

# bday
base <- merge(base, sappat, by.x = 'c_pseudonym' , by.y = 'c_pseudonym', all.x = TRUE)
base$age <- trunc((base$c_gbdat %--% base$aufn) / years(1))

# drop other age stuff
base <- base %>% select(-c('c_gbdat'))

# Elixhauser at admission
diagnosen <- fread('/Users/maximilianlindholz/MobiCovid/sap_diagnoses.csv', sep = '|', colClasses = rep('character', 63)) 
diagnosen <- diagnosen%>%select('c_pseudonym', 'c_dkey1', 'c_diadt')
diagnosen <- diagnosen %>% distinct(c_pseudonym, c_dkey1, .keep_all = TRUE)
check <- base %>% select('aufn', 'c_pseudonym')
diagnosen <- merge(diagnosen, check, by = 'c_pseudonym', allow.cartesian = T)
diagnosen$c_diadt <- ymd(diagnosen$c_diadt)
diagnosen$aufn<-as.Date(diagnosen$aufn)
diagnosen <- subset(diagnosen, diagnosen$c_diadt<= diagnosen$aufn)
charlson <- comorbidity(diagnosen,id = 'c_pseudonym', code = 'c_dkey1', map = "elixhauser_icd10_quan", assign0 = T)
charlson$Elixhauser <- score(charlson, weights = 'swiss', assign0 = TRUE)
base <- merge(base, charlson, by = 'c_pseudonym', all.x = T)
base[,10:42][is.na(base[,10:42])]<-0

# weight
# co6 gewicht
gew <- fread('/Users/maximilianlindholz/Final/CO6_Data_Decimal_Gewicht, Größe, PEEP, AF, etCO2, FiO2, HF, Temp, SaO2, Pinsp, IE, VT, OxyIndex, OraleErnährungMenge, O2-Zufuhr, ECMO-Blutfluss, ECMO-Gasfluss.csv')
weight <- subset(gew, c_var_id == 6)
weight <- weight %>% select('co6_patient_id','c_val')
weight$c_val <- as.integer(weight$c_val)
weight$co6_patient_id <- as.numeric(weight$co6_patient_id)
weight <- aggregate(c_val~co6_patient_id,data = weight, max,na.rm=T)
weight <- merge(weight, key6, by = 'co6_patient_id')
weight <- weight %>% select('c_pseudonym','c_val')
names(weight)[2]<- 'Gewicht'
#co5 gewicht
co5 <- fread('/Users/maximilianlindholz/Final/vierterexpocobra5.csv')
weigth2 <- subset(co5, co5$c_name == 'K_Gewicht')
weigth2 <- weigth2 %>% select('c_dat_id','c_vfloat')
weigth2 <- aggregate(c_vfloat~c_dat_id,data = weigth2, max,na.rm=T)
weigth2 <- merge(weigth2, key5, by.x='c_dat_id', by.y = 'co5_dat_id')
weigth2 <- weigth2 %>% select('c_pseudonym','c_vfloat')
names(weigth2)[2]<- 'Gewicht'

gewicht <- rbind(weight, weigth2)
gewicht$Gewicht <- as.integer(gewicht$Gewicht)
gewicht <- aggregate(Gewicht~c_pseudonym,data = gewicht, mean,na.rm=T)
base <- merge(base, gewicht, by = 'c_pseudonym', all.x = TRUE)

# entry 44000 und 70000 kg => sicher fehler sowohl in gramm als auch kilogramm :D
base$Gewicht[base$Gewicht>250]<-NA

#cleanup of wards that are not part of our scope (e.g. non-ICU, that mistakenly was extracted)
base <- subset(base, !base$stationsbezeichnung %in% c("WAN-PACU", "MPACU", "W50", "M149A","W41","M147I")) # 21491

# Cleanup of wards 
base$stationsbezeichnung[base$stationsbezeichnung == "W9I"] <- "9i"  
base$stationsbezeichnung[base$stationsbezeichnung == "W43I"] <- "43i"   
base$stationsbezeichnung[base$stationsbezeichnung == "WAC-S21I"] <- "21i"  
base$stationsbezeichnung[base$stationsbezeichnung == "MID-144I"] <- "144i"  
base$stationsbezeichnung[base$stationsbezeichnung == "M103I"] <- "103i"

# Stationstyp Covid
base$Stationstyp[base$stationsbezeichnung %in% c("M204AI","M203BI","M203AI","M204BI")]<- 'Surge'
base$Stationstyp[base$stationsbezeichnung %in% c("WAN-S8I","S44I","43i","144i","103i")]<-'Established'
base$Stationstyp[base$stationsbezeichnung %in% c("M101I","M102I","S32B", "21i","WAN-S14I","W1I","9i")]<-'NonCovid'

# Attending Specialty
base$fachrichtung[base$stationsbezeichnung %in% c("M102I")]<- "Neurocritical"
base$fachrichtung[base$stationsbezeichnung %in% c("M101I", "S44I",  "9i","21i", "M204BI","M204AI", "WAN-S14I","WAN-S8I")]<- "InterdisciplinaryOperative"
base$fachrichtung[base$stationsbezeichnung %in% c("103i", "144i", "43i","S32B","M203AI","M203BI" )]<- "InternalMedicine"
base$fachrichtung[base$stationsbezeichnung %in% c( "W1I")]<- "CardiacSurgery"

# Resp. and Circ. Support, further down i check whether the service was provided during ICU-Admission
ops <- fread('/Users/maximilianlindholz/MobiCovid/sap_ops.csv')
ops <- ops %>% select('c_pseudonym', 'c_icpml', 'c_bgdop', 'c_timestamp_')

# cleanup of weird date-structure 
ops$c_timestamp_ <- substr(ops$c_timestamp_, start = 1, stop = 8)
ops$c_timestamp_ <-ymd(ops$c_timestamp_) 
ops$short <- substr(ops$c_icpml, start=1, stop=5)
ops$longer <- substr(ops$c_icpml, start = 1, stop = 7)
ops <- merge(ops, check, by = 'c_pseudonym', allow.cartesian = T)
ops$aufn <- as.Date(ops$aufn)
ops <- subset(ops, ops$c_timestamp_ >= ops$aufn)
# ECMO all starting with 8-852
ecmo <- subset(ops, ops$short=='8-852')
ecmo <- ecmo %>% select('c_pseudonym', 'c_timestamp_')

# Dialyse all starting with 8-854
dialyse <- subset(ops, ops$short == '8-854')
dialyse <- dialyse %>% select('c_pseudonym', 'c_timestamp_')

# Temporäre Tracheostomie - 5-311
tracheostomie <- subset(ops, ops$short == '5-311')
tracheostomie <- tracheostomie %>% select('c_pseudonym', 'c_timestamp_')

# Intubiert ja/nein - 8 -701, 8-704
intubated <- subset(ops, ops$short == '8-701'|ops$short == '8-704')
intubated <- intubated %>% select('c_pseudonym', 'c_timestamp_')

# Anlegen einer Maske zur maschinellen Beatmung 8-706
maskedventilation <- subset(ops, ops$short == '8-706')
maskedventilation <- maskedventilation %>% select('c_pseudonym', 'c_timestamp_')

# High FLow - 8-713
highflow <- subset(ops, ops$short == '8-713')
highflow <- highflow %>% select('c_pseudonym', 'c_timestamp_')

# physio
base$index <- 1:27906
full <- fread('/Users/maximilianlindholz/Final/physiotextelabeled.csv')
full <- full %>% select('c_val','c_dat_id','co6_patient_id','c_date_time_to', 'IMS')

# split into 5 and 6 documentation parts
full5 <- subset(full, !is.na(full$c_dat_id))
full6 <- subset(full, is.na(full$c_dat_id))
full5 <- subset(full5, full5$c_dat_id != "")
full5$c_dat_id<-as.integer64(full5$c_dat_id)
full5 <- subset(full5, full5$c_dat_id > 200000)
full5 <- merge(full5, key5, by.x = 'c_dat_id', by.y = 'co5_dat_id')
full5 <- full5 %>% select('c_pseudonym', 'IMS', 'c_date_time_to', 'c_val')

full6 <- full6 %>% select('IMS', 'c_date_time_to', 'c_val', 'co6_patient_id')
full6 <-  full6[!(is.na(full6$co6_patient_id) | full6$co6_patient_id==""), ]
full6 <-  full6[!(is.na(full6$co6_patient_id) | full6$c_val==""), ]
full6$co6_patient_id<-as.integer64(full6$co6_patient_id)
full6 <- merge(full6, key6, by = 'co6_patient_id')
full6 <- full6 %>% select('c_pseudonym', 'IMS', 'c_date_time_to','c_val')
full <- rbind(full5,full6) 

# counting variable
full$IMS<- as.integer(full$IMS)
full$one <- 1

# remove pt thats not related to the stay
time <- base %>% select('c_pseudonym', 'aufn','entl')
safe <-full
full <- merge(full, time, by = 'c_pseudonym', allow.cartesian = T)
full$controll <- full$c_date_time_to
full$c_date_time_to<-sub(" .*", "", full$c_date_time_to)
full$c_date_time_to <- dmy(full$c_date_time_to)
full$aufn <- as.Date(full$aufn)
full$entl <- as.Date(full$entl)
full <- subset(full, full$c_date_time_to >= full$aufn & full$c_date_time_to <= full$entl)

# now remove unnecessary columns during the relevant stay
full <- full %>% select(-c('aufn','entl'))
full <- full %>% distinct()
a <- full %>% group_by(c_pseudonym) %>% count(one)

# summarize data per patient (Max and Min is for another analysis with the same data, for my analysis I use the long format data)
count <- a 
MaxIMS <-full %>% group_by(c_pseudonym) %>% summarise(max = max(IMS, na.rm=TRUE))
MinIMS <- full %>% group_by(c_pseudonym) %>% summarise(min = min(IMS, na.rm=TRUE))
# this will merge later down

# create one value for each participant with value from start of treatment
data <- base
frame <- data[!duplicated(data$c_pseudonym), ] %>% dplyr::select('c_pseudonym','geschlecht')

# type of ward
stationstypen <- data %>% group_by(c_pseudonym) %>% summarize(stationsbezeichnung = unique(stationsbezeichnung))
stationstypen$value <- 1
stationstypen <- stationstypen %>% pivot_wider(names_from = stationsbezeichnung, values_from = value)
stationstypen[is.na(stationstypen)]<-0

# Length of Stay in ICU 
behdauerframes <- data %>% select('c_pseudonym', 'stationsbezeichnung', 'Behandlungsdauer')
behdauerframes <- behdauerframes %>% distinct()
behdauer <- behdauerframes %>% group_by(c_pseudonym) %>% summarise_at("Behandlungsdauer", sum, na.rm = TRUE)

# type of wards
stationstyp <- data %>% group_by(c_pseudonym) %>% summarize(Stationstyp = unique(Stationstyp))
stationstyp$value <- 1
stationstyp <- stationstyp %>% pivot_wider(names_from = Stationstyp, values_from = value)
stationstyp[is.na(stationstyp)]<-0

#admission and discharge
aufn <- data %>% group_by(c_pseudonym) %>% summarise_at('aufn', min, na.rm = TRUE)
entl <- data %>% group_by(c_pseudonym) %>% summarise_at('entl', max, na.rm = TRUE)
# max values
aufnahmevales <- data %>%arrange(aufn)%>% group_by(c_pseudonym) %>% summarise_at(c('age','chf', 'carit', 'valv', 'pcd', 'pvd', 'hypunc', 'hypc', 'para', 'ond', 'cpd', 'diabunc', 'diabc',
                                                                                   'hypothy', 'rf', 'ld', 'pud', 'aids', 'lymph', 'metacanc','solidtum', 'rheumd', 'coag', 'obes', 'wloss', 
                                                                                   'fed', 'blane', 'dane', 'alcohol', 'drug', 'psycho', 'depre','Elixhauser','Gewicht'), first)


#fachrichtung
fachrichtung <- data %>% group_by(c_pseudonym) %>% summarize(fachrichtung = unique(fachrichtung))
fachrichtung$value <- 1
fachrichtung <- fachrichtung %>% pivot_wider(names_from = fachrichtung, values_from = value)
fachrichtung[is.na(fachrichtung)] <- 0

data <- merge(frame, stationstypen, by = 'c_pseudonym')
data <- merge(data, behdauer, by = 'c_pseudonym')
data <- merge(data, aufn, by = 'c_pseudonym')
data <- merge(data, entl, by = 'c_pseudonym')
data <- merge(data, aufnahmevales, by ='c_pseudonym')
data <- merge(data, fachrichtung, by = 'c_pseudonym')
data <- merge(data, stationstyp, by = 'c_pseudonym', all.x = TRUE)

# now add support variables, checking for whether it was during ICU-Stay
check <- data %>% dplyr::select('c_pseudonym','aufn','entl')
check$aufn <- as.Date(check$aufn)
check$entl <- as.Date(check$entl)
check2 <- merge(check, ecmo, by = 'c_pseudonym', all.x = TRUE)
check2 <- subset(check2, !is.na(check2$c_timestamp_))
check2 <- subset(check2, check2$c_timestamp_ >= check2$aufn & check2$c_timestamp_ <= check2$entl)
ecmo <- unique(check2$c_pseudonym)

check3 <- merge(check, dialyse, by = 'c_pseudonym', all.x = TRUE)
check3 <- subset(check3, !is.na(check3$c_timestamp_))
check3 <- subset(check3, check3$c_timestamp_ >= check3$aufn & check3$c_timestamp_ <= check3$entl)
dialyse <- unique(check3$c_pseudonym)

check4 <- merge(check, tracheostomie, by = 'c_pseudonym', all.x = TRUE)
check4 <- subset(check4, !is.na(check4$c_timestamp_))
check4 <- subset(check4, check4$c_timestamp_ >= check4$aufn & check4$c_timestamp_ <= check4$entl)
tracheostomie <- unique(check4$c_pseudonym)

check5 <- merge(check, intubated, by = 'c_pseudonym', all.x = TRUE)
check5 <- subset(check5, !is.na(check5$c_timestamp_))
check5 <- subset(check5, check5$c_timestamp_ >= check5$aufn & check5$c_timestamp_ <= check5$entl)
intubated <- unique(check5$c_pseudonym)

check6 <- merge(check, maskedventilation, by = 'c_pseudonym', all.x = TRUE)
check6 <- subset(check6, !is.na(check6$c_timestamp_))
check6 <- subset(check6, check6$c_timestamp_ >= check6$aufn & check6$c_timestamp_ <= check6$entl)
maskedventilation <- unique(check6$c_pseudonym)

check7 <- merge(check, highflow, by = 'c_pseudonym', all.x = TRUE)
check7 <- subset(check7, !is.na(check7$c_timestamp_))
check7 <- subset(check7, check7$c_timestamp_ >= check7$aufn & check7$c_timestamp_ <= check7$entl)
highflow <- unique(check7$c_pseudonym)

data$dialyse <- 0
data$dialyse[data$c_pseudonym %in% dialyse]<-1

data$ecmo <-0
data$ecmo[data$c_pseudonym %in% ecmo]<-1

data$highflow <- 0
data$highflow[data$c_pseudonym %in% highflow]<-1

data$intubated <- 0
data$intubated[data$c_pseudonym %in% intubated]<-1

data$maskedventilation <-0
data$maskedventilation[data$c_pseudonym %in% maskedventilation]<-1

data$tracheostomie <-0
data$tracheostomie[data$c_pseudonym %in% tracheostomie]<-1

data <- merge(data, count, by = 'c_pseudonym', all.x = TRUE)
data <- merge(data, MaxIMS, by = 'c_pseudonym', all.x = TRUE)
data <- merge(data, MinIMS, by = 'c_pseudonym', all.x = TRUE)

data <- data %>% select(-c('one'))

# used for the long analysis, names solution is not pretty I know
names(full)[6]<-'exacttime'

write.table(data, '/Users/maximilianlindholz/Final/baseinfo.csv', sep = '|', row.names = FALSE)
write.table(full, '/Users/maximilianlindholz/Final/pt.csv', sep = '|',row.names = F)
write.table(key5, '/Users/maximilianlindholz/Final/key5.csv', sep = '|',row.names = F)
write.table(key6, '/Users/maximilianlindholz/Final/key6.csv', sep = '|',row.names = F)
