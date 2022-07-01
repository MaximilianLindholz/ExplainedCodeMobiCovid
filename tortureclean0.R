rm(list = ls());gc()
library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(comorbidity)
library(bit64)
base <- read_excel('/Users/maximilianlindholz/Final/WIDEALL.xlsx', sheet = 'Sheet1')

# id tabelle
tabelle <- fread('/Users/maximilianlindholz/Final/data_vol4/cohort.csv')
tabelle$co6_patient_id<-as.integer64(tabelle$co6_patient_id)

fallnummer <- fread('/Users/maximilianlindholz/MobiCovid/co6_cohort.csv', sep = ';')
tabelle <- merge(tabelle, fallnummer, by = 'co6_patient_id')
tabelle <- tabelle %>% distinct()
datum <- base %>% select('Fallnr', 'aufn','entl','stationsbezeichnung')
tabelle <- merge(tabelle, datum, by.x = "Fallnummer", by.y = "Fallnr")
tabelle <- tabelle %>% distinct()

# check <- base %>% select('c_pseudonym','aufn','entl', 'COBRA)
test <- fread('/Users/maximilianlindholz/MobiCovid/sap_movements.csv')
test22 <- subset(test, test$c_bewty==2 | test$c_bewty==1)
test22 <- test22 %>% select('c_pseudonym', 'c_bewty','c_bwart', 'c_orgfa', 'c_bwedt','c_orgpf',  'c_zimmr','c_wplid')
test22$c_bwedt<-ymd(test22$c_bwedt)
# some data is 9999-999 => those are duplicates as i found in data scoping
test22 <- subset(test22, test22$c_bwedt < as.Date("2022-01-01"))
test22 <- test22 %>% select(-c('c_orgpf', 'c_zimmr',  "c_wplid"))
test22 <- test22[order(test22$c_pseudonym, test22$c_bwedt, test22$c_bewty), ]
test22 <- test22 %>% distinct()
test22 <- test22 %>% select(-c('c_bwart',  'c_orgfa'))
test22 <- test22 %>% distinct()
# 
# timeframe <- data.frame("start"= as.Date('2030-03-25'), "end" =as.Date('2030-03-25'), 'c_pseudonym' = NA, 'event' = NA)
# 
# for (i in unique(test22$c_pseudonym)){
#   # i = 'boiesd_mobicovid_4628'
#   a <- subset(test22, test22$c_pseudonym==i)
#   b <- 1
#   if(nrow(a)>1){
#     counter <- (nrow(a)/2)+1
#     e <-0
#     for (x in 1:counter){
#       e <-e+1
#       if(nrow(a)>b){
#         c <- a[c(b,b+1),]
#         print(c)
#         
#         if(c$c_bewty[1]==1 &c$c_bewty[2]==2){
#           start <-c$c_bwedt[1]
#           end <- c$c_bwedt[2]
#           timeframe2 <- data.frame("start"= start, "end" =end, 'c_pseudonym' = i, 'event' = e)
#           timeframe <- rbind(timeframe, timeframe2)
#         } else {
#           b<-9000
#         }
#         print(b)
#         b<-b+2
#       }
#     }
#   } 
# }
# timeframe<-na.omit(timeframe)
# write.table(timeframe, '/Users/maximilianlindholz/Final/torture.csv', sep = '|', row.names = FALSE)
torture <- fread('/Users/maximilianlindholz/Final/torture.csv')

torture <- merge(torture, tabelle, by = 'c_pseudonym')
torture <- subset(torture, torture$aufn>= torture$start & torture$entl <= torture$end)
torture <- subset(torture, torture$event<2)

regel <- tabelle %>% select('co6_patient_id','c_pseudonym')
regel <- regel %>% distinct()
regel <- regel %>% group_by(co6_patient_id) %>% mutate(count = n())
regelweg <- subset(regel, regel$count<2)

regelkurz <- regel %>% select('co6_patient_id','count')
regelkurz <- regelkurz %>% distinct()
torture <- merge(torture, regelkurz, by = 'co6_patient_id')
torture2 <- subset(torture, torture$count<2)

# regel 1 wenn patient wiederholt da war, nur fälle wo er beim ersten mal intensivpflichtig wurde
# wenn pat. in diesem zeitraum in 2 systemen geführt worden ist und die identifizierung nicht mehr klar möglich war => selbe ID für >= 2 pseudonyme => Gelöscht da merge unmöglich  14073->12123
write.table(torture2, '/Users/maximilianlindholz/Final/basenew.csv', sep = '|', row.names = FALSE)
