rm(list = ls());gc()
library(data.table)
library(tidyverse)

data <- fread('/Users/maximilianlindholz/Final/baseinfo.csv')
full <- fread('/Users/maximilianlindholz/Final/pt.csv')

# Prone
# Prone 6
prone <- read.csv('/Applications/CO6_Data_String.csv', sep ='|')
prone <- prone %>% select('co6_patient_id', 'c_date_time_to','c_val')
prone$c_val <- tolower(prone$c_val)
test <- prone[grepl("bauch", prone$c_val),]
prone2 <- fread('/Users/maximilianlindholz/Final/vierterexpocobra5.csv')
prone2 <- subset(prone2, prone2$c_name == 'LAGERUNG')
prone2$c_vstring <- tolower(prone2$c_vstring)
test2 <- prone2[grepl("bauch", prone2$c_vstring),]
key6 <- fread('/Users/maximilianlindholz/Final/key6.csv')
test <- merge(test, key6, by = 'co6_patient_id')

# Prone 5
key5 <- fread('/Users/maximilianlindholz/Final/key5.csv')
test2 <- merge(test2, key5, by.x = 'c_dat_id', by.y = 'co5_dat_id')
test <- test %>% select('c_pseudonym', 'c_date_time_to')
test2 <- test2 %>% select('c_pseudonym', 'c_datum_fuer_wann')
names(test2)[2]<-'c_date_time_to'
test <- rbind(test,test2)
test$c_date_time_to <- as.Date(test$c_date_time_to)
check <- data %>% select('c_pseudonym', 'aufn','entl')
test <- merge(test, check, by = 'c_pseudonym')
test <- subset(test, test$c_date_time_to>=test$aufn & test$c_date_time_to <= test$entl)
prone <- unique(test$c_pseudonym)
data$prone <-0
data$prone[data$c_pseudonym %in% prone]<-1

# write part
write.table(data, '/Users/maximilianlindholz/Final/baseinfo2.csv', sep = '|', row.names = FALSE)


