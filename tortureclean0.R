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

# part cobra 6
tabelle <- merge(tabelle, fallnummer, by = 'co6_patient_id')
tabelle <- tabelle %>% distinct()
datum <- base %>% select('Fallnr', 'aufn','entl','stationsbezeichnung','geschlecht')
tabelle <- merge(tabelle, datum, by.x = "Fallnummer", by.y = "Fallnr")
tabelle <- tabelle %>% distinct()

# remove cases with not clear pseudonym, co6 relation
regel <- tabelle %>% select('co6_patient_id','c_pseudonym')
regel <- regel %>% distinct()
regel <- regel %>% group_by(co6_patient_id) %>% mutate(count = n())
regelweg <- subset(regel, regel$count>=2)
torture <- subset(tabelle, !(tabelle$c_pseudonym %in% unique(regelweg$c_pseudonym)))

# regel 1 wenn patient wiederholt da war, nur fälle wo er beim ersten mal intensivpflichtig wurde
# wenn pat. in diesem zeitraum in 2 systemen geführt worden ist und die identifizierung nicht mehr klar möglich war => selbe ID für >= 2 pseudonyme => Gelöscht da merge unmöglich  14073->12123
write.table(torture, '/Users/maximilianlindholz/Final/basenew.csv', sep = '|', row.names = FALSE)




