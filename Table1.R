rm(list = ls());gc()
library(tableone)
library(data.table)

# mobilization with norepinephrine
long <- fread('/Users/maximilianlindholz/Final/during.csv')

# general patient data
data <- fread('/Users/maximilianlindholz/Final/baseinfo5.csv')

myVars <- c("age","Elixhauser","admissionapache", "admissionsofa", "meanAbsoluteRass")

## Vector of categorical variables that need transformation
catVars <- c( "obes","dialyse", "ecmo", "highflow", 
             "intubated", "maskedventilation", "tracheostomie","covid")

data <- data.frame(data)
data[,catVars] <- sapply(data[,catVars],function(x) ifelse(x==1,"yes","no"))

# mulit means patient was on multiple wards with different attending specialties
data$Fachrichtung[data$Fachrichtung=="multi"]<-"Multiple Specialties"
## Create a TableOne object
tab1 <- CreateTableOne(vars = c("age","Elixhauser","admissionapache", "admissionsofa", "medianAbsoluteRass","geschlecht", "obes","dialyse", "ecmo", "highflow", 
                                "intubated", "maskedventilation", "tracheostomie","Fachrichtung", "covid",'CovidStationstyp'), strata = "norepinephrine", 
                       data = data, factorVars = c("geschlecht", "obes","dialyse", "ecmo", "highflow", 
                                                   "intubated", "maskedventilation", "tracheostomie","prone","Fachrichtung",'CovidStationstyp','norepinephrine'))
print(tab1, showAllLevels = TRUE)

tab3Mat <- print(tab1,showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

# table 2
tab2 <- CreateTableOne(vars = c("perday", "frühmobi", "tod"), strata = "norepinephrine", 
                       data = data, factorVars = c("tod","frühmobi"), test = F)

print(tab2, showAllLevels = TRUE,formatOptions = list(big.mark = ","))

tab4Mat <- print(tab2,showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = TRUE)

# rates
long <- fread('/Users/maximilianlindholz/Final/during.csv')
long$outofbed[long$IMS>3]<-'out-of-bed'
long$outofbed[long$IMS<=3]<-'in-bed'
long$highmedlow <- 'low'
long$highmedlow[long$c_rate>0.05] <- 'moderate'
long$highmedlow[long$c_rate>0.2] <- 'high'
long$highmedlow<-factor(long$highmedlow, levels=c("low","moderate","high"))

for (i in unique(long$highmedlow)){
  print(i)
  tesss <- long[long$highmedlow == i]
  tab3 <- CreateTableOne(vars = c("outofbed"), strata = "adv", 
                         data = tesss, factorVars = c("outofbed"), test = T)
  
  outofb <- print(tab3, showAllLevels = TRUE,formatOptions = list(big.mark = ","))
  outofb
}
tab3 <- CreateTableOne(vars = c("outofbed"), strata = "adv", 
                       data = long, factorVars = c("outofbed"), test = T)

outofb <- print(tab3, showAllLevels = TRUE,formatOptions = list(big.mark = ","))




## Save to a CSV file
write.csv(tab3Mat, file = '/Users/maximilianlindholz/Desktop/Promotion:Arbeit/Paper Text/myTable.csv')
write.csv(tab4Mat, file = '/Users/maximilianlindholz/Desktop/Promotion:Arbeit/Paper Text/tab3.csv')
write.csv(outofb, file = '/Users/maximilianlindholz/Desktop/Promotion:Arbeit/Paper Text/outofb.csv')
