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
                                "intubated", "maskedventilation", "tracheostomie","Fachrichtung", "covid"), strata = "norepinephrine", 
                       data = data, factorVars = c("geschlecht", "obes","dialyse", "ecmo", "highflow", 
                                                   "intubated", "maskedventilation", "tracheostomie","prone","Fachrichtung"))
print(tab1, showAllLevels = TRUE)

tab3Mat <- print(tab1,showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

# table 2
tab2 <- CreateTableOne(vars = c("perday", "frühmobi","averagelengthofPT", "tod",  'HospLOS'), strata = "norepinephrine", 
                       data = data, factorVars = c("tod","frühmobi"), test = F)

print(tab2, showAllLevels = TRUE,formatOptions = list(big.mark = ","))

tab4Mat <- print(tab2,showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = TRUE)

# rates
long <- fread('/Users/maximilianlindholz/Final/during.csv')
long$outofbed[long$IMS>3]<-1
long$outofbed[long$IMS<=3]<-0
tab3 <- CreateTableOne(vars = c("c_rate"), strata = "outofbed", 
                       data = long, factorVars = c("outofbed"), test = F)

outofb <- print(tab3, showAllLevels = TRUE,formatOptions = list(big.mark = ","))



## Save to a CSV file
write.csv(tab3Mat, file = '/Users/maximilianlindholz/Desktop/Promotion:Arbeit/Paper Text/myTable.csv')
write.csv(tab4Mat, file = '/Users/maximilianlindholz/Desktop/Promotion:Arbeit/Paper Text/tab3.csv')
write.csv(outofb, file = '/Users/maximilianlindholz/Desktop/Promotion:Arbeit/Paper Text/outofb.csv')
