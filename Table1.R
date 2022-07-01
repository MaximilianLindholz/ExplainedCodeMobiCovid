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
tab1 <- CreateTableOne(vars = c("age","Elixhauser","admissionapache", "admissionsofa", "meanAbsoluteRass","geschlecht", "obes","dialyse", "ecmo", "highflow", 
                                "intubated", "maskedventilation", "tracheostomie","Fachrichtung", "covid"), strata = "norepinephrine", 
                       data = data, factorVars = c("geschlecht", "obes","dialyse", "ecmo", "highflow", 
                                                   "intubated", "maskedventilation", "tracheostomie","prone","Fachrichtung"))
print(tab1, showAllLevels = TRUE)

tab3Mat <- print(tab1,showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

## Save to a CSV file
write.csv(tab3Mat, file = '/Users/maximilianlindholz/Desktop/Promotion:Arbeit/Paper Text/myTable.csv')
