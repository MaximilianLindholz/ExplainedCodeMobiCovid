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

# table
tablemeans <- aggregate(data[, c()], list(data$norepinephrine), mean)
tablemeans <- cbind(tablemeans, (aggregate(data[, c("averagelengthofPT")], list(data$norepinephrine), mean, na.rm=T)))
tablemeans <- tablemeans[,c(1,2,3,4,5,7)]
write.csv(tablemeans, file = "means.csv")

tab2 <- CreateTableOne(vars = c("perday", "frühmobi", "tod", "Behandlungsdauer", 'HospLOS'), strata = "norepinephrine", 
                       data = data, factorVars = c("tod"))
print(tab2, showAllLevels = TRUE)

tab4Mat <- print(tab2,showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)


## Save to a CSV file
write.csv(tab3Mat, file = '/Users/maximilianlindholz/Desktop/Promotion:Arbeit/Paper Text/myTable.csv')
write.csv(tab4Mat, file = '/Users/maximilianlindholz/Desktop/Promotion:Arbeit/Paper Text/tab3.csv')
