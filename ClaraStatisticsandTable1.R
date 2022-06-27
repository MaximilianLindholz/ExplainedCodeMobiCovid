rm(list = ls());gc()
library(tableone)
library(data.table)

# general patient data
data <- fread('/Users/maximilianlindholz/Final/baseinfo5.csv')

myVars <- c("age","Elixhauser","admissionapache", "admissionsofa", "meanAbsoluteRass")

## Vector of categorical variables that need transformation
catVars <- c( "obes","dialyse", "ecmo", "highflow", 
              "intubated", "maskedventilation", "tracheostomie","prone","norepinephrine","covid")

data <- data.frame(data)
data[,catVars] <- sapply(data[,catVars],function(x) ifelse(x==1,"yes","no"))

# mulit means patient was on multiple wards with different attending specialties
data$Fachrichtung[data$Fachrichtung=="multi"]<-"Multiple Wards and Specialties"
## Create a TableOne object
tab1 <- CreateTableOne(vars = c("age","Elixhauser","admissionapache", "admissionsofa", "meanAbsoluteRass","geschlecht", "obes","dialyse", "ecmo", "highflow", 
                                "intubated", "maskedventilation", "tracheostomie","prone", "norepinephrine","Fachrichtung"), strata = "covid", 
                       data = data, factorVars = c("geschlecht", "obes","dialyse", "ecmo", "highflow", 
                                                   "intubated", "maskedventilation", "tracheostomie","prone","Fachrichtung"))
print(tab1, showAllLevels = TRUE)

tab3Mat <- print(tab1,showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab3Mat, file = "clara1.csv")

# statistics
library(car)
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))

# widecalc
pdcov <- perday~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+prone+meanAbsoluteRass+Fachrichtung+norepinephrine+covid
model1 <- lm(pdcov, data = data)
summary(model1)
Anova(model1, type = 3)

# modelfit
plot(model1)

# lm average length
cov2 <- averagelengthofPT~geschlecht+age+obes+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+prone+admissionapache+admissionsofa+meanAbsoluteRass+Fachrichtung+norepinephrine+covid
model2 <- lm(cov2, data = data)
summary(model2)
Anova(model2, type = 3)

