rm(list = ls());gc()
library(data.table)
library(tidyverse)
library(lme4)
library(car)
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))

# mobilization with norepinephrine
long <- fread('/Users/maximilianlindholz/Final/during.csv')

# general patient data
data <- fread('/Users/maximilianlindholz/Final/baseinfo5.csv')
iv <- fread('/Users/maximilianlindholz/Final/norepiclean.csv')
#qc/classes
summary(data)
str(data)
data$geschlecht<-as.factor(data$geschlecht)
data$c_pseudonym <- as.factor(data$c_pseudonym)
data$obes<-as.factor(data$obes)
data$dialyse<-as.factor(data$dialyse)
data$ecmo<-as.factor(data$ecmo)
data$highflow<-as.factor(data$highflow)
data$intubated<-as.factor(data$intubated)
data$maskedventilation<-as.factor(data$maskedventilation)
data$tracheostomie<-as.factor(data$tracheostomie)
data$norepinephrine<-as.factor(data$norepinephrine)
data$Fachrichtung<-as.factor(data$Fachrichtung)
str(data)

# merge long and wide
long <- merge(long, data, by = "c_pseudonym")

# perday
data$perday<- sqrt(data$perday)

pdcov <- perday~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+meanAbsoluteRass+Fachrichtung+norepinephrine
model1 <- lm(pdcov, data = data)
summary(model1)
Anova(model1, type = 3)
confint(model1)

# modelfit
plot(model1)

# Average length of physical therapy
cov2 <- averagelengthofPT~geschlecht+age+obes+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+meanAbsoluteRass+Fachrichtung+norepinephrine
model2 <- lm(cov2, data = data)
summary(model2)
Anova(model2, type = 3)

# test model
plot(model2)

# early mobilization yes/no
earlymob_model <- glm(frühmobi~ geschlecht+age+obes+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+Fachrichtung+meanAbsoluteRass+norepinephrine, data = data, family = 'binomial')
summary(earlymob_model)
Anova(earlymob_model, type = 3)
exp(earlymob_model$coefficients)
exp(confint(earlymob_model))

# table
tablemeans <- aggregate(data[, c("perday", "frühmobi", "tod", "Behandlungsdauer")], list(data$norepinephrine), mean)
tablemeans <- cbind(tablemeans, (aggregate(data[, c("averagelengthofPT")], list(data$norepinephrine), mean, na.rm=T)))
tablemeans <- tablemeans[,c(1,2,3,4,5,7)]
write.csv(tablemeans, file = "means.csv")

# inbed vs out of bed
long$outofbed <-0
long$outofbed[long$IMS >3]<-1
tableout <- aggregate(long[, c("c_rate")], list(long$outofbed), mean)
write.csv(tableout, file = "inbed.csv")

# Calc in vs out of bed (Intensity)
long$outofbed <-0
long$outofbed[long$IMS>3]<-1
intensity_model <- glm(outofbed~RASS+geschlecht+age+obes+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+Fachrichtung+c_rate, data = long, family = "binomial")
summary(intensity_model)




# # show boxplot in vs out of bed
ggplot(long, aes(x = outofbed , y = c_rate, group = outofbed, col = outofbed)) +
  geom_boxplot(notch = TRUE)+
  scale_x_discrete(labels = NULL, breaks = NULL) +
  xlab("In-bed vs. Out-of-bed")+
  ylab("Norepinephrine rate in mcg/kg/min")+
  scale_color_manual(name="Type", labels=c("In bed","Out of bed"), values=c("red","blue"))+
  theme_classic()

# descriptive statistics
a <- long %>%                               # Summary by group using dplyr
  group_by(outofbed) %>% 
  summarize(min = min(c_rate),
            q1 = quantile(c_rate, 0.25),
            median = median(c_rate),
            mean = mean(c_rate),
            q3 = quantile(c_rate, 0.75),
            max = max(c_rate))

write.csv(a, file = "descr.csv")

# mortality
mobimit <- unique(long$c_pseudonym)
data$mortmit <- 0
data$mortmit[data$c_pseudonym%in% mobimit]<-1
katecholgroup <- subset(data, data$norepinephrine==1)
mortmodel <- glm(tod ~geschlecht+age+Elixhauser+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+meanAbsoluteRass+Fachrichtung+mortmit, data = data, family = 'binomial')
summary(mortmodel)
Anova(mortmodel, type = 3)
exp((mortmodel$coefficients))

#los
data$Behandlungsdauer<-sqrt(data$Behandlungsdauer)
losmodel <- lm(Behandlungsdauer ~geschlecht+age+Elixhauser+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+meanAbsoluteRass+Fachrichtung+mortmit, data = data)
summary(losmodel)
Anova(losmodel, type = 3)
plot(losmodel)

