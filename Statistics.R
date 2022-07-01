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
data <- data %>% select('c_pseudonym','geschlecht','age','obes','admissionapache','admissionsofa','Elixhauser','dialyse','ecmo','highflow','intubated','maskedventilation','tracheostomie','medianAbsoluteRass','Fachrichtung','covid','norepinephrine','perday','averagelengthofPT', 'frühmobi','tod','Behandlungsdauer')
iv <- fread('/Users/maximilianlindholz/Final/norepiclean.csv')

onlypos <- subset(data, data$perday>0)

# table
tablemeans <- aggregate(data[, c("perday", "frühmobi", "tod", "Behandlungsdauer")], list(data$norepinephrine), mean)
tablemeans <- cbind(tablemeans, (aggregate(data[, c("averagelengthofPT")], list(data$norepinephrine), mean, na.rm=T)))
tablemeans <- tablemeans[,c(1,2,3,4,5,7)]
write.csv(tablemeans, file = "means.csv")

#qc/classes
summary(data)
str(data)
data$Fachrichtung<-as.factor(data$Fachrichtung)
data$geschlecht<- as.factor(data$geschlecht)

# merge long and wide
long <- merge(long, data, by = "c_pseudonym")

# perday all
data$perday <- sqrt(data$perday)
pdcov <- perday~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+medianAbsoluteRass+Fachrichtung+Behandlungsdauer+norepinephrine
model1 <- lm(pdcov, data = data)
summary(model1)
Anova(model1, type = 3)
confint(model1)

# modelfit
plot(model1)

# perday onlypos
onlypos <- subset(data, data$perday>0)
onlypos$perday <- sqrt(onlypos$perday)
model1_2 <- lm(pdcov, data = onlypos)
summary(model1_2)
Anova(model1_2, type = 3)
confint(model1_2)


# Average length of physical therapy
cov2 <- averagelengthofPT~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+medianAbsoluteRass+Fachrichtung+covid+norepinephrine
model2 <- lm(cov2, data = data)
summary(model2)
Anova(model2, type = 3)

# test model
plot(model2)

# early mobilization yes/no
earlymob_model <- glm(frühmobi~ geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+medianAbsoluteRass+Fachrichtung+covid+norepinephrine, data = data, family = 'binomial')
summary(earlymob_model)
Anova(earlymob_model, type = 3)
exp(earlymob_model$coefficients)
exp(confint(earlymob_model))

# In vs. Out of bed
long$outofbed <-0
long$outofbed[long$IMS>3]<-1
long$outofbed<-as.factor(long$outofbed)

plotting <- long

# rescaling 
long$c_rate<- scale(long$c_rate)
long$RASS<- scale(long$RASS)
long$age<- scale(long$age)
long$admissionapache<- scale(long$admissionapache)
long$admissionsofa<- scale(long$admissionsofa)
long$Elixhauser<- scale(long$Elixhauser)
long$c_rate<- scale(long$c_rate)

intensitymodel <- glmer(outofbed~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+RASS+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+Fachrichtung+covid+c_rate+(1|c_pseudonym), data = long, family = "binomial")
summary(intensitymodel)



# # show boxplot in vs out of bed
ggplot(plotting, aes(x = IMS , y = c_rate, group = IMS, col = outofbed)) +
  geom_boxplot()+
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

