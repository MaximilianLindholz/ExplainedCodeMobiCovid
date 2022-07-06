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
data <- data %>% select('c_pseudonym','geschlecht','age','obes','admissionapache','admissionsofa','Elixhauser','dialyse','ecmo','highflow','intubated','maskedventilation','tracheostomie','medianAbsoluteRass','Fachrichtung','covid','norepinephrine','perday','averagelengthofPT', 'frühmobi','tod','Behandlungsdauer', 'HospLOS')
iv <- fread('/Users/maximilianlindholz/Final/norepiclean.csv')
iv <- subset(iv, iv$length >10 & iv$c_rate <1 )

#qc/classes
summary(data)
str(data)
data$Fachrichtung<-as.factor(data$Fachrichtung)
data$geschlecht<- as.factor(data$geschlecht)
data$obes <- as.factor(data$obes)
data$dialyse <- as.factor(data$dialyse)
data$ecmo <- as.factor(data$ecmo)
data$highflow <- as.factor(data$highflow)
data$intubated <- as.factor(data$intubated)
data$maskedventilation <- as.factor(data$maskedventilation)
data$tracheostomie <- as.factor(data$tracheostomie)

# merge long and wide
long <- merge(long, data, by = "c_pseudonym")

# 1 perday all
# data$perday <- sqrt(data$perday)
pdcov <- perday~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+medianAbsoluteRass+Fachrichtung+Behandlungsdauer+norepinephrine
model1 <- lm(pdcov, data = data)
summary(model1)
Anova(model1, type = 3)
confint(model1)
# modelfit
# plot(model1)

# 2 Average length of physical therapy
cov2 <- averagelengthofPT~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+medianAbsoluteRass+Fachrichtung+covid+norepinephrine
model2 <- lm(cov2, data = data)
summary(model2)
Anova(model2, type = 3)
# test model
#plot(model2)

# 3 early mobilization yes/no
earlymob_model <- glm(frühmobi~ geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+medianAbsoluteRass+Fachrichtung+covid+norepinephrine, data = data, family = 'binomial')
summary(earlymob_model)
Anova(earlymob_model, type = 3)
exp(earlymob_model$coefficients)
exp(confint(earlymob_model))

# 4 In vs. Out of bed
long$outofbed <-0
long$outofbed[long$IMS>3]<-1
long$outofbed<-as.factor(long$outofbed)

plotting <- long

# rescaling 
#long$c_rate<- long$c_rate
long$RASS<- scale(long$RASS)
long$age<- scale(long$age)
long$admissionapache<- scale(long$admissionapache)
long$admissionsofa<- scale(long$admissionsofa)
long$Elixhauser<- scale(long$Elixhauser)
long$c_rate<- scale(long$c_rate)
intensitymodel <- glmer(outofbed~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+RASS+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+Fachrichtung+covid+c_rate+(1|c_pseudonym), data = long, family = "binomial")
summary(intensitymodel)
Anova(intensitymodel)

# # show boxplot in vs out of bed
ggplot(plotting, aes(x = outofbed , y = c_rate, group = outofbed, col = outofbed)) +
  geom_boxplot(notch = T)+
  theme_classic()

# descriptive statistics
a <- plotting %>%                               # Summary by group using dplyr
  group_by(outofbed) %>% 
  summarize(min = min(c_rate),
            fifth = quantile(c_rate, 0.5),
            median = median(c_rate),
            mean = mean(c_rate),
            ninefive = quantile(c_rate, 0.95),
            max = max(c_rate))
write.csv(a, file = "descr.csv")


over <- subset(long, long$c_rate<0.2&long$outofbed==1)
overgroup <- unique(over$c_pseudonym)


# safety 1 mortality
mortmit <- unique(long$c_pseudonym)
data$mortmit<-0
data$mortmit[data$c_pseudonym%in% mortmit]<-1
mortmodel2 <- glm(tod ~ geschlecht+age+Elixhauser+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+medianAbsoluteRass+Fachrichtung+mortmit, data = data[data$norepinephrine==1], family = 'binomial')
summary(mortmodel2)
Anova(mortmodel2, type = 3)
exp(mortmodel2$coefficients)
exp(confint(mortmodel2))

# safety 2 Hospital los
data$Behandlungsdauer<-sqrt(data$Behandlungsdauer)
losmodel <- lm(Behandlungsdauer ~geschlecht+age+Elixhauser+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+medianAbsoluteRass+Fachrichtung+mortmit, data = data[data$norepinephrine==1])
summary(losmodel)
Anova(losmodel, type = 3)
plot(losmodel)

# density plot
#Plot.
outofbed <- data.table(plotting$c_rate[plotting$outofbed==1])
outofbed$type <- 'Out of bed'

rates <- data.table(iv$c_rate)
rates$type <- "General population with norepinephrine"
names(rates)[1]<-'rate'

ratespt <- data.table(plotting$c_rate[plotting$outofbed==0])
ratespt$type <- "In bed"

rate <- rbind(outofbed, ratespt)
names(rate)[1]<-'rate'
rate <- rbind(rate, rates)

ggplot(rate, aes(x = rate, fill = type))+
  geom_density(alpha = 0.5)+
  xlab("Norepinephrine rate in mcg/kg/min")+
  theme_classic()

# 6 mortality
mortmodel2 <- glm(tod ~ geschlecht+age+Elixhauser+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+medianAbsoluteRass+Fachrichtung+covid+norepinephrine, data = data, family = 'binomial')
summary(mortmodel2)
Anova(mortmodel2, type = 3)
exp(mortmodel2$coefficients)
exp(confint(mortmodel2))

# 5 Hospital los
data$Behandlungsdauer<-sqrt(data$Behandlungsdauer)
losmodel2 <- lm(Behandlungsdauer ~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+medianAbsoluteRass+Fachrichtung+covid+norepinephrine, data = data)
summary(losmodel2)
Anova(losmodel2, type = 3)
plot(losmodel2)


