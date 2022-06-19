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

#qc/classes
summary(data)
str(data)
data$geschlecht<-as.factor(data$geschlecht)
data$Fachrichtung<-as.factor(data$Fachrichtung)
str(data)

# merge patient data onto mobilization data
long <- merge(long, data, by = 'c_pseudonym')

# widecalc
# lm perday
pdcov <- perday~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+prone+meanAbsoluteRass+Fachrichtung+norepinephrine
model1 <- lm(pdcov, data = data)
summary(model1)
Anova(model1, type = 3)

# modelfit
plot(model1)

# lm average length
cov2 <- averagelengthofPT~geschlecht+age+obes+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+prone+admissionapache+admissionsofa+meanAbsoluteRass+Fachrichtung+norepinephrine
model2 <- lm(cov2, data = data)
summary(model2)
Anova(model2, type = 3)

# test model
plot(model2)

# long calc in vs out of bed
long$outofbed <-"no"
long$outofbed[long$IMS>3]<-"yes"
long$outofbed <- as.factor(long$outofbed)

# Calc Long Model 
imsmodel <- glmer(outofbed ~geschlecht+age+obes+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+prone+admissionapache+admissionsofa+Fachrichtung+RASS+c_rate+(1|c_pseudonym), data = long, family = 'binomial')
summary(imsmodel)
Anova(imsmodel, type = 3)
exp(fixef(imsmodel))

# show boxplot in vs out of bed
ggplot(long, aes(x = outofbed , y = c_rate, group = outofbed, col = outofbed)) +
  geom_boxplot()+
  scale_x_discrete(labels = NULL, breaks = NULL) +
  xlab("In-bed vs. Out-of-bed")+
  ylab("Norepinephrine rate in mcg/kg/min")+
  scale_color_manual(name="Type", labels=c("In bed","Out of bed"), values=c("red","blue"))+
  theme_classic()

# descriptive statistics
a <- long %>%                               # Summary by group using dplyr
  group_by(outofbed) %>% 
  summarize(min = min(c_rate),
            q1 = quantile(c_rate, 0),
            median = median(c_rate),
            mean = mean(c_rate),
            q3 = quantile(c_rate, 0.95),
            max = max(c_rate))

library(gt)
gt(a)

# mortality
mobimit <- unique(long$c_pseudonym)
data$mortmit <- 0
data$mortmit[data$c_pseudonym%in% mobimit]<-1
katecholgroup <- subset(data, data$norepinephrine==1)
mortmodel <- glm(tod ~geschlecht+age+Elixhauser+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+prone+admissionapache+admissionsofa+meanAbsoluteRass+Fachrichtung+mortmit, data = katecholgroup, family = 'binomial')
summary(mortmodel)
Anova(mortmodel, type = 3)
exp((mortmodel$coefficients))

#los
katecholgroup$Behandlungsdauer<-sqrt(katecholgroup$Behandlungsdauer)
losmodel <- lm(Behandlungsdauer ~geschlecht+age+Elixhauser+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+prone+admissionapache+admissionsofa+meanAbsoluteRass+Fachrichtung+mortmit, data = katecholgroup)
summary(losmodel)
Anova(losmodel, type = 3)
plot(losmodel)



