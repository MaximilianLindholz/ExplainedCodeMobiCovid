rm(list = ls());gc()
library(data.table)
library(tidyverse)
library(lme4)
library(car)

options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
# path needs to be adapted to local structure

# mobilization with norepinephrine
long <- fread('during.csv')
adverse <- subset(long, long$adv ==1)

# general patient data
data <- fread('baseinfo5.csv')

#qc/classes
summary(data)
str(data)

# merge long and wide
long <- merge(long, data, by = "c_pseudonym")

options(scipen=999)
# 1 perday all
pdcov <- perday~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+medianAbsoluteRass+Fachrichtung+covid+norepinephrine
model1 <- lm(pdcov, data = data)
summary(model1)
Anova(model1, type = 3)
confint(model1)

# 2 early mobilization yes/no
earlymob_model <- glm(frühmobi~ geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+medianAbsoluteRass+Fachrichtung+covid+norepinephrine, data = data, family = 'binomial')
summary(earlymob_model)
Anova(earlymob_model, type = 3)
exp(earlymob_model$coefficients)
exp(confint(earlymob_model))

# 3 mortality
mortmodel <- glm(tod ~ geschlecht+age+Elixhauser+obes+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+medianAbsoluteRass+Fachrichtung+covid+norepinephrine, data = data, family = 'binomial')
summary(mortmodel)
Anova(mortmodel, type = 3)
exp(mortmodel$coefficients)
exp(confint(mortmodel))

# 4 Intensity
long$outofbed <-0
long$outofbed[long$IMS>3]<-1
long$outofbed<-as.factor(long$outofbed)

# plotting
plotting <- long

intensitymodel <- glmer(outofbed~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+RASS+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+Fachrichtung+covid+c_rate+(1|c_pseudonym), data = long, family = "binomial",glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
summary(intensitymodel)
Anova(intensitymodel)
exp(fixef(intensitymodel))
exp(confint(intensitymodel,parm="c_rate",method="Wald"))

# show histogram in vs out of bed
plotting$outofbed<-as.character(plotting$outofbed)
plotting$outofbed[plotting$outofbed==0]<- 'In bed'
plotting$outofbed[plotting$outofbed==1]<- 'Out of bed'
plotting$outofbed <- as.factor(plotting$outofbed)
ggplot(plotting, aes(x = c_rate , fill = outofbed)) +
  geom_histogram()+
  labs(fill='Type of mobilization') +
  xlab('Norepinephrine rate in mcg/kg/min')+
  theme_classic()

# 5 mortality mobi with
mobimit <- unique(long$c_pseudonym)
data$mobimit <-0
data$mobimit[data$c_pseudonym %in% mobimit]<-1

mortmodel2 <- glm(tod ~ geschlecht+age+Elixhauser+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+medianAbsoluteRass+Fachrichtung+covid+mobimit, data = data, family = 'binomial')
summary(mortmodel2)
Anova(mortmodel2, type = 3)
exp(mortmodel2$coefficients)
exp(confint(mortmodel2))

# unadjusted part
# Table 2 mit und ohne covariaten stupide anpassen mit lm / glm

# perday
pdcov_unadj <- perday~norepinephrine
model1_unadj <- lm(pdcov_unadj, data = data)
summary(model1_unadj)
Anova(model1_unadj, type = 3)
confint(model1_unadj)

# early mobilization
em_cov <- frühmobi~norepinephrine
model2_unadj <- glm(em_cov, data = data, family = "binomial")
exp(model2_unadj$coefficients)
exp(confint(model2_unadj))

# mortality
death_cov <- tod~norepinephrine
model3_unadj <- glm(death_cov, data = data, family = "binomial")
exp(model3_unadj$coefficients)
exp(confint(model3_unadj))

# 95 percent interval
# out of bed
a <- subset(long, long$outofbed==1)
quantile(tapply(a$c_rate, a$c_pseudonym, mean), probs=c(0.95))

# in bed
b <- subset(long, long$outofbed==0)
quantile(tapply(b$c_rate, b$c_pseudonym, mean), probs=c(0.95))

# descriptive part on sessions
full <- fread('pt.csv')
length(full$IMS[full$IMS>3])/length(full$IMS) # 0.25
long <- fread('during.csv')

# tabelle
long$outofbed <-0
long$outofbed[long$IMS>3]<-1
long$highmedlow <- 0
long$highmedlow[long$c_rate>0.05] <- 1
long$highmedlow[long$c_rate>0.2] <- 2

# adverse events - empirical p-value/Monte Carlo
ftable(long$highmedlow, long$outofbed, long$adv)
myout_orig <- mantelhaen.test(long$highmedlow, long$outofbed, long$adv)
emp_df <- data.frame("Statistic"= NA, "P"= NA, "Durchgang"= NA)
niter <- 10000
for(i in 1: niter){
  print(i)
  av <- sample(long$adv, size= nrow(long), replace=F)
  myout <- mantelhaen.test(long$highmedlow, long$outofbed, av)
  myout_use <- data.frame("Statistic"=myout$statistic, "P"=myout$p.value, "Durchgang"= i)
  emp_df <- rbind(emp_df, myout_use)
}
emp_df <- emp_df[-1,]
nrow(subset(emp_df, Statistic> myout_orig$statistic))/niter
nrow(subset(emp_df, P< myout_orig$p.value))/niter
# always >0.9

