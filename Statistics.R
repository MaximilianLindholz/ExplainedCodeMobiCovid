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

# 4 Hospital los
losmodel <- lm(HospLOS ~geschlecht+age+Elixhauser+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+medianAbsoluteRass+Fachrichtung+covid+norepinephrine, data = data)
summary(losmodel)
Anova(losmodel, type = 3)
confint(losmodel)
# plot(losmodel)

# backup to plot 
plotting <- long

# 5 Intensity
long$outofbed <-0
long$outofbed[long$IMS>3]<-1
long$outofbed<-as.factor(long$outofbed)
intensitymodel <- glmer(outofbed~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+RASS+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+Fachrichtung+covid+c_rate+(1|c_pseudonym), data = long, family = "binomial",glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
summary(intensitymodel)
Anova(intensitymodel)
exp(-3.693469)
exp(confint(intensitymodel,parm="c_rate",method="Wald"))

# show histogram in vs out of bed
plotting$outofbed <- as.character(plotting$outofbed)
plotting$outofbed[plotting$outofbed==0]<- 'In bed'
plotting$outofbed[plotting$outofbed==1]<- 'Out of bed'
ggplot(plotting, aes(x = c_rate , group = outofbed, fill = outofbed)) +
  geom_histogram()+
  labs(fill='Type of mobilization') +
  xlab('Norepinephrine rate in mcg/kg/min')+
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

# in bed
inbed <- subset(long, long$outofbed==0)
length(inbed$c_rate[inbed$c_rate>0.2])
# 508
# out of bed
outofbed <- subset(long, long$outofbed==1)
length(outofbed$c_rate[outofbed$c_rate>0.2])
# 17

# 6 mortality
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
# frequency
pd2 <- lm(perday~norepinephrine, data = data)
summary(pd2)
confint(pd2)

# mortality
M2 <- matrix(c(3189,5277,1009,1182), nrow = 2)
rownames(M2) <- c("Norepinephrine", "No Norepinephrine")
colnames(M2) <- c("Kein Tod", "Tod")
oddsRatio(M2, conf.level = 0.95, verbose = T)

# early mobilization
M3 <- matrix(c(2539,3738,1659,2721), nrow = 2)
rownames(M3) <- c("Norepinephrine", "No Norepinephrine")
colnames(M3) <- c("Keine Frühmobi", "Frühmobi")
(oddsRatio(M3, conf.level = 0.95, verbose = T))

# hospital los
losmodel2<-lm(HospLOS~norepinephrine, data = data)
summary(losmodel2)
confint(losmodel2)

# mean dose per group per lmer
pergr <- lmer(c_rate~outofbed+(1|c_pseudonym), data = long)
summary(pergr)
confint(pergr)

# 95 percent interval
# out of bed
a <- subset(long, long$outofbed==1)
quantile(tapply(a$c_rate, a$c_pseudonym, mean), probs=c(0.95))

# in bed
b <- subset(long, long$outofbed==0)
quantile(tapply(b$c_rate, b$c_pseudonym, mean), probs=c(0.95))


# groups chi square
mydata <- cbind(c(508, 17), c(2428, 311))
mobi=c("in-bed", "out-of-bed")
dose=c("highepinephrine", "lowepinephrine")

# get chisq
M <- as.table(mydata)
dimnames(M) <- list(mobi=mobi, dose=dose)
Xsq <- chisq.test(M)
Xsq

#Xsq.sim <- chisq.test(M, simulate.p.value=T, B = 100000000); Xsq.sim
MS <- as.data.frame(long)
rownames(MS)=mobi
colnames(MS)=dose
MS$Summe <- MS$highepinephrine + MS$lowepinephrine
MS$highperc <- round(100*MS$high/MS$Summe, digits=1)
MS$lowperc <- round(100*MS$low/MS$Summe, digits=1)
MS <- rbind(MS, c(NA, NA, sum(MS$Summe), NA, NA))
rownames(MS)[3] <- "Summe"

MS$highepinephrine[c(1,2)] <- paste0(MS$highepinephrine[c(1,2)], " (", MS$highperc[c(1,2)], " %)")
MS$lowepinephrine[c(1,2)] <- paste0(MS$lowepinephrine[c(1,2)], " (", MS$lowperc[c(1,2)], " %)")
MS$Summe[c(1,2)] <- paste0(MS$Summe[c(1,2)], " (100 %)")
MS$P <- c(NA, NA, signif(Xsq$p.value, 2))
write.table(MS[,c(1:3,6)], "chis2table.txt", row.names=T, col.names=T, sep="\t", quote=F, na="")

