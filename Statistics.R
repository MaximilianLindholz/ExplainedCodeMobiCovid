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
iv <- subset(iv, iv$length >10 & iv$c_rate <1 )

#qc/classes
summary(data)
str(data)
# data$Fachrichtung<-as.factor(data$Fachrichtung)
# data$geschlecht<- as.factor(data$geschlecht)
# data$obes <- as.factor(data$obes)
# data$dialyse <- as.factor(data$dialyse)
# data$ecmo <- as.factor(data$ecmo)
# data$highflow <- as.factor(data$highflow)
# data$intubated <- as.factor(data$intubated)
# data$maskedventilation <- as.factor(data$maskedventilation)
# data$tracheostomie <- as.factor(data$tracheostomie)
# data$covid <- as.factor(data$covid)

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
data$Behandlungsdauer <- log(data$Behandlungsdauer)
losmodel <- lm(Behandlungsdauer ~geschlecht+age+Elixhauser+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+medianAbsoluteRass+Fachrichtung+covid+norepinephrine, data = data)
summary(losmodel)
Anova(losmodel, type = 3)
# plot(losmodel)

# given that is log transformed => re transform to percentage change, number is from summary
value <- (exp(losmodel$coefficients['norepinephrine'])-1)*100
retransform <- function(x){
  a <- (exp(x)-1)*100
  return(a)
}
exp(1)^confint(losmodel)
# 100 × (𝑒𝛽1 − 1)

# 5 In vs. Out of bed
long$outofbed <-0
long$outofbed[long$IMS>3]<-1
long$outofbed<-as.factor(long$outofbed)

plotting <- long

intensitymodel <- glmer(outofbed~geschlecht+age+obes+admissionapache+admissionsofa+Elixhauser+RASS+dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+Fachrichtung+covid+c_rate+(1|c_pseudonym), data = long, family = "binomial",glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
summary(intensitymodel)
Anova(intensitymodel)
exp(-3.693469)
exp(confint(intensitymodel,parm="c_rate",method="Wald"))


# # show boxplot in vs out of bed
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
norepimob <- subset(data, data$norepinephrine==1)
norepimob$mit <-0
norepimob$mit[norepimob$c_pseudonym %in% mobimit]<-1

mortmodel2 <- glm(tod ~ geschlecht+age+Elixhauser+ dialyse+ecmo+highflow+intubated+maskedventilation+tracheostomie+admissionapache+admissionsofa+medianAbsoluteRass+Fachrichtung+covid+mit, data = norepimob, family = 'binomial')
summary(mortmodel2)
Anova(mortmodel2, type = 3)
exp(mortmodel2$coefficients)
exp(confint(mortmodel2))



# plotting part
library(sjPlot)
library(sjlabelled)
library(sjmisc)


# dichotom outcomes
plot_models(intensitymodel, earlymob_model, mortmodel, p.shape=T)


# per day
p2 <- plot_models(model1, grid = TRUE, show.p=TRUE, 
            p.shape=T)
# los model
p3 <- plot_models(losmodel, show.p=TRUE, grid = TRUE, p.shape=T, transform = "retransform", axis.title = "Percent increase/decrease) for every one-unit increase (Log-transformed data)")
# proportional change => e.g. 1.4 times as long as non norepinephrine recieving patients

library(gridExtra)
grid.arrange(p1,p11,p111,p2,p3, nrow = 2)




