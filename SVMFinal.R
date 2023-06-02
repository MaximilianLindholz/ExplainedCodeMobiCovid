rm(list = ls());gc()
library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textmodels)
library(caret)

# paths need to be adapted to local paths

# physio vorbereitet
trainingsdata <- read.csv2('ScoresTrain1.csv')
trainingsdata <- trainingsdata %>% filter_all(all_vars(!is.na(.)))
#unique id
trainingsdata$number <- c(1:1235)

# create Corpus = geordnete Sammlung aller Texte
physio_corpus <- corpus(trainingsdata, docid_field = "number", text_field = "Text")
docvars(physio_corpus, "physio_id") <- docid(physio_corpus)

# Tokens (Aufspaltung in Wörter)
physio_tokens <- tokens(physio_corpus) %>% 
  tokens_tolower() 

# DFM - Dokument Feature Matrix
physio_dfm <- dfm(physio_tokens)

# Split in test and training data, seed for reproducability, choosen randomly
set.seed(7)
train_ids <- slice_sample(trainingsdata, prop = .8) %>% 
  pull(number)

train_dfm <- dfm_subset(physio_dfm, physio_id %in% train_ids)
test_dfm <- dfm_subset(physio_dfm, !physio_id %in% train_ids)

# SVM Classifier
physio_svm <- textmodel_svm(x = train_dfm, y=train_dfm$IMS)

# Modell testen
predicted_IMS <- predict(physio_svm, newdata = test_dfm)
head(predicted_IMS)

# vergleichen mit der tatsächlichen Verteilung
true_IMS <- test_dfm$IMS
table(true_IMS)

# confusion Matrix
conf_tab <- table(predicted_IMS, true_IMS)
conf_tab

#Model-Güte
caret::confusionMatrix(conf_tab)
statsss<- caret::confusionMatrix(conf_tab)

# 'Accuracy : 0.91        
#                  95% CI : (0.87, 0.94)
#     No Information Rate : 0.3603         
#     P-Value [Acc > NIR] : < 2.2e-16 '
#     Kappa : 0.89

# get all physio
# cobra 61 // 20386
physiotexte2 <- read.csv('CO6_Data_String.csv', sep = '|')
physiotexte2 <- subset(physiotexte2, physiotexte2$c_var_id == 108464)
physiotexte2 <- physiotexte2 %>% select('co6_patient_id', 'c_date_time_to', 'c_val')

# cobra 62 // 58502
physiotexte <- read.csv('CO6_Data_String-PhysioTexte_ValidatedNullRemoved.csv',header = T, sep = ';')
physiotexte <- physiotexte %>% select('co6_patient_id', 'c_date_time_to', 'c_val')

# cobra5 // 79384
physiotexte5 <- read.csv('PhysiodatenCobra5.csv',header = T, sep = '|')
physiotexte5 <- physiotexte5 %>% select('c_dat_id', 'c_datum_fuer_wann', 'c_vstring')

physiotexte$c_dat_id <- NA
physiotexte2$c_dat_id <- NA
physiotexte5$co6_patient_id <- NA
names(physiotexte5)[2]<- 'c_date_time_to'
names(physiotexte5)[3]<- 'c_val'

physiotexte <- rbind(physiotexte, physiotexte2)
physiotexte <- rbind(physiotexte, physiotexte5)

# labeling co6
physiotexte$number <- c(1:158272)
physiotexte$IMS <- c(rep(20, 158272))

# create Corpus = geordnete Sammlung aller Texte
physiotexte_corpus <- corpus(physiotexte, docid_field = "number", text_field = "c_val")
docvars(physiotexte_corpus, "id") <- docid(physiotexte_corpus)

# Tokens (Aufspaltung in Wörter)
physiotexte_corpus
physiotexte_tokens <- tokens(physiotexte_corpus) %>% 
  tokens_tolower() 

new_dfm2 <- dfm(physiotexte_tokens)
new_dfm2_matched <- dfm_match(new_dfm2, features = featnames(train_dfm))
new_dfm2_matched

newdata.pred = predict(physio_svm, newdata = new_dfm2_matched)
# hier irgendwie nicht mehr relevant anders als bei bayes:
# predictedIMS <- as.numeric(levels(newdata.pred)[as.integer(newdata.pred)])

physiotexte$IMS <- newdata.pred

physiotexte <- subset(physiotexte, physiotexte$IMS != '-1')
physiotexte <- subset(physiotexte, physiotexte$IMS != '-2')

# einheitliches datum
physiostrich <- subset(physiotexte, grepl('-', as.character(physiotexte$c_date_time_to)))
physiopunkt <- subset(physiotexte, !grepl('-', as.character(physiotexte$c_date_time_to)))
physiostrich$c_date_time_to <- as.POSIXct(physiostrich$c_date_time_to)
physiopunkt <- subset(physiopunkt, physiopunkt$c_date_time_to != "")
physiopunkt$c_date_time_to <- chartr('.', '-', physiopunkt$c_date_time_to)
physiopunkt$c_date_time_to<- as.POSIXct(physiopunkt$c_date_time_to, format = "%d-%m-%y %H:%M")
physiotexte <- rbind(physiostrich, physiopunkt)

# manual cleanup steps after scoping
# removing values that were wrongly labeled manually (Cases, where it was not physiotherapy but was included in the same variable,
# importantly this wont affect the distribution of the regular IMS scores, because it only removes scores where no pt was done and those cases were not included in the
# quality assessment step with caret from above e.g. because of faulty entry in HIS) //
full <- physiotexte

# filter operationen
full <- subset(full, IMS != 7 & IMS<11 & !(c_val %in% c("", "-")) & IMS != "")

# to lowercase
full$c_val <- tolower(full$c_val)

# exclude words 
exclude_words <- c("weaning", "verlegt", " op ", "op ", " op", "psycho", "krisen", 
                   "edk", "intubation", "cough", "beatmungseinstellung", "ecmo", 
                   "keine", "extubation", "screening", "abgesagt", 
                   "angehörigen kommen", "lehnt", "abgelehnt", "kaloriemetrie", 
                   "Sprechaufsatz", "sbt")
full <- full[!sapply(full$c_val, function(x) any(grepl(x, exclude_words))),]

# extract denial of therapy
abgelehnt_words <- c("abgelehnt", "lehnt")
abgelehnt <- full[sapply(full$c_val, function(x) any(grepl(x, abgelehnt_words))),]




write.table(full, 'physiotextelabeled.csv', sep = "|",row.names = FALSE, quote = TRUE)






