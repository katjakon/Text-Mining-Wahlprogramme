library(quanteda)
library(readtext)
library(tidyverse)
library(udpipe)

source("functions/featsinfo_to_column.R")
source("functions/roots_for_group.R")
source("functions/collect_sentiment_for_cat.R")

######################
### Sentiment data ###
######################

# prepare sentiment data
neg <- scan("data/SentiWS_v1.8c_Negative.txt", what = "char", sep = "\n", fileEncoding="utf-8")
pos <- scan("data/SentiWS_v1.8c_Positive.txt", what = "char", sep = "\n", fileEncoding="utf-8")
s <- str_split(neg, "\t")
t <- str_split(pos, "\t")
terms.neg <- sub("([A-Za-zß]+)[|][A-Za-zß]+", "\\1",lapply(s, function(l) l[[1]]))
terms.pos <- sub("([A-Za-zß]+)[|][A-Za-zß]+", "\\1",lapply(t, function(l) l[[1]]))

value.neg <- unlist(lapply(s, function(l) as.double(l[[2]])))
value.pos <- unlist(lapply(t, function(l) as.double(l[[2]])))

positive <- data.frame(term=terms.pos, value=value.pos)
negative <- data.frame(term=terms.neg, value=value.neg)

# add sentiment value

senti_dict <- rbind(positive, negative)

# OR: Load sentiment dictionary
load("RData/senti_dict.RData")

########################
### root information ###
########################

# annotate text (Warning: takes forever. like 30min)
annotated_model <- udpipe_annotate(udmodel_german, x = programs) %>%
  as.data.frame()
# Instead read in this:
load("RData/annotated_corpus.RData")

# partition of corpus
regierung <- c('doc3', 'doc4', 'doc10', 'doc11', 'doc12', 'doc19', 'doc23', 'doc24', 'doc25', 'doc27')
opposition <- c('doc1', 'doc2', 'doc5', 'doc6', 'doc7', 'doc8', 'doc9', 'doc13', 'doc14', 'doc15', 'doc16', 'doc17', 'doc18', 'doc20', 'doc21', 'doc22', 'doc26')

# split into sub groups
sub_model_regierung <- subset(annotated_model, doc_id %in% regierung)
sub_model_opposition <- subset(annotated_model, doc_id %in% opposition)

# list of words a party could use to talk about themselves
# first one is name of the party
regierung_list <- list('doc3' = c('bündnis90/die grünen', 'wir'),
                       'doc4' = c('bündnis90/die grünen', 'wir'),
                       'doc10' = c('cdu', 'wir'),
                       'doc11' = c('cdu', 'wir'),
                       'doc12' = c('cdu', 'wir'),
                       'doc19' = c('fdp', 'wir'),
                       'doc23' = c('spd', 'wir'),
                       'doc24' = c('spd', 'wir'),
                       'doc25' = c('spd', 'wir'),
                       'doc27' = c('spd', 'wir'))

opposition_list <- list('doc1' = c('afd', 'alternative', 'wir'),
                        'doc2' = c('afd', 'alternative', 'wir'),
                        'doc5' = c('bündnis90/die grünen', 'bündnis90', 'grüne', 'wir'),
                        'doc6' = c('bündnis90/die grünen', 'bündnis90', 'grüne', 'wir'),
                        'doc7' = c('bündnis90/die grünen', 'bündnis90', 'grüne', 'wir'),
                        'doc8' = c('cdu', 'wir'),
                        'doc9' = c('cdu', 'wir'),
                        'doc13' = c('die linke', 'linke', 'wir'),
                        'doc14' = c('die linke', 'linke', 'wir'),
                        'doc15' = c('die linke', 'linke', 'wir'),
                        'doc16' = c('fdp', 'wir'),
                        'doc17' = c('fdp', 'wir'),
                        'doc18' = c('fdp', 'wir'),
                        'doc20' = c('fdp', 'wir'),
                        'doc21' = c('pds', 'wir'),
                        'doc22' = c('linkspartei.pds', 'wir'),
                        'doc26' = c('spd', 'wir')) 



# use function on opposition subset of annotated model
opp_roots <- roots_for_group(sub_model_opposition, opposition_list, senti_dict)

# use function on government subset of annotated model
gov_roots <- roots_for_group(sub_model_regierung, regierung_list, senti_dict)

# optional: save to csv                                
write.csv2(opp_roots, 'data/opp_roots_info.csv', fileEncoding = "utf-8")
write.csv2(gov_roots, 'data/gov_roots_info.csv', fileEncoding = "utf-8")

# Self referential verbs
verbs.top.50 <- union(head(gov_roots, 50)$lemma, head(opp_roots, 50)$lemma)
freq.verbs <- data.frame(matrix(ncol=3, nrow=0))
colnames(freq.verbs) <- c("lemma", "freq_gov", "freq_opp")
sum.all.gov <- sum(gov_roots$freq)
sum.all.opp <- sum(opp_roots$freq)
for (i in 1:length(verbs.top.50)){
  curr.verb <- verbs.top.50[i]
  freq.gov <- sum(filter(gov_roots, lemma == curr.verb)$freq)/sum.all.gov
  freq.opp <- sum(filter(opp_roots, lemma == curr.verb)$freq)/sum.all.opp
  curr.row <- data.frame(lemma=curr.verb, freq_gov=freq.gov, freq_opp = freq.opp)
  freq.verbs <- rbind(freq.verbs, curr.row)
}
freq.verbs
gov.greater <- ifelse(freq.verbs$freq_gov >= freq.verbs$freq_opp, TRUE, FALSE)
freq.verbs$gov.greater <- gov.greater

# Plot results
ggplot(freq.verbs, aes(y = freq_gov, x = freq_opp, label =lemma, color = gov.greater))+
  geom_text(size = 5)+
  scale_x_continuous(trans = 'log10', breaks = c(0, 0.001, 0.010, 0.1)) +
  scale_y_continuous(trans = 'log10', breaks = c(0, 0.001, 0.010, 0.05))+
  scale_color_manual(values = c('TRUE' = 'darkblue', 'FALSE' = 'darkred'), guide = "none")+
  theme_minimal()+
  labs(x="Relative Frequenz in Oppositionsparteien",
       y="Relative Frequenz in Regierungsparteien",
       title="Verben in selbstreferentiellen Sätzen")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


##################################################
### extract sentiment based on value of column ###
##################################################

opp_nouns <- collect_sentiment_for_cat(sub_model_opposition, "upos", 'NOUN', senti_dict)
gov_nouns <- collect_sentiment_for_cat(sub_model_regierung, "upos", 'NOUN', senti_dict)
opp_adv <- collect_sentiment_for_cat(sub_model_opposition, "upos", 'ADJ', senti_dict)
gov_adv <- collect_sentiment_for_cat(sub_model_regierung, "upos", 'ADJ', senti_dict)

head(opp_nouns)
head(gov_nouns)
## TOP NOUNS OPPOS AND GOV
noun.top <- union(head(opp_nouns, 60)$lemma, head(gov_nouns, 40)$lemma)

sum.gov <- sum(gov_nouns$freq)
sum.opp <- sum(opp_nouns$freq)
freq.nouns <- data.frame(matrix(ncol=3, nrow=0))
colnames(freq.nouns) <- c("lemma", "freq_gov", "freq_opp")

for (i in 1:length(noun.top)){
  curr.noun <- noun.top[i]
  gov.freq <- sum(filter(gov_nouns, lemma == curr.noun)$freq)/sum.gov
  opp.freq <- sum(filter(opp_nouns, lemma == curr.noun)$freq)/sum.opp
  tmp.data <- data.frame(lemma=curr.noun, gov_freq = gov.freq, opp_freq = opp.freq)
  freq.nouns <- rbind(freq.nouns, tmp.data)
  
}
freq.nouns$gov.greater <- ifelse(freq.nouns$gov_freq >= freq.nouns$opp_freq, TRUE, FALSE)


ggplot(freq.nouns, aes(y = gov_freq, x = opp_freq, label =lemma, color = gov.greater))+
  geom_text(size = 5)+
  scale_x_continuous(trans = 'log10', breaks = c(0, 0.001, 0.010, 0.1)) +
  scale_y_continuous(trans = 'log10', breaks = c(0, 0.001, 0.010, 0.05))+
  scale_color_manual(values = c('TRUE' = 'darkblue', 'FALSE' = 'darkred'), guide = "none")+
  theme_minimal()+
  labs(x="Relative Frequenz in Oppositionsparteien",
       y="Relative Frequenz in Regierungsparteien",
       title="Häufige Nomen bei Oppositions- und Regierungsparteien")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


## OPP GOV ADJ
head(opp_adv)
head(gov_adv)

adj.top <- union(head(opp_adv, 50)$lemma, head(gov_adv, 50)$lemma)

sum.gov <- sum(gov_adv$freq)
sum.opp <- sum(opp_adv$freq)
freq.adj <- data.frame(matrix(ncol=3, nrow=0))
colnames(freq.nouns) <- c("lemma", "freq_gov", "freq_opp")

for (i in 1:length(adj.top)){
  curr.adj <- adj.top[i]
  gov.freq <- sum(filter(gov_adv, lemma == curr.adj)$freq)/sum.gov
  opp.freq <- sum(filter(opp_adv, lemma == curr.adj)$freq)/sum.opp
  tmp.data <- data.frame(lemma=curr.adj, gov_freq = gov.freq, opp_freq = opp.freq)
  freq.adj <- rbind(freq.adj, tmp.data)
  
}

freq.adj$gov.greater <- ifelse(freq.adj$gov_freq >= freq.adj$opp_freq, TRUE, FALSE)


ggplot(freq.adj, aes(y = gov_freq, x = opp_freq, label =lemma, color = gov.greater))+
  geom_text(size = 5)+
  scale_x_continuous(trans = 'log10', breaks = c(0, 0.005, 0.010, 0.020)) +
  scale_y_continuous(trans = 'log10', breaks = c(0, 0.020, 0.010, 0.005))+
  scale_color_manual(values = c('TRUE' = 'darkblue', 'FALSE' = 'darkred'), guide = "none")+
  theme_minimal()+
  labs(x="Relative Frequenz in Oppositionsparteien",
       y="Relative Frequenz in Regierungsparteien",
       title="Häufige Adjektive bei Oppositions- und Regierungsparteien")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"))


## TENSE

s.gov <- sum(gov_roots$freq)
s.opp <- sum(opp_roots$freq)

gov.type <- c("opposition", "goverment")
pres.type <- c(sum(filter(opp_roots, tense == "Pres")$freq)/s.opp,
               sum(filter(gov_roots, tense == "Pres")$freq)/s.gov)
past.type <- c(sum(filter(opp_roots, tense == "Past")$freq)/s.opp,
               sum(filter(gov_roots, tense == "Past")$freq)/s.gov)

tense <- data.frame(type=gov.type, pres =pres.type, past=past.type)

## VERBFORM

inf.type <-  c(sum(filter(opp_roots, verbform == "Inf")$freq)/s.opp,
               sum(filter(gov_roots, verbform == "Inf")$freq)/s.gov)
fin.type <- c(sum(filter(opp_roots, verbform == "Fin")$freq)/s.opp,
              sum(filter(gov_roots, verbform == "Fin")$freq)/s.gov)
part.type <- c(sum(filter(opp_roots, verbform == "Part")$freq)/s.opp,
               sum(filter(gov_roots, verbform == "Part")$freq)/s.gov)

verbform <- data.frame(type=gov.type, inf=inf.type, fin =fin.type, part = part.type)


### Sentiment -- Adjectives

# Government
adj.s.gov <- sum(gov_adv$freq)

tmp.sent <- ifelse(gov_adv$senti_ws == "-", 0, gov_adv$senti_ws)
gov_adv$tmp.sent <- as.numeric(tmp.sent)

neg.adj <- filter(gov_adv, tmp.sent<0)
f <- sum(neg.adj$tmp.sent * neg.adj$freq/adj.s.gov)

pos.adj <- filter(gov_adv, tmp.sent>0)
g <- sum(pos.adj$tmp.sent * pos.adj$freq/adj.s.gov)

# Opposition
adj.s.opp <- sum(opp_adv$freq)
tmp.sent <- ifelse(opp_adv$senti_ws == "-", 0, opp_adv$senti_ws)
opp_adv$tmp.sent <- as.numeric(tmp.sent)

neg.adj <- filter(opp_adv, tmp.sent<0)
h <- sum(neg.adj$tmp.sent * neg.adj$freq/adj.s.opp )

pos.adj <- filter(opp_adv, tmp.sent>0)
i <- sum(pos.adj$tmp.sent * pos.adj$freq/adj.s.opp )


