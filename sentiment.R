library(quanteda)
library(readtext)
library(tidyverse)
library(udpipe)

#################################
### Read in corpus and stops ####
################################

load("RData/custom_stopwords.RData")
load("RData/lemmatized_corpus.RData")

# Convert characters in year column to integers
docvars(programs, field="year") <- as.integer(docvars(programs, field="year"))

# Create tokens object for whole corpus, filter out stopwords.
program_toks <- tokens(programs,remove_punct = TRUE) %>% tokens_remove(custom_stops)

# Create dfm for corpus
program_dfm <- dfm(program_toks)


#################################
### Read in Sentiment words #####
################################

neg <- scan("data/SentiWS_v1.8c_Negative.txt", what = "char", sep = "\n", fileEncoding="utf-8")
pos <- scan("data/SentiWS_v1.8c_Positive.txt", what = "char", sep = "\n", fileEncoding="utf-8")
s <- str_split(neg, "\t")
t <- str_split(pos, "\t")
terms.neg <- sub("([A-Za-zß]+)[|][A-Za-z]+", "\\1",lapply(s, function(l) l[[1]]))
terms.pos <- sub("([A-Za-zß]+)[|][A-Za-z]+", "\\1",lapply(t, function(l) l[[1]]))

value.neg <- unlist(lapply(s, function(l) as.double(l[[2]])))
value.pos <- unlist(lapply(t, function(l) as.double(l[[2]])))

positive <- data.frame(term=terms.pos, value=value.pos)
negative <- data.frame(term=terms.neg, value=value.neg)

# Or:
# load(file = "RData/senti_dict.RData")
# senti_dict

#################################
### Sentiment over years #######
################################

years <- dfm(program_dfm, groups = "year") %>% 
  textstat_frequency(groups = "year") %>% 
  as.data.frame()

tmp.pos <- filter(years, feature %in% positive$term)
tmp.neg <- filter(years, feature %in% negative$term)

lvl.year <- levels(factor(years$group))
sent.years <- data.frame(matrix(ncol = 3, nrow=0))
colnames(sent.years) <- c("year", "rel_freq", "sent")

# Collect sentiment for positive terms
for (i in 1:length(lvl.year)){
  curr.year <- filter(tmp.pos, group == lvl.year[i])
  sum.all <- sum(filter(years, group == lvl.year[i])$frequency)
  sum.freq <- sum(curr.year$frequency)
  curr.row <- data.frame(year = lvl.year[i], rel_freq=sum.freq/sum.all, sent = "positive")
  sent.years <- rbind(sent.years, curr.row)
}

# Collect sentiment for negative terms
for (i in 1:length(lvl.year)){
  curr.year <- filter(tmp.neg, group == lvl.year[i])
  sum.all <- sum(filter(years, group == lvl.year[i])$frequency)
  sum.freq <- sum(curr.year$frequency)
  curr.row <- data.frame(year = lvl.year[i], rel_freq=sum.freq/sum.all, sent = "negative")
  sent.years <- rbind(sent.years, curr.row)
}

# Plot sentiment over years.
ggplot(sent.years, aes(fill = sent, x=year, y=rel_freq)) +
  geom_bar(position = "dodge", stat="identity")+
  labs(y = "Relative Häufigkeit", x = "Jahre",
       fill="Sentiment",
       title = "Relative Häufigkeit von Sentimenttermen")+
  scale_fill_manual(values = c("darkred", "darkgreen"))+
  theme_minimal()


#################################
### Sentiment for parties #######
#################################

parties <- dfm(program_dfm) %>% 
  textstat_frequency(groups = "party") %>% 
  as.data.frame()

tmp.pos <- filter(parties, feature %in% positive$term)
tmp.neg <- filter(parties, feature %in% negative$term)

party.vec <- levels(factor(parties$group))

# Intialize data frame
sent.party <- data.frame(matrix(ncol=3, nrow=0))
colnames(sent.party) <- c("party", "frequency", "sent")

# Collect negative and positive terms
for (i in 1:length(party.vec)){
  curr.party <- party.vec[i]
  sum.all <- sum(filter(parties, group == curr.party)$frequency)
  curr.pos <- sum(filter(tmp.pos, group == curr.party)$frequency)
  curr.neg <- sum(filter(tmp.neg, group == curr.party)$frequency)
  row.pos <- data.frame(party = curr.party, sent="positiv", frequency=curr.pos/sum.all)
  row.neg <- data.frame(party = curr.party, sent="negativ", frequency=curr.neg/sum.all)
  sent.party <- rbind(sent.party, row.pos, row.neg)
}

# Plot sentiment for parties.
ggplot(sent.party, aes(fill = sent, x=party, y=frequency)) +
  geom_bar(position = "dodge", stat="identity")+
  labs(y = "Relative Häufigkeit",
       x = "Jahre",
       fill="Sentiment",
       title = "Relative Häufigkeit von Sentimenttermen nach Parteien")+
  scale_fill_manual(values = c("darkred", "darkgreen"))+
  theme_minimal()


######################################
#Sentiment for parties and years #####
######################################

part.year <- dfm(program_dfm) %>% 
  textstat_frequency(groups = c("party", "year")) %>% 
  as.data.frame()

# Add columns for party and year.
part.year$party <- unlist(lapply(strsplit(part.year$group, "[.]"), function(l) l[[1]]))
part.year$year <- unlist(lapply(strsplit(part.year$group, "[.]"), function(l) l[[2]]))

# Initialze data frame for positive terms.
pos.year.party <- data.frame(matrix(ncol=3, nrow=0))
colnames(pos.year.party ) <- c("year", "party", "rel")

lvl <- levels(factor(part.year$group))
for (i in 1:length(lvl)){
  curr.lvl <- lvl[i]
  sum.lvl <- sum(filter(part.year, group == curr.lvl)$frequency)
  pos.lvl <- filter(part.year, feature %in% positive$term & group== curr.lvl)
  tmp.row <- data.frame(year = levels(factor(pos.lvl$year)),
                        party = levels(factor(pos.lvl$party)),
                        rel=sum(pos.lvl$frequency)/sum.lvl)
  pos.year.party  <- rbind(pos.year.party , tmp.row)
  
}

# Plot positive terms for parties over years.
ggplot(pos.year.party, aes(fill = party, x=year, y=rel)) +
  geom_bar(position = "dodge", stat="identity")+
  labs(y = "Relative Häufigkeit", 
       x = "Jahre",
       fill="Partei",
       title = "Relative Häufigkeit von Termen mit positivem Sentiment")+
  theme_minimal()+
  scale_fill_manual(values = c("blue", "#009933", "black", "#CC0066", "#FFFF00", "brown", "red"))


# Initialze data frame for negative terms.
neg.year.party <- data.frame(matrix(ncol=3, nrow=0))
colnames(neg.year.party) <- c("year", "party", "rel")

lvl <- levels(factor(part.year$group))

for (i in 1:length(lvl)){
  curr.lvl <- lvl[i]
  sum.lvl <- sum(filter(part.year, group == curr.lvl)$frequency)
  neg.lvl <- filter(part.year, feature %in% negative$term&group== curr.lvl)
  tmp.row <- data.frame(year = levels(factor(neg.lvl$year)),
                        party = levels(factor(neg.lvl$party)),
                        rel=sum(neg.lvl$frequency)/sum.lvl)
  neg.year.party <- rbind(neg.year.party, tmp.row)
}

# Plot negative sentiment over years for parties.
ggplot(neg.year.party, aes(fill = party, x=year, y=rel)) +
  geom_bar(position = "dodge", stat="identity")+
  labs(y = "Relative Häufigkeit",
       x = "Jahre",
       fill="Partei",
       title = "Relative Häufigkeit von Termen mit negativem Sentiment")+
  theme_minimal()+
  scale_fill_manual(values = c("blue", "#009933", "black", "#CC0066", "#FFFF00", "brown", "red"))


######################################
# Sentiment Values  ##################
######################################
part.sent <- textstat_frequency(program_dfm, groups = "party") %>% as.data.frame()

tmp.pos <- filter(part.sent, feature %in% positive$term)
tmp.neg <- filter(part.sent, feature %in% negative$term)

# Intialize data frame for positiv terms
freq.pos <- as.data.frame(matrix(ncol = 4, nrow = 0))
colnames(freq.pos) <- c("term", "sentiment", "frequency", "party")

# Collect sentiment values for positive terms.
for (i in 1:nrow(tmp.pos)){
  curr.term <- tmp.pos$feature[i]
  curr.value <- filter(positive, term == curr.term)$value
  curr.freq <- tmp.pos$frequency[i]
  curr.row <- data.frame(term = curr.term,
                         sentiment = curr.value,
                         frequency = curr.freq,
                         party = tmp.pos$group[i])
  freq.pos <- rbind(freq.pos, curr.row)
}

# Intialize data frame for negative terms
freq.neg <- as.data.frame(matrix(ncol = 4, nrow = 0))
colnames(freq.neg) <- c("term", "sentiment", "frequency", "year")

# Collect sentiment values for negative terms.
for (i in 1:nrow(tmp.neg)){
  curr.term <- tmp.neg$feature[i]
  curr.value <- filter(negative, term == curr.term)$value
  curr.freq <- tmp.neg$frequency[i]
  curr.row <- data.frame(term = curr.term,
                         sentiment = curr.value,
                         frequency = curr.freq,
                         party = tmp.neg$group[i])
  freq.neg<- rbind(freq.neg, curr.row)
}

part.lvl <- levels(factor(freq.neg$party))
sent.values <- data.frame(matrix(ncol=3, nrow=0))
colnames(sent.values) <- c("party", "pos", "neg")

# Compute sum for positive and negative terms
for (i in 1:length(part.lvl)){
  tmp.party <- part.lvl[i]
  curr.pos <- filter(freq.pos, party == tmp.party)
  curr.neg <- filter(freq.neg, party == tmp.party)
  sum.all <- sum(filter(textstat_frequency(program_dfm, group = "party"),
                        group == tmp.party)$frequency)
  p <- sum(curr.pos$sentiment * curr.pos$frequency/sum.all)
  n <- sum(curr.neg$sentiment * curr.neg$frequency/sum.all)
  tmp <- data.frame(party=tmp.party, pos=p, neg=n)
  sent.values <- rbind(sent.values, tmp)
}

# Plot sentiment scores for parties.
ggplot(sent.values) +
  geom_segment( aes(x=party, xend=party, y=pos, yend=neg), color="black") +
  geom_point( aes(x=party, y=pos), color="darkgreen", size=5 ) +
  geom_point( aes(x=party, y=neg), color="darkred", size=5 )+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  labs(x = "Partei", y="Score", title="Positive und Negative Sentimentscores")

