library(quanteda)
library(readtext)
library(tidyverse)
library(udpipe)

source("functions/lemmatize.R")


# Load model necessary for lemmatization and tagging. 
model_deutsch <- udpipe_load_model(file="data/german-gsd-ud-2.5-191206.udpipe")

# Add custom stopwords
custom_stops <- c(stopwords("german"), c("", "|","dass", "dabei", "dafür", "sowie", "daher", "deshalb"))
# OR:
load("RData/custom_stopwords.RData")

# Working directory needs to be set to same directory as corpus directory for this to work!
# Read in files, set document level variables.
programs_texts <- readtext("Korpus-Dateien", 
                           docvarsfrom = "filenames",
                           docvarnames = c("type", "party", "year"),
                           dvsep = "-",
                           encoding="utf-8")

# Lemmatize texts, this will take a while, alternatively load RData file
programs <- lemmatize(programs_texts, model_deutsch) %>% corpus()
# OR:
load("RData/lemmatized_corpus.RData")

# Convert characters in year column to integers
docvars(programs, field="year") <- as.integer(docvars(programs, field="year"))

# Create tokens object for whole corpus, filter out stopwords.
program_toks <- tokens(programs,remove_punct = TRUE) %>% tokens_remove(custom_stops)

# Create dfm for corpus
program_dfm <- dfm(program_toks)


#######################
### Top 30 Terms ######
#######################

# Compare 30 most frequent terms for each party
top.30 <- head(textstat_frequency(dfm(program_dfm, groups="party")), 30)

parties <- c("AfD", "CDU", "SPD", "PDS", "FDP", "DIELINKE", "B90dieGruene")

# Intialize data frame
terms.ranked <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(terms.ranked) <- c("feature", "party", "relative")

# Iterate over parties and get relative frequency for each term.
for (i in 1:length(parties)){
  # Get programs for each party.
  stat.party <- dfm_subset(program_dfm, party == parties[i]) %>%
    textstat_frequency()
  # Get full term frequency for each party.
  sum.freq <- sum(stat.party$frequency)
  # Filter out top terms.
  stat.party.filtered <- filter(stat.party, feature %in% top.30$feature)
  tmp.data <- data.frame(feature=stat.party.filtered$feature,
                         party=parties[i],
                         relativ=stat.party.filtered$frequency/sum.freq)
  terms.ranked <- rbind(terms.ranked, tmp.data)
}


# Plot Heatmap for top 30 terms.
ggplot(terms.ranked, aes(y=feature, x=party)) + 
  geom_tile(aes(fill = relativ)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ggtitle("Relative Häufigkeit der 30 frequentesten Terme")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.009)+
  labs(x="Partei", y = "Term", fill ="Relative Häufigkeit")

#######################
## Cosine similiarity #
#######################

party.dfm <- dfm(program_dfm, groups = "party")
textstat_simil(party.dfm,margin ="documents", method = "cosine")


#######################
###### Tf- Idf ########
#######################

tfidf <- dfm_tfidf(party.dfm)

## AfD - Tf-IDF
tfidf.afd <- dfm_subset(tfidf, party == "AfD")
top.afd <- head(textstat_frequency(tfidf.afd, force =TRUE), 30) %>% filter(feature != "afd")


# Plot 30 terms with highest tf idf score.
ggplot(top.afd , aes(x=feature, y=frequency)) +
  geom_segment( aes(x=top.afd $feature, xend=top.afd $feature, y=0, yend=frequency), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(x ="Term", y="TF-IDF-Score")+
  ggtitle("Alternative für Deutschland - Terme mit höchsten TF-IDF-Wert")

## CDU - Tf-IDF

tfidf.cdu <- dfm_subset(tfidf, party == "CDU")
top.cdu <- head(textstat_frequency(tfidf.cdu, force =TRUE), 30) %>%  filter(!(feature %in%  c("cdu", "csu")))


# Plot 30 terms with highest tf idf score.
ggplot(top.cdu, aes(x=feature, y=frequency)) +
  geom_segment( aes(x=top.cdu$feature, xend=top.cdu$feature, y=0, yend=frequency), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(x ="Term", y="TF-IDF-Score")+
  ggtitle("CDU - Terme mit höchsten TF-IDF-Wert")


## SPD - Tf-IDF
tfidf.spd <- dfm_subset(tfidf, party == "SPD")
top.spd <- head(textstat_frequency(tfidf.spd, force =TRUE), 30) %>% filter(!(feature %in%  c("spd")))


# Terms with highest if idf score
ggplot(top.spd, aes(x=feature, y=frequency)) +
  geom_segment( aes(x=top.spd$feature, xend=top.spd$feature, y=0, yend=frequency), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(x ="Term", y="TF-IDF-Score")+
  ggtitle("SPD - Terme mit höchsten TF-IDF-Wert")


## PDS - Tf- IDF
tfidf.pds<- dfm_subset(tfidf, party == "PDS")
top.pds <- head(textstat_frequency(tfidf.pds, force =TRUE), 30) %>% filter(!(feature %in%  c("pds", "linkspartei.pds")))

# Plot terms with high Tf-IDF score
ggplot(top.pds, aes(x=feature, y=frequency)) +
  geom_segment( aes(x=top.pds$feature, xend=top.pds$feature, y=0, yend=frequency), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(x ="Term", y="TF-IDF-Score")+
  ggtitle("PDS - Terme mit höchsten TF-IDF-Wert")


# GRUENE - TF-IDF
tfidf.gruene <- dfm_subset(tfidf, party == "B90dieGruene")
top.gruene <- head(textstat_frequency(tfidf.gruene, force =TRUE), 30) %>% filter(!(feature %in%  c("b90diegruene")))


# Plot terme with highest tf-idf
ggplot(top.gruene, aes(x=feature, y=frequency)) +
  geom_segment( aes(x=top.gruene$feature, xend=top.gruene$feature, y=0, yend=frequency), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(x ="Term", y="TF-IDF-Score")+
  ggtitle("B90 Die Grüne - Terme mit höchsten TF-IDF-Wert")




# FDP -TF-IDF
tfidf.fdp <- dfm_subset(tfidf, party == "FDP")
top.fdp <- head(textstat_frequency(tfidf.fdp, force =TRUE), 30) %>% filter(c, !(feature %in%  c("fdp")))

ggplot(top.fdp, aes(x=feature, y=frequency)) +
  geom_segment( aes(x=top.fdp$feature, xend=top.fdp$feature, y=0, yend=frequency), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(x ="Term", y="TF-IDF-Score")+
  ggtitle("FDP - Terme mit höchsten TF-IDF-Wert")


# DIE LINKE -TF-IDF
tfidf.linke <- dfm_subset(tfidf, party == "DIELINKE")
top.linke <- head(textstat_frequency(tfidf.afd, force =TRUE), 30) %>% filter(!(feature %in%  c("dielinke")))

# Plot terms with highest score
ggplot(c, aes(x=feature, y=frequency)) +
  geom_segment( aes(x=top.linke$feature, xend=top.linke$feature, y=0, yend=frequency), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(x ="Term", y="TF-IDF-Score")+
  ggtitle("DIE LINKE - Terme mit höchsten TF-IDF-Wert")

