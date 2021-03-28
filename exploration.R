# This code explores the corpus 
# For explanation see exploration.Rmd
library(quanteda)
library(readtext)
library(seededlda)
library(tidyverse)
library(udpipe)

#######################
### First Steps #######
#######################

# Lemmatize a corpus while keeping docvars etc.
lemmatize <- function(readtext_obj, model_udpipe) {
  for (n in 1:nrow(readtext_obj)) {
    text <- readtext_obj[n,]$text
    annotated <- udpipe_annotate(model_udpipe, x = text, parser = "none")
    # Filter out NA cells.
    annotated.df <- subset(as.data.frame(annotated), lemma != "NA")
    # Special case where lemma is not useful when text is collapsed again.
    annotated.df$lemma <- replace(annotated.df$lemma, annotated.df$lemma == "er|es|sie", "sich")
    readtext_obj[n,]$text <- paste(annotated.df$lemma, collapse = " ")
  }
  return(readtext_obj)
}

# Load model necessary for lemmatization and tagging. 
model_deutsch <- udpipe_load_model(file="german-gsd-ud-2.5-191206.udpipe")

# Add custom stopwords
custom_stops <- c(stopwords("german"), c(""," ", "|","dass", "dabei", "dafür", "sowie", "daher"))

# Working directory needs to be set to same directory as corpus directory for this to work!
# Read in files, set document level variables.
programs_texts <- readtext("Korpus-Dateien", 
                     docvarsfrom = "filenames",
                     docvarnames = c("type", "party", "year"),
                     dvsep = "-",
                     encoding="utf-8")

# Lemmatize texts
programs <- lemmatize(programs_texts, model_deutsch) %>% corpus()

# Convert characters in year column to integers
docvars(programs, field="year") <- as.integer(docvars(programs, field="year"))

# Create tokens object for whole corpus. filter out stopwords.
program_toks <- tokens(programs,remove_punct = TRUE) %>% tokens_remove(custom_stops, padding = TRUE ) %>% tokens_remove(c("", "|"))

# Create dfm for corpus
program_dfm <- dfm(program_toks)


#######################
### Bag of words ######
#######################


# Topfeatures of whole corpus
# TO DO: Get rid of wordclouds, use lollipop plots instead!
#textplot_wordcloud(program_dfm, max_words = 100, min_size = 1.7, color = "darkslategrey")

top.corpus <- topfeatures(program_dfm, n = 100)

# Compare parties and years -- WHAT SHOULD WE USE INSTEAD?
# Keyness?? 
years <- dfm(program_toks, groups = "year")
#textplot_wordcloud(years, max_words = 100, min_size = 0.5, comparison=TRUE, color=c("darkolivegreen", "cadetblue4", "deeppink3", "darkorange", "darkred"))
parties <- dfm(program_toks, groups = "party")
#textplot_wordcloud(parties, max_words = 100, min_size = 0.5, comparison=TRUE, color=c("blue3", "darkgreen", "black", "red", "deeppink", "darkred", "brown2"))

#### Topfeatures for parties ####

cdu <-  programs[programs$party == "CDU"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
top.cdu <- as.data.frame(topfeatures(cdu, n= 100))

#textplot_wordcloud(cdu, min_size = 1, max_words = 100, color = "black")

spd <- programs[programs$party == "SPD"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
#textplot_wordcloud(spd, min_size = 1, max_words = 100, color = "red")
top.spd<- as.data.frame(topfeatures(spd, n= 100))

fdp <- programs[programs$party == "FDP"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
#textplot_wordcloud(fdp, min_size = 1, max_words = 100, color = "yellow3")
top.fdp <- as.data.frame(topfeatures(fdp, n= 100))

linke <- programs[programs$party == "DIELINKE"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
#textplot_wordcloud(linke, min_size = 1, max_words = 100, color = "red")
top.linke <- as.data.frame(topfeatures(linke, n= 100))

gruene <- programs[programs$party == "B90dieGruene"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
#textplot_wordcloud(gruene, min_size = 1, max_words = 100, color = "green4")
top.gruene <- as.data.frame(topfeatures(gruene, n= 100))

afd <- programs[programs$party == "AfD"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
top.afd <- as.data.frame(topfeatures(afd, n= 100))

pds <- programs[programs$party == "PDS"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
top.pds <- as.data.frame(topfeatures(pds, n= 100))

### Topfeatures Jahre ###
y2002 <- programs[programs$year == 2002] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
top.2002 <- as.data.frame(topfeatures(y2002, n= 100))

y2005 <- programs[programs$year == 2005] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
top.2005 <- as.data.frame(topfeatures(y2005, n= 100))

y2009 <- programs[programs$year == 2009] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
top.2009 <- as.data.frame(topfeatures(y2009, n= 100))

y2013 <- programs[programs$year == 2013] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
top.2013 <- as.data.frame(topfeatures(y2013, n= 100))

y2017 <- programs[programs$year == 2017] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(custom_stops, padding = FALSE) %>% dfm()
top.2017 <- as.data.frame(topfeatures(y2017, n= 100))


#######################
### Topic Modeling ####
#######################

# whole corpus, 10 topics
program_lda_k10 <- textmodel_lda(program_dfm, k=10)
terms(program_lda_k10)

# topics from DIELINKE programs
dielinke_lda_k20 <- textmodel_lda(dfm(programs[programs$party == "DIELINKE"], remove_punct=TRUE, remove=(stopwords("german"))), k=20)
terms(dielinke_lda_k20)

# topics from B90/Die GrÃ¼ne programs
gruene_lda_k20 <- textmodel_lda(dfm(programs[programs$party == "B90dieGruene"], remove_punct=TRUE, remove=(stopwords("german"))), k=20)
terms(gruene_lda_k20)

# topics from SPD programs
spd_lda_k20 <- textmodel_lda(dfm(programs[programs$party == "SPD"], remove_punct=TRUE, remove=(stopwords("german"))), k=20)
terms(spd_lda_k20)

# topics from CDU programs
cdu_lda_k20 <- textmodel_lda(dfm(programs[programs$party == "CDU"], remove_punct=TRUE, remove=(stopwords("german"))), k=20)
terms(cdu_lda_k20)

#  topics from AfD programs
afd_lda_k20 <- textmodel_lda(dfm(programs[programs$party == "AfD"], remove_punct=TRUE, remove=(stopwords("german"))), k=20)
terms(afd_lda_k20)

#  topics from FDP programs
fdp_lda_k20 <- textmodel_lda(dfm(programs[programs$party == "FDP"], remove_punct=TRUE, remove=(stopwords("german"))), k=20)
terms(fdp_lda_k20)

# topics from 2002
programs_2002_lda <- textmodel_lda(dfm(programs[programs$"year" == 2002], remove_punct=TRUE, remove=(stopwords("german"))), k=10)
terms(programs_2002_lda)

# topics from 2005
programs_2005_lda <- textmodel_lda(dfm(programs[programs$"year" == 2005], remove_punct=TRUE, remove=(stopwords("german"))), k=10)
terms(programs_2005_lda)

# topics from 2009
programs_2009_lda <- textmodel_lda(dfm(programs[programs$"year" == 2009], remove_punct=TRUE, remove=(stopwords("german"))), k=10)
terms(programs_2009_lda)

# topics from 2013
programs_2013_lda <- textmodel_lda(dfm(programs[programs$"year" == 2013], remove_punct=TRUE, remove=(stopwords("german"))), k=10)
terms(programs_2013_lda)

# topics from 2017
programs_2017_lda <- textmodel_lda(dfm(programs[programs$"year" == 2017], remove_punct=TRUE, remove=(stopwords("german"))), k=10)
terms(programs_2017_lda)

#######################
# Keywords in Context #
#######################

# Word cloud for deutsch* and whole corpus
deutsch <- kwic(program_toks,"deutsch*", window=10) %>% corpus() %>% dfm()
textplot_wordcloud(deutsch, max_words = 100)

# climate change dictionary
climate_dict <- c( "klimawandel", 
                   "treibhaus*", 
                   "CO2", 
                   "erderwärmung", 
                   phrase("erneuerbare energien"),
                   "2-Grad-Ziel",
                   "zwei-grad-ziel",
                   "klimakrise",
                   "klimakatastrophe",
                   "klimaschutz",
                   "abholzung",
                   phrase("fossile energie*"),
                   "atmosphäre",
                   "kohlenstoffdioxid",
                   "emission*")

klima.dfm <- dfm(program_dfm, select = climate_dict) %>% dfm(groups = "party")
klima.df <- convert(klima.dfm, to = "data.frame")
climate_col <- colnames(klima.df)[2:length(colnames(klima.df))]
climate_terms <- data.frame(climate_col)

for (n in 1:nrow(klima.df)){
  curr.party <- klima.df[n,1]
  curr.dfm <- dfm_subset(klima.dfm, party == curr.party)
  curr.df <- convert(curr.dfm, to = "data.frame")
  climate_terms[curr.party] = as.numeric(curr.df[1,2:length(curr.df)])
}
klima.all <- dfm(klima.dfm, groups = "type")
climate_terms["insgesamt"] <- as.numeric(convert(klima.all, to = "data.frame")[1,2:length(klima.df)])

ggplot(climate_terms, aes(fill =c("AfD", "B90dieGruene" ,"CDU", "DIELINKE","FDP","PDS","SPD"), y=insgesamt, x=climate_col)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Anzahl der Klimabegriffe über Jahre")+
  xlab("Jahre")+
  ylab("Begriffe")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


years <- klima.dfm$year
Parteien <- klima.dfm$party
Parteien
sum_begriffe <-  c()
my.data <- data.frame(years, Parteien)

for (n in 1:nrow(my.data)){
  s <- sum(klima.dfm[n, 2:length(klima.dfm[2,])])
  sum_begriffe <- c(sum_begriffe, s)
}
my.data$nterms <- sum_begriffe

ggplot(my.data, aes(fill=Parteien, y=nterms, x=years)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Anzahl der Klimabegriffe über Jahre")+
  xlab("Jahre")+
  ylab("Begriffe")+
  theme_minimal()

# Word cloud for climate change dictionary
klima <- kwic(program_toks,climate_dict, window=10) 
#textplot_wordcloud(klima %>% corpus() %>% dfm(), max_words = 100, color= "chartreuse4")

# Lexical dispersion plot for climate 
textplot_xray(klima)

# eu dictionary
eu_dict <- c("eu*", "europ*")

eu <- kwic(program_toks,eu_dict, window=10) 
textplot_wordcloud(eu%>% corpus() %>% dfm(), max_words = 100)

# Lexical dispersion for european
textplot_xray(eu)

years <- dfm(corpus_subset(programs, year %in% c(2002, 2017)),
              remove = stopwords("german"), remove_punct = TRUE, groups = "year") 
textplot_wordcloud(years, comparison = TRUE, max_words = 300,
                   color = c("blue", "red"))


#######################
#### Collocations #####
#######################

# With stopwords
textstat_collocations(programs, min_count = 500)

# Without stopwords
textstat_collocations(program_toks, min_count = 100)

#######################
####### TF-IDF ########
#######################

tfidf <- dfm_tfidf(program_toks %>% dfm(groups = "party"))
topfeatures(tfidf[1,])
# Distinctive terms for each party
afd <- dfm_subset(tfidf, tfidf$"party"=="AfD")
topfeatures(afd, n=10)

spd <- dfm_subset(tfidf, tfidf$"party"=="SPD")
topfeatures(spd, n=10)

cdu <- dfm_subset(tfidf, tfidf$"party"=="CDU")
topfeatures(cdu, n=10)

linke <- dfm_subset(tfidf, tfidf$"party"=="DIELINKE")
topfeatures(linke, n=10)

gruene <- dfm_subset(tfidf, tfidf$"party"=="B90dieGruene")
topfeatures(gruene, n=10)

fdp <- dfm_subset(tfidf, tfidf$"party"=="FDP")
topfeatures(fdp, n=10)

pds <- dfm_subset(tfidf, tfidf$"party"=="PDS")
topfeatures(pds, n=10)

# Distinctive terms for each year
tfidf_year <- dfm_tfidf(program_toks %>% dfm(groups = "year"))

year_2002 <- dfm_subset(tfidf, tfidf$"year"==2002)
topfeatures(year_2002)

year_2005 <- dfm_subset(tfidf, tfidf$"year"==2005)
topfeatures(year_2005, n=20)

year_2009 <- dfm_subset(tfidf, tfidf$"year"==2009)
topfeatures(year_2009, n=20)

year_2013 <- dfm_subset(tfidf, tfidf$"year"==2013)
topfeatures(year_2013, n=20)

year_2017 <- dfm_subset(tfidf, tfidf$"year"==2017)
topfeatures(year_2017, n=20)

#########################################################################
# whatever this will be - GANZ UND GAR UNFERTIG!!!
#########################################################################

annotated_model <- udpipe_annotate(udmodel_german, x = programs) %>%
  as.data.frame()

regierung <- c('doc3', 'doc4', 'doc10', 'doc11', 'doc12', 'doc19', 'doc23', 'doc24', 'doc25', 'doc27')
opposition <- c('doc1', 'doc2', 'doc5', 'doc6', 'doc7', 'doc8', 'doc9', 'doc13', 'doc14', 'doc15', 'doc16', 'doc17', 'doc18', 'doc20', 'doc21', 'doc22', 'doc26')

sub_model_regierung <- subset(annotated_model, doc_id %in% regierung)
sub_model_opposotion <- subset(annotated_model, doc_id %in% opposition)

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
                        'doc13' = c('linke', 'wir'),
                        'doc14' = c('linke', 'wir'),
                        'doc15' = c('linke', 'wir'),
                        'doc16' = c('fdp', 'wir'),
                        'doc17' = c('fdp', 'wir'),
                        'doc18' = c('fdp', 'wir'),
                        'doc20' = c('fdp', 'wir'),
                        'doc21' = c('pds', 'wir'),
                        'doc22' = c('linkspartei.pds', 'wir'),
                        'doc26' = c('spd', 'wir'))


# für die opposition
wir_die_opposition <- subset(sub_model_opposotion, dep_rel == 'nsubj' & tolower(token) %in% unlist(opposition_list[doc_id]))

sents_opp_wir_list <- wir_die_opposition$sentence

wir_die_opposition_sents <- subset(sub_model_opposotion, sentence %in% sents_opp_wir_list)
wir_die_opp_roots <- subset(wir_die_opposition_sents, dep_rel == 'root')

opp_df <- wir_die_opp_roots[c(1, 6:7, 10)]

# mood
opp_df$mood <- '-'
opp_df_with_mood <- opp_df[grepl('Mood', opp_df$feats) == TRUE,]
opp_df_without_mood <- opp_df[grepl('Mood', opp_df$feats) == FALSE,]

opp_df_with_mood$mood <- sub(".*(Mood=)([A-Za-z]+)[|]?.*", "\\2", opp_df_with_mood$feats)

opp_df <- rbind(opp_df_with_mood, opp_df_without_mood)

# verbform
opp_df$verbform <- '-'
opp_df_with_verbform <- opp_df[grepl('VerbForm', opp_df$feats) == TRUE,]
opp_df_without_verbform <- opp_df[grepl('VerbForm', opp_df$feats) == FALSE,]

opp_df_with_verbform$verbform <- sub(".*(VerbForm=)([A-Za-z]+)[|]?.*", "\\2", opp_df_with_verbform$feats)

opp_df <- rbind(opp_df_with_verbform, opp_df_without_verbform)

# cut out feats
opp_df <- opp_df[-c(2,4)]

# sentiment
# set up dicts
neg <- scan("SentiWS_v1.8c_Negative.txt", what = "char", sep = "\n", fileEncoding="utf-8")
pos <- scan("SentiWS_v1.8c_Positive.txt", what = "char", sep = "\n", fileEncoding="utf-8")
s <- str_split(neg, "\t")
t <- str_split(pos, "\t")
terms.neg <- sub("([A-Za-zß]+)[|][A-Za-z]+", "\\1",lapply(s, function(l) l[[1]]))
terms.pos <- sub("([A-Za-zß]+)[|][A-Za-z]+", "\\1",lapply(t, function(l) l[[1]]))

value.neg <- unlist(lapply(s, function(l) as.double(l[[2]])))
value.pos <- unlist(lapply(t, function(l) as.double(l[[2]])))

positive <- data.frame(term=terms.pos, value=value.pos)
negative <- data.frame(term=terms.neg, value=value.neg)

# add sentiment value
positive[positive$term == 'befürworten',]
negative[negative$term == 'befürworten',]
length(positive$term)

# # devtools::install_github("sebastiansauer/pradadata")
# library("pradadata")
# data('germanlex')

# das ist alles nur geklaut
# read the data
germanlex <- read_delim(file = "germanlex.txt",
                        skip = 27,
                        delim = " ",
                        col_names = c("word", "qualifier", "pos"))

# separate column
germanlex %>%
  separate(col = qualifier, into = c("qualifier", "polarity_strength"), sep = "=") -> germanlex

# remove comment lines
germanlex %>%
  filter(word != "%%") -> germanlex

# change to type numeric
germanlex %>%
  mutate(polarity_strength = parse_number(polarity_strength)) -> germanlex

# so
germanlex[germanlex$word == 'beenden',]$qualifier

opp_df$qualifier <- '-'
opp_df$polarity_strength <- '-'

# with germanlex
for (w in unique(opp_df$lemma)) {
  if (w %in% germanlex$word) {
    opp_df[opp_df$lemma == w,]$qualifier <- as.character(germanlex[germanlex$word == w,]$qualifier[1])
    opp_df[opp_df$lemma == w,]$polarity_strength <- as.character(germanlex[germanlex$word == w,]$polarity_strength[1])
  }
}

# the other dict
opp_df$senti_ws <- '-'

for (w in unique(opp_df$lemma)) {

  if (w %in% positive$term) {

    opp_df[opp_df$lemma == w,]$senti_ws <- as.character(positive[positive$term == w,]$value)
  }
  if (w %in% negative$term) {
    opp_df[opp_df$lemma == w,]$senti_ws <- as.character(negative[negative$term == w,]$value)
  }
}

# party
opp_df$party <- as.character(lapply(opposition_list[opp_df$doc_id], function(elem) elem[[1]]))

opp_df
                                    
# just terms
just_terms <- opp_df[2:8]

counted_terms <- aggregate(cbind(just_terms[0],numdup=1), just_terms, length)

counted_terms <- counted_terms[order(counted_terms$numdup, decreasing = TRUE),]

just_terms <- filter(counted_terms, numdup > 2)
names(just_terms)[names(just_terms) == 'numdup'] <- 'freq'
# write.csv2(just_terms, 'opposition_roots_counted.csv')
 ################################## ...regierung fehlt noch und schick machen auch.                             
                                    
