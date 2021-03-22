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
programs <- program


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
write.csv(top.2017, "data/top_2017.csv", fileEncoding = "utf-8")


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

klima.dfm <- dfm(program_dfm, select = climate_dict)
years <- klima.dfm$year
Parteien <- klima.dfm$party

sum_begriffe <-  c()
my.data <- data.frame(years, Parteien)

for (n in 1:nrow(my.data)){
  s <- sum(klima.dfm[n, 2:length(klima.dfm[2,])])
  sum_begriffe <- c(sum_begriffe, s)
}
my.data$nterms <- sum_begriffe

ggplot(my.data, aes(fill=Parteien, y=nterms, x=years)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Anzahl der Klimabegriffe")+
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
