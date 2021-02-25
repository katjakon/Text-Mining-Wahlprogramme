# This code explores the corpus 
# For explanation see exploration.Rmd
library(quanteda)
library(readtext)
library(seededlda)

#######################
### First Steps #######
#######################
# Working directory needs to be set to same directory as corpus directory for this to work!
# Read in files and set document level variables
programs <- readtext("Korpus-Dateien", 
                     docvarsfrom = "filenames",
                     docvarnames = c("type", "party", "year"),
                     dvsep = "-",
                     encoding="utf-8") %>% corpus()

# Convert characters in year column to integers
docvars(programs, field="year") <- as.integer(docvars(programs, field="year"))
summary(programs)
# Create tokens object for whole corpus
program_toks <- tokens(programs,remove_punct = TRUE) %>% tokens_remove(stopwords("german"), padding = TRUE )
# Create dfm for corpus
program_dfm <- dfm(program_toks)


#######################
### Bag of words ######
#######################

set.seed(2021)
# word cloud for whole corpus
textplot_wordcloud(program_dfm, max_words = 100, min_size = 1.7, color = "darkslategrey")
topfeatures(program_dfm, n = 100)

# Compare parties and years
years <- dfm(program_toks, groups = "year")
textplot_wordcloud(years, max_words = 100, min_size = 0.5, comparison=TRUE, color=c("darkolivegreen", "cadetblue4", "deeppink3", "darkorange", "darkred"))

parties <- dfm(program_toks, groups = "party")
textplot_wordcloud(parties, max_words = 100, min_size = 0.5, comparison=TRUE, color=c("blue3", "darkgreen", "black", "red", "deeppink", "darkred", "brown2"))

# Word cloud for parties
cdu <-  programs[programs$party == "CDU"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(stopwords("german"), padding = TRUE) %>% dfm()
textplot_wordcloud(cdu, min_size = 1, max_words = 100, color = "black")

spd <- programs[programs$party == "SPD"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(stopwords("german"), padding = TRUE) %>% dfm()
textplot_wordcloud(spd, min_size = 1, max_words = 100, color = "red")

fdp <- programs[programs$party == "FDP"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(stopwords("german"), padding = TRUE) %>% dfm()
textplot_wordcloud(fdp, min_size = 1, max_words = 100, color = "yellow3")

linke <- programs[programs$party == "DIELINKE"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(stopwords("german"), padding = TRUE) %>% dfm()
textplot_wordcloud(linke, min_size = 1, max_words = 100, color = "red")

gruene <- programs[programs$party == "B90dieGruene"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(stopwords("german"), padding = TRUE) %>% dfm()
textplot_wordcloud(gruene, min_size = 1, max_words = 100, color = "green4")

afd <- programs[programs$party == "AfD"] %>% tokens(remove_punct = TRUE) %>% 
  tokens_remove(stopwords("german"), padding = TRUE) %>% dfm()

#######################
### Topic Modeling ####
#######################

# whole corpus, 10 topics
program_lda_k10 <- textmodel_lda(program_dfm, k=10)
terms(program_lda_k10)

# topics from DIELINKE programs
dielinke_lda_k20 <- textmodel_lda(dfm(programs[programs$party == "DIELINKE"], remove_punct=TRUE, remove=(stopwords("german"))), k=20)
terms(dielinke_lda_k20)

# topics from B90/Die Grüne programs
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
climate_dict <- c("klima*", "umwelt*", "nachhalt*")

# Word cloud for climate change dictionary
klima <- kwic(program_toks,climate_dict, window=10) 
textplot_wordcloud(klima %>% corpus() %>% dfm(), max_words = 100, color= "chartreuse4")

# Lexical dispersion plot for climate 
textplot_xray(klima)

# eu dictionary
eu_dict <- c("eu*", "europ*")

# Word cloud for climate change dictionary
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
