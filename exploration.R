# This code produces some plots to explore the corpus 
library(quanteda)
library(readtext)

# Working directory needs to be set to same directory as corpus directory for this to work!
# Read in files and set document level variables
programs <- readtext("Korpus-Dateien", 
                     docvarsfrom = "filenames",
                     docvarnames = c("type", "party", "year"),
                     dvsep = "-",
                     encoding="utf-8") %>% corpus()
# Convert characters in year column to integers
docvars(programs, field="year") <- as.integer(docvars(programs, field="year"))

# Create tokens object for whole corpus
program_toks <- tokens(programs,remove_punct = TRUE) %>% tokens_remove(stopwords("german"), padding = TRUE )

# Word cloud for deutsch* and whole corpus
deutsch <- kwic(program_toks,"deutsch*", window=10) %>% corpus() %>% dfm()
textplot_wordcloud(deutsch, max_words = 100)

# Word cloud for umwelt*, klima* and whole corpus
klima <- kwic(program_toks,c("klima*", "umwelt*"), window=10) %>% corpus() %>% dfm()
textplot_wordcloud(klima, max_words = 100)

# Word cloud for eu* and europ* and whole corpus
eu <- kwic(program_toks,c("eu*", "europ*"), window=10) %>% corpus() %>% dfm()
textplot_wordcloud(eu, max_words = 100)

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
textplot_wordcloud(afd, min_size = 1, max_words = 100, color = "cyan2")

years <- dfm(corpus_subset(programs, year %in% c(2002, 2017)),
              remove = stopwords("german"), remove_punct = TRUE, groups = "year") 
textplot_wordcloud(years, comparison = TRUE, max_words = 300,
                   color = c("blue", "red"))

# Similarity 
# Why are they so similar??
progr_simil <- textstat_simil(dfm(programs), margin = "documents", method = 'cosine')
progr_simil
