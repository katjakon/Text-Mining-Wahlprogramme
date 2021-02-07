library(quanteda)
library(readtext)

# Read in files and set document level variables
programs <- readtext("Korpus-Dateien", 
                     docvarsfrom = "filenames",
                     docvarnames = c("type", "party", "year"),
                     dvsep = "-",
                     encoding="utf-8") %>% corpus()



# Create tokens object and dfm
program_toks <- tokens(programs,remove_punct = True) %>% tokens_remove(stopwords("german"), padding = TRUE )
deutsch <- kwic(program_toks,"regier*", window=10) 
deutsch_toks <- corpus(deutsch) %>% tokens(remove_punct = TRUE) %>% tokens_remove(stopwords("german"), padding = TRUE )
dfm_deutsch <- dfm(deutsch_toks)
docvars(dfm_deutsch)
textplot_wordcloud(dfm_deutsch, max_words = 100)

cdu <-  programs[programs$party == "CDU"]
spd <- programs[programs$party == "SPD"]
nullzwei <- programs[programs$year == "2002"]
nullzwei
