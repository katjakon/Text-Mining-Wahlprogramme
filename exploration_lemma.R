library(udpipe)
library(quanteda)
library(readtext)
library(tidyverse)
# Add custom stopwords
custom_stops <- c(stopwords("german"), c("dass", "dabei", "dafür", "sowie", "daher"))

# Lemmatize a corpus while keeping docvars etc.
lemmatize <- function(readtext_obj, model_udpipe) {
  for (n in 1:nrow(readtext_obj)) {
    text <- readtext_obj[n,]$text
    annotated <- udpipe_annotate(model_udpipe, x = text, parser = "none")
    # Filter out NA cells.
    annotated.df <- subset(as.data.frame(annotated), lemma != "NA")
    readtext_obj[n,]$text <- paste(annotated.df$lemma, collapse = " ")
  }
  return(readtext_obj)
}
programs <- readtext("Korpus-Dateien", 
                     docvarsfrom = "filenames",
                     docvarnames = c("type", "party", "year"),
                     dvsep = "-",
                     encoding="utf-8")


# Load model necessary for lemmatization and tagging. 
model_deutsch <- udpipe_load_model(file="german-gsd-ud-2.5-191206.udpipe")

# Lemmatize text of programs. This will take a while!
lemma.corpus <- lemmatize(programs, model_deutsch) 
lemma.corpus <- lemma.corpus %>% corpus()

# Create tokens object for whole corpus
program_toks <- tokens(lemma.corpus,remove_punct = TRUE) %>% tokens_remove(custom_stops, padding = TRUE )
# Create dfm for corpus
program_dfm <- dfm(program_toks, tolower = TRUE)

#######################
### Bag of words ######
#######################

set.seed(2021)
# word cloud for whole corpus
textplot_wordcloud(program_dfm, max_words = 100, min_size = 1.7, color = "darkslategrey")
topfeatures(program_dfm, n = 100)

# Compare parties and years
years <- dfm(program_toks, groups = "year")
textplot_wordcloud(years, max_words = 150, min_size = 0.5, comparison=TRUE, color=c("darkolivegreen", "cadetblue4", "deeppink3", "darkorange", "darkred"))

parties <- dfm(program_toks, groups = "party")
textplot_wordcloud(parties, max_words = 150, min_size = 0.5, comparison=TRUE, color=c("blue3", "darkgreen", "black", "red", "deeppink", "darkred", "brown2"))

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
                  "atmosph?re",
                  "kohlenstoffdioxid",
                  "emission*")

klima.dfm <- dfm(program_dfm, select = climate_dict)
years <- klima.dfm$year
Parteien <- klima.dfm$party

sum_begriffe <-  c()
my.data <- data.frame(years, Parteien)
my.data
for (n in 1:nrow(my.data)){
  s <- sum(klima.dfm[n, 2:length(klima.dfm[2,])])
  sum_begriffe <- c(sum_begriffe, s)
}
my.data$nterms <- sum_begriffe
my.data

ggplot(my.data, aes(fill=Parteien, y=nterms, x=years)) + 
  geom_bar(position="stack", stat="identity")+
  ggtitle("Anzahl der Klimabegriffe")+
  xlab("Jahre")+
  ylab("Begriffe")+
  theme_minimal()


features_dfm_inaug <- textstat_frequency(klima.dfm, n = 100)

# Word cloud for climate change dictionary
klima <- kwic(program_toks,pattern = climate_dict, window=10) 

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
help(dfm)


