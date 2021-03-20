library(udpipe)
library(quanteda)
library(readtext)

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
textplot_wordcloud(years, max_words = 150, min_size = 0.5, comparison=TRUE, color=c("darkolivegreen", "cadetblue4", "deeppink3", "darkorange", "darkred"))

parties <- dfm(program_toks, groups = "party")
textplot_wordcloud(parties, max_words = 150, min_size = 0.5, comparison=TRUE, color=c("blue3", "darkgreen", "black", "red", "deeppink", "darkred", "brown2"))



