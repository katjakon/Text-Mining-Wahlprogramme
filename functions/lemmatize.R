# This script contains the lemmatization function used in this project.
# Necessary library
library(udpipe)

# Lemmatize a readtext object, while keeping docvars etc.
# Parameter: 
# readtext object --> contains a column text with characters
# model_udpipe --> A trained model from the udpipe package
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