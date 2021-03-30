
# function for collecting root information
# input: 
# annotated_data - (subset of) corpus, annotated with udpipe
# translation_list - list with doc_id and corresponding terms which the document author could use
# to refer to themselves
# senti_data - a dataframe with two columns: term (filled with the terms...)
# and value (their corresponding sentiment score
roots_for_group <- function(annotated_data, translation_list, senti_data) {
  # extract roots
  roots <- annotated_data %>%
    subset(sentence %in% subset(., dep_rel == 'nsubj' & tolower(token) %in% unlist(translation_list[doc_id]))$sentence) %>%
    subset(dep_rel == 'root')
  
  # extract relevant information from "feats" column
  roots <- roots[c(1, 6:7, 10)] %>%
    featsinfo_to_column('Tense') %>%
    featsinfo_to_column('Mood') %>%
    featsinfo_to_column('VerbForm')
  
  roots <- roots[-c(2,4)]  # cut out feats and token
  
  roots$senti_ws <- '-'  # add sentiment column
  for (w in unique(roots$lemma)) {
    if (w %in% senti_data$term) {
      if (length(senti_data[senti_data$term == w,]$value) < 2) {
        roots[roots$lemma == w,]$senti_ws <- senti_data[senti_data$term == w,]$value
      }
    }
  }
  roots <- roots[2:6]
  
  roots <- aggregate(cbind(roots[0],numdup=1), roots, length)
  roots <- roots[order(roots$numdup, decreasing = TRUE),] %>%
    filter(numdup > 2)
  names(roots)[names(roots) == 'numdup'] <- 'freq'
  
  return(roots)
}