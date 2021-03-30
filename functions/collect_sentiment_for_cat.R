# function to collect sentiment for category
# input: 
# annotated_data - (subset of) corpus, annotated with udpipe
# category...
# value...
# senti_data - a dataframe with two columns: term (filled with the terms...)
# and value (their corresponding sentiment score
collect_sentiment_for_cat <- function(annotated_data, category, value, senti_data) {
  # filter data
  index <- grep(category, colnames(annotated_data))
  filtered_data <- subset(annotated_data, annotated_data[,index] == value)
  filtered_data$senti_ws <- '-'
  senti_df <- filtered_data[c(7,8)]
  
  senti_df$senti_ws <- '-'  # add sentiment column
  for (w in unique(senti_df$lemma)) {
    if (w %in% senti_data$term) {
      if (length(senti_data[senti_data$term == w,]$value) < 2) {
        senti_df[senti_df$lemma == w,]$senti_ws <- senti_data[senti_data$term == w,]$value
      }
    }
  }
  # count words
  senti_df <- aggregate(cbind(senti_df[0],numdup=1), senti_df, length)
  senti_df <- senti_df[order(senti_df$numdup, decreasing = TRUE),] %>%
    filter(numdup > 1)
  names(senti_df)[names(senti_df) == 'numdup'] <- 'freq'
  return(senti_df[-2])
}
