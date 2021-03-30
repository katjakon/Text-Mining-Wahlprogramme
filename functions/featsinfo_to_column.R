

# function to extract info from "feats"-column to separate column
featsinfo_to_column <- function(data, name) {
  data <- cbind(data, c('-'))
  names(data)[length(names(data))] <-  tolower(name)
  # separate data
  data_part1 <- data[grepl(name, data$feats) == TRUE,]
  data_part2 <- data[grepl(name, data$feats) == FALSE,]
  # extract data
  pattern <- paste0(".*(", name, "=)([A-Za-z]+)[|]?.*")
  data_part1[,ncol(data)] <- sub(pattern, "\\2",
                                 data_part1$feats)
  modified_data <- rbind(data_part1, data_part2)
  return(modified_data)
}
