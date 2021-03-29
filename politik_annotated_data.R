# This code explores the corpus 
# For explanation see exploration.Rmd
library(quanteda)
library(readtext)
library(seededlda)
library(udpipe)
library(tidyverse)
library(devtools)

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

# wir fordern - opposition?
# sammeln, was partei fordert

# next try
dl <- udpipe_download_model(language = "german")
udmodel_german <- udpipe_load_model(file = dl$file_model)

years <- unique(programs$year)

# afd 2013
afd_2013_raw <- udpipe_annotate(udmodel_german, x = programs[1]) %>%
  as.data.frame()

ve <- subset(afd_2013_raw, dep_rel == "root")
rots <- unique(ve$lemma)
mydf <- data.frame(rots, rep("NA", length(rots)))
colnames(mydf)[2] <- "count"
for (rot in rots) {
  noc <- length(subset(ve, lemma == rot)$lemma)
  mydf[mydf$rots == rot,][2] <- noc
}
head(mydf[order(mydf$count, decreasing = TRUE),], n = 20)


cats <- c('nouns', 'verbs', 'adjectives', 'adverbs', 'tf-idf')

###################################################################################
docs <- summary(programs)$Text

for (program in docs) {
  name <- gsub(".txt",".csv", program)  # define name for csv
  # annotate and data frame
  annotated_model <- udpipe_annotate(udmodel_german, x = programs[program]) %>%
    as.data.frame()
  
  # top roots
  roots_subset <- subset(annotated_model, dep_rel == "root")
  roots <- unique(roots_subset$lemma)
  roots_df <- data.frame(roots, rep("NA", length(roots)))
  colnames(roots_df)[2] <- "root_count"
  for (root in roots) {
    noc <- length(subset(roots_subset, lemma == root)$lemma)
    roots_df[roots_df$roots == root,][2] <- noc
  }
  root_top20 <- head(roots_df[order(as.numeric(roots_df$root_count), decreasing = TRUE),], n = 20)
  
  # top ADJ
  ADJ_subset <- subset(annotated_model, upos == "ADJ")
  ADJs <- unique(ADJ_subset$lemma)
  ADJ_df <- data.frame(ADJs, rep(0, length(ADJs)))
  colnames(ADJ_df)[2] <- "ADJ_count"
  for (ADJ in ADJs) {
    noc <- length(subset(ADJ_subset, lemma == ADJ)$lemma)
    ADJ_df[ADJ_df$ADJs == ADJ,][2] <- noc
  }
  ADJs_df_top20 <- head(ADJ_df[order(as.numeric(ADJ_df$ADJ_count), decreasing = TRUE),], n = 20)
  
  final_df <- data.frame(root_top20$roots, root_top20$root_count,
                         ADJs_df_top20$ADJs, ADJs_df_top20$ADJ_count)
  write.csv2(final_df, name)
}

###############################################################################

parties <- unique(programs$party)

for (stuff in parties[2]) {
  name = paste(stuff, ".csv")
  # set up data frame
  mydf <- data.frame(matrix(ncol=length(programs[programs$party == stuff]$year),
                            nrow=20,
                            dimnames=list(NULL, programs[programs$party == stuff]$year)))
  colnames(mydf) <- as.character(programs[programs$party == stuff]$year)
  # collect data
  for (y in programs[programs$party == stuff]$year) {
    annotated_model <- udpipe_annotate(udmodel_german, x = programs[programs$year == y]) %>%
      as.data.frame()

    # top roots
    roots_subset <- subset(annotated_model, dep_rel == "root")
    roots <- unique(roots_subset$lemma)
    roots_df <- data.frame(roots, rep("NA", length(roots)))
    colnames(roots_df)[2] <- "root_count"
    for (root in roots) {
      noc <- length(subset(roots_subset, lemma == root)$lemma)
      roots_df[roots_df$roots == root,][2] <- noc
    }
    root_top20 <- head(roots_df[order(as.numeric(roots_df$root_count), decreasing = TRUE),], n = 20)
    
    # fill in results
    y_char <- as.character(y)
    mydf[,y_char] <- root_top20$roots
    
  }
}
write.csv2(mydf, 'gruene_verben.csv')
######################################################################

dfm2 <- dfm(program_toks)
annotated_model <- udpipe_annotate(udmodel_german, x = programs) %>%
  as.data.frame()

regierung <- c('doc3', 'doc4', 'doc10', 'doc11', 'doc12', 'doc19', 'doc23', 'doc24', 'doc25', 'doc27')
opposition <- c('doc1', 'doc2', 'doc5', 'doc6', 'doc7', 'doc8', 'doc9', 'doc13', 'doc14', 'doc15', 'doc16', 'doc17', 'doc18', 'doc20', 'doc21', 'doc22', 'doc26')

sub_model_regierung <- subset(annotated_model, doc_id %in% regierung)
sub_model_opposotion <- subset(annotated_model, doc_id %in% opposition)

regierung_list <- list('doc3' = c('bündnis90/die grünen', 'wir'),
                       'doc4' = c('bündnis90/die grünen', 'wir'),
                       'doc10' = c('cdu', 'wir'),
                       'doc11' = c('cdu', 'wir'),
                       'doc12' = c('cdu', 'wir'),
                       'doc19' = c('fdp', 'wir'),
                       'doc23' = c('spd', 'wir'),
                       'doc24' = c('spd', 'wir'),
                       'doc25' = c('spd', 'wir'),
                       'doc27' = c('spd', 'wir'))

opposition_list <- list('doc1' = c('afd', 'alternative', 'wir'),
                        'doc2' = c('afd', 'alternative', 'wir'),
                        'doc5' = c('bündnis90/die grünen', 'bündnis90', 'grüne', 'wir'),
                        'doc6' = c('bündnis90/die grünen', 'bündnis90', 'grüne', 'wir'),
                        'doc7' = c('bündnis90/die grünen', 'bündnis90', 'grüne', 'wir'),
                        'doc8' = c('cdu', 'wir'),
                        'doc9' = c('cdu', 'wir'),
                        'doc13' = c('linke', 'wir'),
                        'doc14' = c('linke', 'wir'),
                        'doc15' = c('linke', 'wir'),
                        'doc16' = c('fdp', 'wir'),
                        'doc17' = c('fdp', 'wir'),
                        'doc18' = c('fdp', 'wir'),
                        'doc20' = c('fdp', 'wir'),
                        'doc21' = c('pds', 'wir'),
                        'doc22' = c('linkspartei.pds', 'wir'),
                        'doc26' = c('spd', 'wir'))


# für die opposition
wir_die_opposition <- subset(sub_model_opposotion, dep_rel == 'nsubj' & tolower(token) %in% unlist(opposition_list[doc_id]))

sents_opp_wir_list <- wir_die_opposition$sentence

wir_die_opposition_sents <- subset(sub_model_opposotion, sentence %in% sents_opp_wir_list)
wir_die_opp_roots <- subset(wir_die_opposition_sents, dep_rel == 'root')

opp_df <- wir_die_opp_roots[c(1, 6:7, 10)]

# tense
opp_df$tense <- '-'
opp_df_with_tense <- opp_df[grepl('Tense', opp_df$feats) == TRUE,]
opp_df_without_tense <- opp_df[grepl('Tense', opp_df$feats) == FALSE,]

opp_df_with_tense$tense <- sub(".*(Tense=)([A-Za-z]+)[|]?.*", "\\2", opp_df_with_tense$feats)

opp_df <- rbind(opp_df_with_tense, opp_df_without_tense)

# mood
opp_df$mood <- '-'
opp_df_with_mood <- opp_df[grepl('Mood', opp_df$feats) == TRUE,]
opp_df_without_mood <- opp_df[grepl('Mood', opp_df$feats) == FALSE,]

opp_df_with_mood$mood <- sub(".*(Mood=)([A-Za-z]+)[|]?.*", "\\2", opp_df_with_mood$feats)

opp_df <- rbind(opp_df_with_mood, opp_df_without_mood)

# verbform
opp_df$verbform <- '-'
opp_df_with_verbform <- opp_df[grepl('VerbForm', opp_df$feats) == TRUE,]
opp_df_without_verbform <- opp_df[grepl('VerbForm', opp_df$feats) == FALSE,]

opp_df_with_verbform$verbform <- sub(".*(VerbForm=)([A-Za-z]+)[|]?.*", "\\2", opp_df_with_verbform$feats)

opp_df <- rbind(opp_df_with_verbform, opp_df_without_verbform)

# cut out feats
opp_df <- opp_df[-c(2,4)]

# sentiment
# set up dicts
neg <- scan("SentiWS_v1.8c_Negative.txt", what = "char", sep = "\n", fileEncoding="utf-8")
pos <- scan("SentiWS_v1.8c_Positive.txt", what = "char", sep = "\n", fileEncoding="utf-8")
s <- str_split(neg, "\t")
t <- str_split(pos, "\t")
terms.neg <- sub("([A-Za-zß]+)[|][A-Za-z]+", "\\1",lapply(s, function(l) l[[1]]))
terms.pos <- sub("([A-Za-zß]+)[|][A-Za-z]+", "\\1",lapply(t, function(l) l[[1]]))

value.neg <- unlist(lapply(s, function(l) as.double(l[[2]])))
value.pos <- unlist(lapply(t, function(l) as.double(l[[2]])))

positive <- data.frame(term=terms.pos, value=value.pos)
negative <- data.frame(term=terms.neg, value=value.neg)

# add sentiment value
positive[positive$term == 'befürworten',]
negative[negative$term == 'befürworten',]
length(positive$term)

# the other dict
opp_df$senti_ws <- '-'

for (w in unique(opp_df$lemma)) {

  if (w %in% positive$term) {

    opp_df[opp_df$lemma == w,]$senti_ws <- as.character(positive[positive$term == w,]$value)
  }
  if (w %in% negative$term) {
    opp_df[opp_df$lemma == w,]$senti_ws <- as.character(negative[negative$term == w,]$value)
  }
}

# party
opp_df$party <- as.character(lapply(opposition_list[opp_df$doc_id], function(elem) elem[[1]]))

opp_df

# save to csv
write.csv2(opp_df, 'opposition_stuff1_with_party.csv')

# just terms
just_terms <- opp_df[2:6]

counted_terms <- aggregate(cbind(just_terms[0],numdup=1), just_terms, length)

counted_terms <- counted_terms[order(counted_terms$numdup, decreasing = TRUE),]

just_terms <- filter(counted_terms, numdup > 2)
names(just_terms)[names(just_terms) == 'numdup'] <- 'freq'
write.csv2(just_terms, 'opposition_roots_counted.csv')












