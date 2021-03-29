library(quanteda)
library(readtext)
library(tidyverse)
library(udpipe)

source("functions/lemmatize.R")

# Load model necessary for lemmatization and tagging. 
model_deutsch <- udpipe_load_model(file="data/german-gsd-ud-2.5-191206.udpipe")

# Add custom stopwords
custom_stops <- c(stopwords("german"), c(""," ", "|","dass", "dabei", "dafür", "sowie", "daher"))
# OR:
load("RData/custom_stopwords.RData")


# Working directory needs to be set to same directory as corpus directory for this to work!
# Read in files, set document level variables.
programs_texts <- readtext("Korpus-Dateien", 
                           docvarsfrom = "filenames",
                           docvarnames = c("type", "party", "year"),
                           dvsep = "-",
                           encoding="utf-8")

# Lemmatize texts
programs <- lemmatize(programs_texts, model_deutsch) %>% corpus()
# OR load:
load("RData/lemmatized_corpus.RData")

# Convert characters in year column to integers
docvars(programs, field="year") <- as.integer(docvars(programs, field="year"))

# Create tokens object for whole corpus. filter out stopwords.
program_toks <- tokens(programs,remove_punct = TRUE) %>% tokens_remove(custom_stops)

# Create dfm for corpus
program_dfm <- dfm(program_toks)

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
                   "atmosphäre",
                   "kohlenstoffdioxid",
                   "emission*")

####################################
#####Frequency of climate terms ####
####################################

klima.party <- dfm(program_dfm, select = climate_dict)

## Plot frequency of each climate terms.
ggplot(textstat_frequency(klima.party, groups="party")) + 
  geom_bar(aes(fill=group, y=frequency, x=feature),position="stack", stat="identity")+
  ggtitle("Häufigkeit der Klimabegriffe")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "Terme", y = "Häufigkeit", fill = "Partei")+
  scale_fill_manual(values = c("blue", "#009933", "black", "red", "#FFFF00", "brown","#CC0066"))


klima.year <- textstat_frequency(dfm(program_dfm, select = climate_dict), groups=c("party", "year"))
# Add columns for parties and year
klima.year$party <- lapply(strsplit(klima.year$group, "[.]"), function(l) l[[1]])
klima.year$year <- lapply(strsplit(klima.year$group, "[.]"), function(l) l[[2]])
klima.year$year <- as.character(klima.year$year)
klima.year$party <- as.character(klima.year$party)

# Plot frequency of terms over years.
ggplot(klima.year) + 
  geom_bar(aes(y=frequency, x=year, fill = party), 
           position="stack", 
           stat="identity")+
  ggtitle("Klimabegriffe über Jahre")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Frequenz", x = "Jahre", fill = "Partei")+
  scale_fill_manual(values = c("blue", "#009933", "black", "red", "#FFFF00", "brown","#CC0066"))



#??????????????Maybe????????????????
####################################
# Kontextwords for climate terms ###
####################################

context.all <- kwic(program_toks, pattern = climate_dict) %>% corpus() %>% dfm()
top.30 <- head(textstat_frequency(context.all), 30)
parties <- c("AfD", "CDU", "SPD", "PDS", "FDP", "DIELINKE", "B90dieGruene")
terms.ranked <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(terms.ranked) <- c("feature", "party", "frequency")

for (i in 1:length(parties)){
  tokens.kwic <- tokens_subset(program_toks, party == parties[i]) %>%
    kwic(pattern = climate_dict) %>% corpus() %>% dfm() %>% textstat_frequency()
  x <- ifelse(top.30$feature %in% tokens.kwic$feature, tokens.kwic$frequency, 0)
  y <- data.frame(freq = x, feat = top.30$feature)
  tmp.data <- data.frame(feature=y$feat,
                         party=parties[i],
                         frequency=y$freq)
  terms.ranked <- rbind(terms.ranked, tmp.data)
}

# AfD - Contect Words
kwic.afd <- tokens_subset(program_toks, party == "AfD") %>%
  kwic(pattern = climate_dict) %>% corpus() %>% dfm() %>% textstat_frequency() %>% head(20)


ggplot(kwic.afd, aes(x=feature, y=frequency)) +
  geom_segment( aes(x=kwic.afd$feature, xend=kwic.afd$feature, y=0, yend=frequency), color="black") +
  geom_point( color="darkgreen", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(x ="Term", y="Frequenz")+
  ggtitle("AfD - Kontextwörter zu Klimabegriffen")

ggsave("plots/climate_context_afd.png")

# CDU - Contect Words
kwic.cdu<- tokens_subset(program_toks, party == "CDU") %>%
  kwic(pattern = climate_dict) %>% corpus() %>% dfm() %>% textstat_frequency() %>% head(20)


ggplot(kwic.cdu, aes(x=feature, y=frequency)) +
  geom_segment( aes(x=feature, xend=feature, y=0, yend=frequency), color="darkgreen") +
  geom_point( color="darkgreen", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(x ="Term", y="Frequenz")+
  ggtitle("CDU - Kontextwörter zu Klimabegriffen")



# LINKE - Contect Words
kwic.linke<- tokens_subset(program_toks, party == "DIELINKE") %>%
  kwic(pattern = climate_dict) %>% corpus() %>% dfm() %>% textstat_frequency() %>% head(20)


ggplot(kwic.linke, aes(x=feature, y=frequency)) +
  geom_segment( aes(x=feature, xend=feature, y=0, yend=frequency), color="darkgreen") +
  geom_point( color="darkgreen", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(x ="Term", y="Frequenz")+
  ggtitle("DIE LINKE - Kontextwörter zu Klimabegriffen")


# Heatmap
# ggplot(terms.ranked, aes(y=feature, x=party)) + 
#   geom_tile(aes(fill = frequency)) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
#   ggtitle("Absolute Häufigkeit der Kontextwörter zum Klima")+
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 15)+
#   labs(x="Partei", y = "Term", fill ="Absolute Häufigkeit")




