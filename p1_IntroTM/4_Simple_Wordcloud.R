#' Title: Intro: Simple Wordcloud
#' Purpose: Learn about wordclouds and make one
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2017-10-24
#' 

# Set the working directory
setwd("C:/Users/JP/Desktop/ODSC-R-Text-Mining/ODSC-master/workshop_data")

# Libs
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) #new: isn't to is not
  corpus <- tm_map(corpus, content_transformer(replace_symbol)) #new: @ to "at"
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
customStopwords <- c(stopwords('english'), 'lol', 'smh', 'chardonnay')

#bigram token maker
bigramTokens <-function(x)
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

# Data
text<-read.csv('chardonnay.csv', header=TRUE)

# Keep the meta data, apply the functions to make a clean corpus
customReader <- readTabular(mapping=list(content="text", id="id"))
txtCorpus <- VCorpus(DataframeSource(text), readerControl=list(reader=customReader))
txtCorpus<-cleanCorpus(txtCorpus)

# # Make bi-gram TDM
wineTDM<-TermDocumentMatrix(txtCorpus, control=list(tokenize=bigramTokens))
wineTDMm <- as.matrix(wineTDM)

# See a bi-gram
grep('wine country', rownames(wineTDMm))
wineTDMm[4849:4850,870:871]

# Get Row Sums
wineTDMv <- sort(rowSums(wineTDMm),decreasing=TRUE)
wineDF <- data.frame(word = names(wineTDMv),freq=wineTDMv)

# Review all Pallettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(wineDF$word,wineDF$freq,max.words=50, random.order=FALSE, colors=pal)

# End