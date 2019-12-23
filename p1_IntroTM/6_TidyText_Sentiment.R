#' Title: Intro: TidyText Sentiment
#' Purpose: Construct a treemap showing multiple information levels
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2017-10-24
#'

# Set the working directory
setwd("C:/Users/JP/Desktop/ODSC-R-Text-Mining/ODSC-master/workshop_data")

# Libs
library(tidytext)
library(dplyr)
library(qdap)
library(tm)
library(radarchart)

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
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}


# Data
text<-readLines('Weeknd.txt')
text  #output text

# Create custom stop words
customStopwords <- c(stopwords('english'))

# Clean Corpus
txtCorpus <- VCorpus(VectorSource(text))
txtCorpus<-cleanCorpus(txtCorpus)

# DTM Document Term Matrix
txtDTM<-DocumentTermMatrix(txtCorpus)
txtDTM  #output
dim(txtDTM)

# Tidy
tidyCorp<-tidy(txtDTM)
tidyCorp
dim(tidyCorp)

# Get bing lexicon
# "afinn", "bing", "nrc", "loughran"
bing<-get_sentiments(lexicon = c("bing"))
head(bing)

# join by word and term
# Perform Inner Join
bingSent<-inner_join(tidyCorp,bing, by=c('term'='word'))
bingSent

# Quick Analysis
table(bingSent$sentiment)

# Compare with Polarity
# polarity looks at clusters - looks 4 words ahead.  so not very good - not and very cancel out
polarity(text)

# Get afinn lexicon
afinn<-get_sentiments(lexicon = c("afinn"))
head(afinn)

# Perform Inner Join
afinnSent<-inner_join(tidyCorp,afinn, by=c('term'='word'))
afinnSent

# Quick Analysis
summary(afinnSent$score)
plot(afinnSent$score, type="n", main="Quick Timeline of Identified Words") 
lines(afinnSent$score)

# Get nrc lexicon
# in this one words can be multi-counted b/c can have more than one label
nrc<-get_sentiments(lexicon = c("nrc"))
head(nrc)

# Perform Inner Join
nrcSent<-inner_join(tidyCorp,nrc, by=c('term'='word'))
nrcSent

# Quick Analysis - Spider chart
table(nrcSent$sentiment)
emos<-data.frame(table(nrcSent$sentiment))
chartJSRadar(scores=emos)

# End

