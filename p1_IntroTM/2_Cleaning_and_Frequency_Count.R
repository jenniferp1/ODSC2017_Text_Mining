#' Title: Intro: Cleaning and Frequency Count
#' Purpose: Learn some basic cleaning functions & term frequency
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2017-10-24
#'

# Set the working directory
setwd("C:/Users/JP/Desktop/ODSC-R-Text-Mining/ODSC-master/workshop_data")

# Libs
library(tm)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
customStopwords <- c(stopwords('english'), 'lol', 'smh')

# Data
text<-read.csv('coffee.csv', header=TRUE)

# Keep the meta data, apply the functions to make a clean corpus
# voatile corpus (VCorpus) means held in memory will lose if close R
customReader <- readTabular(mapping=list(content="text", id="id"))
txtCorpus <- VCorpus(DataframeSource(text), readerControl=list(reader=customReader))
txtCorpus<-cleanCorpus(txtCorpus)

# Check Meta Data
txtCorpus[[4]]
txtCorpus[[4]][1]
txtCorpus[[4]][2]

# Need to plain text cleaned copy?
df<-data.frame(text=unlist(sapply(txtCorpus, `[`, "content")), stringsAsFactors=F)
write.csv(df,'plain_coffee.csv',row.names = F)

# Compare 
df[4,]  #4th text contents
text$text[4]  #4th text and url

# Make a Document Term Matrix or Term Document Matrix depending on analysis
txtDtm<-DocumentTermMatrix(txtCorpus)
txtTdm<-TermDocumentMatrix(txtCorpus)
txtDtmM<-as.matrix(txtDtm)  #Document by Term Matrix
txtTdmM<-as.matrix(txtTdm)  #Term by document Matrix

# Examine
dim(txtDtmM)
txtDtmM[362:365,2960:2970]
dim(txtTdmM)
txtTdmM[2960:2970,362:365]

# End