#' Title: Intro: Frequency Count & Dendrogram
#' Purpose: Learn about and visualize a dendrogram
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2017-10-24
#'

# Set the working directory
setwd("C:/Users/JP/Desktop/ODSC-R-Text-Mining/ODSC-master/workshop_data")

#supress warning
suppressPackageStartupMessages(library(dendextend))

#pointing to Java
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_151') # for 64-bit version
Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_151') # for 32-bit version
library(rJava)

# Libs
library(qdap)
library(tm)
library(ggplot2)  #grammar graphics package
library(ggthemes) #easier to set aestectics than ggplot2
library(dendextend)

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
customStopwords <- c(stopwords('english'), 'lol', 'smh', 'beer')

# Dendogram coloring function
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

# Data 
text<-read.csv('beer.csv', header=TRUE)

# Keep the meta data, apply the functions to make a clean corpus
customReader <- readTabular(mapping=list(content="text", id="id"))
txtCorpus <- VCorpus(DataframeSource(text), readerControl=list(reader=customReader))
txtCorpus<-cleanCorpus(txtCorpus)

# Make TDM (term-document matrix)
beerTDM<-TermDocumentMatrix(txtCorpus)
beerTDMm<-as.matrix(beerTDM)

# Frequency Data Frame - - can sum rows / cols of dataframe
beerFreq<-rowSums(beerTDMm)
beerFreq<-data.frame(word=names(beerFreq),frequency=beerFreq)

# Simple barplot
topWords<-subset(beerFreq, beerFreq$frequency>=90)
topWords <- topWords[order(topWords$frequency, decreasing=F),]
topWords$word<-factor(topWords$word, levels=unique(as.character(topWords$word)))
ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

# qdap version
plot(freq_terms(text$text, top=35, at.least=2))

# Inspect word associations
associations<-findAssocs(beerTDM, 'zombie', 0.30)
str(associations)

# Make a dot plot
zombieDF<-data.frame(terms=names(associations[[1]]),
                     value=unlist(associations))
zombieDF$terms<-factor(zombieDF$terms, levels=zombieDF$terms)

ggplot(zombieDF, aes(y=terms)) +
  geom_point(aes(x=value), data=zombieDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust=-.25, size=3)

# Reduce TDM
beerTDM2 <- removeSparseTerms(beerTDM, sparse=0.97) #shoot for ~50 terms
beerTDM2<-as.data.frame(as.matrix(beerTDM2))

# Basic Hierarchical Clustering
hc <- hclust(dist(beerTDM2))
plot(hc,yaxt='n')

# Improved visual
hcd <- as.dendrogram(hc)
clusMember <- cutree(hc, 4)
labelColors <- c("#CDB380", "#036564", "#EB6841", "#EDC951")
clusDendro <- dendrapply(hcd, colLab)

plot(clusDendro, main = "Hierarchical Dendrogram", type = "triangle",yaxt='n')
rect.dendrogram(hcd, k = 5, border = "grey50")

# End