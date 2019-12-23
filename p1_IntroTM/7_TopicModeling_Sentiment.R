#' Title: Intro: Topic Modeling, Sentiment and Length Treemap
#' Purpose: Construct a treemap showing multiple information levels
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2017-10-24
#'

# Set the working directory
setwd("C:/Users/JP/Desktop/ODSC-R-Text-Mining/ODSC-master/workshop_data")

# Libs
library(treemap)
library(portfolio)
library(qdap)
library(GuardianR)
library(lda)
library(LDAvis)
library(tm)
library(SnowballC)
library(pbapply)
library(dplyr)

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
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  #corpus <- tm_map(corpus, content_transformer(replace_symbol)) #removed 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# function to remove blanks - needed to keep LDA from failing
blankRemoval<-function(x){
  x<-unlist(strsplit(x,' '))
  x<-subset(x,nchar(x)>0)
  x<-paste(x,collapse=' ')
}

docAssignment<-function(x){
  x<-table(x)
  x<-as.matrix(x)
  x<-t(x)
  x<-max.col(x)
}



# Create custom stop words
customStopwords <- c(stopwords('english'), 'pakistan', 'gmt','pm')

## Get Text 
#apiKey<-'GUARDIAN_KEY'
#text <- get_guardian("Pakistan", 
#                     from.date="2015-11-01",to.date="2015-11-08",
#                     api.key=apiKey)
#
## Data
#saveRDS(text, "text.rds")

# Data
text <- readRDS("Guardian_text.rds")

# String clean up 
body<-iconv(text$body, "latin1", "ASCII", sub="")
body<-gsub('http\\S+\\s*', '', body) #remove URLs
body<-bracketX(body, bracket="all") #remove strings in between parenteses
body<-replace_abbreviation(body) # replaces a.m. to AM etc

# Organized cleaned text
textBody<-data.frame(id=text$id,text=body)

# Keep the meta data, apply the functions to make a clean corpus
customReader <- readTabular(mapping=list(content="text", id="id"))
txtCorpus <- VCorpus(DataframeSource(textBody), 
                     readerControl=list(reader=customReader))
txtCorpus<-cleanCorpus(txtCorpus)

# Extract plain text
plainTxt<-unlist(sapply(txtCorpus, `[`, "content"))

# Remove any blanks, happens sometimes w/tweets
txt<-pblapply(plainTxt,blankRemoval)

# Lexicalize
txtLex<-lexicalize(txt)
str(txtLex) #print out
txtWordCount<-word.counts(txtLex$documents, txtLex$vocab)
txtDocLength<-document.lengths(txtLex$documents)
#CAN PRINT BY HIGHLIGHTING JUST THE VARIABLE NAME AND HIT Run

# LDA Topic Modeling
k <- 5  #find 5 topics
num.iter <- 25 
alpha <- 0.02 
eta <- 0.02
set.seed(1234) 
fit <- lda.collapsed.gibbs.sampler(documents = txtLex$documents, 
                                   K = k, 
                                   vocab = txtLex$vocab, 
                                   num.iterations = num.iter, 
                                   alpha = alpha, 
                                   eta = eta, 
                                   initial = NULL, 
                                   burnin = 0,
                                   compute.log.likelihood = TRUE)


# Prototypical Document
top.topic.documents(fit$document_sums,1)

# LDAvis params
theta <- t(pbapply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(pbapply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

ldaJSON <- createJSON(phi = phi,
                      theta = theta, 
                      doc.length = txtDocLength, 
                      vocab = txtLex$vocab, 
                      term.frequency = as.vector(txtWordCount))

serVis(ldaJSON)

# Topic Extraction
top.topic.words(fit$topics, 10, by.score=TRUE)

# Name Topics
topFive<-top.topic.words(fit$topics, 5, by.score=TRUE)
topFive<-apply(topFive,2,paste, collapse=' ')

# Topic fit for first 10 words of 2nd doc
fit$assignments[[2]][1:10]

# Get numeric assignments
topicAssignments<-unlist(pblapply(fit$assignments,docAssignment))


# Recode to the top words for the topics
length(topicAssignments)
topicAssignments
assignments<-recode(topicAssignments, topFive[1], topFive[2], 
                    topFive[3],topFive[4],topFive[5])

# Polarity calc
txtPolarity<-polarity(plainTxt)[[1]][3]

# Final Organization
# Squares of color
allTree<-data.frame(topic=assignments, 
                    polarity=txtPolarity,
                    length=txtDocLength)

set.seed(1237)
treemap(allTree,index=c("topic","length"),
        vSize="length",vColor="polarity", type="value", 
        title="Guardan Articles mentioning Pakistan",
        palette=c("red","white","green"))

map.market(id=seq(1:nrow(allTree)),
           area=allTree$length, 
           group=allTree$topic, 
           color=allTree$polarity, 
           main="Sentiment/Color, Length/Area, Group/Topic")

# End