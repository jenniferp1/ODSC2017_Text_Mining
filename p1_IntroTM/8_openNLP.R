#' Title: Intro: Named Entity Recognition
#' Purpose: Use OpenNLP to do NER
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2017-10-24
#'

# Set the working directory
setwd("C:/Users/JP/Desktop/ODSC-R-Text-Mining/ODSC-master/workshop_data")

# Libs
library(qdap)
library(openNLP)
library(tm)

# Options & Functions
options(stringsAsFactors = FALSE) 
Sys.setlocale('LC_ALL','C')

# Extract Entities
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

# Data 
text <- read.csv('chardonnay.csv')
textStr<-as.String(text$text)

# Preprocessing/Encoding 
textStr<-iconv(textStr, "latin1", "ASCII", sub="")
textStr<-gsub('http\\S+\\s*', '', textStr) #another way to remove URLs
textStr<-bracketX(textStr, bracket="all") #remove phrases in between brackets
textStr<-replace_abbreviation(textStr) # replaces a.m. to AM etc

# OpenNLP Annotators
persons <- Maxent_Entity_Annotator(kind = 'person')
locations <- Maxent_Entity_Annotator(kind = 'location')
organizations <- Maxent_Entity_Annotator(kind = 'organization')
sentTokenAnnotator <- Maxent_Sent_Token_Annotator(language = "en")
wordTokenAnnotator <- Maxent_Word_Token_Annotator(language = "en")
posTagAnnotator <- Maxent_POS_Tag_Annotator(language = "en")

# Annotate text - Error if don't use NLP:: since also a function in ggplot
# ??annotate to get info on function
annotations <- annotate(textStr,
                        list(sentTokenAnnotator,
                             wordTokenAnnotator,posTagAnnotator,
                             persons,locations,organizations))

## Good idea to save for a lot of documents
#saveRDS(annotations, "annotations.rds")
#annotations <- readRDS("annotations.rds")

# Crazy town
str(annotations[[1]]) #sentence
str(annotations[[6100]]) #PRP Personal pronoun
str(annotations[[1300]]) #NN=Noun, singular or mass

# Attach annotations back on the original text
textAnnotations<-AnnotatedPlainTextDocument(textStr,annotations)

people<-entities(textAnnotations, kind = "person")
locations<-entities(textAnnotations, kind = "location")
organization<-entities(textAnnotations, kind = "organization")

head(people)
peopleTable<-table(as.factor(people))
topPeeps<-peopleTable[peopleTable>5]
barplot(topPeeps)

# End
