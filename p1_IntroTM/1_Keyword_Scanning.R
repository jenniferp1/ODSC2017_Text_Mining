#' Title: Intro: Keyword Scanning
#' Purpose: Learn some basic string manipulation
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2017-10-24
#'

# Set the working directory
setwd("C:/Users/JP/Desktop/ODSC-R-Text-Mining/ODSC-master/workshop_data")

# Libs - bringing in libraries
library(stringi)

# Options & Functions
options(stringsAsFactors = FALSE) #text strings will not be factors of categories
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

# Get Data
text<-read.csv('coffee.csv', header=TRUE)

# Logical T/F vector that a string appears at least ONCE
coffee<-grepl("coffee", text$text, ignore.case=TRUE)
starbucks<-grepl("starbucks", text$text, ignore.case=TRUE)

# Find the row positions of a specific word appearing at least ONCE
#this shows the difference between grep and grepl
grep("mug", text$text, ignore.case=TRUE)

# Grep for indexing
text[grep('mug', text$text),]

# Logical T/F for one word OR another appears at least ONCE
keywords<-"mug|glass|cup"
mugGlassCup<-grepl(keywords, text$text,ignore.case=TRUE)

# Calculate the % of times among all tweets
sum(coffee)/nrow(text)
sum(starbucks)/nrow(text)
sum(mugGlassCup)/nrow(text)

# Count occurences of words per tweet
theCoffee<-stri_count(text$text, fixed="the")
sum(theCoffee)/nrow(text)

#print part of array to Console
head(theCoffee,100)

# Example data export
keywordScans<-data.frame(text,coffee,starbucks,mugGlassCup)
write.csv(keywordScans,'grepl_example.csv', row.names=FALSE)

# End