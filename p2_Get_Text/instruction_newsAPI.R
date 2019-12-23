#' Title: Get Text : API
#' Purpose: Grab some text from an API for instruction 
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2017-10-24
#' 

# Load Libraries
library(jsonlite)
library(pbapply)

# Options
options(stringsAsFactors = F)

# www.newsapi.org Key
apiKey<-'XXXXXXXXXXXXXXXXXXXXX'

# Examine a single API endpoint
url<-paste0('https://newsapi.org/v1/articles?source=techcrunch&apiKey=',apiKey) 
url

# Endpoint for all news sources
newsSources<-fromJSON('https://newsapi.org/v1/sources?language=en')

# Examine the response
str(newsSources[[1]])
str(newsSources[[2]])

# Example Loop
for (i in 1:length(newsSources$sources$id)){
  print('ok')
  print(newsSources$sources$id[i])
}

# Url construction
newsUrls<-vector()  
for (i in 1:length(newsSources$sources$id)){
  x<-paste0('https://newsapi.org/v1/articles?source=',
         newsSources$sources$id[i],
         '&apiKey=',apiKey)
  naming<-newsSources$sources$name[i]
  newsUrls[naming]<-x
}


# Examine some of the constructed urls
newsUrls[23]
newsUrls[47]

# GET request from each endpoint & examine 
allNews<-pblapply(newsUrls,fromJSON)  #progress bar
str(allNews[[1]])

# Another loop to append source to EACH article & examine
allDescriptions<-list()
for (i in 1:length(allNews)){
  x<-as.data.frame(allNews[[i]][4])
  x$source<-unlist(allNews[[i]][2])
  nam<-i
  allDescriptions[[i]]<-x
}

str(allDescriptions[[1]])

# Organize into a single df
newsDescriptions<-do.call(rbind,allDescriptions)

# Organize all the text & examine
#allNews<-do.call(rbind,allNews)
txt<-data.frame(id=newsDescriptions$source, text=newsDescriptions$articles.description)
txt[1,]

# Save a copy
write.csv(txt,'C:/Users/JP/Desktop/ODSC-R-Text-Mining/ODSC-master/workshop_data/news.csv', row.names=F)

# End
