#' Title: Intro: R Setup
#' Purpose: Install packages
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2017-10-24
#'

# Download the zip file repo here: https://github.com/kwartler/ODSC or 
# git clone https://github.com/kwartler/ODSC.git


# Get CRAN packages
libs<-c('stringi', #1_Keyword_Scanning.R
        'tm', #2_Cleaning and Frequency Count.R
        'qdap', #3_Dendrogram.R
        'ggplot2', 
        'ggthemes',
        'dendextend',
        'wordcloud', #4_Simple_Wordcloud.R & #5_Other_Wordclouds.R
        'RColorBrewer',
        'tidytext', #6_TidyText_Sentiment.R
        'treemap', #7_Topic_Modeling_Sentiment.R
        'portfolio',
        'GuardianR',
        'lda', #previous ODSC workshops used library(topicmodels)
        'LDAvis',
        'dplyr',
        'servr',
        'SnowballC',
        'openNLP', #8_Open_Langugage_Processingv3.R
        'jsonlite', #instruction_newsAPI.R
        'pbapply',
        'wordcloud2', #instruction_htmlWidgets.R
        'rbokeh',
        'radarchart',
        'stringr',
        'flexdashboard', #blank_dashboard.Rmd
        'rmarkdown', #cronJob.R
        'httr')

# Get most of the libs
install.packages(libs)


# WINDOWS Scheduler
install.packages('taskscheduleR')

# LINUX Scheduler
install.packages('cronR')

# Get a NLP Model for use w/openNLP
install.packages('openNLPmodels.en', 
                 repos = "http://datacube.wu.ac.at/", 
                 type = "source")

# End
