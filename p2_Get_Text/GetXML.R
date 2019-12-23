#' Title: Get Text : XML example
#' Purpose: Grab some text from an XML file for instruction
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2017-10-27
#' 

# Libs
library(xml2)
library(stringr)

# Chrome>Right-Click>Inspect>Network>"timedtext", right-click to open in new tab
url<-'https://www.youtube.com/api/timedtext?key=yttt1&sparams=caps%2Cv%2Cexpire&expire=1509111926&v=34Na4j8AVgA&caps&hl=en_US&signature=733EEA3F50F7B0CA7AE01DE1CB53D0E26B12F3A6.EA855C62A273BF3E312AB5B7E8613E4C28189171&lang=en&fmt=srv3'

# Read in the closed caption info
x<-read_xml(url)

# Extract text, remove carriage returns, remove special characters
text<-xml_text(x)
text<-str_replace_all(text, "[\r\n]" , "")
text<-iconv(text, "latin1", "ASCII", sub="")

# Save
writeLines(text,'~/workshop_data/Weeknd.txt')

# End
