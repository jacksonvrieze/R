#install.packages("rvest")
library(rvest)
library(stringr)
#web address https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm
#class of interest
library(RCurl)
library(XML)
library(rjson)
#/newsevents/pressreleases/monetary20140917a.htm
#[[:punct:]][[:lower:]]{10}[[:punct:]][[:lower:]]{13}[[:punct:]][[:lower:]]{8}[[:digit:]]{6,8}[[:lower:]]{1}[[:punct:]][[:lower:]]{3}
setwd("C:/Users/Jackson/Documents/Marquette Classes/Thesis")



#https://fraser.stlouisfed.org/title/677/item/23254/content/pdf/20030506min
#https://fraser.stlouisfed.org/title/677/item/23267/content/pdf/20070918min
#https://fraser.stlouisfed.org/title/677/item/23297/content/pdf/fomcminutes20101214


#https://fraser.stlouisfed.org/files/docs/historical/FOMC/meetingdocuments/20070918min.pdf
#https://fraser.stlouisfed.org/files/docs/historical/FOMC/meetingdocuments/fomcminutes20091216.pdf
#https://fraser.stlouisfed.org/files/docs/historical/FOMC/meetingdocuments/fomcminutes20120313.pdf

html <- getURL("https://fraser.stlouisfed.org/title/677#23262")




############################3
#Get the html from the following website
#html <- getURL('https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm')
#write this file off to a document
write(html, file="fed.html")
#reread the html file back in and get each line to format how the html page looks like 
fedhtml<- readLines("fed.html")
#check to see how the document looks
#fedhtml

#/files/docs/historical/FOMC/meetingdocuments/fomcminutes20091216.pdf"
#[[:punct:]][[:lower:]]{14}[[:punct:]][[:lower:]]{5}[[:punct:]][[:lower:]]{11}[[:digit:]]{6,8}[[:punct:]]pdf'
#"/title/677/item/23219/content/pdf/FOMC19940816Agenda"
pattern <-'[[:punct:]]title[[:punct:]]677[[:punct:]]item[[:punct:]][[:digit:]]{5,6}[[:punct:]]content[[:punct:]]pdf[[:punct:]](FOMC[[:digit:]]{8}Agenda|[[:digit:]]{8}beige|[[:digit:]]{8}statement|BeigeBook[[:punct:]][[:digit:]]{8})'

has.data <-grep(x=fedhtml, pattern =pattern)
has.data <-has.data[!has.data <5307]
extenstions<- regmatches(fedhtml[has.data], gregexpr(pattern, fedhtml[has.data]))
fedpdfURL <-paste("https://fraser.stlouisfed.org", extenstions, sep = "")


#must go one html layer deeper


html2 <- getURL(fedpdfURL)
#write this file off to a document
write(html2, file="fed2.html")
#reread the html file back in and get each line to format how the html page looks like 
fedhtml2<- readLines("fed2.html")
#https://fraser.stlouisfed.org/title/677/item/23268/content/pdf/fomcminutes20071031
#https://fraser.stlouisfed.org/title/677/item/22531/content/pdf/20070807min
patternPDF <-'https[[:punct:]]{3}fraser[[:punct:]]stlouisfed[[:punct:]]org[[:punct:]]title[[:punct:]][[:digit:]]{3}[[:punct:]]item[[:punct:]][[:digit:]]{5,6}[[:punct:]]content[[:punct:]]pdf[[:punct:]](fomcminutes[[:digit:]]{8}|[[:digit:]]{8}min)'
has.dataPDF <-grep(x=fedhtml2, pattern =patternPDF)
extenstionsPDF<- regmatches(fedhtml2[has.dataPDF], gregexpr(patternPDF, fedhtml2[has.dataPDF]))




#/newsevents/pressreleases/monetary20140129a.htm"
#create the pattern that contains the important data
#pattern <-'[[:punct:]][[:lower:]]{10}[[:punct:]][[:lower:]]{13}[[:punct:]][[:lower:]]{8}[[:digit:]]{6,8}[[:lower:]]{1}[[:punct:]][[:lower:]]{3}'
#pattern <-'[[:punct:]][[:lower:]]{14}[[:punct:]][[:lower:]]{5}[[:punct:]][[:lower:]]{11}[[:digit:]]{6,8}a[[:digit:]]{1}[[:punct:]]pdf'
#pattern2<-'[[:punct:]][[:lower:]]{14}[[:punct:]][[:lower:]]{5}[[:punct:]][[:lower:]]{11}[[:digit:]]{6,8}[[:punct:]]pdf'
#get the line index of where the data is located in the thousands of lines


#has.data2 <-grep(x=fedhtml, pattern =pattern2)
#extenstions2<- regmatches(fedhtml[has.data2], gregexpr(pattern2, fedhtml[has.data2]))

#fedpdfURL <-paste("https://www.federalreserve.gov", extenstions2, sep = "")

#extenstionsPDF <- sub("[[:punct:]]", '                    ', extenstionsPDF)


extenstionsPDF<- str_replace_all(extenstionsPDF,"[[:punct:]]","                         ")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

dates<-substrRight(extenstionsPDF, 20)
dates<- str_replace_all(dates,"[[:space:]]","")
dates_for_Folder = dates
dates<-paste(dates,'.pdf', sep = "")

#fedpdfURL<-paste('https://fraser.stlouisfed.org/files/docs/historical/FOMC/meetingdocuments/',dates, sep = "")






setwd("C:/Users/Jackson/Documents/Marquette Classes/Thesis/minutes")




#download.file(fedpdfURL, 'fedminutes.pdf',mode="wb")
#dates<-dates[-(1)]


for(pdf in dates) {
  link <- paste0('https://fraser.stlouisfed.org/files/docs/historical/FOMC/meetingdocuments/',dates)
  download.file(link,pdf,mode='wb')
  dates<-dates[-(1)]
}






