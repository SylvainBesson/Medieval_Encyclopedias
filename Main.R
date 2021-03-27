library(data.table)
library(ggplot2)
library(plotly)
library(tidyr)

sourceDir<-paste0(getwd(),"/Documents/Master_Mondes_Medievaux/Memoire/data/TraditionManuscriptCSV")
listSourceFiles <- list.files(sourceDir)


#Extract Data
allBooks <- NULL
for(sourcefile in listSourceFiles) {
  bookName<-gsub("Tradition manuscrite: ", "", sourcefile)
  bookName<-gsub(".csv", "", bookName)
  bookData <- read.csv2(paste0(sourceDir, "/",sourcefile))
  setDT(bookData)
  bookData <- bookData[,bookName:=bookName]
  colnames(bookData)[1] <- "Côte"
  print(allBooks)
  if(!is.null(allBooks)){
    setDT(allBooks)
    for(colname in colnames(bookData)){
      if(!colname %in% colnames(allBooks)){
        allBooks <- allBooks[,eval(colname):="NULL"]
      }
    }
    for(colname in colnames(allBooks)){
      if(!colname %in% colnames(bookData)){
        bookData <- bookData[,eval(colname):="NULL"]
      }
    }
  }
  allBooks <- rbind(allBooks,bookData)
}


#View(allBooks)

allBooks$Date<-gsub("\n","",allBooks$Date)
allBooksDated<-allBooks[allBooks$Date != "NULL" & allBooks$Date != "" & allBooks$Date != "nul"]
allBooksDated$Date<-gsub(" ", "",allBooksDated$Date)
allBooksDated$Date<-gsub("[)(]*", "",allBooksDated$Date)
allBooksDated$Date<-gsub("--", "",allBooksDated$Date)
allBooksDated$Date<-gsub("peuaprès", "",allBooksDated$Date)
allBooksDated$Date<-gsub("±", "",allBooksDated$Date)
#Create column mean Date

setDT(allBooksDated)

allBooksDated[,meanDate:=sapply(Date, function(x) {
    if(grepl("-",x)){
      startDate <- gsub("-.*", "",x)
      endDate <- gsub(".*-", "",x)
      meanDate <- mean(c(as.numeric(startDate),as.numeric(endDate)))
      return(as.character(meanDate))
    } 
    else
    {
      return(x)
    }
  }
)] 

allBooksDated$meanDate<-as.numeric(allBooksDated$meanDate)


unique(allBooksDated$bookName)
# Histograms with every books century timeline 
p <- plotly::plot_ly(alpha = 0.6) %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "", allBooksDated$meanDate[allBooksDated$bookName=="Elucidarium"]), name = "Elucidarium") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Sidrac"]), name = "Sidrac") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Dragmaticon"]), name = "Dragmaticon") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Liber de natura rerum"]), name = "Liber de natura rerum") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Divisiones Mundi"]), name = "Divisiones Mundi") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Imago Mundi"]), name = "Imago Mundi") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Le livre des propriétés des choses"]), name = "Le livre des propriétés des choses") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Lucidaire"]), name = "Lucidaire") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Lumere as lais"]), name = "Lumere as lais") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Placides et Timéo"]), name = "Placides et Timéo") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Quaestiones naturales"]), name = "Quaestiones naturales") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Speculum naturale"]), name = "Speculum naturale") %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="Trésor"]), name = "Trésor") %>% 
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "",allBooksDated$meanDate[allBooksDated$bookName=="De proprietatibus rerum"]), name = "De proprietatibus rerum") %>%
  layout(title = "Nombre de copies par siècle et par oeuvre", xaxis=list(title="Siècle"), yaxis=list(title="Nombre de copies"))
print(p)



#Histograms with all books aggregated by century
p <- plotly::plot_ly(alpha = 0.6) %>%
  plotly::add_histogram(x = gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "", allBooksDated$meanDate)) %>%
  plotly::layout(barmode = "overlay", title = "Nombre de copies par siècle", xaxis=list(title="Siècle"), yaxis=list(title="Nombre de copies"))
print(p)
allBooksDated$Date

tmp<-allBooksDated


allBooksDated[,Date:=sapply(Date, function(x) {
  if(grepl("-",x)){
    startDate <- gsub("-.*", "",x)
    endDate <- gsub(".*-", "",x)
    meanDate <- mean(c(as.numeric(startDate),as.numeric(endDate)))
    if(as.numeric(meanDate) == as.numeric(startDate)){
      return(meanDate)
    }
    return(x)
  } 
  else
  {
    return(x)
  }
}
)]


allBooksCountDate <- NULL
setDT(allBooksCountDate)
setDT(allBooksDated)
for(i in 1:nrow(allBooksDated)) {
  if(grepl("-",allBooksDated[i]$Date)){
    currentDate<-allBooksDated[i]$Date
    startDate <- gsub("-.*", "",currentDate)
    endDate <- gsub(".*-", "",currentDate)
    dateRange <- as.numeric(endDate) - as.numeric(startDate)
    numberOfNewLines<- dateRange/20
    numberOfNewLines<-ceiling(numberOfNewLines)
    if(numberOfNewLines > 0){
      for(j in 0:numberOfNewLines-1){
        allBooksCountDate <- rbind(allBooksCountDate, cbind(allBooksDated[i], data.table(count=1/numberOfNewLines)))
        if((round(as.numeric(startDate), digits=-1 )/10)%%2== 1){
          allBooksCountDate[nrow(allBooksCountDate), Date := round(as.numeric(startDate), digits=-1) + 20 * j + 10]
        } else {
          allBooksCountDate[nrow(allBooksCountDate), Date := round(as.numeric(startDate), digits=-1) + 20 * j]
        }  
      }
      
    }
  } else {
    allBooksCountDate <- rbind(allBooksCountDate, cbind(allBooksDated[i], data.table(count=1)))
    if((round(as.numeric(allBooksCountDate[nrow(allBooksCountDate)]$Date), digits=-1 )/10)%%2== 1){
      allBooksCountDate[nrow(allBooksCountDate), Date := round(as.numeric(allBooksCountDate[nrow(allBooksCountDate)]$Date), digits=-1) + 10]
    } else {
      allBooksCountDate[nrow(allBooksCountDate), Date := round(as.numeric(allBooksCountDate[nrow(allBooksCountDate)]$Date), digits=-1)]
    }
  }
  
  if(is.na(allBooksCountDate[nrow(allBooksCountDate)]$count) || allBooksCountDate[nrow(allBooksCountDate)]$count == "NA") {
    print(allBooksCountDate[nrow(allBooksCountDate)]$Date)
    print(allBooksDated[i]$Date)
    print(allBooksCountDate[nrow(allBooksCountDate)]$count)
    stop()
  }
}

View(allBooksCountDate)


finalDS <- NULL
+setDT(finalDS)
finalDS=data.table(date=unique(allBooksCountDate$Date))
finalDS[,count:=sapply(date, function(x) {
  sum(allBooksCountDate$count[which(allBooksCountDate$Date==x)])
}
)]



finalDS<-finalDS[which(finalDS$date!="1401-1500")]
finalDS<-finalDS[which(finalDS$date!="980"&finalDS$date!="1000"&finalDS$date!="1020"&finalDS$date!="1040"&finalDS$date!="1060")]
p <- plotly::plot_ly(alpha = 0.6,x = paste0(finalDS$date,"-",as.numeric(finalDS$date)+19), y = finalDS$count, type = "bar") %>%
  plotly::layout(barmode = "overlay", xaxis=list(title="Période", tickangle = 75), yaxis=list(title="Nombre de copies"))
print(p)

