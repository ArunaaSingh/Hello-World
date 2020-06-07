library(jsonlite)

x <- iris[1:3,]
toJSON(x, pretty=TRUE)
stream_out(x)

myjson <- toJSON(iris, pretty=TRUE)
cat(myjson)

iris2 <- fromJSON(myjson)
head(iris2)

iris2


library(XML)
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileUrl, useInternal=TRUE)
rootNode <- xmlRoot(doc)
sum(xpathSApply(rootNode, "//zipcode", xmlValue)==21231)


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl, destfile="./data/microdata3.csv", method="curl")
DT <- fread("./data/microdata3.csv")
file.info("./data/microdata3.csv")$size
system.time(DT[,mean(pwgtp15),by=SEX])
system.time(mean(DT[DT$SEX==1,]$pwgtp15))+system.time(mean(DT[DT$SEX==2,]$pwgtp15))
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time(mean(DT$pwgtp15,by=DT$SEX))
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(rowMeans(DT)[DT$SEX==1])+system.time(rowMeans(DT)[DT$SEX==2])


microData <- read.csv(file.choose())
sum(!is.na(microData$VAL[microData$VAL==24]))


microData$VAL == 24   ## this will give boolean result then same will map to  dataset$VAL[filteredboolean]

(microData$FES[!is.na(microData$FES)])


download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx','./Q3.xlsx')
dat = read.xlsx('./Q3.xlsx',1,rowIndex = 18:23,colIndex = 7:15,header = TRUE)
sum(dat$Zip*dat$Ext,na.rm=T)



