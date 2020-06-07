#####################Week2#########################################################

#Q4

require(httr);
require(XML)

url = url("C:\Users\aruna.singh\Desktop\Rebl.txt")

lines = readLines(url)

nchar(lines[10])

fileName <- "C:/Users/aruna.singh/Desktop/Rebl.txt"  #it reads charater present wrt to lines, space is included
con <- file(fileName,open="r")
line <- readLines(con)
nchar(line[10])


#Q5

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
widths <- c(1, 9, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3)
fixed <- read.fwf(url, widths, header = FALSE, skip = 4)
sum(fixed$V8)
nrow(fixed)

#####################Week3#########################################################
#Q1

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "acs.csv")

acs <- read.csv("acs.csv")
head(acs)

rowgten <- acs$ACR == 3
rowgten

is.data.frame(acs)

logic <- acs$ACR ==3 & acs$AGS == 6
which(logic)

#Q2

install.packages("jpeg")

library(jpeg)

jpegUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"

download.file(jpegUrl, destfile = "./data/image.jpeg", mode="wb")

image <- readJPEG("./data/image.jpeg", native= TRUE) 
#native = TRUE means raster represtation, FALSE means vector representation

quantileResult <- quantile(image, probs = c(0.30, 0.80))

quantileResult

#Q3

url1 = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"

##Load the educational data from this data set:
  
url2 = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

file1 = read.csv(url1, skip=4, nrows=190)
newData <- file1[, c(1, 2, 4,5)]
head(newData,10)

colnames(newData) <- c("CountryCode", "Ranking", "Economy", "GDP")
file2 = read.csv(url2)

names(file2)

mergedData =merge(newData, file2, by = "CountryCode", all = FALSE)

mergedData$Special.Notes
names(mergedData)

head(mergedData)
sorted<- arrange(mergedData,desc(Ranking))

mergedData[13,'Economy']

#Q4
install.packages("dplyr")

library(dplyr)

mergedData
groupedDF <- group_by(mergedData, Income.Group)  # will consider income.group explicitly
groupedDF

avgRankings<- summarize(groupedDF, agvGDP = mean(Ranking)) #split the income group by summarizing

avgRankings
filter(avgRankings, Income.Group %in% c('High income: nonOECD', 'High income: OECD'))



#Q5
head(mergedData)
library(Hmisc)

install.packages("Hmisc")
quantileRanking <- cut2(mergedData$Ranking, g=5)
quantileRanking

table(quantileRanking, mergedData$Income.Group)


##############Week4


strsplit()
sub()  # for replacing first underscore
gsub() # for replacing mulpliple underscore
grep() #to find values

##########Quiz 4

#Q1. 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "acs.csv")

acs <- read.csv("acs.csv")
names(acs)

?strsplit
strsplit(names(acs), "wgtp")

#Q2.

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
file1 = read.csv(fileUrl, skip=4, nrows=190)
newData <- file1[, c(1, 2, 4,5)]
head(newData,10)

colnames(newData) <- c("CountryCode", "Ranking", "Economy", "GDP")

GDP<- gsub( ",", "", newData$GDP)
mean(as.integer(GDP))


#Q3

x <- length(grep("^United", newData$Economy))

x

#Q4
x <- length(grep("Fiscal year end: June 30;", mergedData$Special.Notes))

x

#Q5

install.packages("quantmod")
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

timeDT <- data.table::data.table(timeCol = sampleTimes)

# How many values were collected in 2012? 
timeDT[(timeCol >= "2012-01-01") & (timeCol) < "2013-01-01", .N ]
# Answer: 
# 250

# How many values were collected on Mondays in 2012?
timeDT[((timeCol >= "2012-01-01") & (timeCol < "2013-01-01")) & (weekdays(timeCol) == "Monday"), .N ]
#