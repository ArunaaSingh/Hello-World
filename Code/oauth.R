library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at
#    https://github.com/settings/developers. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("github",
                   key = "a92c5af184b3ef8bb9e3",
                   secret = "f4dc6674b6bb28e01db851d15a08fea577f9f1bc"
)

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
req <- GET("http://api.twitter.com/1.1/statuses/home_timeline.json", config(token = github_token))
stop_for_status(req)
output <- content(req)

datashare <- which(sapply(output, FUN=function(X) "datasharing" %in% X))
datashare

list(output[[15]]$name, output[[15]]$created_at)


install.packages("RMySQL")
library(sqldf)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl, destfile = "acs.csv")

acs <- read.csv("acs.csv")
head(acs)

library(RMySQL)
con <- dbConnect(MySQL(), host="127.0.0.1", port= 3306, user="user",
                 password = "password", dbname="db")

unique(acs$AGEP)

sqldf("select * from acs where AGEP<50")


library("swirl")
swirl::install_course("Getting and Cleaning Data").



