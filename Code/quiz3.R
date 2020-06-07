best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("C:/Users/aruna.singh/Desktop/r-projects/Data/Hospital/outcome-of-care-measures.csv")
  
  ## Get the columns below from 'data' and place it in 'data' with new names ("name", "state", "heart attack", etc)
  #"Hospital.Name"                                              
  #"State"                                                     
  #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
  #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  #"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"    
  
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  head(data)
  
  ## All of the possible outcome strings
  outcomes = c("heart attack", "heart failure", "pneumonia")
  
  ## Check if outcome is one of the strings in outcomes
  ## %in% is a more intuitive interface as a binary operator, which returns a logical vector indicating if there is a match or not
  if( tolower(outcome) %in% outcomes == FALSE ) {
    stop("invalid outcome")
  }
  ## Get a vector of all of the states in 'data' now at column 2, NOTE: could've also used data["state"]
  states <- data[, 2]
  states <- unique(states)
  if( state %in% states == FALSE ) {
    stop("invalid state")
  }
  
  data <- data[((data$state == state) & (data[tolower(outcome)] != "Not Available")), ]
  
  vals <- data[,tolower(outcome)]
  
  rowname<- which.min(vals)
  
  data[rowname, ]$name
}

best("SC", "heart attack")

best("NY", "pneumonia")

best("AK", "pneumonia")

rankhospital <- function(state, outcome, num = "best"){
  
  ## Read outcome data
  data <- read.csv("C:/Users/aruna.singh/Desktop/r-projects/Data/Hospital/outcome-of-care-measures.csv")
  
  ## All of the possible outcome strings
  outcomes = c("heart attack", "heart failure", "pneumonia")
  
  states = data[,7]
  
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  if(state %in% states == FALSE){
    stop("invalid state")    
  }
  
  else if(outcome %in% outcomes == FALSE){
    stop("invalid outcome")
  }
  
  else{
    
    dataPerState <- split(data, data$state)
    
    stateWiseData <- dataPerState[[state]] 
    
    dataOutcome <- suppressWarnings(as.numeric(stateWiseData[, outcome]))
    
    good <- complete.cases(dataOutcome)
    
    dataOutcome <- dataOutcome[good]
    
    stateWiseData <- stateWiseData[good, ]
    
    stateWiseData <- stateWiseData[order(dataOutcome, stateWiseData["name"]), ]
    
    if (grepl("^[0-9]+$", num)) {
      if (as.numeric(num) > length(dataOutcome)) {
        result <- NA
      }
      else {
        result <- stateWiseData[as.numeric(num), "name"]
      }
    }    
    else if (num == "best") {
      result <- stateWiseData[1, "name"]
    }
    else if (num == "worst") {
      result <- stateWiseData[length(dataOutcome), "name"]
    }
    else result <- NA
  }  
  
  result
}

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)

rankhospital("TX", "pneumonia", 10)

rankhospital("NY", "heart attack", 7)
rankall<- function(outcome, num = "best") {
  
  dataAll <- data.frame(hospital = character(), state = character())
  
  numState <- c()
  ## Read outcome data
  data <- read.csv("C:/Users/aruna.singh/Desktop/r-projects/Data/Hospital/outcome-of-care-measures.csv")
  
  ## All of the possible outcome strings
  outcomes = c("heart attack", "heart failure", "pneumonia")
  
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  
  if(outcome %in% outcomes == FALSE){
    stop("invalid outcome")
  }
  
  else{
    
    
    dataPerState <- split(data, data$state)
    
    for( stat in names(dataPerState)){
      
      stateWiseData <- dataPerState[[stat]]
      dataOutcome <- suppressWarnings(as.numeric(stateWiseData[, outcome]))
      
      good <- complete.cases(dataOutcome)
      
      dataOutcome <- dataOutcome[good]
      
      stateWiseData <- stateWiseData[good, ]
      
      stateWiseData <- stateWiseData[order(dataOutcome, stateWiseData["name"]), ]
      
      if(as.numeric(num) == TRUE){
        print("Aruna")
        numState <- c(1) 
      } 
      else if (num == "best"){
        numState <- 1
      }
      else if(num == "worst"){
        numState <- length(dataOutcome)
      }
      
      dataPart <- data.frame(hospital = stateWiseData[numState, "name"], state = stat, row.names = stat)
      
    }
  }
  
  
  dataPart
  
} 

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

rankall <- function(outcome,num='best') {
  
  if(is.character(num)){
    if (num %in% c('best','worst')==FALSE){
      stop('Invalid Rank')
    } }
  
  
  data <- read.csv("C:/Users/aruna.singh/Desktop/r-projects/Data/Hospital/outcome-of-care-measures.csv")
  
  
  if (outcome %in% c("heart failure","heart attack","pneumonia")==FALSE){
    stop("invalid outcome")}
  
  
  
  
  if (outcome %in% c("heart failure","heart attack","pneumonia")){
    if(outcome=="heart attack"){
      a<-data[,c(2,7,11)]
    }
    
    if(outcome=="heart failure"){
      a<-data[,c(2,7,17)]
    }
    
    if(outcome=="pneumonia"){
      a<-data[,c(2,7,23)]
    }}
  
  
  a[,3]<-as.numeric(a[,3])
  
  
  colnames(a)<-cbind('hospital','state','value')
  
  states<-sort(unique(a$state))
  
  b<-split(a,a$state)
  
  ans_state<-c()
  ans_hospital<-c()
  
  
  
  
  for (letter in states){
    
    
    g<-b[[letter]]
    
    g<-g[,c(1,3)]
    
    if (num=='best'){
      
      d<- c[which(g[,2]==min(g[,2],na.rm=TRUE)),]
      
      hospitals<-d$hospital
      hospital<-sort(hospitals)[1]
      
    }
    
    if (num=='worst'){
      
      d<- g[which(g[,2]==max(g[,2],na.rm=TRUE)),]
      
      hospitals<-d$hospital
      hospital<-sort(hospitals)[1]
      
    }
    
    if(is.numeric(num) & num < length(g$hospital)){
      
      
      l<-g[!is.na(g[,2]),]
      ll<-unique(sort(l[,2]))
      
      m<-c()
      
      for ( i in 1:length(ll)){
        temp<-which(l[,2]==ll[i])
        m<-rbind(m,l[temp,])
      }
      
      k<-m[num,2]
      p1<-which(m[,2]<k)
      p<-which(m[,2]==k)
      
      num2<-num-length(p1)
      
      hospitals<-sort(m[p,1])
      
      hospital<-hospitals[num2]
    }
    
    if (is.numeric(num)){
      if (num > length(g$hospital)){ 
        hospital<-NA}}
    
    ans_state<-c(ans_state,letter)
    ans_hospital<-c(ans_hospital,hospital)
    
  }
  
  df = data.frame(ans_hospital, ans_state)       # df is a data frame 
  colnames(df)<-c('hospital','state')
  
  df
}
rankall("heart attack", 20)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

head(rankall("heart attack", 20), 10)

class(2)

rankhospital("WA", "heart attack", 7)