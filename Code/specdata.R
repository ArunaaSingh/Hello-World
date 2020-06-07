
pollutantmean <- function( directory, pollutant, id = 1:332){
  
  means <- c()
  for(monitor in id){
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
    monitor_data <- read.csv(path)
    interested_data <- monitor_data[pollutant]
    means <- c(means, interested_data[!is.na(interested_data)])
  }
  
  mean(means)
}

pollutantmean("specdata", "nitrate")

complete <- function(directory, id=1:332){
  
  results <- data.frame(id=numeric(0), nobs=numeric(0))
  for(monitor in id){
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
    monitor_data <- read.csv(path)
    
    interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
    interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
    
    nobs <- nrow(interested_data)
    results <- rbind(results, data.frame(id=monitor, nobs=nobs))
  }
  results
}
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

complete("specdata", c(1,2,3,4,5))
complete_cases <- complete(directory)
nrow(complete_cases)

?cor

corr <- function(directory, threshold = 0){
  result <- numeric(0)
  complete_cases <- complete(directory)
  complete_cases <- complete_cases[complete_cases$nobs>=threshold,]
  
  if(nrow(complete_cases) > 0){
    for(monitor in complete_cases$id){
      path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
      monitor_data <- read.csv(path)

      independent_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
      independent_data <- independent_data[(!is.na(independent_data$nitrate)), ]
      
      result <- c(result, cor(independent_data["sulfate"], independent_data["nitrate"]))
    }
    result
  }
  else{
    print("No data found")
  }
}
cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
print(out)