#PART 3
#Write a function that takes a directory of data files and a threshold for complete cases and calculates 
#the correlation between sulfate and nitrate for monitor locations where the number of completely observed 
#cases (on all variables) is greater than the threshold. The function should return a vector of
#correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold
#requirement, then the function should return a numeric vector of length 0.
## Creating a user-defined function to take directory of data files and threshold for complete cases 
## It calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold
work_dir <- "C:/Users/Aspire A314-32/Desktop/ACADS/ACADS T.T/STAT 197- Data Science/specdata"

setwd(work_dir)
corr <- function(directory, threshold = 0) {
  
  folderloc <- paste0(getwd(), "/", directory) ## Vector to store the location of the folder "specdata" the user would want to access
  
  crrltion <- NULL
  
  for (i in 1:332) {
    
    if (i < 10) {
      
      rwdata <- read.csv(paste0(folderloc, "/00", as.character(i), ".csv"),  ## To access csv files with folder names 001 to 009
                         as.is = TRUE,
                         header = TRUE)  
      
    }
    
    else if (i >= 10 & i < 100) {
      
      rwdata <- read.csv(paste0(folderloc, "/0", as.character(i), ".csv"),   ## Accessing files with names 010 to 099
                         as.is = TRUE,
                         header = TRUE)  
      
    }
    
    else {
      
      rwdata <- read.csv(paste0(folderloc, "/", as.character(i), ".csv"),    ## For files with folder names > 100
                         as.is = TRUE,
                         header = TRUE)  
      
    }
    
    newdata <- rwdata[complete.cases(rwdata), ]  ## A data frame with the computed number of cases with complete obserevations as the rows for the whole spec data directory
    
    if (nrow(newdata) > threshold) {             ## Checking if number of rows are greater than the defined value of threshold, 
      
      crrltion <- c(crrltion, cor(newdata[,"sulfate"], newdata[, "nitrate"]))   ## Concatenating crrltion and computed correlation between sulfate and nitrate for monitor locations into the variable crrltion
    }
    
  }
  
  return(crrltion)
  
}

#EXAMPLE
cr <- corr("specdata", 150)
head(cr); summary(cr)
cr <- corr("specdata", 400)
head(cr); summary(cr)
cr <- corr("specdata", 5000)
head(cr); summary(cr) ; length(cr)
cr <- corr("specdata") # default threshold value  is ZERO
head(cr); summary(cr) ; length(cr)