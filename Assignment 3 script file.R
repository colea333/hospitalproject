
## importing our hospital dataset into R
outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("NA", "Not Available"))
View(outcomes)


## making a histogram of the 30-day death rates (column 11)
outcomes[, 11] <- as.numeric(outcome[, 11])
hist(outcome[,11])


## We want to make a function for finding the hospital with the best mortality rates in a given state

  ## We start by taking only the data we need, and renaming the columns to shorter, simpler names

rates <- as.data.frame(cbind(outcomes[, 2],  # hospital
                             outcomes[, 7],   # state
                             outcomes[, 11],  # heart attack mortality rate
                             outcomes[, 17],  # heart failure mortality rate
                             outcomes[, 23]),  # pneumonia
                             stringsAsFactors = FALSE)

colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")

View(rates)


## writing the function


best <- function(state, outcome) {
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
  
  rates <- as.data.frame(cbind(outcomes[, 2],  # hospital
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack mortality rate
                               outcomes[, 17],  # heart failure mortality rate
                               outcomes[, 23]),  # pneumonia
                         stringsAsFactors = FALSE)
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
        if(!state %in% rates[,"state"]){  ## these two if statements check if the state or outcome are valid
          stop('invalid state')
        }
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
          stop('invalid outcome')
  }
  ## Return hospital name in that state with lowest 30-day death rate
  
  ## create a data set that is a subset of the variable 'state' in the function input
  hRates <- rates[(rates[, "state"] == state), ] ## the second 'state' is the one referring to the variable in the function input
  
  ## Convert outcome rate to numeric
  hRates[, outcome] <- as.numeric(hRates[, outcome])
  
  ## Remove NA values
  hRates <- hRates[!is.na(hRates[, outcome]), ]
  
  ## Order by outcome rate
  hRates <- hRates[order(hRates[, outcome]), ]
  
  ## Get names of hospital with the lowest rate
  hNames <- hRates[hRates[, outcome] == min(hRates[,outcome]),1]
  
  ## Sort by hospital name if tie
  sort(hNames)[1]
  
}

best("TX", "heart attack")

#######################################################################



    ## the "num" argument can take the values "best", "worst", or an integer indicating the ranking.

rankhospital <- function(state, outcome, num = "best") {
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("NA", "Not Available"))
  
  rates <- as.data.frame(cbind(outcomes[, 2],  # hospital
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack mortality rate
                               outcomes[, 17],  # heart failure mortality rate
                               outcomes[, 23]),  # pneumonia
                         stringsAsFactors = FALSE)
  
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  if(!state %in% rates[,"state"]){  ## these two if statements check if the state or outcome are valid
    stop('invalid state')
  }
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
    
    ## create a data set that is a subset of the variable 'state' in the function input
    hRates <- rates[(rates[, "state"] == state), ] ## the second 'state' is the one referring to the variable in the function input
    
    ## Convert outcome rate to numeric
    hRates[, outcome] <- as.numeric(hRates[, outcome])
    
    ## Remove NA values
    hRates <- hRates[complete.cases(hRates), ]
    
    if(num == "best") {
        num <- 1
    }
    
    if(num == "worst") {
      num <- nrow(hRates)
    }
    
    ## Order by outcome rate
    hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
    
    ## Get names of hospital 
    
    hRates[num,1]
  }
  


rankhospital("TX", "heart failure", 1)

rankhospital("MD", "heart attack", "worst")


########################

rankall <- function(outcome, num) {
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("NA", "Not Available"))
  
  rates <- as.data.frame(cbind(outcomes[, 2],  # hospital
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack mortality rate
                               outcomes[, 17],  # heart failure mortality rate
                               outcomes[, 23]),  # pneumonia
                         stringsAsFactors = FALSE)
  
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
 if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
   stop("invalid outcome")
 }

  hRank <- data.frame()
  
  for(i in sort(unique(rates[, "state"]))) {
    
    hRates <- rates[(rates[, "state"]== i),]
    
    hRates[, outcome] <- as.numeric(hRates[, outcome])
    
    hRates <- hRates[!is.na(hRates[, outcome]), ]
    
    if(num == "best") {
      rnum <- 1
    } 
    else if(num == "worst") {
      rnum <- nrow(hRates)
    }
    else {
      rnum = num
    }
    
  hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
  
  hName <- hRates[rnum, 1]
  
 
  
  hRank <- rbind(hRank, data.frame(hospital = hName, state = i))
    
    
  }
  
  hRank
  
}

rankall("heart attack", "best")

tail(rankall("pneumonia", "worst"), 3)

