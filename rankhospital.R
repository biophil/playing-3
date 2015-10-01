rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(outcome == 'heart attack') {
        outcomeData <- outcomeData[outcomeData[[11]]!="Not Available",]
        thirtyMort <- outcomeData[[11]]
    }
    else if(outcome == 'heart failure') {
        outcomeData <- outcomeData[outcomeData[[17]]!="Not Available",]
        thirtyMort <- outcomeData[[17]]
    }
    else if(outcome == 'pneumonia') {
        outcomeData <- outcomeData[outcomeData[[23]]!="Not Available",]
        thirtyMort <- outcomeData[[23]]
    }
    else {stop("invalid outcome")}
    thirtyMort <- as.numeric(thirtyMort)
    thirtyMort ## thirty-day mortality vector for specific outcome & state
    
    ## now check validity of the state
    stateLogIdx <- outcomeData$State==state # stateLogIdx is logical-idx of desired state
    if(length(outcomeData[stateLogIdx,]$State)==0) {
        stop("invalid state")
    }
    
    hospNames <- outcomeData[stateLogIdx,]$Hospital.Name
    outies <- thirtyMort[stateLogIdx]
    
    # make a frame of hosp names and mortalities, exclude na's
    stateFrame <- data.frame(hospNames,outies)
    stateFrameSorted <- stateFrame[order(outies,hospNames),]
    
    ## now check validity of num and make it numeric
    if(num=="best") {
        num <- 1
    } 
    else if(num=="worst") {
        num <- nrow(stateFrameSorted)
    }
    else if(!is.numeric(num)) {
        stop("invalid num")
    }
    
    to.return <- as.character(stateFrameSorted[num,1])
    if(is.null(to.return)) {
        to.return <- NA
    }
    to.return
}