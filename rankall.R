rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if(outcome == 'heart attack') {
        outcomeData <- outcomeData[outcomeData[[11]]!="Not Available",]
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }
    else if(outcome == 'heart failure') {
        outcomeData <- outcomeData[outcomeData[[17]]!="Not Available",]
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }
    else if(outcome == 'pneumonia') {
        outcomeData <- outcomeData[outcomeData[[23]]!="Not Available",]
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    else {stop("invalid outcome")}
    outcomeData[[outcome]] <- as.numeric(outcomeData[[outcome]]) ## cleaned thirty-day mortality vector for specific outcome & state

    states <- factor(outcomeData$State)
    
    splitted <- split(outcomeData[,c("Hospital.Name",outcome)],states)
    
    for(state in levels(states)) {
        splitted[[state]] <- splitted[[state]][order(splitted[[state]][[outcome]],splitted[[state]][["Hospital.Name"]]),]
    }
    splitted
}