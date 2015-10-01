best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(outcome == 'heart attack') {
        thirtyMort <- outcomeData[[11]]
    }
    else if(outcome == 'heart failure') {
        thirtyMort <- outcomeData[[17]]
    }
    else if(outcome == 'pneumonia') {
        thirtyMort <- outcomeData[[23]]
    }
    else {stop("invalid outcome")}
    thirtyMort <- as.numeric(thirtyMort)
    thirtyMort ## where I am: thirtyMort is thirty-day mortality vector for specific outcome
}