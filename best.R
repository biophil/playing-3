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
    
    ## now check validity of the state
    stateLogIdx <- outcomeData$State==state # stateLogIdx is logical-idx of desired state
    if(length(outcomeData[stateLogIdx,]$State)==0) {
        stop("invalid state")
    }
    
    hospNames <- outcomeData[stateLogIdx,]$Hospital.Name
    outies <- thirtyMort[stateLogIdx]
    
    # make a frame of hosp names and mortalities, exclude na's
    stateFrame <- data.frame(hospNames[!is.na(outies)],outies[!is.na(outies)])
    minimumMort <- min(stateFrame$outies[!is.na(stateFrame$outies)]) # for reference
    
    hospitals <- stateFrame$hospNames[stateFrame$outies==minimumMort]
    
    min(as.character(hospitals)) ## just returns the hospitals in state in question
}