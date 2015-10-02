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
    
    ## now check validity of num and make it numeric
    if(!is.numeric(num)){
        if(num=="best") {
            num <- 1
        }
        else if(num!="worst") {
            stop("invalid num")
        }
    }
    
    state <- levels(states)
    hospital <- character()
    for(sn in state) {
        splitted[[sn]] <- splitted[[sn]][order(splitted[[sn]][[outcome]],splitted[[sn]][["Hospital.Name"]]),]
        thisnum <- num
        if(thisnum=="worst"){
            thisnum <- nrow(splitted[[sn]])
        }
        hospital <- append(hospital,splitted[[sn]][thisnum,"Hospital.Name"])
    }
    to.return <- data.frame(hospital,state)
    to.return
}