# best.R

# setwd("c:/users/jd022981/dropbox/dev/r/2014.04.09 R Programming (Coursera)/ProgAssign3")

best <- function( state, outcome ) { 
    
    ## Read outcome data.  
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid.  
    if( !( state %in% state.abb ) )
        stop("invalid state")
    if( !( outcome %in% c("heart attack","heart failure","pneumonia") ) )
        stop("invalid outcome")

    ## coerc the numeric fields
    outcomeData[,11] <- as.numeric(outcomeData[,11])
    outcomeData[,17] <- as.numeric(outcomeData[,17])
    outcomeData[,23] <- as.numeric(outcomeData[,23])

    ## filter on state
    outcomeData <- subset( outcomeData, State==state )

    ## get the death rate based on the outcome.  
    if( outcome == "heart attack" )
        outcomeDataSort <- outcomeData[order(outcomeData[11],outcomeData[2]) , ]
    if( outcome == "heart failure" )
        outcomeDataSort <- outcomeData[order(outcomeData[17],outcomeData[2]) , ]
    if( outcome == "pneumonia" )
        outcomeDataSort <- outcomeData[order(outcomeData[23],outcomeData[2]) , ]
    
    ## Return hospital name in that state with lowest 30-day death rate.  
    outcomeDataSort[1,2]

}

## TEST CASES

## best("TX", "heart attack")
## "CYPRESS FAIRBANKS MEDICAL CENTER"

## best("TX", "heart failure")
## "FORT DUNCAN MEDICAL CENTER"

## best("MD", "heart attack")
## "JOHNS HOPKINS HOSPITAL, THE"

## best("MD", "pneumonia")
## "GREATER BALTIMORE MEDICAL CENTER"

## best("BB", "heart attack")
## Error in best("BB", "heart attack") : invalid state

## best("NY", "hert attack")
## Error in best("NY", "hert attack") : invalid outcome
