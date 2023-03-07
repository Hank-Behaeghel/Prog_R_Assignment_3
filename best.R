#This is the attempt to build the "Best" function for Prog Assign 3
#Using if statements we test the inputs and print ties in alphabetical order
best <- function(state, outcome) {
    #First thing we need to do is load in the data
    outcomeM <- read.csv("outcome-of-care-measures.csv")
    #make our outcomes of interest into numeric to make things easier late
    
    #NEED COLUMNS 7,11,23 to be numeric (That way we can filter on value)
    #when using as.numeric we want to supress warnings so they dont pop up
    outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- suppressWarnings(as.numeric(outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- suppressWarnings(as.numeric(outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- suppressWarnings(as.numeric(outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    
    #test the validity of arguments
    #first if tests that the "state" arg is fine
    stopifnot("invalid state" = state %in% outcomeM[,7])
    #second is to test whether the oucome argument is valid
    stopifnot("invalid outcome" = outcome %in% c("heart attack", "heart failure", "pneumonia"))
    #if both args valid now its time to print out the hoptal with lowest death rate
    #first we will address heart attacks, this if function tests that "heart attack"
    #was the input
    if(outcome == "heart attack") {
        #create a new dataframe of the outcomes by state passed through
        #as an input of "best"
        outcome_by_state <- outcomeM[outcomeM$State == state,]
        #then to find the best outcome we want the minimum mortality rate
        minHA <- min(outcome_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm = TRUE)
        #now that we have the best outcome we want to pull the hospital it occurs at
        #we want to store this in a character vector
        best_hospital <- outcome_by_state[(outcome_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minHA), 2]
        #sort alphabetically
        ordered_bh <- sort(best_hospital)
        #print the outcome
        print(ordered_bh)
    }
    
    #the next if statement tests whether the outcome is "heart failure and
    #follows the same structure as the previous if statement
    if(outcome == "heart failure") {
        outcome_by_state <- outcomeM[outcomeM$State == state,]
        minHF <- min(outcome_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm = TRUE)
        best_hospital <- outcome_by_state[(outcome_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == minHF), 2]
        ordered_bh <- sort(best_hospital)
        print(ordered_bh)
    }
    
    if(outcome == "pneumonia") {
        outcome_by_state <- outcomeM[outcomeM$State == state,]
        minP <- min(outcome_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm = TRUE)
        best_hospital <- outcome_by_state[(outcome_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == minP), 2]
        ordered_bh <- sort(best_hospital)
        print(ordered_bh)
    }
}




