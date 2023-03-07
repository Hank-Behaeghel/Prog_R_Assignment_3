#This is the function rankhospitals for R Programming Assignment 3
#the function takes 3 inputs: state, outcome, num (The rank)

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomeM <- read.csv("outcome-of-care-measures.csv")
    #making the columns of the outcomes numeric bc they are "character" when
    #read in
    outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- suppressWarnings(as.numeric(outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- suppressWarnings(as.numeric(outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- suppressWarnings(as.numeric(outcomeM$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
   
     ## Check that state and outcome are valid
    #first if tests that the "state" arg is fine
    stopifnot("invalid state" = state %in% outcomeM[,7])
    #second is to test whether the oucome argument is valid
    stopifnot("invalid outcome" = outcome %in% c("heart attack", "heart failure", "pneumonia"))
    ## Return hospital name in that state with the given rank
    
    #i can use the rank function to rank the outcomes, need to figure out how to use order within rank
    #first to handle heart attacks, using if to test if that was the outcome passed into rankhospital
    if(outcome == "heart attack") {
        #create a new dataframe of the outcomes by state passed through
        #as an input of "rankhospital
        outcome_by_state <- outcomeM[outcomeM$State == state,]
        #now we can rank the hospitals based on their mortality rate
        #modify the outcome_by_state datafram in order to add a column with ranks
        outcome_by_state$rank_HA <- rank(outcome_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.last = "keep")
        #reorder the data frame with ties being broken by alphabet
        outcome_by_state <- outcome_by_state[with(outcome_by_state, order(outcome_by_state$rank_HA, outcome_by_state$Hospital.Name)),]
        #Now we want to extract
        if(num == "best" | num == "worst") {
            if(num == "best") {
                print(outcome_by_state[1,2])
            } else {
                worst <- max(outcome_by_state$rank_HA, na.rm = TRUE)
                print(outcome_by_state[worst, 2])
            }
        } else {
            print(outcome_by_state[as.numeric(num),2])
        }
    }
    
    if(outcome == "heart failure") {
        #create a new dataframe of the outcomes by state passed through
        #as an input of "rankhospital
        outcome_by_state <- outcomeM[outcomeM$State == state,]
        #now we can rank the hospitals based on their mortality rate
        #modify the outcome_by_state datafram in order to add a column with ranks
        outcome_by_state$rank_HF <- rank(outcome_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.last = "keep")
        #reorder the data frame with ties being broken by alphabet
        outcome_by_state <- outcome_by_state[with(outcome_by_state, 
                                                  order(outcome_by_state$rank_HF, outcome_by_state$Hospital.Name)),]
        #Now we want to return the hospital with associated rank
        if(num == "best" | num == "worst") {
            if(num == "best") {
                print(outcome_by_state[1,2])
            } else {
                worst <- max(outcome_by_state$rank_HF, na.rm = TRUE)
                print(outcome_by_state[worst, 2])
            }
        } else {
        print(outcome_by_state[as.numeric(num),2])
        }
    }
    if(outcome == "pneumonia") {
        outcome_by_state <- outcomeM[outcomeM$State == state,]
        outcome_by_state$rank_P <- rank(outcome_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.last = "keep")
        outcome_by_state <- outcome_by_state[with(outcome_by_state, order(outcome_by_state$rank_P, outcome_by_state$Hospital.Name)),]
        if(num == "best" | num == "worst") {
            if(num == "best") {
                print(outcome_by_state[1,2])
            } else {
                worst <- max(outcome_by_state$rank_P, na.rm = TRUE)
                print(outcome_by_state[worst, 2])
            }
        } else {
            print(outcome_by_state[as.numeric(num),2])
        }
    }
}    