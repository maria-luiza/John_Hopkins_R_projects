# Response for Final test

directory = "Data_assignment/"

best <- function(state, outcome){
  # Read the dataset available
  data <- read.csv(paste(directory, "outcome-of-care-measures.csv", sep=''), colClasses = "character")

  # Check valid of arguments state and outcome
  if(!(state %in% data$State)){
    stop("invalid state")
  }

  if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
    stop("invalid outcome")
  }
  
  # Once we verified that arguments are valids, filter the dataframe based on State
  data <- data[data$State == state, ]
  
  if(outcome == "heart attack"){
    data <- data[order(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
    
  }
  else if(outcome == "heart failure"){
    data <- data[order(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
  }
  else{
    data <- data[order(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
  }
  
  # Returns the first value on Hospital Name column
  data$Hospital.Name[1]
}
