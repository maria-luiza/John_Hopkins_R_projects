directory <- "Data_assignment/"

rankHospital <- function(state, outcome, num = "best"){
  data <- read.csv(paste(directory, "outcome-of-care-measures.csv", sep=''), colClasses="character")
  
  # Check valid of arguments state and outcome
  if(!(state %in% data$State)){
    stop("invalid state")
  }
  
  col <- if(outcome == "heart attack"){
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if(outcome == "heart failure"){
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if(outcome == "pneumonia"){
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else{
    stop("invalid outcome")
  }
  
  # Once we verified that arguments are valids, filter the dataframe based on State
  data <- data[data$State == state, c("Hospital.Name", col)]
  
  data[, 2] <- suppressWarnings(as.numeric(data[,2]))
  data <- data[!is.na(data[, 2]), ]
  
  ord <- order(data[col], data$Hospital.Name, na.last=NA)
  
  
  # In cases where the num was not defined, best is the 1st and worst is the last
  if(num == "best"){
    as.character(data$Hospital.Name[ord[1]])
  }
  else if(num == "worst"){
    as.character(data$Hospital.Name[ord[length(ord)]])
  }
  else{
    as.character(data$Hospital.Name[ord[num]])
  }
}