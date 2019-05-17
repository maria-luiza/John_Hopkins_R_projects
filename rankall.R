directory <- "Data_assignment/"

rankall <- function(outcome, num = "best"){
  data <- read.csv(paste(directory, "outcome-of-care-measures.csv", sep=''), colClasses="character")

  col <- if(outcome == "heart attack"){
    11
  } else if(outcome == "heart failure"){
    17
  } else if(outcome == "pneumonia"){
    23
  } else{
    stop("invalid outcome")
  }
  
  data[, col] <- suppressWarnings(as.numeric(data[, col]))
  data <- data[!is.na(data[, col]), ]
  
  #Sort our data by specified mortality rate and hospital name
  data.sorted <- data[order(data[,col], data[,2], na.last=TRUE),]
  data.sorted <- data.sorted[!is.na(data.sorted[,col]),]
  
  #Get all states available
  states <- sort(unique(data.sorted[, 7]))
  
  state_data <- function(state){
    out <- subset(data.sorted, State == state)
    
    num <- if(num == "best"){
      1
    }else if(num == "worst"){
      num = length(data.sorted)
    }
    else{
      as.numeric(num)
    }
    
    out <- out[num, c(2, 7, col)]
    out$State <- state
    return(out)
  }
  
  state_d <- lapply(states, state_data)
  dframe <- as.data.frame(do.call(rbind, lapply(states, state_data)), row.names=states)
  colnames(dframe) <- c("hospital", "state")
  
  return (dframe)
  
}