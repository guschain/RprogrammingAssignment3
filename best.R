best <- function (state, outcome)
{
       data <- read.csv("outcome-of-care-measures.csv")
       validState <- unique(data$State)
       validOutcome <- c("heart attack", "heart failure", "pneumonia")
       
       # if invalid state, stop
       
       if (state %in% validState) {
         print("State is valid")
       } else {stop("Invalid State")}
       
       if (outcome %in% validOutcome) {
         print("Outcome is valid")
       } else {stop("Invalid Outcome")}      
  
        # filter data by State
       sub <- subset(data,subset = data$State == state)
      

        # select only the outcome column and hospital column
        if (outcome == "heart attack") {
    
          sub[, 11] <- as.numeric(sub[, 11])
          x <- sub[,c(2,11)]
          
        } else if (outcome == "heart failure") {
          
          sub[, 17] <- as.numeric(sub[, 17])
          x <- sub[,c(2,17)]
          
        } else if (outcome == "pneumonia") {
          
          sub[, 23] <- as.numeric(sub[, 23])
          x <- sub[,c(2,23)]
        }
       
        # remove NA
        x <- x[!is.na(x[,2]),]
        
        # select minimum mortality
        x <- subset (x, subset = x[,2] == min(x[,2]))
        
        # sort hospital alphabetically in case of ties and choose the first
        x[order(x$Hospital.Name),][1,]
      

  
}