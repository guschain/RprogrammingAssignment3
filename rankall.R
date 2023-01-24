rankall <- function (outcome, rank)
{
      data <- read.csv("outcome-of-care-measures.csv")
      
      # select only the outcome column and hospital column
      if (outcome == "heart attack") {
        
        data[, 11] <- as.numeric(data[, 11])
        x <- data[,c(2,7,11)]
        
      } else if (outcome == "heart failure") {
        
        data[, 17] <- as.numeric(data[, 17])
        x <- data[,c(2,7,17)]
        
      } else if (outcome == "pneumonia") {
        
        data[, 23] <- as.numeric(data[, 23])
        x <- data[,c(2,7,23)]
      }
      
      # remove NA
      x <- x[!is.na(x[,3]),]
      print("ok until remove na")
      # 
      if (rank == "best") {
        
        # sort lowest rank and then by hospital name, ascending
        x <- x[order(x[,3],x[,1]),]
        
        ranked <- lapply(split(x, x$State),function (x) cbind(seq(nrow(x)),x))
        #rank hospitals
        
        
        lapply(ranked, function(x) subset(x,subset = x[,1] == 1))
        
        
      } else if (rank == "worst") {
        
        # sort highest rank and then by hospital name, descending
        x <- x[order(-x[,3],x[,1]),]
        
        #rank hospitals
        ranked <- lapply(split(x, x$State),function (x) cbind(seq(nrow(x),1),x))
      
        lapply(ranked, function(x) subset(x,subset = x[,1] == nrow(x)))
        
      } else if (rank > nrow(x)) {
        
        NA
        
        
      } else if (is.numeric(rank)) {
        
        # sort lowest rank and then by hospital name, ascending
        x <- x[order(x[,3],x[,1]),]
        
        #rank hospitals
        ranked <- lapply(split(x, x$State),function (x) cbind(seq(nrow(x)),x))
        
        # return the requested rank
        lapply(ranked, function(x) subset(x,subset = x[,1] == rank))
        
        
      }
      
  
}