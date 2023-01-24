rankhospital <- function (state, outcome, rank)
{
  
  data <- read.csv("outcome-of-care-measures.csv")
  # rank options: "best","worst", or an integer
  
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

  if (rank == "best") {
    
    # sort lowest rank and then by hospital name, ascending
    x <- x[order(x[,2],x[,1]),]
    
    #rank hospitals
    x['Rank'] <- seq(nrow(x))
    x[1,]

    
  } else if (rank == "worst") {
    
    # sort highest rank and then by hospital name, descending
    x <- x[order(-x[,2],x[,1]),]
    
    #rank hospitals
    x['Rank'] <- seq(nrow(x),1)
    x[1,]
    
  } else if (rank > nrow(x)) {
    
    NA
    
    
  } else if (is.numeric(rank)) {
    
    # sort lowest rank and then by hospital name, ascending
    x <- x[order(x[,2],x[,1]),]
    
    #rank hospitals
    x['Rank'] <- seq(nrow(x))
    
    # return the requested rank
    subset(x, subset = x[,3] == rank)
    
    
  }
  
 
  
  
  
}