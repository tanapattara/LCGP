create.sample.data <- function(){
  # Create a, b, c, d variables
  hp1 <- c(4,5,NaN,NaN)
  hp2 <- c(NaN,5,NaN,3)
  hp3 <- c(NaN,4,NaN,NaN)
  tw <- c(5,NaN,2,NaN)
  sw1 <- c(1,NaN,4,NaN)
  sw2 <- c(NaN,NaN,5,NaN)
  sw3 <- c(NaN,NaN,NaN,3)
  # Join the variables to create a data frame
  x <- data.frame(hp1,hp2,hp3,tw,sw1,sw2,sw3)
  rm(hp1,hp2,hp3,tw,sw1,sw2,sw3)
  rownames(x) = c('a','b','c','d')
  # set na value to zero
  # x[is.na(x)] <- 0
  return(x)
}

create.similarity <- function(x){
  # create empty matrix
  w <- matrix(0, nrow(x), nrow(x))
  
  for(i in 1:nrow(x)){
    for(j in i:nrow(x)){
      if(i == j)
        next
      if(w[i,j] != 0)
        next
      
      sum.r <- 0
      sum.ui <- 0;
      sum.uj <- 0;
      
      for(l in 1:ncol(x)){
        # ri x rj
        if(is.nan(x[i,l]) || is.nan(x[j,l])){
          next
        }
        
        sum.r <- sum.r + (x[i,l] * x[j,l])
        sum.ui <- sum.ui + (x[i,l] * x[i,l])
        sum.uj <- sum.uj + (x[j,l] * x[j,l])
      }
      
      result <- 0
      
      if(sum.r != 0)
        result <- sum.r / (sqrt(sum.ui) * sqrt(sum.uj))
      
      w[i,j] <- result  
      w[j,i] <- result
    }
    w[i,i] <-1
  }
  w <- as.data.frame(w)
  return(w)
}

# User characteristics based CF
rm(i,j,l,val,temp,sum.r,result,sum.uj,sum.ui)