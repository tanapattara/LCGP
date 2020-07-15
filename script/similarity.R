create.similarity <- function(x){
  # create empty similarity weight matrix
  w <- matrix(0, nrow(x), nrow(x))
  w <- as.data.frame(w)
  
  # create empty network
  network <- w
  network <- as.data.frame(network)
  for(i in 1:nrow(x)){
    for(j in i:nrow(x)){
      
      if(i == j)
        next
      if(w[i,j] != 0)
        next
      
      sum.r <- 0
      sum.ui <- 0;
      sum.uj <- 0;
      
      # TRUE; user j is in the same network of i
      # FALSE; user j is not in the same network of i
      networkij <- FALSE
      
      for(l in 1:ncol(x)){
        # ri x rj
        if(is.na(x[i,l]) || is.na(x[j,l])){
          next
        }
        
        if(!networkij)
          networkij <- !is.na(x[i,l]) && !is.na(x[j,l])
        
        sum.r <- sum.r + (x[i,l] * x[j,l])
        sum.ui <- sum.ui + (x[i,l] * x[i,l])
        sum.uj <- sum.uj + (x[j,l] * x[j,l])
      }
      
      result <- 0
      if(is.nan(sum.r))
        reuslt <- 0
      else
        result <- sum.r / (sqrt(sum.ui) * sqrt(sum.uj))
      
      if(is.na(result))
        result <- 0
    
      w[i,j] <- result  
      w[j,i] <- result
      
      network[i,j] <- networkij
      network[j,i] <- networkij
    }
    w[i,i] <-1
    network[i,i] <- 1
    
    names(network)[i] <- row.names(x)[i]
    row.names(network)[i] <- row.names(x)[i]
    
    names(w)[i] <- row.names(x)[i]
    row.names(w)[i] <- row.names(x)[i]
  }
  
  result <- list(network = network, w = w)
  class(result) <- "similarity"
  return(result)
}

# User characteristics based CF
rm(i,j,l,val,temp,sum.r,result,sum.uj,sum.ui)