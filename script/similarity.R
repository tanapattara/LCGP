get.similarity <- function(x){
  # create empty similarity weight matrix
  w <- matrix(0, nrow(x), nrow(x))
  w <- as.data.frame(w)
  network <- w
  
  # create empty social similarity weight matrix
  social <- w
  social.sim <- social
  
  # create empty network
  network <- w
  
  for(i in 1:nrow(x)){
    for(j in i:nrow(x)){
      
      if(i == j)
        next
      if(w[i,j] != 0)
        next
      
      sum.r <- 0
      sum.ui <- 0
      sum.uj <- 0
      
      sum.pr <- 0
      sum.pri <- 0
      sum.prj <- 0
      
      # check user i, j in data.socialgraph
      socialij <- FALSE
      x <- subset(data.socialgraph, data.socialgraph$first_user_id == row.names(x)[i])
      y <- subset(x, x$second_user_id == row.names(x)[j])
      socialij <- (nrow(x) > 0) && (nrow(Y) > 0)
      
      # get mean rating for Pearson Correlation
      meani = rowMeans (x[i,], na.rm = TRUE, dims = 1)
      meanj = rowMeans (x[j,], na.rm = TRUE, dims = 1)
      
      # TRUE; user j is in the same network of i
      # FALSE; user j is not in the same network of i
      networkij <- FALSE
      
      for(l in 1:ncol(x)){
        # ri x rj
        if(is.na(x[i,l]) || is.na(x[j,l])){
          # not same network
          # Skip for luvan
          
          # sum value for Pearson correlation
          if(!is.na(x[i,l]))
            sum.pri = sum.pri + ((x[i,l] - meani) * (x[i,l] - meani))
          if(!is.na(x[j,l]))
            sum.prj = sum.prj + ((x[j,l] - meanj) * (x[j,l] - meanj))
        }else{
        
          # same network
          if(!networkij)
            networkij <- !is.na(x[i,l]) && !is.na(x[j,l])
          
          sum.r <- sum.r + (x[i,l] * x[j,l])
          sum.ui <- sum.ui + (x[i,l] * x[i,l])
          sum.uj <- sum.uj + (x[j,l] * x[j,l])
          
          ri <- x[i,l] - meani
          rj <- x[j,l] - meanj
          
          sum.pr <- sum.pr + (ri * rj)
        }
      }
      
      # luvan similarity
      result <- 0
      if(is.nan(sum.r))
        result <- 0
      else
        result <- sum.r / (sqrt(sum.ui) * sqrt(sum.uj))
      
      if(is.na(result))
        result <- 0
    
      w[i,j] <- result  
      w[j,i] <- result
      
      #social similarity
      result <- 0
      if(is.nan(sum.pr))
        result <- 0
      else
        result <- sum.pr / (sqrt(sum.prj) * sqrt(sum.prj))
      
      social.sim <- result
      
      #------------------------------------------------------------
      
      network[i,j] <- networkij
      network[j,i] <- networkij
      
      social[i,j] <- socialij
      social[j,i] <- socialij
    }
    
    w[i,i] <-1
    network[i,i] <- 1
    social[i,j] <- 1
    social.sim[i,j] <- 1
    
    # edit row and column name
    names(network)[i] <- row.names(x)[i]
    row.names(network)[i] <- row.names(x)[i]
    names(w)[i] <- row.names(x)[i]
    row.names(w)[i] <- row.names(x)[i]
    
    names(social)[i] <- row.names(x)[i]
    row.names(social)[i] <- row.names(x)[i]
    names(social.sim)[i] <- row.names(x)[i]
    row.names(social.sim)[i] <- row.names(x)[i]
  }
  
  result <- list(network = network, w = w, social = social, social.sim = social.sim)
  class(result) <- "similarity"
  return(result)
}

# User characteristics based CF
rm(i,j,l,val,temp,sum.r,result,sum.uj,sum.ui)
