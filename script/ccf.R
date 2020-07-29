# C => the users’ characteristics based recommendation rating for the target user i on location l
# loop user
get.ccf <- function(data, sim){
  df <- data.frame()
  tl <- vector()
  w <- sim$w
  network <- sim$network
  
  # loop all location
  for(l in 1:ncol(data)){
    # empty venue l
    cl <- vector()
    # for location popularity
    t <- 0
    for(i in 1:nrow(data)){
      sum <- 0
      div <- 0
      
      for(j in 1:nrow(data)){
        # popularity
        # count rating on l location only user i
        if(i == 1 && !is.na(data[j,l])){
          t = t + 1
        }
        # if same user skip this user
        if(i == j){
          next  
        }
      
        if(network[i,j] == 0)
          next
        
        datajl <- data[j,l]
        if(is.na(datajl))
          datajl <- 0
  
        rjl <- datajl
        sum <- sum + (w[i,j] * rjl)
        div <- div + w[i,j]
        
      } #end j, next j
      
      # save to vector
      cil <- sum / div
      if(length(cl) == 0){
        cl <- cil
      }
      else{
        cl <- c(cl, cil)
      }
      
    } # end i
    
    # add cl to dataframe
    if(ncol(df) == 0){
      df <- data.frame(cl)
    }
    else{
      df <- data.frame(df, cl)
    }
    
    names(df)[l] <- names(data)[l]
    
    # add tl to vector
    if(length(tl) == 0){
      tl <- t
    }else{
      tl <- c(tl, t)
    }
  }# end l
  
  # add user to row
  for(i in 1:nrow(data))
    row.names(df)[i] <- row.names(data)[i]
  
  pl <- vector()
  min <- min(tl)
  max <- max(tl)
  for (t in tl){
    pt = (t - min) / (max - min)
    
    if(length(pl) == 0){
      pl <- pt
    }else{
      pl <- c(pl, pt)
    }
  }
  
  result <- list(c = df, p = pl,w = sim$w, network = sim$network)
  class(result) <- "lcgp"
  
  return(result)
}
