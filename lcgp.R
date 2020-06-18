# Comprehensiverecommendation (LC–G–P)
# recommendation rating of location l to user i
# 0.3 x C + 0.3 x G + 0.4 x P

# C => the users’ characteristics based recommendation rating for the target user i on location l
# loop user
lcgp <- function(data, w){
  df <- data.frame()
  tl <- vector()
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
        if(i == 1 && !is.nan(data[j,l])){
          t = t + 1
        }
        # check for not same user
        if(i == j){
          next  
        }
        
        datajl <- data[j,l]
        if(is.nan(datajl))
          datajl <- 0
        
        rjl <- datajl
        sum <- sum + (w[i,j] * rjl)
        div <- div + w[i,j]
      } #end j
      
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
    
    # add tl to vector
    if(length(tl) == 0){
      tl <- t
    }else{
      tl <- c(tl, t)
    }
  }# end l
  
  pl <- vector()
  for (t in tl){
    pt = (t - min(tl)) / (max(tl) - min(tl))
    
    if(length(pl) == 0){
      pl <- pt
    }else{
      pl <- c(pl, pt)
    }
  }
  
  result <- list(c = df, p = pl)
  class(result) <- "lcgp"
  
  return(result)
}

x <- create.sample.data()
W <- create.similarity(x)
obj <- lcgp(x,W)

