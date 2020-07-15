recommendation <- function(lcgp,location.user, location.venue){
  c <- lcgp$c
  pl <- lcgp$p
  r <- matrix(0, nrow(c), nrow(c))
  r <- as.data.frame(r)
  
  for(i in 1:nrow(c)){
    for(l in 1:ncol(c)){
      
      userid <- row.names(c[i,])
      venueid <- names(c[l])
      
      # get location
      user <- location.user[location.user[,"user_id"] == userid,]
      venue <- location.venue[location.venue[,"venue_id"] == venueid,]
      g <- get.geo_distance(user[,3],user[,2],venue[,3],venue[,2])
      gl <- 0.1668 * (g ^ -0.9463)
      cil <- c[i,l]
      popularl <- pl[l]
      ratingil <- (0.3 * cil) + (0.3 * gl) + (0.4 * popularl)
      r[i,l] <- ratingil
    }
    row.names(r)[i] <- row.names(c)[i]
    names(r)[i] <- names(c)[i]
  }
  
  result <- list(rating = r, c = lcgp$c, p = lcgp$p, w = lcgp$w, network = lcgp$network)
  class(result) <- "lcgp"
  
  return(result)
}
