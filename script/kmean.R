# https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/
# install.packages("factoextra")
 library(factoextra)
 library(readr)
 library(lubridate)
 library(magrittr) # needs to be run every time you start R and want to use %>%
 library(dplyr) 

# Run algorithm over a range of k 
# wss <- sapply(2:20, kmean_withinss)
# Create a data frame to plot the graph
# elbow <-data.frame(2:20, wss)
# Plot the graph with gglop
# ggplot(elbow, aes(x = X2.20, y = wss)) +
#   geom_point() +
#   geom_line() +
#   scale_x_continuous(breaks = seq(1, 20, by = 1))

kmean_withinss <- function(k) {
  cluster <- kmeans(df, k)
  return (cluster$tot.withinss)
}

kmean_clustering <- function(k){
  data.checkings <- raw.checkins
  data.checkings$Weekday <- as.numeric(as.character(data.checkings$Weekday))
  data.checkings$Hour <- as.numeric(as.character(data.checkings$Hour))
  df <- data.checkings[,c('Weekday','Hour')]
  df <- scale(data.checkings[,7:8])  # Scaling the data
  
  # Compute k-means with k
  set.seed(123)
  # kmeans(x, centers, iter.max = 10, nstart = 1)
  # x: numeric matrix, numeric data frame or a numeric vector
  # centers: Possible values are the number of clusters (k) or a set of initial (distinct) cluster centers. If a number, a random set of (distinct) rows in x is chosen as the initial centers.
  # iter.max: The maximum number of iterations allowed. Default value is 10.
  # nstart: The number of random starting partitions when centers is a number. Trying nstart > 1 is often recommended.
  km.res <- kmeans(df, k, nstart = 25)
  df <- cbind(data.checkings, cluster = km.res$cluster)
  df <- df[,c('user_id','venue_id','Weekday','Hour','cluster')]
  return(df)
}

getClusterRecommendation <- function(){
  activeDF <- error
  list.a <- vector()
  list.v <- vector()
  list.r <- vector()
  list.cluster <- vector()
  list.cluster.mean <- vector()
  list.cluster.error <- vector()
  
  progressbar.max <- nrow(activeDF)
  progressbar <- txtProgressBar(min = 0, max = progressbar.max, style = 3)
  
  for(i in 1:nrow(activeDF)){
    user.a <- activeDF[i,]
    setTxtProgressBar(progressbar, i)
    user.cluster <- data.checkings.clustered[data.checkings.clustered$user_id == user.a$user_id & data.checkings.clustered$venue_id == user.a$venue_id,]
    
    list.a <- list.add(list.a, user.a$user_id)
    list.v <- list.add(list.v, user.a$venue_id)
    list.r <- list.add(list.r, user.a$user_rating)
    
    if(nrow(user.cluster) == 0){
      # no user checkin
      user.cluster <- data.checkings.clustered[data.checkings.clustered$user_id == user.a$user_id,]
      
      if(nrow(user.cluster) == 0){
        list.cluster <- list.add(list.cluster, NaN)
        list.cluster.mean <- list.add(list.cluster.mean, NaN)
        list.cluster.error <- list.add(list.cluster.error, NaN)
        
        next
      }
    }
      # checkin data exist
      user.cluster.freq <- data.frame(table(user.cluster$cluster))
      colnames(user.cluster.freq) <- c("cluster", "freq")
        
      user.a.cluster <- as.numeric(as.character(user.cluster.freq[which.max(user.cluster.freq$cluster),'cluster']))
      
      # get all user cluster
      user.cluster.target <- data.checkings.clustered %>% filter(cluster == user.a.cluster )
      
      # calculate mean venue from cluster
      data.rating.cluster <- data.ratings[data.ratings$user_id %in% user.cluster.target$user_id,]
      
      # get mean of venue --------------------------------------------------------------------------------------
      venue.target.mean <- aggregate(data.rating.cluster$rating,
                               by=list(venue_id=data.rating.cluster$venue_id),
                               data=data.rating.cluster,
                               FUN=mean)
      colnames(venue.target.mean) <- c("venue_id", "mean")
      venue.target.rating <- venue.target.mean[venue.target.mean$venue_id == user.a$venue_id,]
      error.cluster <- (user.a$user_rating - venue.target.rating$mean) ^ 2
      
      list.cluster <- list.add(list.cluster, user.a.cluster)
      list.cluster.mean <- list.add(list.cluster.mean, venue.target.rating$mean)
      list.cluster.error <- list.add(list.cluster.error, error.cluster)
  }# end for loop
  
  close(progressbar)
  
  resultCluster <- data.frame(list.a, list.v, list.r, list.cluster, list.cluster.mean, list.cluster.error)
  colnames(resultCluster) <- c("user_id", "venue_id", "user_rating", "cluster", "cluster_mean", "cluster_error")
  
  result <- list(cluserMean = resultCluster, ClusterRating = data.rating.cluster)
  class(result) <- "ClusterMean"
  
  return(result)
}# end function

# y <- getClusterRecommendation()

getSimilarityFromTarget <- function(df){
  
  activeDF <- error # %>% select('user_id','venue_id', 'user_rating')
  df <- clusterModel$ClusterRating
  
  list.a <- vector()
  list.b <- vector()
  list.v <- vector()
  list.r <- vector()
  list.sim <- vector()
  
  progressbar.max <- nrow(activeDF)
  progressbar <- txtProgressBar(min = 0, max = progressbar.max, style = 3)
  
  for(i in 1:nrow(activeDF)){
    a <- activeDF[i,]
    
    setTxtProgressBar(progressbar, i)
    
    if(a$user_id %in% list.a)
      next
    
    a.sqrt <- 0
    # get active user's rating
    active.rating <- data.ratings[data.ratings$user_id == a$user_id,] 
    active.rating.non.active.venue <- active.rating[!active.rating$venue_id == a$venue_id,]
    # find mean of active user
    
    if(nrow(active.rating.non.active.venue) == 0){
      list.a <- list.add(list.a, a$user_id)
      list.b <- list.add(list.b, a$user_id)
      list.v <- list.add(list.v, a$venue_id)
      list.r <- list.add(list.r, NaN)
      list.sim <- list.add(list.sim, NaN)
      next
    }
    
    active.mean <- getMeanofUserRating(active.rating.non.active.venue)
    # find a.sqrt
    
    for(b in 1:nrow(active.rating)){    
      a.rating <- active.rating[b,]
      a.sqrt <- a.sqrt + ((a.rating$rating - active.mean$mean) ^ 2)
    }
    
    if(a.sqrt == 0){
      # if rating value as same as mean value
      list.a <- list.add(list.a, a$user_id)
      list.b <- list.add(list.b, a$user_id)
      list.v <- list.add(list.v, a$venue_id)
      list.r <- list.add(list.r, NaN)
      list.sim <- list.add(list.sim, NaN)
      
      next
    }
    
    # find exist target active venue
    target.venue <- df[df$venue_id == a$venue_id,]
    
    if(nrow(target.venue) == 0)
      next
    
    # loop all user to find similarity
    # from target venue
    for(j in 1:nrow(target.venue)){
      u <- target.venue[j,]
      
      
      if(u$user_id == a$user_id)
        next
      
      # get user u rating venue as same as active rating
      u.rating <- df[df$user_id == u$user_id,] 
      
      if(nrow(u.rating) == 0){
        next
      }
      else if(nrow(u.rating) == 1){
        u.mean <- u.rating
        colnames(u.mean) <- c("user_id", "venue_id", "mean")
      }
      else{
        u.mean <- getMeanofUserRating(u.rating)
      }
      
      sum <- 0
      u.sum <- 0
      u.sqrt <- 0
      
      #check u and active user as same place
      sameplace <- merge(u.rating,active.rating, by.x="venue_id", by.y = "venue_id")
      
      if(nrow(sameplace) == 0){
        
        list.a <- list.add(list.a, a$user_id)
        list.b <- list.add(list.b, u$user_id)
        list.v <- list.add(list.v, a$venue_id)
        list.r <- list.add(list.r, u$rating)
        list.sim <- list.add(list.sim, 0)
        
        next
      }
      
      #loop rating from user u
      for(k in 1:nrow(u.rating)){
        u.active <- u.rating[k,]
        u.sqrt <- u.sqrt + ((u.active$rating - u.mean$mean) ^ 2)
        
        # check user u and active user rating same place
        i.venue.rating <- active.rating[active.rating$venue_id == u.active$venue_id,]
        
        # next if not rating as same venue
        if(nrow(i.venue.rating) == 0)
          next
        
        # active and u user rating same venue
        # calculate similarity
        sum <- sum + ((i.venue.rating$rating - active.mean$mean) * (u.active$rating - u.mean$mean))
        
      } # end of loop k
      
      if(u.sqrt == 0 | a.sqrt == 0){
        sim.a.u <- NaN
      }
      else{
        sim.a.u <- sum / ( sqrt(a.sqrt) * sqrt(u.sqrt) )
      }
      
      list.a <- list.add(list.a, a$user_id)
      list.b <- list.add(list.b, u$user_id)
      list.v <- list.add(list.v, a$venue_id)
      list.r <- list.add(list.r, u$rating)
      list.sim <- list.add(list.sim, sim.a.u)
    } # end of j
    
   
  }# end of i
  
  close(progressbar)
  
  result <- data.frame(list.a, list.b, list.v, list.r, list.sim)
  colnames(result) <- c("active_user", "user_u", "venue", "rating", "sim_active_u")
  
  return(result)
  
}

cluster.sim <- getSimilarityFromTarget(cluster.rating)

getClusterCFRecommendation <- function(){
  
  list.rating <- vector()
  list.error <- vector()
  
  progressbar.max <- nrow(cluster.mean)
  progressbar <- txtProgressBar(min = 0, max = progressbar.max, style = 3)
  
  for(i in 1:nrow(cluster.mean)){
    setTxtProgressBar(progressbar, i)
    #loop for all error data
    active <- cluster.mean[i,]
    
    # find best similarity
    sim.best <- cluster.sim[social.sim$active_user %in% active$user_id,]
    sim.best <- sim.best %>% filter(sim_active_u > 0.1)
    
    if(nrow(sim.best) == 0){
      list.rating <- list.add(list.rating, NaN)
      list.error <- list.add(list.error, NaN)
      next
    }else{
      #loop on sim.best to predict rating
      sum <- 0
      sum.sim <- 0
      for(j in 1:nrow(sim.best)){
        u <- sim.best[j,]
        sum <- sum + (u$rating * u$sim_active_u)
        sum.sim <- sum.sim + u$sim_active_u
      }
      
      r <- sum / sum.sim
      e <- ((active$user_rating - r) ^ 2) 
      
      list.rating <- list.add(list.rating, r)
      list.error <- list.add(list.error, e)
    }
    
    
  }# end of i
  
  close(progressbar)
  
  df <- error
  df['cluster_rating'] <- list.rating
  df['cluster_error'] <- list.error
  
  return(df)
}

# cluster.cf <- getClusterCFRecommendation()
