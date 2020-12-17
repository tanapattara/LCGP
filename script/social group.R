getSocialRecommend <- function(){
  
  # temp.user <- data.ratings
  temp.user <- data.frame(table(data.ratings$user_id))
  colnames(temp.user) <- c("user_id", "freq")
  data.user <- temp.user[1:100,]
  # get test data
  row <- floor(nrow(data.user)*0.1)
  test_data <- data.user[sample(nrow(data.user), row), ]
  train_data <- data.user
  
  for(i in 1:nrow(test_data)){
    active = test_data[i,]
    train_data <- train_data[!train_data$user_id == active$user_id,]
  }
  
  sum.rmse <- 0
  ntest <- nrow(test_data)
  
  list.active.user <- vector()
  list.active.venue <- vector()
  list.active.rating <- vector()
  list.social.rating <- vector()
  list.predict.error <- vector()
  list.propular.rating <- vector()
  list.propular.error <- vector()
  
  mean.venues.all <- aggregate(data.ratings$rating,
                               by=list(venue_id=data.ratings$venue_id),
                               data=data.ratings,
                               FUN=mean)
  colnames(mean.venues.all) <- c("venue_id", "rating")
  
  
  progressbar.max <- nrow(test_data)
  progressbar <- txtProgressBar(min = 0, max = progressbar.max, style = 3)
  
  for(i in 1:nrow(test_data)){
    active <- test_data[i,]
    
    social <- raw.socialgraph[raw.socialgraph$first_user_id == active$user_id,]
    socials <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("user_id", "venue_id", "rating"))))
    
    active.rating <- data.ratings[data.ratings$user_id == active$user_id,]
    
    # find top rating form social friend
    for(j in 1:nrow(social)){
      social_friend_active <- social[j,]
      
      social_friend_active_rating <- data.ratings
      social_friend_active_rating <- social_friend_active_rating[social_friend_active_rating$user_id == social_friend_active$second_user_id,]
      

      if(j == 1){
        socials <- social_friend_active_rating
      }else{
        socials <- rbind(socials, social_friend_active_rating)
      }
    }
   
    if(nrow(socials) == 0){
      ntest <- ntest - 1
      
      list.active.user <- list.add(list.active.user, active.rating$user_id)
      list.active.venue <- list.add(list.active.venue, active.rating$venue_id)
      list.active.rating <- list.add(list.active.rating, active.rating$rating)
      list.social.rating <- list.add(list.social.rating, NaN)
      
      prop <- mean.venues.all[mean.venues.all$venue_id == active.rating$venue_id,]
      list.propular.rating <- list.add(list.propular.rating, prop$rating)
      
      error <- NaN
      error.pro <- ((active.rating[,'rating'] - prop$rating) * (active.rating[,'rating'] - prop$rating))
      
      list.predict.error <- list.add(list.predict.error, error)
      list.propular.error <- list.add(list.propular.error, error.pro)
      
      next
    }
    
    mean.venues <- aggregate(socials$rating,
                            by=list(venue_id=socials$venue_id),
                            data=socials,
                            FUN=mean)
    
    
    colnames(mean.venues) <- c("venue_id", "rating")
    

    
    ## rmse of social friend
    rmse <- 0
    n <- nrow(active.rating)
    
    for(k in 1:nrow(active.rating)){
      active.venue <- active.rating[k,]
      social.rating <- mean.venues[mean.venues$venue_id == active.venue$venue_id,]
      prop <- mean.venues.all[mean.venues.all$venue_id == active.venue$venue_id,]
      
      if(nrow(social.rating)==0){
       n <- n - 1
         next
      }
      
      list.active.user <- list.add(list.active.user, active.venue$user_id)
      list.active.venue <- list.add(list.active.venue, active.venue$venue_id)
      list.active.rating <- list.add(list.active.rating, active.venue$rating)
      list.social.rating <- list.add(list.social.rating, social.rating$rating)
      list.propular.rating <- list.add(list.propular.rating, prop$rating)
      
      error <- ((active.venue[,'rating'] - social.rating[,'rating']) * (active.venue[,'rating'] - social.rating[,'rating']))
      error.pro <- ((active.venue[,'rating'] - prop$rating) * (active.venue[,'rating'] - prop$rating))
      
      list.predict.error <- list.add(list.predict.error, error)
      list.propular.error <- list.add(list.propular.error, error.pro)
      
    }
    
    if(n == 0)
      next
    
    setTxtProgressBar(progressbar, i)
  } # end of test data
  
  close(progressbar)
  
  rmseDF <- data.frame(list.active.user,list.active.venue,list.active.rating, list.social.rating, list.propular.rating, list.predict.error, list.propular.error)
  colnames(rmseDF) <- c("user_id", "venue_id", "user_rating", "social_rating", "propular_rating", "social_error", "propular_error")
  
  sum.rmse <-  sqrt(sum(rmseDF$list.predict.error) / nrow(rmseDF))
  
  result <- list(error = rmseDF, rmse = sum.rmse, activeuser = test_data, activerating = active.rating)
  class(result) <- "SocialPrediction"
  
  return(result)
}


getSimilarity <- function(){
  
  activeDF <- error %>% select('user_id','venue_id', 'user_rating')
 
  list.a <- vector()
  list.b <- vector()
  list.v <- vector()
  list.r <- vector()
  list.sim <- vector()
  
  progressbar.max <- nrow(activeDF)
  progressbar <- txtProgressBar(min = 0, max = progressbar.max, style = 3)
  
  for(i in 1:nrow(activeDF)){
    a <- activeDF[i,]

    if(a$user_id %in% list.a)
      next
    
    a.sqrt <- 0
    # get active user's rating
    active.rating <- data.ratings[data.ratings$user_id == a$user_id,]
    active.rating.non.active.venue <- active.rating[!active.rating$venue_id == a$venue_id,]
    # find mean of active user
    
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
    target.venue <- data.ratings[data.ratings$venue_id == a$venue_id,]
    
    # loop all user to find similarity
    # from target venue
    for(j in 1:nrow(target.venue)){
      u <- target.venue[j,]
      
      if(u$user_id == a$user_id)
        next
      
      # get user u rating venue as same as active rating
      u.rating <- data.ratings[data.ratings$user_id == u$user_id,]
      
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
      # sameplace <- u.rating[u.rating$venue_id == active.rating$venue_id,]
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
      
      if(u.sqrt == 0 | a.sqrt == 0)
        sim.a.u <- NaN
      else
        sim.a.u <- sum / ( sqrt(a.sqrt) * sqrt(u.sqrt) )
      
      list.a <- list.add(list.a, a$user_id)
      list.b <- list.add(list.b, u$user_id)
      list.v <- list.add(list.v, a$venue_id)
      list.r <- list.add(list.r, u$rating)
      list.sim <- list.add(list.sim, sim.a.u)
    } # end of j
    
    setTxtProgressBar(progressbar, i)
  }# end of i
  
  close(progressbar)
  
  result <- data.frame(list.a, list.b, list.v, list.r, list.sim)
  colnames(result) <- c("active_user", "user_u", "venue", "rating", "sim_active_u")
  
  return(result)
}


# sim <- getSimilarity()

getCFRecommendation <- function(){
 
  list.rating <- vector()
  list.error <- vector()
  
  
  progressbar.max <- nrow(error)
  progressbar <- txtProgressBar(min = 0, max = progressbar.max, style = 3)
  
  for(i in 1:nrow(error)){
    #loop for all error data
    active <- error[i,]
    
    # find best similarity
    sim.best <- social.sim[social.sim$active_user %in% active$user_id,]
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
    
    setTxtProgressBar(progressbar, i)
  }# end of i
  
  close(progressbar)
  
  df <- error
  df['sim_rating'] <- list.rating
  df['sim_error'] <- list.error
  
  return(df)
}


list.add <- function(baselist, newvalue){
  if(length(baselist) == 0){
    baselist <- newvalue
  }else{
    baselist <- c(baselist, newvalue)
  }
  
  return(baselist)
}

getMeanofUserRating <- function(active.rating){
  
  active.mean <- aggregate(active.rating$rating,
            by=list(user_id=active.rating$user_id),
            data=active.rating,
            FUN=mean)
  
  colnames(active.mean) <- c("user_id", "mean")
  return(active.mean)
}

getRatingPower <- function(df){
  sum <- 0
  mean <- getMeanofUserRating(df)
  for(i in 1:nrow(df)){
    sum <- sum + ((df[i,'rating'] - mean$mean) * (df[i,'rating'] - mean$mean))
  }
  return(sqrt(sum))
}

rm(i,j,k,rmse,row,test_data,train_data,temp.user,social.rating,
   social_friend_active,social_friend_active_rating,social,socials,
   mean.venues, active,active.rating,active.venue, ratingDF, ratingsDF,
   data.user)
