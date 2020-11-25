library(reshape2)

getSocialRecommend <- function(){
  
  # temp.user <- data.ratings
  temp.user <- data.frame(table(data.ratings$user_id))
  colnames(temp.user) <- c("user_id", "freq")
  data.user <- temp.user[1:50,]
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
  
  for(i in 1:nrow(test_data)){
    active <- test_data[i,]
    
    social <- raw.socialgraph[raw.socialgraph$first_user_id == active$user_id,]
    socials <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("user_id", "venue_id", "rating"))))
    
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
      next
    }
    
    mean.venues <- aggregate(socials$rating,
                            by=list(venue_id=socials$venue_id),
                            data=socials,
                            FUN=mean)
    colnames(mean.venues) <- c("venue_id", "rating")
    
    active.rating <- data.ratings[data.ratings$user_id == active$user_id,]
    
    ## rmse of social friend
    rmse <- 0
    n <- nrow(active.rating)
    for(k in 1:nrow(active.rating)){
      active.venue <- active.rating[k,]
      social.rating <- mean.venues[mean.venues$venue_id == active.venue$venue_id,]
      
      if(nrow(social.rating)==0){
       n <- n - 1
         next
      }
      
      list.active.user <- list.add(list.active.user, active.venue$user_id)
      list.active.venue <- list.add(list.active.venue, active.venue$venue_id)
      list.active.rating <- list.add(list.active.rating, active.venue$rating)
      list.social.rating <- list.add(list.social.rating, social.rating$rating)
      
      error <- ((active.venue[,'rating'] - social.rating[,'rating']) * (active.venue[,'rating'] - social.rating[,'rating']))

      list.predict.error <- list.add(list.predict.error, error)
    }
    
    if(n == 0)
      next
    
  } # end of test data
  
  
  rmseDF <- data.frame(list.active.user,list.active.venue,list.active.rating, list.social.rating, list.predict.error)
  sum.rmse <-  sqrt(sum(rmseDF$list.predict.error) / nrow(rmseDF))
  
  result <- list(error = rmseDF, rmse = sum.rmse)
  class(result) <- "SocialPrediction"
  
  return(result)
}

x <- getSocialRecommend()
error <- x$error
error.rmse <- x$rmse

list.add <- function(baselist, newvalue){
  if(length(baselist) == 0){
    baselist <- newvalue
  }else{
    baselist <- c(baselist, newvalue)
  }
  
  return(baselist)
}

rm(i,j,k,rmse,row,test_data,train_data,temp.user,social.rating,
   social_friend_active,social_friend_active_rating,social,socials,
   mean.venues, active,active.rating,active.venue, ratingDF, ratingsDF,
   data.user)
