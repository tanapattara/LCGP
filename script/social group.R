library(reshape2)

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
  
  for(i in 1:nrow(test_data)){
    active <- test_data[1,]
    
    social <- raw.socialgraph[raw.socialgraph$first_user_id == active$user_id,]
    socials <- social
    
    # find top rating form social friend
    for(j in 1:nrow(social)){
      social_friend_active <- social[j,]
      
      social_friend_active_rating <- data.ratings
      social_friend_active_rating <- social_friend_active_rating[social_friend_active_rating$user_id == social_friend_active$second_user_id,]
      
      if(nrow(social_friend_active_rating) == 0)
        next
      
      if(j == 1){
        socials <- social_friend_active_rating
      }else{
        socials <- rbind(socials, social_friend_active_rating)
      }
    }
   
    mean.venues <- aggregate(socials$rating,
                            by=list(venue_id=socials$venue_id),
                            data=socials,
                            FUN=mean)
    colnames(mean.venues) <- c("venue_id", "rating")
    
    active.rating <- data.ratings[data.ratings$user_id == active$user_id,]
    
    ## rmse of social friend
    rmse <- 0
    for(k in 1:nrow(active.rating)){
      active.venue <- active.rating[1,]
      social.rating <- mean.venues[mean.venues$venue_id == active.venue$venue_id,]
      rmse <- rmse + ((active.venue[,'rating'] - social.rating[,'rating']) * (active.venue[,'rating'] - social.rating[,'rating']))
    }
    sum.rmse <- sum.rmse + (sqrt(rmse / nrow(active.rating)))
    
  } # end of test data
  
  sum.rmse <- sum.rmse / nrow(test_data)
  
  return(sum.rmse)
}

rm(i,j,k,rmse,row,test_data,train_data,temp.user,social.rating,
   social_friend_active,social_friend_active_rating,social,socials,
   mean.venues, active,active.rating,active.venue, ratingDF, ratingsDF,
   data.user)
