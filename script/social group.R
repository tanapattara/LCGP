library(reshape2)

getSocialRecommend <- function(ratingDF, socialDF){
  
  temp.user <- data.user
  data.user <- temp.user[1:100,]
  # get test data
  row <- floor(nrow(data.user)*0.1)
  test_data <- data.user[sample(nrow(data.user), row), ]
  train_data <- data.user
  
  for(i in 1:nrow(test_data)){
    active = test_data[i,]
    train_data <- train_data[!train_data$user_id == active$user_id,]
  }
  
  for(i in 1:nrow(test_data)){
    active <- test_data[i,]
    
    social <- data.socialgraph[data.socialgraph$first_user_id == active$user_id,]
    socials <- social
    
    # find top rating form social friend
    for(j in 1:nrow(social)){
      social_friend_active <- social[j,]
      
      social_friend_active_rating <- raw.ratings
      social_friend_active_rating <- social_friend_active_rating[social_friend_active_rating$user_id == social_friend_active$second_user_id,]
      
      if(j == 1){
        socials <- social_friend_active_rating
      }else{
        socials <- rbind(socials, social_friend_active_rating)
      }
    }
   
    #mean.venues <- aggregate(socials$rating, 
    #                         by=list(venue_id=socials$venue_id), 
    #                         data=socials,
    #                         FUN=mean)
    
    active.rating <- raw.ratings[raw.ratings$user_id == active$user_id,]
    socials <- rbind(socials, active.rating)
    
    rmat <- as.matrix(socials)
    rmat <- as(rmat,"realRatingMatrix")
    
  }
  
  # find friend
  
}