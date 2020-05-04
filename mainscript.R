# 1.Louvain algorithm and user characteristics based Collaborative Filtering
# 2.Geographical distance
# 3.Location popularity         

# Data preparation 
# Active users: Screen out users who have checked in less than 10 times;
# Popular location: Screen out locations with less than 5 check-ins;
# Social contact degree: Select users with more than 30 social contacts;

  library(readr)
  library(reshape2)
  library(data.table)
  library(dplyr)
  
  data_checkins <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/checkins.dat", "|", escape_double = FALSE, trim_ws = TRUE)
  data_checkins = data_checkins[-1,]
  
  data_socialgraph <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/socialgraph.dat", "|",
                  escape_double = FALSE, 
                  col_types = cols(first_user_id = col_integer(), second_user_id = col_integer()), 
                  trim_ws = TRUE)
  
  data_socialgraph <- data_socialgraph[rowSums(is.na(data_socialgraph)) != ncol(data_socialgraph),]
  
  data_venues <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/venues.dat", "|", escape_double = FALSE, trim_ws = TRUE)
  data_venues = data_venues[-1,]
  
  data_ratings <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/ratings.dat", 
                        "|", escape_double = FALSE, col_types = cols(user_id = col_double()), 
                        trim_ws = TRUE)

  data_ratings <- data_ratings[rowSums(is.na(data_ratings)) != ncol(data_ratings),]
  # merge duplicate checkin data by mean function 
  data_ratings <- aggregate(data_ratings$rating, 
                            by=list(user_id=data_ratings$user_id, venue_id=data_ratings$venue_id), 
                            data=data_ratings,
                            FUN=mean)
  
  
  #### Active users: 
  # Screen out users who have checked in less than 10 times;
  filter.user <- data.frame(table(data_checkins$user_id))
  colnames(filter.user) <- c("user_id", "freq")
  # filter user checkin frequency more than 10
  filter.user <- filter.user[(filter.user$freq > 9),]
  
  ### Popular location: 
  # Screen out locations with less than 5 check-ins;
  filter.venue <- data.frame(table(data_checkins$venue_id))
  colnames(filter.venue) <- c("venue_id", "freq")
  # filter venue checkin frequency more than 5 check-ins
  filter.venue <- filter.venue[(filter.venue$freq > 4),]
  
  ### Social contact degree: 
  filter.socialgraph <- data.frame(table(data_socialgraph$first_user_id))
  colnames(filter.socialgraph) <- c("user_id", "freq")
  # filter user relation frequency more than 30 check-ins
  filter.socialgraph <- filter.socialgraph[(filter.socialgraph$freq > 30),]
  
  # ----------------------------------------------------------------------------------------------------------------------
  
  # filter checkin data base from user_id wirh user freq checkin more than 10
  data.checkins <- merge(data_checkins, filter.user, by.data_checkins = "user_id", by.filter.user = "user_id")
  # remove column id
  data.checkins <- subset(data.checkins, select = -c(id))
  data.checkins <- subset(data.checkins, select = -c(freq))
  
  # filter checkin data base from venue frequency more than 5 check-ins
  data.checkins <- merge(data.checkins, filter.venue, by.data.checkins = "venue_id", by.filter.venue = "venue_id")
  # remove column freq
  data.checkins <- subset(data.checkins, select = -c(freq))
  
  # filter checkin data base from socialgraph frequency more than 30 contract
  data.checkins <- merge(data.checkins, filter.socialgraph, by.data.checkins = "user_id", by.filter.socialgraph = "user_id")
  # remove column freq
  data.checkins <- subset(data.checkins, select = -c(freq))
  
  # ----------------------------------------------------------------------------------------------------------------------
  
  # filter checkin data base from user_id wirh user freq checkin more than 10
  data.ratings <- merge(data_ratings, filter.user, data_ratings = "user_id", by.filter.user = "user_id")
  # remove column id
  data.ratings <- subset(data.ratings, select = -c(id))
  data.ratings <- subset(data.ratings, select = -c(freq))
  
  # filter checkin data base from venue frequency more than 5 check-ins
  data.ratings <- merge(data.ratings, filter.venue, data.ratings = "venue_id", by.filter.venue = "venue_id")
  # remove column freq
  data.ratings <- subset(data.ratings, select = -c(freq))
  
  # filter checkin data base from socialgraph frequency more than 30 contract
  data.ratings <- merge(data.ratings, filter.socialgraph, data.ratings = "user_id", by.filter.socialgraph = "user_id")
  # remove column freq
  data.ratings <- subset(data.ratings, select = -c(freq))
  
  # Add prefix to id
  data.ratings$user_id <- paste0('U',data.ratings$user_id)
  data.ratings$venue_id <- paste0('V',data.ratings$venue_id)
  # change column name
  colnames(data.ratings) <- c("user_id", "venue_id", "rating")

  df.rating <- as.data.frame(acast(data.ratings, user_id~venue_id, value.var="rating"))
  