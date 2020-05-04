# 1.Louvain algorithm and user characteristics based Collaborative Filtering
# 2.Geographical distance
# 3.Location popularity         

# Data preparation 
# Active users: Screen out users who have checked in less than 10 times;
# Popular location: Screen out locations with less than 5 check-ins;
# Social contact degree: Select users with more than 30 social contacts;

# load data
load_data <- function() {
  library(readr)
  x <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/checkins.dat", "|", escape_double = FALSE, trim_ws = TRUE)
  data_checkins = x[-1,]
  
  x <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/socialgraph.dat", "|",
                  escape_double = FALSE, 
                  col_types = cols(first_user_id = col_integer(), second_user_id = col_integer()), 
                  trim_ws = TRUE)
  
  data_socialgraph <- x[rowSums(is.na(x)) != ncol(x),]
  
  x <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/venues.dat", "|", escape_double = FALSE, trim_ws = TRUE)
  data_venues = x[-1,]
  
  rm(x)
  
  #### Active users: 
  # Screen out users who have checked in less than 10 times;
  user.freq <- data.frame(table(data_checkins$user_id))
  colnames(user.freq) <- c("user_id", "freq")
  # filter user checkin frequency more than 10
  user.freq <- user.freq[(user.freq$freq > 9),]
  
  ### Popular location: 
  # Screen out locations with less than 5 check-ins;
  venue.freq <- data.frame(table(data_checkins$venue_id))
  colnames(venue.freq) <- c("venue_id", "freq")
  # filter user checkin frequency more than 5 check-ins
  venue.freq <- venue.freq[(venue.freq$freq > 4),]
  
  ### Social contact degree: 
  socialgraph.firstuser.freq <- data.frame(table(data_socialgraph$first_user_id))
  colnames(socialgraph.firstuser.freq) <- c("user_id", "freq")
  # filter user relation frequency more than 30 check-ins
  socialgraph.firstuser.freq <- socialgraph.firstuser.freq[(socialgraph.firstuser.freq$freq > 30),]
  
  
  
  # merge 2 df base from user_id
  checkins <- merge(data_checkins,user.freq, by.data_checkins = "user_id", by.user.freq = "user_id")
  # remove column id
  checkins <- subset(checkins,select = -c(id))
  # merge 2 df base from user_id
  checkins <- merge(data_checkins,user.freq, by.data_checkins = "user_id", by.user.freq = "user_id")
  # remove column id
  checkins <- subset(checkins,select = -c(id))
  
}