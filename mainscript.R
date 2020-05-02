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
  x <- read_delim("Data/checkins.dat", "|", escape_double = FALSE, trim_ws = TRUE)
  data_checkins = x[-1,]
  
  x <- read_delim("Data/socialgraph.dat", "|", escape_double = FALSE, trim_ws = TRUE)
  data_socialgraph = x[-1,]
  
  x <- read_delim("Data/venues.dat", "|", escape_double = FALSE, trim_ws = TRUE)
  data_venues = x[-1,]
  
  rm(x)
  
  #### Active users: 
  # Screen out users who have checked in less than 10 times;
  user.freq <- data.frame(table(data_checkins$user_id))
  colnames(user.freq) <- c("user_id", "freq")
  # filter user checkin frequency more than 10
  user.freq <- user.freq[(user.freq$freq > 10),]
  
  # merge 2 df base from user_id
  checkins <- merge(data_checkins,user.freq, by.data_checkins = "user_id", by.user.freq = "user_id")
  # remove column id
  checkins <- subset(checkins,select = -c(id))
  
  ### Popular location: 
  # Screen out locations with less than 5 check-ins;
  venue.freq <- data.frame(table(data_checkins$venue_id))
  colnames(venue.freq) <- c("venue_id", "freq")
  # filter user checkin frequency less than 5 check-ins
  venue.freq <- venue.freq[(venue.freq$freq < 5),]
  
  # merge 2 df base from user_id
  checkins <- merge(data_checkins,user.freq, by.data_checkins = "user_id", by.user.freq = "user_id")
  # remove column id
  checkins <- subset(checkins,select = -c(id))
  
  ### Social contact degree: 
  # Select users with more than 30 social contacts;
}