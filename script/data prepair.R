library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# load data
raw.checkins <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/checkins.dat", 
                       "|", escape_double = FALSE, col_types = cols(created_at = col_datetime(format = "%Y-%m-%d %H:%M:%S")), 
                       trim_ws = TRUE)
raw.checkins = raw.checkins[rowSums(is.na(raw.checkins[ ,2:3])) == 0, ]
raw.checkins <- subset(raw.checkins, select = -c(id, latitude, longitude))
raw.checkins$Year <- factor(year(raw.checkins$created_at))
raw.checkins$Month <- factor(month(raw.checkins$created_at))
raw.checkins$Day <- factor(day(raw.checkins$created_at))
raw.checkins$Weekday <- factor(wday(raw.checkins$created_at))
raw.checkins$Hour <- factor(hour(raw.checkins$created_at))
raw.checkins$Minute <- factor(minute(raw.checkins$created_at))
raw.checkins$Second <- factor(second(raw.checkins$created_at))

raw.socialgraph <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/socialgraph.dat", "|",
                               escape_double = FALSE, 
                               col_types = cols(first_user_id = col_integer(), second_user_id = col_integer()), 
                               trim_ws = TRUE)

raw.socialgraph <- raw.socialgraph[rowSums(is.na(raw.socialgraph[ , 1])) == 0,]
raw.socialgraph <- distinct(raw.socialgraph)

raw.venues <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/venues.dat", 
                          "|", escape_double = FALSE, col_types = cols(id = col_integer()), 
                          trim_ws = TRUE)
colnames(raw.venues) <- c('venue_id','latitude','longitude')
raw.venues = raw.venues[rowSums(is.na(raw.venues[ , 1:3])) == 0,]

raw.users <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/users.dat", 
                         "|", escape_double = FALSE, col_types = cols(id = col_integer()), 
                         trim_ws = TRUE)
colnames(raw.users) <- c('user_id','latitude','longitude')
raw.users = raw.users[rowSums(is.na(raw.users[ , 1:3])) == 0,]

raw.ratings <- read_delim("G:/My Drive/Research/Data/Dataset/data_umn_foursquare_datasets/ratings.dat", 
                           "|", escape_double = FALSE, col_types = cols(user_id = col_integer(), 
                                                                        venue_id = col_integer()), trim_ws = TRUE)

raw.ratings <- raw.ratings[rowSums(is.na(raw.ratings[ , 1])) == 0,]

# merge duplicate checkin data by mean function

raw.ratings <- aggregate(raw.ratings$rating, 
                          by=list(user_id=raw.ratings$user_id, venue_id=raw.ratings$venue_id), 
                          data=raw.ratings,
                          FUN=mean)
colnames(raw.ratings) <- c("user_id", "venue_id", "rating")

# Add prefix --------------------------------------------------------------------------------
raw.ratings$user_id <- paste0('U',raw.ratings$user_id)
raw.ratings$venue_id <- paste0('V',raw.ratings$venue_id)

raw.users$user_id <- paste0('U',raw.users$user_id)
raw.venues$venue_id <- paste0('V',raw.venues$venue_id)

raw.checkins$user_id <- paste0('U',raw.checkins$user_id)
raw.checkins$venue_id <- paste0('V',raw.checkins$venue_id)

raw.socialgraph$first_user_id <- paste0('U', raw.socialgraph$first_user_id)
raw.socialgraph$second_user_id <- paste0('U', raw.socialgraph$second_user_id)

#-----------------------------------------------------------------------------------------------------------------------
data.checkins <- raw.checkins
data.socialgraph <- raw.socialgraph
data.user <- raw.users
data.venues <- raw.venues
# ----------------------------------------------------------------------------------------------------------------------

# filter checkin data base from user_id wirh user freq raing more than 10
data.ratings <- raw.ratings

filter.user <- data.frame(table(data.ratings$user_id))
colnames(filter.user) <- c("user_id", "freq")
filter.venue <- data.frame(table(data.ratings$venue_id))
colnames(filter.venue) <- c("venue_id", "freq")

filter.user <- filter.user[(filter.user$freq < 3),]
filter.venue <- filter.venue[(filter.venue$freq < 3),]

data.ratings <- data.ratings[!(data.ratings$user_id %in% filter.user$user_id),]
data.ratings <- data.ratings[!(data.ratings$venue_id %in% filter.venue$venue_id),]


hist(filter.user$freq,
     main="Histogram for user rating frequency",
     xlab="Frequency",
     border="blue",
     col="yellow",
     xlim=c(10,50),
     las=1,
     breaks=200)

hist(filter.venue$freq,
     main="Histogram for venue rating frequency",
     xlab="Frequency",
     border="blue",
     col="yellow",
     xlim=c(10,100),
     las=1,
     breaks=200)

# re-order column
data.ratings <- data.ratings[c("user_id", "venue_id", "rating")]
data.ratings$user_id <- paste0('U',data.ratings$user_id)
data.ratings$venue_id <- paste0('V',data.ratings$venue_id)

ratingDF <- as.data.frame(acast(data.ratings, user_id~venue_id, value.var="rating"))
ratingDF <- data.matrix(ratingDF)

heatmap(ratingDF)

# -----------------------------------------------------------------------------------------------------------------------
# filter social graph

data.socialgraph <- raw.socialgraph
filter.social <- data.frame(table(data.socialgraph$first_user_id))
colnames(filter.social) <- c("user", "freq")

filter.social <- filter.social[(filter.social$freq > 4),]

# -----------------------------------------------------------------------------------------------------------------------

# change column name
colnames(data.ratings) <- c("user_id", "venue_id", "rating")
library(reshape2)
ratingsDF <- as.data.frame(acast(data.ratings, user_id~venue_id,  value.var="rating"))

# -----------------------------------------------------------------------------------------------------------------------
rm(raw.ratings,raw.socialgraph,raw.users,raw.venues,raw.checkins)
rm(filter.user,filter.venue,filter.socialgraph)
gc()

# -----------------------------------------------------------------------------------------------------------------------

create.sample.data <- function(){
  V1000005 <- c(4,5,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
  V1000097 <- c(NaN,NaN,NaN,3,NaN,NaN,NaN,NaN,NaN,NaN)
  V100017 <- c(NaN,NaN,NaN,NaN,NaN,5,3,3,NaN,NaN)
  V100043 <- c(NaN,NaN,NaN,NaN,5,NaN,4,3,4,NaN)
  V100048 <- c(NaN,NaN,NaN,NaN,NaN,4,5,4,NaN,5)
  V100051 <- c(NaN,4,5,NaN,NaN,NaN,NaN,NaN,NaN,NaN)
  V100057 <- c(4,NaN,5,3,NaN,NaN,NaN,NaN,NaN,NaN)
  V10006 <- c(3,NaN,5,3,NaN,NaN,NaN,NaN,NaN,NaN)
  V10007 <- c(4,2,NaN,3,NaN,NaN,NaN,NaN,NaN,NaN)
  V10011 <- c(NaN,4,NaN,3,NaN,NaN,NaN,NaN,NaN,NaN)
  # Join the variables to create a data frame
  x <- data.frame(V1000005,V1000097,V100017,V100043,V100048,V100051,V100057,V10006,V10007,V10011)
  rownames(x) = c('U1000235','U100053','U100070','U1001208','U100175','U100256','U10035','U1003659','U100366','U1004058')
  # set na value to zero
  # x[is.na(x)] <- 0
  return(x)
}
rm(create.sample.data)
# write csv
# -----------------------------------------------------------------------------------------------------------------------

write.csv(y,"C:\\Users\\dell\\Desktop\\y", row.names = FALSE)
write.csv(z,"C:\\Users\\dell\\Desktop\\z", row.names = FALSE)
