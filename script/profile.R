library(lubridate)

timeprofile <- function(df){

  u.time.profile <- data.frame(matrix(ncol = 25, nrow = 0))
  cname <- c("0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24")
  colnames(u.time.profile) <- cname
  
  u.day.profile  <- data.frame(matrix(ncol = 7, nrow = 0))
  cname <- c("1", "2", "3", "4", "5", "6", "7")
  colnames(u.day.profile) <- cname
  
  u.month.profile
  cname <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
  colnames(u.month.profile) <- cname
  
  v.time.profile <- data.frame(matrix(ncol = 25, nrow = 0))
  cname <- c("0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24")
  colnames(v.time.profile) <- cname
  
  v.day.profile
  cname <- c("1", "2", "3", "4", "5", "6", "7")
  colnames(v.day.profile) <- cname
  
  v.month.profile
  cname <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
  colnames(v.month.profile) <- cname
  
  # loop on user check-in
  for(i in 1:nrow(df)){
    userid <- df[i,2]
    venueid <- df[i,3]
    checkid <- df[i,6]
    
    time <- mdy_hms(checkid)
    year <- factor(year(time))
    month <- factor(month(time))
    day <- factor(day(time))
    wday <- factor(wday(time))
    hour <- factor(hour(time))
    minute <- factor(minute(time))
    second <- factor(second(time))
   
  }
  
  result <- list(user.profile = u, venue.profile = v)
  class(result) <- "profile"
  
  return(result)
}
