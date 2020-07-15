library(readr)
library(reshape2)
library(data.table)
library(dplyr)
library(lsa)
library(tidyr)

ratings100 <- create.sample.data()
w <- create.similarity(ratings100)
lcgp <- characteristics.cf(ratings100, w)
recommend <- recommendation(lcgp,data.user,data.venues)

# setting R not use exponential notation
options("scipen"=100)
ratings <- as.data.frame(acast(data.ratings, user_id~venue_id,  value.var="x"))
testingData <- ratings[1:20,]
validated <- lcgp.validation(testingData,data.user,data.venues)
gc()

# tenfold validation
lcgp.validation <- function(df, location.user, location.venue){
  
  # split data
  row <- nrow(df) * 90 / 100
  row <- round(row, 0)
  # divide data
  df.train <- df[1:row,]
  df.test <- df[row:nrow(df),]
  
  df.precision = matrix(0, nrow(df.test), 5)
  df.precision <- as.data.frame(df.precision)
  
  df.recall = matrix(0, nrow(df.test), 5)
  df.recall <- as.data.frame(df.precision)
  
  precision <- vector()
  recall <- vector()
  
  for(i in 1:nrow(df.test)){
    # get active user row
    active.user <- df.test[i,]
    # at to training data
    df.temp <- rbind(df.train, active.user)
    # calculate similartity on df.temp
    w <- create.similarity(df.temp)
    # calculate cf on temp data
    lcgp <- characteristics.cf(df.temp, w)
    # predict rating
    lcgp <- recommendation(lcgp,location.user,location.venue)
    #lcgp <- recommendation(lcgp,data.user,data.venues)
    
    # validation
    # get top n DESCENDING from active.user
    active.topn <- active.user[order(-active.user)]
    active.username <- row.names(active.user[1,])
    # get active prediction from model
    active.predicted <- lcgp$rating[active.username,]
    active.topnp <- active.predicted[order(-active.predicted)]
    
    for(k in 2:5){
      item.relevant <- get.item.relevant(active.topn)
      item.recommend <- get.item.recommended(active.topnp,n = k)
      
      # precision
      pc <- get.precision(item.relevant,item.recommend)
      # recall
      rc <- get.recall(item.relevant,item.recommend);
      
      # add to result
      df.precision[i,k-1] = pc
      df.recall[i,k-1] = rc
    }
  }
  
  result <- list(precision = df.precision, recall = df.recall, train = df.train,test = df.test)
  class(result) <- "lcgp"
  
  return(result)
}
  get.precision <- function(relevant, recommend){
    recommend.count <- ncol(recommend)
    intersec <- length(intersect(names(relevant),names(recommend)))
    return(intersec/recommend.count)
  }
  get.recall <- function(relevant, recommend){
    recommend.count <- ncol(recommend)
    relevant.count <- ncol(relevant)
    return(intersec/relevant.count)
  }
  get.item.relevant <- function(df, r = 3.5,n = 2){
    # DEFAULT rating 3.5
    cond1 <- sapply(df, function(col) all(is.na(col)))
    cond2 <- sapply(df, function(col) all(col < r))
    mask <- !(cond1 | cond2)
    result <- df[,mask]
    
    if(ncol(result) < n)
      return(result)
    else
      return(result[,1:n])
  }
  get.item.recommended <- function(df, r = 3.5, n = 2){
    # DEFAULT rating 3.5
    cond1 <- sapply(df, function(col) all(is.na(col)))
    cond2 <- sapply(df, function(col) all(col < r))
    mask <- !(cond1 | cond2)
    result <- df[,mask]
    
    if(ncol(result) < n)
      return(result)
    else
      return(result[,1:n])
  }
  get.discovering.location <- function(df, r = 3){
    df <- colSums(df > r)
    result <- length(which(df > 0))
    return(result)
  }

