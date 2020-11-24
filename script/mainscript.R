library(reshape2)
library(data.table)
library(dplyr)
library(lsa)
library(tidyr)
# setting R not use exponential notation
options("scipen"=100)

ratings100 <- create.sample.data()
w <- get.similarity(ratings100)
lcgp <- characteristics.cf(ratings100, w)
recommend <- get.lcgp.prediction(lcgp, data.user, data.venues)

ratings <- as.data.frame(acast(data.ratings, user_id~venue_id,  value.var="rating"))
df <- ratings[1:1000,]
validated <- get.validation(df, data.user, data.venues)
gc()

# tenfold validation
get.validation <- function(df, location.user, location.venue){
  
  # split data
  row <- nrow(df) * 90 / 100
  row <- round(row, 0)
  # divide data
  df.train <- df[1:row,]
  df.test <- df[row:nrow(df),]

  #for(i in 1:nrow(df.test)){
  active.i <- df.test[1,]
  prediction <- get.predictions(active.i, df.train)
  #}
    
  # Validation
  
  # remove na column
  x <- df.test[1,]
  x <- x[,colSums(is.na(x))<nrow(x)]
  n <- ncol(x)  
  
  lcf <- prediction$lcf;
  
  sum <- 0
  
  # RMSE
  for(i in 1:ncol(x)){
    rname <- rownames(x)[1]
    cname <- colnames(x)[i]
    
    vx <- x[[cname]]
    
    predicted <- lcf[[rname]]
    px <- predicted[[cname]]
    
    sum <- sum + ((px-vx)*(px-vx))
  }
  
  rmse <- sqrt(sum/n)
  
  result <- list(rmse = rmse, 
                 rating.lcgp = prediction$rating.lcgp,
                 rating.lcf = prediction$rating.lcf)
  
  class(result) <- "recommendation"
  
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

