library(reshape2)
library(dplyr)
library(factoextra)
library(readr)
library(lubridate)
library(magrittr) # needs to be run every time you start R and want to use %>%

x <- getSocialRecommend()
error <- x$error
activeuser <- x$activeuser

sim <- getSimilarity()
cf <- getCFRecommendation()

data.checkings.clustered <- kmean_clustering(7)
y <- getClusterRecommendation()


progressbar.max <- 20 # nrow(activeDF)
progressbar <- txtProgressBar(min = 0, max = progressbar.max, style = 3)

for(i in 1:total){
  Sys.sleep(0.1)
  # update progress bar
  setTxtProgressBar(progressbar, i)
}
close(progressbar)
