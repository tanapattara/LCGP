library(reshape2)
library(dplyr)
library(factoextra)
library(readr)
library(lubridate)
library(magrittr) # needs to be run every time you start R and want to use %>%

socialModel <- getSocialRecommend()
error <- social$error
activeuser <- social$activeuser

social.sim <- getSimilarity()
cf <- getCFRecommendation()

data.checkings.clustered <- kmean_clustering(7)
clusterModel <- getClusterRecommendation()
cluster.rating <- clusterModel$ClusterRating
cluster.mean <- clusterModel$cluserMean

cluster.sim <- getSimilarityFromTarget(y$ClusterRating)
cluster.cf <- getClusterCFRecommendation()
