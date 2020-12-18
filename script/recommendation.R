library(reshape2)
library(dplyr)
library(factoextra)
library(readr)
library(lubridate)
library(magrittr) # needs to be run every time you start R and want to use %>%

socialModel <- getSocialRecommend()
error <- socialModel$error
activeuser <- socialModel$activeuser

social.sim <- getSimilarity()
cf <- getCFRecommendation()

data.checkings.clustered <- kmean_clustering(7)
clusterModel <- getClusterRecommendation()
cluster.rating <- clusterModel$ClusterRating
cluster.mean <- clusterModel$cluserMean

cluster.sim <- getSimilarityFromTarget(cluster.rating)
cluster.cf <- getClusterCFRecommendation()

printRMSEChart <- function(){
  propularError <- cf[,c('propular_error')]
  socialMeanError <-cf[,c('social_error')]
  SocialCFError <- cf[,c('sim_error')]
  ClusterMeanError <- cluster.mean[,c('cluster_error')]
  ClusterCFError <- cluster.cf[,c('cluster_error')]
  
  mean.propularError = mean(propularError, na.rm=TRUE)
  mean.socialMeanError = mean(socialMeanError, na.rm=TRUE)
  mean.SocialCFError = mean(SocialCFError, na.rm=TRUE)
  mean.ClusterMeanError = mean(ClusterMeanError, na.rm=TRUE)
  mean.ClusterCFError = mean(ClusterCFError, na.rm=TRUE)
  
  # Create the data for the chart
  H <- c(mean.propularError,mean.socialMeanError,mean.SocialCFError,mean.ClusterMeanError,mean.ClusterCFError)
  M <- c("Popular","Social Popular","Social CF","Cluster Popular","Cluster CF")
  
  # Give the chart file a name
  # png(file = "barchart_months_revenue.png")
  
  # Plot the bar chart 
  barplot(H,names.arg=M,xlab="Method",ylab="RMSE",col="blue",
          main="Error chart of STARs",border="red")
  
  # Save the file
  # dev.off()
}

printRMSEChart()
