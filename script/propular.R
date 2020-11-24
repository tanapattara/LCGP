library("dplyr")          ## load

getPropularVenue <- function(df){

  result <- df %>%
    group_by(venue_id)   %>%
    summarize(mean_rating = mean(rating, na.rm = TRUE), .groups = 'drop')
  
  return(result)
}
