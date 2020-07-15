# Comprehensiverecommendation (LC–G–P)
# recommendation rating of location l to user i
# 0.3 x C + 0.3 x G + 0.4 x P

lcgp <- function(data){
  w <- create.similarity(data)
  lcgp <- characteristics.cf(data, w)
  return(lcgp)
}
