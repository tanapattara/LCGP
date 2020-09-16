library(geosphere)

get.geo_distance = function(long1, lat1, long2, lat2, units = "km") {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  
  distance_m = tryCatch(
    {
      distance_list[[1]]
      #list_extract(distance_list, position = 1)
    },error=function(cond) {
      message(cond)
      return(100000000)
    }
  )
  
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
  }
  return(distance)
}
