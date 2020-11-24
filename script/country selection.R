#Northernmost point: Myanmar border, Mae Sai District, Chiang Rai Province, at 20°28′N 99°57′E
#Southernmost point: Malaysian border, Betong District, Yala Province, at 5°37′N 101°8′E
#Easternmost point: Laos border, Khong Chiam District, Ubon Ratchathani Province, at 15°38′N 105°38′E
#Westernmost point: Myanmar border, Mae Sariang District, Mae Hong Son Province, at 18°34′N 97°21′E

# thailand
getThailanVenue = function(df){
  x <- df[df[, "latitude"] > 5,]
  x <- x[x[, "latitude"] < 20,]
  x <- x[x[, "longitude"] > 97,]
  x <- x[x[, "longitude"] < 105,]
  return(x)
}