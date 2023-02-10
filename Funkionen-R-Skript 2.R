#Bsp : daten$MatheLK = convToLogic(daten$MatheLK,"Ja")
#"Nein" wird zu False & "Ja" zu True
convToLogic <- function(vec,key){
  vec <- vec==key
  vec
}
#Funktion für dichotome Masszahlen

mode <- function(vec){
  u <- unique(vec)
  tab <- tabulate(match(vec, u))
  u[tab == max(tab)]
}
#Funktion um den Modus zu bestimmen

#Funktionen um Schiefe und Kurtosis zu bestimmen:

skewness <- function(vec){
  
  length <- length(vec)
  mean <- mean(vec)
  sd <- sd(vec)
  
  result <- (1/length)*sum(((vec-mean)/sd)^3)

  return(result)
}

kurtosis <- function(vec){
  
  length <- length(vec)
  mean <- mean(vec)
  sd <- sd(vec)
  
  result <- (1/length)*sum(((vec-mean)/sd)^4)
  
  return(result)
}
