convToLogic <- function(daten){
  daten$MatheLK[daten$MatheLK=="Nein"] <- F
  daten$MatheLK[daten$MatheLK=="Ja"] <- T
  daten$MatheLK <- as.logical(daten$MatheLK)
}
convToLogic(daten)
#damit man z.B. einfach die cov berechnet

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
