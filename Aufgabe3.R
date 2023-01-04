#Aufgabe 3

#Teil a)

#Funktion descr_metric erwartet einen numerischen Vektor als Eingabe
#Ausgabe: arithmetisches Mittel, Median, Varianz, Standardabweichung, Minimum, Maximum, Spannweite, Quartile und Interquartilsabstand der Daten in einer Liste 

descr_metric <- function(vec){
 
  mean <- mean(vec)
  median <- median(vec)
  var <- var(vec)
  sd <- sd(vec)
  min <- min(vec)
  max <- max(vec)
  range <- max - min
  uq <- as.numeric(quantile(vec, 0.25))
  oq <- as.numeric(quantile(vec, 0.75))
  iq <- oq - uq
  
  return(list("mean" = mean, "median" = median, "variance" = var,
         "standard deviation" = sd, "minimum" = min,
         "maximum" = max, "range" = range, "unteresQuartil" = uq, 
         "oberesQuartil" = oq, "Interquartilsabstand" = iq))
}

#Teil b)

#Funktion descr_cat erwartet einen Vektor, der Daten einer nominal- bzw. ordinalskalierten Variable enthaelt
#Ausgabe: absolute, relative Haeufigkeitstabelle und Modus in einer Liste

descr_cat <- function(vec){
  
  abs_haeuf <- table(vec)
  rel_haeuf <- round(prop.table(table(vec)), 2)
  modus <- mode(vec)  #Funktion mode in der anderen Datei
  
  return(list("Absolute Haeufigkeiten" = abs_haeuf, "Relative Häufigkeiten" =
                rel_haeuf, "Modus" = modus))

}
