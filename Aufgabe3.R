
#Teil a)

#nicht sicher, ob man mean(), median(),... verwenden darf oder die nochmal selber programmieren soll
#eventuell anpassen, nach Absprache

#Funktion descr_metric erwartet einen numerischen Vektor als Eingabe
#Ausgabe: arithmetisches Mittel, Median, Varianz und Standardabweichung der Daten in einer Liste 

descr_metric <- function(vec){
 
  mean <- mean(vec)
  median <- median(vec)
  var <- var(vec)
  sd <- sd(vec)
  
  return(list("mean" = mean, "median" = median, "variance" = var,
         "standard deviation" = sd))
}

#eventuell gibt es eine bessere Art das Ergebnis auszugeben, statt einer Liste

#Teil b)

#Funktion descr_cat erwartet einen Vektor, der Daten einer nominal- bzw. ordinalskalierten Variable enthaelt
#Ausgabe: absolute und relative Haeufigkeitstabelle in einer Liste

descr_cat <- function(vec){
  
  abs_haeuf <- table(vec)
  rel_haeuf <- round(prop.table(table(vec)), 2)
  
  return(list("Absolute Haeufigkeiten" = abs_haeuf, "Relative Häufigkeiten" =
                rel_haeuf))

}
