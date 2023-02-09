#Aufgabe 3

#Teil a)

#Funktion descr_metric erwartet einen numerischen Vektor als Eingabe
#Ausgabe: arithmetisches Mittel, Median, Modus, Varianz, Standardabweichung, Minimum, Maximum, Spannweite, Quartile und Interquartilsabstand sowie die Schiefe und Kurtosis der Daten in einer Liste 

descr_metric <- function(vec){
 
  mean <- mean(vec)
  median <- median(vec)
  modus <- mode(vec)         # Funktion mode in der anderen Datei
  var <- var(vec)
  sd <- sd(vec)
  min <- min(vec)
  max <- max(vec)
  range <- max - min
  uq <- as.numeric(quantile(vec, 0.25))
  oq <- as.numeric(quantile(vec, 0.75))
  iq <- oq - uq
  skewness <- skewness(vec) # Funktion skewness in der anderen Datei
  kurtosis <- kurtosis(vec) # Funktion kurtosis in der anderen Datei
  
  return(list("mittelwert" = mean, "median" = median, "modus" = modus, 
              "varianz" = var, "standardabweichung" = sd, "minimum" = min,
              "maximum" = max, "spannweite" = range, "unteresQuartil" = uq, 
              "oberesQuartil" = oq, "Interquartilsabstand" = iq, 
              "schiefe" = skewness, "kurtosis" = kurtosis))
}

#Teil b)

#Funktion descr_cat erwartet einen Vektor, der Daten einer nominal- bzw. ordinalskalierten Variable enthaelt
#Ausgabe: absolute, relative Haeufigkeitstabelle und Modus in einer Liste

descr_cat <- function(vec){
  
  abs_haeuf <- table(vec)
  rel_haeuf <- round(prop.table(table(vec)), 2) # Werte werden auf zwei Nachkommastellen gerundet
  modus <- mode(vec)  # Funktion mode in der anderen Datei
  
  return(list("Absolute Haeufigkeiten" = abs_haeuf, "Relative Häufigkeiten" =
                rel_haeuf, "Modus" = modus))

}


# Teil c)

# Funktion erwartet zwei kategoriale Vektoren
# Ausgabe: Kreuztabelle, Spalten- und Zeilensummen dieser Tabelle, Cramer's V

library(confintr)
relat_cat <- function(vec, vec2, name1, name2){
  
  tab <- table(vec, vec2, dnn = as.list(c(name1, name2)))
  # mit dem Argument dnn koennen die Namen festgelegt werden
  
  zeilen <- margin.table(tab, 1)
  spalten <- margin.table(tab, 2)
  cramersv <- cramersv(tab)
  
  return(list("Kreuztabelle" = tab, "Zeilensummen" = zeilen, "Spaltensummen" = spalten,
              "Cramer's V" = cramersv))
}

#d)

#Funktion bivar_dichotom erwartet eine metrischen und eine dichotome Variable
#vecDichtom ist der dichotome Vektor vec der Metrische, key für die Uebergabe, 
#für die convToLogic
#Ausgabe: Kreuztabelle, Pearson-, Kendall- und Spearmankovarianz und Pearson-, Kendall- und Spearmankorrelationskoeffizienten in einer Liste
bivar_dichotom <- function(vec, vecDichotom, key, name1 = "name1", name2 = "name2"){
  
  tab <- table(vec, vecDichotom, dnn = as.list(c(name1, name2)))
  
  zeilen <- margin.table(tab, 1)
  spalten <- margin.table(tab, 2)
  
  #Falls der vecDichtom nicht dichotom sein sollte wird hier dieser in einen
  #konvertiert
  if(!is.logical(vecDichotom)){
    vecDichotom <- convToLogic(vecDichotom, key) # Funktion in der anderen Datei
    bivar_dichotom(vec,vecDichotom)
  }
  
  covPears <- cov(vec, vecDichotom)
  covKend <- cov(vec, vecDichotom, method = "kendall")
  covSpear <- cov(vec, vecDichotom, method = "spearman")
  
  corPears <- cor(vec, vecDichotom)
  corKend <- cor(vec, vecDichotom, method = "kendall")
  corSpear <- cor(vec, vecDichotom, method = "spearman")
  
  return(list("Kreuztabelle" = tab, "CovariancePearson" = covPears, "CovarianceKendall" =
                covKend, "CovarianceSpearman" = covSpear, "CorrelationPearson" = corPears,
              "CorrelationKendall" = corKend,"CorrelationSpearman" = corSpear))
}

#e)

#Funktion classify erwartet einen Vektor mit einer mindestens ordinal-skalierten Variable
#Ausgabe: Kategorisierung in "niedrig", "mittel", "hoch"

classify <- function(vec){
  
  vec <- as.numeric(vec)
  
  q33 <- quantile(vec, 1/3)
  q67 <- quantile(vec, 2/3)
  
  cut <- cut(vec, breaks = c(-Inf, q33, q67, Inf),
             labels = c("niedrig", "mittel", "hoch"), right = FALSE)
  
  return(cut)
  
}


# Teil f)

# Funktion visual_cat erwartet bis zu vier kategoriale Vektoren
# Ausgabe: bis zu vier Säulendiagramme

visual_cat <- function(vec1, vec2, vec3, vec4){
  par(mfrow = c(2,2))
  barplot(table(vec1), ylab = "Absolute Häufigkeit")
  barplot(table(vec2), ylab = "Absolute Häufigkeit")
  barplot(table(vec3), ylab = "Absolute Häufigkeit")
  barplot(table(vec4), ylab = "Absolute Häufigkeit")
}

# Funktion grouped_barplot erwartet einen Vektor zur Gruppierung, einen Vektor mit den Daten, die dargestellt werden sollen,
# einen Wert für die Größe der Legende und einen Titel für die x-Achse
# Ausgabe: gruppierter Barplot

grouped_barplot <- function(groupVec, vec2, sizeLegend, xlab){
  
  table <- table(groupVec, vec2)
  
  barplot(table, col = 1:nrow(table), ylab = "Absolute Häufigkeit", xlab = xlab)
  
  legend("topleft", legend = row.names(table), pch = 15, col = 1:nrow(table),
         cex = sizeLegend)
  
}

# weitere Funktionen 
# die Funktion stellt alle Variablen (ausser ID) in einzelnen Grafiken dar, um 
# die Verteilung auf einen Blick sehen zu koennen
# die Eingabe ist fuer unseren Datensatz bereits als default eingestellt, sodass
# nur zusatz() ausgefuehrt werden muss
zusatz <- function(alter = daten$Alter, InteresseAnMathematik = daten$InteresseAnMathematik,
                   InteresseAnProgrammieren = daten$InteresseAnProgrammieren,
                   Studienfach = daten$Studienfach, LK = daten$MatheLK){
  layout(matrix(c(1:6), ncol = 3))
  hist(alter, freq = FALSE, main = "Histogramm vom Alter")
  barplot(table(InteresseAnMathematik), main = "Interesse an Mathematik")
  barplot(table(InteresseAnProgrammieren), main = "Interesse an Programmieren")
  barplot(table(Studienfach), main = "Studienfach")
  barplot(table(LK), main = "Mathe-Lk")
}

