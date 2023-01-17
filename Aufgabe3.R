#Aufgabe 3

#Teil a)

#Funktion descr_metric erwartet einen numerischen Vektor als Eingabe
#Ausgabe: arithmetisches Mittel, Median, Modus, Varianz, Standardabweichung, Minimum, Maximum, Spannweite, Quartile und Interquartilsabstand der Daten in einer Liste 

descr_metric <- function(vec){
 
  mean <- mean(vec)
  median <- median(vec)
  modus <- mode(vec)         #Funktion mode in der anderen Datei
  var <- var(vec)
  sd <- sd(vec)
  min <- min(vec)
  max <- max(vec)
  range <- max - min
  uq <- as.numeric(quantile(vec, 0.25))
  oq <- as.numeric(quantile(vec, 0.75))
  iq <- oq - uq
  skewness <- skewness(vec)
  kurtosis <- kurtosis(vec)
  
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
  rel_haeuf <- round(prop.table(table(vec)), 2)
  modus <- mode(vec)  #Funktion mode in der anderen Datei
  
  return(list("Absolute Haeufigkeiten" = abs_haeuf, "Relative Häufigkeiten" =
                rel_haeuf, "Modus" = modus))

}


# Teil c)

# Funktion erwartet zwei kategoriale Vektoren
# Ausgabe: Kreuztabelle, Spalten- und Zeilensummen dieser Tabelle

relat_cat <- function(vec, vec2, name1, name2){
  
  tab <- table(vec, vec2, dnn = as.list(c(name1, name2)))
  # mit dem Argument dnn koennen die Namen festgelegt werden
  
  zeilen <- margin.table(tab, 1)
  spalten <- margin.table(tab, 2)
  
  return(list("Kreuztabelle" = tab, "Zeilensummen" = zeilen, "Spaltensummen" = spalten))
}

#d)
#vecDichtom ist der dichotome Vektor vec der Metrische, key für die Uebergabe, 
#für die convToLogic
bivar_dichotom <- function(vec, vecDichotom, key){
  
  #Falls der vecDichtom nicht dichotom sein sollte wird hier dieser in einen
  #konvertiert
  if(!is.logical(vecDichotom)){
    vecDichotom <- convToLogic(vecDichotom, key)
    bivar_dichotom(vec,vecDichotom)
  }
  
  
  covPears <- cov(vec, vecDichotom)
  covKend <- cov(vec, vecDichotom, method = "kendall")
  covSpear <- cov(vec, vecDichotom, method = "spearman")
  
  corPears <- cor(vec, vecDichotom)
  corKend <- cor(vec, vecDichotom, method = "kendall")
  corSpear <- cor(vec, vecDichotom, method = "spearman")
  
  return(list("CovariancePearson" = covPears, "CovarianceKendall" =
                covKend, "CovarianceSpearman" = covSpear, "CorrelationPearson" = corPears,
              "CorrelationKendall" = corKend,"CorrelationSpearman" = corSpear))
}

#e)

#Funktion classify erwartet einen Vektor mit einer mindestens ordinal-skalierten Variable
#Ausgabe: Kategorisierung in "niedrig", "mittel", "hoch"

classify <- function(vec){
  
  vec <- as.numeric(vec)
  
  q50 <- quantile(vec,0.5)
  q75 <- quantile(vec,0.75)
  
  for(i in 1:length(vec)){
    
    if(vec[i] >= q75){
      vec[i] <- "hoch"
    }
    else if(vec[i] >= q50){
      vec[i] <- "mittel"
    }
    else{
      vec[i] <- "niedrig"
    }
    
  }
  return(vec)
  
  # Version 2
  q33 <- quantile(vec, 1/3)
  q67 <- quantile(vec, 2/3)
  cut <- cut(vec, breaks = c(-Inf, q33, q67, Inf), labels = c("niedrig", "mittel", "hoch"))

}


# Teil f)

# Funktion erwartet bis zu vier kategoriale Vektoren
# Ausgabe: bis zu vier Säulendiagramme

visual_cat <- function(vec1, vec2, vec3, vec4){
  par(mfrow = c(2,2))
  barplot(table(vec1), ylab = "Absolute Häufigkeit")
  barplot(table(vec2), ylab = "Absolute Häufigkeit")
  barplot(table(vec3), ylab = "Absolute Häufigkeit")
  barplot(table(vec4), ylab = "Absolute Häufigkeit")
}