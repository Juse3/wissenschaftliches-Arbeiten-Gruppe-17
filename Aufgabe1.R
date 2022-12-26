################################################################################
#                                 Aufgabe 1                                    #
################################################################################

set.seed(17)
## seed setzen um Ergebnisse reproduzieren zu koennen

ID <- 1:100
## ID erzeugen. Es gibt 100 Personen, deswegen die ID von 1 bis 100

Alter <- floor(rnorm(n = 100, mean = 25, sd = 2))
## Das Alter simuliert aus einer Normalverteilung mit Erwartungswert 25 und
## einer Standardabweichung von 2.
## Die Zahlen werden abgerundet, damit das Alter in ganze Zahlen angegeben wird.

Studienfach <- sample(x = c("Statistik", "Data Science", "Mathe", "Informatik"), 
                      size = 100, replace = TRUE, prob = c(30, 30, 15, 25))
## Studienfach, zufällig gezogen für alle Personen aus einer Auswahl von
## „Statistik“, „Data Science“, „Mathe“ und „Informatik“, wobei die Fächer
## „Statistik“ und „Data Science“ mit gleicher Wahrscheinlichkeit von 30 Prozent
## studiert werden sollen, „Informatik“ mit einer etwas geringeren Wahrschein-
## lichkeit von 25 Prozent und „Mathe“ mit der geringsten Wahrscheinlichkeit von
## 15 Prozent.

InteresseAnMathematik <- sample(c(1:7), size = 100, replace = TRUE, 
                                prob = c(10, 15, 15, 10, 15, 20, 15))
## Interesse an Mathematik, wobei hier kein direkter Zusammenhang zwischen 
## Studienfach und dem Interesse besteht.
## Die Variable nimmt Werte ∈ {1,2,3,4,5,6,7} an, wobei 1 = sehr geringes 
## Interesse, und 7 = sehr hohes Interesse abbildet, und 7 = sehr hohes Interesse 
## abbildet.
## Die Wahrscheinlichkeiten fuer das Auftreten der verschiedenen Werte sind an 
## den Studiengaengen orientiert. Insgesamt ist allerdings in allen Studiengaengen 
## Mathematik vertreten, sodass der Unterschied nicht so gross sein wird, 
## zwischen den verschiedenen Studiengaengen. 

InteresseAnProgrammieren <- as.numeric(as.character(
  factor(InteresseAnMathematik, levels = c(1:7), labels = c(7:1))))
## Interesse an Programmieren, wobei hier ein Zusammenhang mit dem Interesse an
## Mathematik besteht. Das Interesse an Mathematik und das Interesse an
## Programmieren korrelieren negativ miteinander. Die Werte der beiden Variablen
## zusammenaddiert ergeben immer acht. 

## Mit labels werden den vorhandenen levels genau die entgegengesetzten Bezeichnungen
## zugeordnet. Um dies tun zu koennen, brauchen wir einen Faktor. Dieser wird erst
## als character und dann als numerischer Vektor abgespeichert, damit die Zahlen 
## wirklich richtig da stehen und nicht nur die levels veraendert werden. 

MatheLk <- function(Studienfach, InteresseAnMathematik){
  i <- 1    # Zaehlvariabel
  MathematikLK <- character(length(Studienfach)) #Zielvektor erzeugen (speicherplatz sparen)
  while (i < length(Studienfach) + 1){    # Schleife fuer alle 100 Personen 
    if (Studienfach[i] == "Mathematik" | Studienfach[i] == "Statistik" & 
        InteresseAnMathematik[i] >= 4){   # Bedingung 
      MathematikLK[i] <- "Ja"
    } if ((Studienfach[i] == "Informatik" | Studienfach[i] == "Data Science") & 
          InteresseAnMathematik[i] >= 3){ # Bedingung
      MathematikLK[i] <- "Ja" 
    } else{    #Alternative
      MathematikLK[i] <- "Nein"
    }
    i <- i + 1
  }
  return(MathematikLK)  # finaler Vektor
}
## Mathe-LK (ja/nein), eine dichotome Variable, die kodiert, ob jemand in der
## Schule Mathe-LK hatte oder nicht; hierbei besteht ein Zusammenhang zum 
## Studienfach und dem Interesse an Mathematik. 
## Wenn das Studienfach Mathematik oder Statistik ist, muss das Interesse an 
## Mathematik >= 4 sein, damit entschieden wird, dass die Person Mathe-LK hatte.
## Bei den Studienfaechern Informatik und Data Science muss das Interesse an 
## Mathematik >= 3 sein, um zu entscheiden, dass die Person Mathe-LK hatte. 

MatheLK <- MatheLk(Studienfach, InteresseAnMathematik)
## Die programmierte Funktion wird mit den bereits erzeugten Daten ausgefuehrt.

daten <- data.frame(ID, Alter, Studienfach, InteresseAnMathematik, 
                    InteresseAnProgrammieren, MatheLK)
## gesammelte Variablen werden in einen Data Frame zusammengefuehrt. 


################################################################################
#                                 Aufgabe 2                                    #
################################################################################

write.csv2(daten, file = "daten.csv", row.names = FALSE)
## csv Datei im deutschen Format erzeugen. Mit dem Argument row.names = FALSE 
## werden die Zeilennummern nicht in die csv-Datei geschrieben. 