## Aufgabe 1

set.seed(17)

ID <- 1:100

Alter <- floor(rnorm(n = 100, mean = 25, sd = 2))

Studienfach <- sample(x = c("Statistik", "Data Science", "Mathe", "Informatik"), 
                      size = 100, replace = TRUE, prob = c(30, 30, 15, 25))

InteresseAnMathematik <- sample(c(1:7), size = 100, replace = TRUE, 
                                prob = c(10, 15, 15, 10, 15, 20, 15))

InteresseAnProgrammieren <- as.numeric(as.character(
  factor(InteresseAnMathematik, levels = c(1:7), labels = c(7:1))))

MatheLK <- function(Studienfach, InteresseAnMathematik){
  i <- 1
  MathematikLK <- character(length(Studienfach))
  while (i < length(Studienfach) + 1){
    if (Studienfach[i] == "Mathematik" & InteresseAnMathematik[i] >= 4){
      MathematikLK[i] <- "Ja"
    }
    if (Studienfach[i] == "Statistik" & InteresseAnMathematik[i] >= 4){
      MathematikLK[i] <- "Ja"
    }
    if ((Studienfach[i] == "Informatik" | Studienfach[i] == "Data Science") & 
        InteresseAnMathematik[i] >= 3){
      MathematikLK[i] <- "Ja" 
    }
    else{
      MathematikLK[i] <- "Nein"
    }
    i <- i + 1
  }
  
  return(MathematikLK)
}

MathematikLK <- MatheLK(Studienfach, InteresseAnMathematik)

daten <- data.frame(ID, Alter, Studienfach, InteresseAnMathematik, 
                    InteresseAnProgrammieren, MathematikLK)

write.csv(daten, "C:/Users/laris/Desktop/wissenschaftliches-Arbeiten-Gruppe-17/daten.csv")