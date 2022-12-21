## Aufgabe 1
set.seed(17)

ID <- 1:100

Alter <- floor(rnorm(n = 100, mean = 25, sd = 2))

Studienfach <- sample(x = c("Statistik", "Data Science", "Mathe", "Informatik"), 
                      size = 100, replace = TRUE, prob = c(30, 30, 15, 25))

InteresseAnMathematik <- sample(c(1:7), size = 100, replace = TRUE, 
                                prob = c(5, 5, 10, 15, 15, 25, 25))
