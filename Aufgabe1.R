## Aufgabe 1

ID <- 1:100

Alter <- round(rnorm(n = 100, mean = 25, sd = 2))

Studienfach <- sample(x = c("Statistik", "Data Science", "Mathe", "Informatik"), 
                      size = 100, replace = TRUE, prob = c(30, 30, 15, 25))