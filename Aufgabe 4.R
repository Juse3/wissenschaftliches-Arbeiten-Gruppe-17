#Aufgabe 4

#Daten einlesen
daten <- read.csv("daten.csv", header = TRUE, sep = ";")

#Überblick über vorhandene Variablen
summary(daten)

zusatz()
#Überblick über alle Daten

#Überblick über Variable Alter
descr_metric(daten$Alter)
#Mittelwert liegt bei 24.51, Median und Modus liegen bei 25
#Standardabweichung liegt bei 2.11
#Min=17, Max=29

#Interesse an Mathematik, aufgeteilt nach Studiengang
#Data Science
descr_metric(daten[daten$Studienfach == "Data Science",4])
#Mittelwert = 4.375
#Informatik
descr_metric(daten[daten$Studienfach == "Informatik",4])
#Mittelwert = 4.435
#Mathe
descr_metric(daten[daten$Studienfach == "Mathe",4])
#Mittelwert = 3.765
#Statistik
descr_metric(daten[daten$Studienfach == "Statistik",4])
#Mittelwert = 4.536

#Es wirkt nicht so als würde ein Zusammenhang zwischen Studiengang und Interesse
#an Mathematik bestehen, da die Mathematiker den geringsten Mittelwert aufweisen

#Interesse an Programmieren, aufgeteilt nach Studiengang
#Data Science
descr_metric(daten[daten$Studienfach == "Data Science",5])
#Mittelwert = 3.625
#Informatik
descr_metric(daten[daten$Studienfach == "Informatik",5])
#Mittelwert = 3.565
#Mathe
descr_metric(daten[daten$Studienfach == "Mathe",5])
#Mittelwert = 4.235
#Statistik
descr_metric(daten[daten$Studienfach == "Statistik",5])
#Mittelwert = 3.464

#Es wirkt auch nicht so als würde ein Zusammenhang zwischen Studiengang und Interesse
#am Programmieren bestehen, da die Informatiker einen deutlich kleineren Mittelwert
#als zum Beispiel die Mathematiker haben
