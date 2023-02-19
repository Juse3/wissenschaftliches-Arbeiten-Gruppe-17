#Aufgabe 4

#Daten einlesen
daten <- read.csv("daten.csv", header = TRUE, sep = ";")

#Ueberblick ueber vorhandene Variablen
summary(daten)

#Ueberblick über alle Daten
zusatz()

#Ueberblick ueber Variable Alter
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

#Es wirkt nicht so als wuerde ein Zusammenhang zwischen Studiengang und Interesse
#an Mathematik bestehen, da die Mathematiker den geringsten Mittelwert aufweisen

#Visualisierung Interesse an Mathematik, aufgeteilt nach Studiengang
grouped_barplot(daten$Studienfach, daten$InteresseAnMathematik,sizeLegend = 1,
                xlab = "Interesse an Mathematik")

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

#Es wirkt auch nicht so als wuerde ein Zusammenhang zwischen Studiengang und Interesse
#am Programmieren bestehen, da die Informatiker einen deutlich kleineren Mittelwert
#als zum Beispiel die Mathematiker haben

#Visualisierung Interesse an Programmieren, aufgeteilt nach Studiengang
grouped_barplot(daten$Studienfach, daten$InteresseAnProgrammieren,sizeLegend = 0.533,
                xlab = "Interesse an Programmieren")

#Ueberblick ueber Variable Studienfach
descr_cat(daten$Studienfach)
#Die meisten Studierenden aus dem Datensatz studieren Data Science (32%)
#Es folgt Statistik (28%)
#Informatik(23%), Mathe(17%)

#Ueberblick ueber Variable MatheLK
descr_cat(daten$MatheLK)
#Fast die Haelfte war im MatheLK(45%)

relat_cat(daten$MatheLK,daten$Studienfach,"MatheLK","Studienfach")
#Es besteht ein Zummmenhang zwischen MatheLK und und dem Studienfach, nur Studenten
#mit Fach Data Science und Informatik hatten MatheLK

#Visualisierung Belegung Mathe LK, aufgeteilt nach Studiengang
grouped_barplot(daten$Studienfach, daten$MatheLK,sizeLegend = 0.7,
                xlab = "MatheLK")

bivar_dichotom(daten$Alter,daten$MatheLK,"Ja","Alter","MatheLK")
#Es besteht kein Zusammenhang zwischen Alter und MatheLK, da nur geringe
#negative Korrelation bzw. Kovarianz festgestellt werden kann.

#Zusammenhang zwischen Interesse an Mathematik und Mathe LK
bivar_dichotom(daten$InteresseAnMathematik, daten$MatheLK, "Ja",
               "Interesse an Mathematik", "MatheLK")
#Es besteht ein leicht positiver Zusammenhang zwischen MatheLK und Interesse an Mathematik.
#Leute mit einem sehr geringen Interesse an Mathematik (Interesse = 1 oder 2)
#waren nicht im MatheLK. Das sind insgesamt 21 Studierende. Ab einem Interesse
#von 3 gibt es immer mindestens so viele Studierende, die im LK waren wie Studierende,
#die nicht im Mathe-LK waren.
#Korrelationskoeffizient nach Pearson: 0.3220041

#Zusammenhang zwischen Interesse an Programmieren und Mathe LK
bivar_dichotom(daten$InteresseAnProgrammieren, daten$MatheLK, "Ja",
               "Interesse an Programmieren", "MatheLK")
#Es besteht ein leicht negativer Zusammenhang zwischen MatheLK und Interesse an Programmieren.
#Leute mit einem sehr hohen Interesse an Mathematik (Interesse = 6 oder 7)
#waren nicht im MatheLK. Das sind insgesamt 21 Studierende. Bis einem Interesse von 5
#gibt es immer mindestens so viele Studierende, die im LK waren wie Studierende, die
#nicht im Mathe-LK waren.
#Korrelationskoeffizient nach Pearson: -0.3146465

#Quantilbasierte Kategorisierung
classify(daten$Alter)
classify(daten$InteresseAnMathematik)
classify(daten$InteresseAnProgrammieren)

#Visuelle Dartstellung kategorialer Variablen
visual_cat(daten$Studienfach, daten$MatheLK, daten$InteresseAnMathematik,
           daten$InteresseAnProgrammieren)
