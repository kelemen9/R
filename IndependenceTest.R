#Függetlenségvizsgálat

#Adatok megadása(row=sor, col=oszlop)
#kekSzem <- c(42, 28, 3)
#barnaSzem <- c(17, 89, 21)
#N <- 200
adat_matrix <- matrix(c(42, 28, 3, 17, 89, 21), nrow = 2, ncol = 3, byrow = TRUE)

fuggetlenseg_vizsgalat <- function(adatok) {
#Adatok lekérdezése
  sorok <- dim(adatok)[1]
  oszlopok <- dim(adatok)[2]
  
#Szorzatok, összegek kiszámítása(osszeg=N)
  osszeg <- sum(adatok)
  sor_osszegek <- rowSums(adatok)
  oszlop_osszegek <- colSums(adatok)
  
#Khi-négyzet érték kiszámítása
  khi_negyzet <- 0
  for (i in 1:sorok) {
    for (j in 1:oszlopok) {
     khi_negyzet <- khi_negyzet + ((adatok[i, j] - ((sor_osszegek[i] * oszlop_osszegek[j]) / osszeg)) ^ 2)/((sor_osszegek[i]*oszlop_osszegek[j])/osszeg)
    }
  }
  
#Eredmény kiíratása
  cat("Khi-négyzet érték:", khi_negyzet, "\n")
}

#Függetlenségvizsgálat futtatasa
fuggetlenseg_vizsgalat(adat_matrix)
