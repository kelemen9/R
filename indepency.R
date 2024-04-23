#FUGGETLENSEG VIZSGALAT

fuggetlenseg_vizsgalat <- function(k) {

# Adat mátrix definiálása

adat_matrix <- matrix(c(42, 28, 3, 17, 89, 21), nrow = 2, ncol = 3, byrow = TRUE)

  #Sorok és oszlopok összegének meghatározása
  sor_osszegek <- rowSums(k)
  oszlop_osszegek <- colSums(k)
  
  # Összes adat elem meghatározása
  n <- sum(k)
  
  # Sorok és oszlopok száma
  r <- length(sor_osszegek)
  s <- length(oszlop_osszegek)
  
  sor_osszeg <- 0
  
  # Khi-négyzet érték kiszámítása
  for (i in 1:r) {
    for (j in 1:s) {
      sor_osszeg <- sor_osszeg + ((k[i, j] - ((sor_osszegek[i] * oszlop_osszegek[j]) / n)) ^ 2) / ((sor_osszegek[i] * oszlop_osszegek[j]) / n)
    }
  }
  
  # Eredmény kiíratása
  khi_negyzet <- sor_osszeg
  cat("Khi-négyzet érték:", khi_negyzet, "\n")
}

# Függetlenségvizsgálat futtatása
fuggetlenseg_vizsgalat(adat_matrix)

