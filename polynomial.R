#n-ed fokú polinomiális regresszió

#véletlen adatok generálása
X <- rnorm(100)
Y <- rnorm(100)
n <- length(X)

#PolynomialRegression fv. megjeleníti az adatokat és kiszámítja a másodfokú egyenlet együtthatóját.
PolynomialRegression <- function(X, Y) {
  plot(X, Y)
  szamolX2(X, Y)
#calculateA1 és calculateA0 függvény üres
}

#A másodfokú együttható kiszámítása
szamolX2 <- function(X, Y) {
  osszegX <- Summary(X)
  osszegY <- Summary(Y)
  osszegXY <- Summary(X, Y)
  osszegX2Y <- Summary(X^2, Y)
  osszegX2 <- Summary(X^2)
  osszegX3 <- Summary(X^3)
  osszegX4 <- Summary(X^4)

#számláló és nevező kiszámolása
  szamlalo <- osszegX2Y * ((osszegX2 * n) - (osszegX * osszegX)) - osszegX3 * ((osszegXY * n) - (osszegY * osszegX)) + osszegX2 * ((osszegXY * osszegX) - (osszegY * osszegX2))
  nevezo <- osszegX4 * ((osszegX2 * n) - (osszegX * osszegX)) - osszegX3 * ((osszegX3 * n) - (osszeX2  * osszegX)) + osszegX2 * ((osszegX3 * osszegX) - (osszegX2 * osszegX2))
  
#nullával való osztás ellenőrzése
  if (nevezo == 0) {
    print("Nullával való osztás történt, az eredmény nem értelmezhető.")
    return(NULL)
  }
  
  eredmeny <- szamlalo / nevezo
  print(eredmeny)
}

#összegzés funkció, amely két változót fogad el és visszaadja azok összegét
Summary <- function(X, Y) {
  osszeg <- 0
  n <- length(X)
  if (missing(Y)) {
    for (i in 1:n) {
      osszeg <- osszeg + X[i]
    }
    return(osszeg)
  }
  for (i in 1:n) {
    osszeg <- osszeg + X[i] * Y[i]
  }
  return(osszeg)
}

#futtatás
PolynomialRegression(X, Y)

