#n-ed fokú polinomiális regresszió

#véletlen adatok generálása
x <- c(1,2,3,4)
y <- c(5*x^2+3*x+7)
n <- length(X)

#PolynomialRegression fv. megjeleníti az adatokat és kiszámítja a másodfokú egyenlet együtthatóját.

PolynomialRegression <- function(X, Y) {

  plot(X, Y)
  a0 <- szamolA0(X, Y)
  a1 <- szamolA1(X, Y)
  a2 <- szamolA2(X, Y)

   lines(X, a2 * X^2 + a1 * X + a0, type = "l", lty = 1, col = "blue")

   cat("a0:", a0, "\n")
   cat("a1:", a1, "\n")
   cat("a2:", a2, "\n")
}

#A1 másodfokú együttható kiszámítása
  szamolA1 <- function(X, Y) {

  osszegX <- Summary(X)
  osszegY <- Summary(Y)
  osszegXY <- Summary(X, Y)
  osszegX2Y <- Summary(X^2, Y)
  osszegX2 <- Summary(X^2)
  osszegX3 <- Summary(X^3)
  osszegX4 <- Summary(X^4)

#számláló és nevező kiszámolása
  szamlalo <- osszegX4 * ((osszegXY * n) - (osszegY * osszegX)) - osszegX2Y * ((osszegX3 * n) - (osszegX2 * osszegX)) + osszegX2 * ((osszegX3 * osszegY) - (osszegX2 * osszegXY))
  nevezo <- osszegX4 * ((osszegX2 * n) - (osszegX * osszegX)) - osszegX3 * ((osszegX3 * n) - (osszegX2  * osszegX)) + osszegX2 * ((osszegX * osszegX) - (osszegX2 * osszegX2))
  return(szamlalo/nevezo)

#A0 másodfokú együttható kiszámítása
szamolA0 <- function(X, Y) {

  osszegX <- Summary(X)
  osszegY <- Summary(Y)
  osszegXY <- Summary(X, Y)
  osszegX2Y <- Summary(X^2, Y)
  osszegX2 <- Summary(X^2)
  osszegX3 <- Summary(X^3)
  osszegX4 <- Summary(X^4)

#számláló és nevező kiszámolása
  szamlalo <- osszegX4 * ((osszegX2 * osszegY) - (osszegX * osszegXY)) - osszegX3 * ((osszegX3 * osszegY) - (osszegX2 * osszegXY)) + osszegX2Y * ((osszegX3 * osszegX) - (osszegY* osszegX2))
  nevezo <- osszegX4 * ((osszegX2 * n) - (osszegX * osszegX)) - osszegX3 * ((osszegX3 * n) - (osszegX2  * osszegX)) + osszegX2 * ((osszegX3 * osszegX) - (osszegX2 * osszegX2))
  return(szamlalo/nevezo)
}


#A2 másodfokú együttható kiszámítása
szamolA2 <- function(X, Y) {
  osszegX <- Summary(X)
  osszegY <- Summary(Y)
  osszegXY <- Summary(X, Y)
  osszegX2Y <- Summary(X^2, Y)
  osszegX2 <- Summary(X^2)
  osszegX3 <- Summary(X^3)
  osszegX4 <- Summary(X^4)

#számláló és nevező kiszámolása
  szamlalo <- osszegX2Y * ((osszegX2 * n) - (osszegX * osszegX)) - osszegX3 * ((osszegXY * n) - (osszegY * osszegX)) + osszegX2 * ((osszegXY * osszegX) - (osszegY * osszegX2))
  nevezo <- osszegX4 * ((osszegX2 * n) - (osszegX * osszegX)) - osszegX3 * ((osszegX3 * n) - (osszegX2  * osszegX)) + osszegX2 * ((osszegX3 * osszegX) - (osszegX2 * osszegX2))
  return(szamlalo/nevezo)

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

PolynomialRegression(X, Y)
