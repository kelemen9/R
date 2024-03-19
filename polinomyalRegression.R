#n-ed fokú polinomiális regresszió
#adatok generálása
x <- c(1,2,3,4)
y <- c(5*x^2+3*x+7)
n <- length(x)

# fv. megjeleníti az adatokat és kiszámítja a másodfokú egyenlet együtthatóját.
PolynomialRegression <- function(X, Y) {

	plot(X, Y)
	a0 <- calcA0(X, Y)
	a1 <- calcA1(X, Y)
	a2 <- calcA2(X, Y) 
#pontok kirajzolása
	lines(X, a2 * X^2 + a1 * X + a0, type = "l", lty = 1, col = "blue")
#kiiratas
	cat("a0:", a0, "\n")
  	cat("a1:", a1, "\n")
	cat("a2:", a2, "\n")
}

#A0 másodfokú együttható kiszámítása
calcA0 <- function(X, Y) {

	sumX <- Summary(X)
      sumY <- Summary(Y)
      sumXY <- Summary(X,Y)
	sumX2Y <- Summary(X^2, Y)
      sumX2 <- Summary(X^2)
	sumX3 <- Summary(X^3)
	sumX4 <- Summary(X^4)

#szamlalo es nevezo kiszamolasa
	szamlalo <- sumX4 * ((sumX2 * sumY) - (sumX * sumXY)) - sumX3 * ((sumX3 * sumY) - (sumX2 * sumXY)) + sumX2Y * ((sumX3 * sumX) - (sumX2 * sumX2))
	nevezo <- sumX4 * ((sumX2 * n) - (sumX * sumX)) - sumX3 * ((sumX3 * n) - (sumX2 * sumX)) + sumX2 * ((sumX3 * sumX) - (sumX2 * sumX2))
	return(szamlalo/nevezo)
}

#A1 másodfokú együttható kiszámítása
calcA1 <- function(X, Y) {

	sumX <- Summary(X)
      sumY <- Summary(Y)
      sumXY <- Summary(X,Y)
	sumX2Y <- Summary(X^2, Y)
      sumX2 <- Summary(X^2)
	sumX3 <- Summary(X^3)
	sumX4 <- Summary(X^4)

#szamlalo es nevezo kiszamolasa
	szamlalo <- sumX4 * ((sumXY * n) - (sumY * sumX)) - sumX2Y * ((sumX3 * n) - (sumX2 * sumX)) + sumX2 * ((sumX3 * sumY) - (sumX2 * sumXY))
	nevezo <- sumX4 * ((sumX2 * n) - (sumX * sumX)) - sumX3 * ((sumX3 * n) - (sumX2 * sumX)) + sumX2 * ((sumX3 * sumX) - (sumX2 * sumX2))
	return(szamlalo/nevezo)
}

#A2 masodfoku egyutthato kiszamolasa
calcA2 <- function(X, Y) {
	
	sumX <- Summary(X)
      sumY <- Summary(Y)
      sumXY <- Summary(X,Y)
	sumX2Y <- Summary(X^2, Y)
      sumX2 <- Summary(X^2)
	sumX3 <- Summary(X^3)
	sumX4 <- Summary(X^4)

#szamlalo es nevezo kiszamolasa
	szamlalo <- sumX2Y * ((sumX2 * n) - (sumX * sumX)) - sumX3 * ((sumXY * n) - (sumY * sumX)) + sumX2 * ((sumXY * sumX) - (sumY * sumX2))
	nevezo <- sumX4 * ((sumX2 * n) - (sumX * sumX)) - sumX3 * ((sumX3 * n) - (sumX2  * sumX)) + sumX2 * ((sumX3 * sumX) - (sumX2 * sumX2))
	return(szamlalo/nevezo)
}

#összegzés funkció, amely két változót fogad el és visszaadja azok összegét
Summary <- function(X, Y) {

	sum <- 0
	n <- length(X)

    if (missing(Y)) {
    	for (i in 1:n) {
      		sum <- sum + X[i]
    	}
   		return(sum)
  	}

	for (i in 1:n) {
        sum <- sum + X[i] * Y[i]
    }
    return(sum)
}

#futtatás
PolynomialRegression(x, y)
