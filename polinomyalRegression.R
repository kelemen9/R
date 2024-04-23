#n-ed fokú polinomiális regresszió
#adatok generálása
x <- c(1,2,3,4)
y <- c(5*x^2+3*x+7)
n <- length(x)

# fv. megjeleníti az adatokat és kiszámítja a másodfokú egyenlet együtthatóját.
PolinomiálisRegresszió <- function(X, Y) {

	plot(X, Y)
	a0 <- számolA0(X, Y)
	a1 <- számolA1(X, Y)
	a2 <- számolA2(X, Y) 
#pontok kirajzolása
	lines(X, a2 * X^2 + a1 * X + a0, type = "l", lty = 1, col = "blue")
#kiiratas
	cat("a0:", a0, "\n")
  	cat("a1:", a1, "\n")
	cat("a2:", a2, "\n")
}

#A0 másodfokú együttható kiszámítása
számolA0 <- function(X, Y) {

	összegX <- Összegzés(X)
      összegY <- Összegzés(Y)
      összegXY <- Összegzés(X,Y)
	összegX2Y <- Összegzés(X^2, Y)
      összegX2 <- Összegzés(X^2)
	összegX3 <- Összegzés(X^3)
	összegX4 <- Összegzés(X^4)

#szamlalo es nevezo kiszamolasa
	számláló <- összegX4 * ((összegX2 * összegY) - (összegX * összegXY)) - összegX3 * ((összegX3 * összegY) - (összegX2 * összegXY)) + összegX2Y * ((összegX3 * összegX) - (összegX2 * összegX2))
	nevező <- összegX4 * ((összegX2 * n) - (összegX * összegX)) - összegX3 * ((összegX3 * n) - (összegX2 * összegX)) + összegX2 * ((összegX3 * összegX) - (összegX2 * összegX2))
	return(számláló/nevező)
}

#A1 másodfokú együttható kiszámítása
számolA1 <- function(X, Y) {

	összegX <- Összegzés(X)
      összegY <- Összegzés(Y)
      összegXY <- Összegzés(X,Y)
	összegX2Y <- Összegzés(X^2, Y)
      összegX2 <- Összegzés(X^2)
	összegX3 <- Összegzés(X^3)
	összegX4 <- Összegzés(X^4)

#számláló és nevező kiszámolása
	számláló <- összegX4 * ((összegXY * n) - (összegY * összegX)) - összegX2Y * ((összegX3 * n) - (összegX2 * összegX)) + összegX2 * ((összegX3 * összegY) - (összegX2 * összegXY))
	nevező <- összegX4 * ((összegX2 * n) - (összegX * összegX)) - összegX3 * ((összegX3 * n) - (összegX2 * összegX)) + összegX2 * ((összegX3 * összegX) - (összegX2 * összegX2))
	return(számláló/nevező)
}

#A2 masodfoku egyutthato kiszamolasa
számolA2 <- function(X, Y) {
	
	összegX <- Összegzés(X)
      összegY <- Összegzés(Y)
      összegXY <- Összegzés(X,Y)
	összegX2Y <- Összegzés(X^2, Y)
      összegX2 <- Összegzés(X^2)
	összegX3 <- Összegzés(X^3)
	összegX4 <- Összegzés(X^4)

#szamlalo es nevezo kiszamolasa
	számláló <- összegX2Y * ((összegX2 * n) - (összegX * összegX)) - összegX3 * ((összegXY * n) - (összegY * összegX)) + összegX2 * ((összegXY * összegX) - (összegY * összegX2))
	nevező <- összegX4 * ((összegX2 * n) - (összegX * összegX)) - összegX3 * ((összegX3 * n) - (összegX2  * összegX)) + összegX2 * ((összegX3 * összegX) - (összegX2 * összegX2))
	return(számláló/nevező)
}

#összegzés funkció, amely két változót fogad el és visszaadja azok összegét
Összegzés <- function(X, Y) {

	összeg <- 0
	n <- length(X)

    if (missing(Y)) {
    	for (i in 1:n) {
      		összeg <- összeg + X[i]
    	}
   		return(összeg)
  	}

	for (i in 1:n) {
        összeg <- összeg + X[i] * Y[i]
    }
    return(összeg)
}

#futtatás
PolinomiálisRegresszió(x, y)


