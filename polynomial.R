x <- c(1,2,3,4)
y <- c(5*x^2+3*x+7)
n <- length(x)

PoliRegres <- function(X, Y) {

	plot(X, Y)
	a2 <- calculateA2(X, Y) 
	a1 <- calculateA1(X, Y)
	a0 <- calculateA0(X, Y)
	lines(X, a2 * X^2 + a1 * X + a0, type = "l", lty = 1, col = "red")

	cat("a2:", a2, "\n")
  	cat("a1:", a1, "\n")
  	cat("a0:", a0, "\n")
}

calculateA2 <- function(X, Y) {
	
	sumX <- Summary(X)
      sumY <- Summary(Y)
      sumXY <- Summary(X,Y)
	sumX2Y <- Summary(X^2, Y)
      sumX2 <- Summary(X^2)
	sumX3 <- Summary(X^3)
	sumX4 <- Summary(X^4)

	#számláló
	numerator <- sumX2Y * ((sumX2 * n) - (sumX * sumX)) - sumX3 * ((sumXY * n) - (sumY * sumX)) + sumX2 * ((sumXY * sumX) - (sumY * sumX2))
	#nevező
	denominator <- sumX4 * ((sumX2 * n) - (sumX * sumX)) - sumX3 * ((sumX3 * n) - (sumX2  * sumX)) + sumX2 * ((sumX3 * sumX) - (sumX2 * sumX2))
	return(numerator/denominator)
}

calculateA1 <- function(X, Y) {

	sumX <- Summary(X)
      sumY <- Summary(Y)
      sumXY <- Summary(X,Y)
	sumX2Y <- Summary(X^2, Y)
      sumX2 <- Summary(X^2)
	sumX3 <- Summary(X^3)
	sumX4 <- Summary(X^4)

	#számláló
	numerator <- sumX4 * ((sumXY * n) - (sumY * sumX)) - sumX2Y * ((sumX3 * n) - (sumX2 * sumX)) + sumX2 * ((sumX3 * sumY) - (sumX2 * sumXY))
	#nevező
	denominator <- sumX4 * ((sumX2 * n) - (sumX * sumX)) - sumX3 * ((sumX3 * n) - (sumX2 * sumX)) + sumX2 * ((sumX3 * sumX) - (sumX2 * sumX2))
	return(numerator/denominator)
}

calculateA0 <- function(X, Y) {

	sumX <- Summary(X)
      sumY <- Summary(Y)
      sumXY <- Summary(X,Y)
	sumX2Y <- Summary(X^2, Y)
      sumX2 <- Summary(X^2)
	sumX3 <- Summary(X^3)
	sumX4 <- Summary(X^4)

	#számláló
	numerator <- sumX4 * ((sumX2 * sumY) - (sumX * sumXY)) - sumX3 * ((sumX3 * sumY) - (sumX2 * sumXY)) + sumX2Y * ((sumX3 * sumX) - (sumX2 * sumX2))
	#nevező
	denominator <- sumX4 * ((sumX2 * n) - (sumX * sumX)) - sumX3 * ((sumX3 * n) - (sumX2 * sumX)) + sumX2 * ((sumX3 * sumX) - (sumX2 * sumX2))
	return(numerator/denominator)
}

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

PoliRegres(x, y)
