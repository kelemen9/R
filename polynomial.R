#véletlen adatok generálása
set.seed(123)
X <- 1:10
Y <- 5*X^2 + 3*X + rnorm(length(X),  mean=0, sd= 5) 
#Másodfokú polinom regresszió
PolynomialRegression <- function (X, Y) {
	plot (X, Y, main = "Polynomial Regression", xlab = "X", ylab = "Y")

#Másodfokú együttható kiszámítása
coef <- coefficients(lm(Y - poly(X, 2, raw = TRUE)))
	a2 <- coef[3]
	a1 <- coef[2]
	a0 <- coef[1]

#Kirajzolás
curve(a2*x^2 + a1*x + a0, add = TRUE, col="blue")
legend("topleft", legend = c("Data", "Regression"), col = c("black", "blue"), lty = 1)

#Együtthatók kiiratása
cat("A2 (quadratic coefficient): ", a2, "\n")
cat("A1 (linear coefficient): ", a1, "\n")
cat("A0 (constant coefficient): ", a0, "\n")

#Futtatás
PolynomialRegression(X, Y)
	
