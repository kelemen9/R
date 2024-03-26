#Tiszta illeszkedésvizsgálat

#kapott szamok(k),valoszinuseg(p),dobasok szama(N)
k <- c(83, 91, 122, 107, 74, 123)
p <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)  
N <- 600   


Illeszkedésvizsgálat <- function(k, p, N){
	Khi^2 <- 0
	n <- length(k)

	for (i in 1:n) {
        Khi^2 <- Khi^2 + ((( k[i] - N * p[i] ) ^2 ) / ( N * p[i] ))
    	}

    	return(Khi^2)
}

Illeszkedésvizsgálat(k, p, N)
