mncn <- function(X) {
	ndim <- dim(X)
	m = ndim[1]
	meanx <- apply(X,2,mean)
	ONE <- matrix(1,m,1)
	mx <- (X - (ONE %*% meanx))
	return(mx)
}

