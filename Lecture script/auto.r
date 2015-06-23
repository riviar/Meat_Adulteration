auto <- function(X) {
	ndim <- dim(X)
	m = ndim[1]
	meanx <- apply(X,2,mean)
	stdx <- apply(X,2,std)
	ONE <- matrix(1,m,1)
	ax <- (X - (ONE %*% meanx))/(ONE %*% stdx)
	return(ax)
}

