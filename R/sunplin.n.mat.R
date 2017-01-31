sunplin.n.mat <-
function ( fileName, nth ){
	ret <- scan(fileName, what = character(), nlines = 1)
	if( is.numeric(ret[1]) == TRUE ) qtdNodes <- as.numeric(ret[1])
	else qtdNodes <- as.numeric(ret[2])
	ret <- scan(fileName, what = character(), sep = '\n', skip = (((nth-1)*(qtdNodes+1) + 1)), nlines =(qtdNodes) + 1)
	ret <- strsplit(ret,"\t")
	mat <- round(lower.tri(matrix(1,qtdNodes+1,qtdNodes+1)))
	for(i in 1 : qtdNodes+1 )
		mat[1,i] <- ret[[1]][i]
	for(i in 2 : qtdNodes+1){
		for(j in 1 : i-1 ){
			mat[i,j] <- ret[[i]][j]
		}
	}
	return (mat)
}
