# use this function by creating a NULL object like
# > test <- makeCacheMatrix(NULL)
# after set the matrix with the "imposta" function
# here is an example with a 3x3 matrix:
# > test$imposta(matrix(rnorm(9), nrow = 3, ncol = 3))
# cacheSolve(test) give the inverse matrix
# test$prendi() give the original matrix

makeCacheMatrix <- function(x=matrix()){
	# create a NULL object called "matrice"
	matrice <- NULL
	# this function set x<-y in environment outside the one of
	# the function where it is been defined 
	imposta <- function(y){
		x <<- y
		matrice <<- NULL
	}
	# simply "take" x
	prendi <- function(){x}
	impinve <- function(inversa){matrice <<- inversa}
	# take the inverse (if any) of the matrix
	prendinve <- function(){matrice}
	list(imposta = imposta,
		 prendi = prendi,
		 impinve = impinve,
		 prendinve = prendinve)
}


cacheSolve <- function(x, ...){
	# this string set matrice equal to the one at prendinve
	# (if any)
	matrice <- x$prendinve()
	# if "matrice" is not NULL, R print a message and return
	# the inverse that is been calculated previously
	if(!is.null(matrice)){
		message("I'm a lazybones, I get the cached data")
		return(matrice)
		}
	# if the inverse does not exist, R take the original
	# matrix, calculate the inverse matrix and print it 
		else{
		data <- x$prendi()
		inversa <- solve(data,...)
		x$impinve(inversa)
		}
	inversa
}
