## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a 'special matrix' which contains original matrix,set(),get(),setInverse(),getInverse() functions, in a 5x1 matrix
makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<- function(y)
	{
		x<<-y
		i<<-NULL
	}
	get<- function() x
	setInverse<-function(inverse) i<<-inverse
	getInverse<-function() i
	##matrix of funtions is not supported, so we store functions inside lists
	m<-matrix(list(x,set=set,get=get,setInverse=setInverse,getInverse=getInverse))
	return(m)
}


## Write a short comment describing this function
## This function returns a inverse of a matrix from cache, if it is already computed before,else computes,stores in caches and returns inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i<-x[[5,1]]()
		if(!is.null(i) && all.equal(x[[1,1]],x[[3,1]]()))
		{
			message("getting cached data")
			return(i)
		}
	
		x[[4,1]]((solve(x[[3,1]]())))
	##get the numeric matrix from cache, compute inverse, and set inverse in cache
		return(x[[5,1]]())
}
