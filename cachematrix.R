## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
	m<-matrix(list(x,set=set,get=get,setInverse=setInverse,getInverse=getInverse))
	return(m)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i<-x[[5,1]]()
		if(!is.null(i) )
		{
			message("getting cached data")
			return(i)
		}
		x[[4,1]]((solve(x[[3,1]]())))
		return(x[[5,1]]())
}
