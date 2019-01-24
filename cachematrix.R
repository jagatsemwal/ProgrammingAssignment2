## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## resetting inverse to NULL
i <- NULL
set <- function(y) {
		x<<- y
		m <<- NULL
	}
get <- function() x

## set the inverse matrix
setinv <- function(inv) i<<-inv

## retreives to stored inverted matrix
getinv <- function() i

##Returns list of functions

list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        
## checking if there is inverted matrix chache is available
i <- x$getinv()
if(!is.null(i)) {
	message("getting cached data")
	return(i)
	}
## if no cache found then the below code will compute the inverse
data <- x$get()
i <- solve(data, ...)
## this will store the inverse of matrix in cache
x$setinv(i)

## Return a matrix that is the inverse of 'x'
i

}
