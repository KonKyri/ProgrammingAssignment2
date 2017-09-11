
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){     # set the value of the vector
                x <<- y
                m <<- NULL
        }
        
        get <- function() x		# get the value of the matrix
        setinverse <- function(solve) m <<- solve 	# set the value of the inverse
        getinverse <- function() m 		# get the value of the inverse
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {	  # check if the the inverse matrix already exists
                message("getting cached data")		# retrieve from cache
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)		# calculate the inverse
        x$setinverse(m)				# set the inverse in the cache
        m
}