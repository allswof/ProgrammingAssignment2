## From the original matrix, the functions create a "special
## matrix that stores the matrix and caches its inverse


## Creates the special matrix (list containing a function that
## sets and gets the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL				## initializes the inverse
        set <- function(y) {
               	 x <<- y		## assigns the supplied y to x
               	 i <<- NULL		## initializes the cached inverse
        		}
        get <- function() x		## retrieves the value of x
        setinv <- function(solve) i <<- solve		## calculates inverse and stores it in the cache
        getinv <- function() i				## retrieves inverese from cache
        list(set = set, get = get,		## list of the 4 functions used
             setinv = setinv,
             getinv = getinv)
	}

## Checks if the inverse of the matrix has been calculated, if it has,
## retrives its value, if not, calculates it and sets its value in the cache

cachesolve <- function(x, ...) {
        i <- x$getinv()			## Retrieves the value stored in the cache
        if(!is.null(i)) {		## Checks if the value exists
                message("Getting cached data...")	## if exists prints a message
                return(i)					## and displays the inverse
        		}
        data <- x$get()			## gets the matrix
        i <- solve(data, ...)		## calculates the inverse
        x$setinv(i)			## and stores the inverse in the cache
        i					## displays the inverse
	}
