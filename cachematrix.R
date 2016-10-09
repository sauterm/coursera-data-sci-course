## This functions calculates the mean of a matrix and saves it
## to the cache so that the next time the user attempts to calculate the
## matrix mean, the previously saved value is returned instead of
## repeating the calculation.

## This function creates a special "matrix" object, which is really
## a list containing a function to
## a. set the value of the matrix
## b. get the value of the matrix
## c. set the value of the mean
## d. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y ## assign the input matrix y to the variable x in the
        
                ## parent environment
                m <<- NULL ## re-initialize m in the parent environment to null
        }
        get <- function() x ## return the matrix x
        setmean <- function(mean) m <<- mean ## set the cache m equal

        ## to the mean of the matrix x
        getmean <- function() m ## return the cached mean of x
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## Following function calculates the mean of the special "matrix" created
## with the function above. However, it first checks to see if the mean
## has already been caclulated. If so, it 'get's the mean from the cache
## and skips the execution. Otherwise, it calculates the matrix mean
## and sets the value of the mean in the cache via the 'setmean' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the mean of 'x'
        
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmean(m)
        m
}