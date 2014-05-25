## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions: 
##      a getter (get) which returns the matrix itself
##      a setter (set) which sets the matrix for the instantiated object
##      a getter (getinv) which returns the inverse of the matrix
##      a setter (setinv) which computes (using the solve() function) the inverse

makeCacheMatrix <- function(x = matrix()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function "caches" the inverse of the matrix
## If the value of the inv variable is null, it will compute it; 
##      otherwise it returns the value computed previously

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#         m <- x$getmean()
#         if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m
        inv <- x$getinv()
        #First, check to see if the value is null
        if(!is.null(inv)) {
                #If we have a value, just return the value along with a message
                message("getting cached data")
                return(inv)
        }
        #If the value has not been previously computed, we first retrieve the matrix using our getter
        data <- x$get()
        #Then we use the solve() function to compute the inverse
        inv <- solve(data, ...)
        #Finally, we use our setter to store the inverse
        x$setinv(inv)
        inv
}
