## This two functions help to introduce new matrix object. It will cache its inverse.
## In this script the special matrix - t.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
t <- NULL
# It helps us to set the value of the our matrix. And also it can cache the old inverse.
set <- function(y) {
x <<- y # It is setting the value
t << NULL # It is clearing the cache 
}
get <- function() x
# It is defining the function that will be setting the inverse
setInverse <- function(inverse) t <<- inverse
# It is defining the function that will be getting the inverse
getInverse <- function() t
# It is returning a list of four functions
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# It is retutning inverse of matrix x
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
t <- x$getInverse() # It is caching the value for the inverse
if(!is.null(t)) { # We need to return cache if it is not empty
message("Processing. The cached data is getting.")
return(t)
}
# If the cache was empty. We need to calculate, cache and return it.
data <- x$get() # It is getting the value of the matrix
t <- solve(data) # It is calculation of the inverse
x$setInverse(t) # It is proccess of caching the results
t # It is returning of the inverse
} 
