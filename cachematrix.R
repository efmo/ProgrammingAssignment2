
# This function returns a list of four functions that return or redefine a matrix
#and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        cacheSolve(cacheList)
    }
    evn <- environment()
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    cacheList <- list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
    cacheSolve(cacheList)
    cacheList
}


## the 'x' argument in cacheSolve is a list of the type returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    invToCache <- x$get()
    invToCache <- solve(invToCache, ...)
    x$setinverse(invToCache)
    invisible(invToCache)
}

#Example usage:
#
#First we create a simple 2x2 matrix.
#mTest <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
#
#Then we create our 'cache matrix', which is really a list of four functions.
#mCache <- makeCacheMatrix(mTest)
#
#Now we can call mCache$get() or mCache$getinverse() to see the matrix or 
#inverse stored by mCache.
#
#We can also call mCache$set(newMatrix) to change the value of the matrix
#stored by mCache. The inverse is automatically updated.
#
#I modified the example code given in the project description to automatically 
#calculate the inverse the first time makeCacheMatrix is called or whenever
#the matrix is changed by calling set(). This way the user doesn't need to 
#call setinverse() or cacheSolve() on their own and a call to getinverse() 
#always returns the correct inverse while still using a cached value.
#Also, this removes the need for cacheSolve() to check if the inverse is null.
#
#If the automation idea is taken further we could remove the cacheSolve() 
#function entirely and do everything within the cached matrix list.


