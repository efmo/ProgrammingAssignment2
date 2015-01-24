#See end of file for a more thorough description of these functions and their
#behavior.

#makeCacheMatrix
#This function returns a list of four functions that return or redefine a matrix
#and its inverse. The argument x is a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #this function changes the value of the matrix and automatically 
    #calculates a new inverse
    set <- function(y) {
        x <<- y
        cacheSolve(cacheList)
    }
    
    #return the original (non-inverted) matrix
    get <- function() x
    
    #this function sets the value of the inverse that is cached. in this
    #implementation, the user never calls this function; cacheSolve() does
    setinverse <- function(inverse) inv <<- inverse
    
    #return the cached inverse
    getinverse <- function() inv
    
    #construct the list of functions describing the cached matrix
    cacheList <- list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
    #solve for the inverse (this happens when the cached matrix is first created)
    cacheSolve(cacheList)
    
    #return the cached matrix
    cacheList
}

#cacheSolve
#This function inputs a list of the type returned by makeCacheMatrix, calculates
#the inverse of the matrix within the list, and returns the inverse

cacheSolve <- function(x, ...) {
        #Note that there is no check for a null inverse here, which is intentional.
        #The implementation of cacheSolve() within makeCacheMatrix() eliminates the
        #need to check.
    
    #get the matrix using the list's get() function
    invToCache <- x$get()

    #find the inverse of the matrix
    invToCache <- solve(invToCache, ...)

    #set the inverse, which is now cached
    x$setinverse(invToCache)

    #suppress output
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


