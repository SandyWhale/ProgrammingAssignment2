## The following function makes a matrix that can cache its inverse, assuming that the matrix is inverseable. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL 
        
        set <- function(y){
        
        x <<- y
                
        i <<- NULL       
        
        }
        
            
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        
        getinverse <- function() i
        
        list( set = set,
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse
             )
}


## Function below computes the inversed matrix converted by makeCacheMatrix above ^. If the inverse has already been calculated
## and the matrix has not changed, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        
        if(!is.null(i)) {
        
         message(" retrieving inversed matrix from cache")
                
         return(i)       
        
        }
        
        data <- x$get()
        
        y <- solve(data, ...)
        
        x$setinverse(y)
        
        y
        
}
