

## list of functions to store the matrix and its inverse value

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        
        #set the cached matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        
        # get the matrix cached
        get <- function() x
        
        # store the inverse of the matrix
        setinverse <- function(inverse) i <<- inverse
        
        # get the inverse of the matrix
        getinverse <-  function() i
        list(set= set, get=get, setinverse= setinverse, getinverse= getinverse)
}



## function to return the inverse of the matrix

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        
        # return the inverse if already computed and stored
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
                
        }
        
        #compute the inverse and return if still not  computed 
        
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
        
        
            
}


#exemple of cached inverse
cachedex <- makeCacheMatrix(matrix(c(1,4,9,0,-3,2,2,7,8),3,3))
cacheSolve(cachedex)


