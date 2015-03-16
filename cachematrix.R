## The following functions implement a way of caching the value of a matrix inverse
## 
## Usage : makeCacheMatrix() to convert a matrix to the cache enabled version
##         cacheSolve() to solve the matrix possibly using the cache
##
## Example : > mat <- matrix(c(1,1,1,0), 2, 2)
##           > cmat <- makeCacheMatrix(mat)
##           > cacheSolve(cmat) ##will use cache if it exists, otherwise it will create it


## makeCacheMatrix() - creates the cache enabled version of a matrix
makeCacheMatrix <- function(x = matrix()) {
        #Cached matrix solution
        inv <- NULL
        
        #set (or reset) value of matrix
        set <- function(y) {
                x <<- y
                inv <- NULL
        }
        #get the value of the matrix
        get <- function() x
        
        #get/set the inverse
        setinv <- function(xInverse) inv <<- xInverse
        getinv <- function() inv
        
        #create the list of functions so they can be accessed
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve() - solves (gets the inverse) of the cacheMatrix x
##                note: x must be created by the makeCacheMatrix() function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                # Have already solved the matrix - just return cache
                message("retrieving cache")
                return (inv)
        }
        # Calculate the solution and store it into the cacheMatrix
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv
}
