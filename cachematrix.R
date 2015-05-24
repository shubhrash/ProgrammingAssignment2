##Constructor function to create a square invertible matrix with getters and setters
##input parameter: any square invertible matrix x
##returns list of set, get, setinverse and getinverse closure functions defined inside makeCacheMatrix
## Note: set and setinverse are closures. Everything inside a closure retains the enclosing environment 
##without defaulting to the global environment of the constructor. 

makeCacheMatrix <- function(x = matrix(,n,n)) {
        i <- NULL
        
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse) }

##cacheSolve() returns the inverse matrix from cache if present else computes inverse using solve()
##Input parameters: return value of makeCacheMatrix 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                # Returns a matrix i that is the inverse of 'x' from cache
                message("getting cached data")
                return(i)
        }
        ## Compute and return a matrix i that is the inverse of 'x' as cached value is null
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        
        i }

