## Put comments here that give an overall description of what your functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialising the "cached_inverse" variable to NULL
    cached_inverse<-NULL
    
    ## set the value of a matrix
    set<-function(y){
        
        ## using "<<-" to assign a value to an object in an environment dufferent from the current
        ## environment: setting the cached x value to y, setting the cached_inverse to NULL
        
        x<<-y
        cached_inverse<<-NULL
    }
    ## get the value of a matrix (return x)
    get<-function()x
    
    ## set the inverse of the matrix, setting cached_inverse to inverse
    setinverse<-function(inverse) cached_inverse<<-inverse
    
    ## get the inverse of the matrix (return cached_inverse)
    getinverse<-function() cached_inverse
    
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated, and its value hasn't changed, it
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x', whereas x is output of makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
    
    cached_inverse = x$getinverse()
    
    ## if the inverse has already been calculated
    
    if (!is.null(cached_inverse)){
        
        # get it from the cache and skip the computation. 
        
        message("getting cached data")
        return(cached_inverse)
    }
    
    ## otherwise, calculates the inverse 
    mat.data = x$get()
    cached_inverse = solve(mat.data, ...)
    
    ## sets the value of the inverse in the cache via the setinverse function.
    x$setinverse(cached_inverse)
    
    return(cached_inverse)
}
