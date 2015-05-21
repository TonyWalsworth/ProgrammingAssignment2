## Put comments here that give an overall description of what your
## functions do
 ## R programming - Assignment 2 - make a caching matrix
 ## To use (example):
 ## Create a special caching matrix...
 ##     a<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2)) 
 ## use the $get method to access the special matrix...
 ##     b<-a$get()  (b now holds the 'a' matrix)
 ## access the solved (inverse) matrix using the $getInverse method...
 ##     c<-a$getInverse()  (returns NULL if not available else returns the inverse matrix im)
 ## Cache the solved matrix value (im) using the $setInverse method...
 ##     a$setInverse(im)
 ## cacheSolve (fn) uses the above methods to manage the cache.
 ##
makeCacheMatrix<- function(x = matrix()) {
    imCache <- NULL                         ## This vector holds the 'solved' or inverse of  matrix x
    set <- function(y) {                    ## set initialises the cacheable matrix to the passed value,
        x <<- y                             ## and sets the imCache to NULL (empty)
        imCache <<- NULL
    }
    get <- function() x                        ## get method returns the passed matrix
    setInverse <- function(im) imCache <<- im  ## sets the imCache to a new value
    getInverse <- function() imCache           ## returns the current value of imCache.
    ##
    list(set = set, ## this bit labels the methods to make them accessible using '$'
         get = get,
         setInverse = setInverse,
         getInverse = getInverse
         )
}
##
## The cacheSolve function only works with invertable numeric matrices created using makeCacheMatrix()
## Initially tries to get the cached inverse of the passed matrix,
## If it isn't available, cacheSolve grabs the passed matrix (using the $get method) and solves it.
## The solution (inverse matrix) is then cached using the setInverse method.
##
cacheSolve <- function(x, ...) {
    im <- x$getInverse()        ## claim the solved matrix from cache
    if(!is.null(im)) {          ## if the cached value is valid (ie not NULL), return it with a message
        message("getting cached data")
        return(im)              ## then return from the function.
    }
    else{                       ## else if the cached value is NULL (empty),
       data <- x$get()             ## grab the initial matrix x
       im <- solve(data)           ## solve it to gain the inverse of x
       x$setInverse(im)            ## cache the solved matrix using the $setInverse method
    }
    im                          ## return the solved matrix
}
