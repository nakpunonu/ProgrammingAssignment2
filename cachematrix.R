## Put comments here that give an overall description of what your functions do

## The code in this script is particularly useful for heavy invertible matrices that are non-changing. 
##The script caches the inverse of a matrix so that when the inverse is needed again, 
##it can be looked up in the cache rather than recomputed.

## Write a short comment describing this function

## The funtion below is essentially a list of children functions. It sets the matrix, gets the maxtrix,
## sets/caches the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {    ##This function sets the value of the matrix
                x <<- y         ##This uses the special <<- operator to assign the value of the matrix in y to object x
                m <<- NULL
        }
        get <- function()x      ##This returns the value of x set in the set() function above
        setInverse <- function(inverse) m <<- inverse   ##This caches the inverse of the matrix after it is computer in the CacheSolve function
        getInverse <- function() m              ## This returns the cached value of the inverse in the event that the inverse of the same matrix is requested again
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## List of the children functions
                
}


## Write a short comment describing this function

## This function computes the inverse of teh matrix returned by the makeCacheMatrix above. 
## In the event that the inverse has already been computed, the this retrieves the inverse
## from the getinverse() function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                
        m <- x$getInverse()     ##This gets the value of the inverse of the matrix stored in object m
        if(!is.null(m)){        ##this checks whether the m is NULL (which means that the inverse is yet to be computed), or if it not NULL, which means the inverse is already computed.
                message("getting cached data")
                return(m)
        }
        data <- x$get() ## This retrieves the matrix in the event that the inverse has not been computed
        m <- solve(data) ##This computes the inverse of the matrix
        x$setInverse(m)  ## This sends it to the setInverse() function for caching
        m               ## This returns the inverse of the matrix to be printed on the console

}
