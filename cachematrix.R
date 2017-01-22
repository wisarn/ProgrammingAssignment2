
## Function "makeCacheMatrix" 
##      Creates special object that is used in function
## "cacheSolve" below for finding inverse matrix.
## Input:
##      x - matrix (can be NULL)
## Output:
##      list of 4 functions
##          set() - set cached matrix
##          get() - retrive cached matrix
##          setinv() - set cached inverse matrix
##          getinv() - get cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv = getinv)
}


## Function "cacheSolve" 
##      Computes inverse of matrix, where Matrix y will be 
## compared with cached matrix in object x. If they are the 
## same and its inverse is cached already, the inverse will be 
## retrived from cache stored in object x. Otherwise, inverse 
## of y will be computed. When inverse is recomputed, y and 
## new inverse will be copied into cached matrix and cached 
## inverse in object x, respectively.
## Input:
##      x - object which is returned from function "makeCacheMatrix".
##      y - target (regular) matrix that is going to find 
##          its inverse.
## Output:
##      inverse matrix of y

cacheSolve <- function(x, y, ...) {
    ## check whether matrix has changed or not
    chk <- as.vector(y)
    cached <- as.vector(x$get())
    if (length(cached) == length(chk)) { 
        if (sum(cached-chk) == 0) {
            inv <- x$getinv()
            if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
            }        
        }
    }
    ## new matrix, set and compute inverse matrix
    x$set(y)
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}