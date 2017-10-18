## return a list of get set
## INPUT - a square matrix 
## OUTPUT 4 elements - a.Set Matrix ;b.Get Matrix ;c.setInverse Matrix ;d.getInverseMatrix
makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinverse <- function(invmatrix) invm <<- invmatrix
    getinverse <- function() invm
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return a matrix that is the inverse input matrix x
## INPUT - a square matrix
## OUTPUT - the inverse matrix of INPUT
cacheSolve <- function(x, ...) { 
    invm <- x$getinverse()
    ##if inverse exists, direct return that
    if(!is.null(invm)) {  
        message("getting cached data")
        return(invm)
    }
    ##get inverse matrix of INPUT, set inverse matrix if that's not exist
    data <- x$get() 
    invm <- solve(data, ...)
    x$setinverse(invm)
    invm
}
##> A <- matrix(c(1,1,4,0,3,1,4,4,0), nrow = 3, ncol = 3)
##> vA <- makeCacheMatrix(A)
##> vA$get()
##[,1] [,2] [,3]
##[1,]    1    0    4
##[2,]    1    3    4
##[3,]    4    1    0
##> cacheSolve(vA)
##[,1]        [,2]    [,3]
##[1,]  0.08333333 -0.08333333  0.2500
##[2,] -0.33333333  0.33333333  0.0000
##[3,]  0.22916667  0.02083333 -0.0625
##> vA$getinverse()
##[,1]        [,2]    [,3]
##[1,]  0.08333333 -0.08333333  0.2500
##[2,] -0.33333333  0.33333333  0.0000
##[3,]  0.22916667  0.02083333 -0.0625
##> cacheSolve(vA)
##getting cached data
##[,1]        [,2]    [,3]
##[1,]  0.08333333 -0.08333333  0.2500
##[2,] -0.33333333  0.33333333  0.0000
##[3,]  0.22916667  0.02083333 -0.0625