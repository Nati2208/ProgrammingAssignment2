## Put comments here that give an overall description of what your
## functions do

## Create a special matrix that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        va <- NULL
        set <- function(y) {
              x <<- y
              va <<- NULL
        }
        get <- function(){x}
        setINVE <- function(inve_CA) {va <<- inve_CA}
        getINVE <- function(){
                va
        }
        list(set = set, get = get,
             setINVE = setINVE,
             getINVE = getINVE)
}

## It returns the inverse of the special matrix done before

cacheSolve <- function(x, ...) {
        va <- x$getINVE()
        if(!is.null(va)) {
                message("getting cached data")
                return(va)
        }
        data <- x$get()
        va <- solve(data, ...)
        x$setINVE(va)
        va
}


