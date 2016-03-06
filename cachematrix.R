## Week 3 practical assignment for R Programming course: Lexical Scoping


## This function takes a squared matrix and buils a list with special functions,
## so that the matrix could be cached with the function "cacheSolve"
makeCacheMatrix <- function(x = matrix()) {
    if (!is.matrix(x)) { # check we have a matrix as an input
        stop("the argument must be a matrix!")
    } else {
        if (ncol(x) != nrow(x)) {
            stop("the argument is not a squared matrix!")
        }
        
        inverse.matrix <- NULL
        set <- function(y) { # function to set the data (matrix)
            x <<- y
            inverse.matrix <<- NULL # if we change matrix, clean the cache
        }
        
        get <- function() { # function to get the data (matrix)
            x
        }
        
        setinverse <- function(inv) {
            inverse.matrix <<- inv
        }
        
        getinverse <- function() {
            inverse.matrix
        }
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    
    }
}


## This function calculates the inverse of a special matrix created with
## the function "makeCacheMatrix". If the function runs first time for the matrix
## the function makes the calculations and put in the cache
## if the calcualtion is already made and available in the cache, then it returns
## cached value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse.matrix.from.cache <- x$getinverse() # checking what we have cached
    if (!is.null(inverse.matrix.from.cache)) { # there is something in cache
        message("getting cached data")
        return(inverse.matrix.from.cache) # return what we have in cache
    } else { # if cache is empty
        message("cashing data")
        data <- x$get()
        inverse.matrix <- solve(data) # calculate the inverse
        x$setinverse(inverse.matrix) # put in cache
        return(inverse.matrix) # return
    }
}
