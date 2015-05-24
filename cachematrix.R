## The following two functions are used to create, store and recall a matrix 
## and its inverse matrix in or from cache.

## makeCacheMatrix creates a list with 4 functions (set, get, setsolve, getsolve)
## to store/recall the created matrix and its inverse matrix in/from the cache.
## <<-operator is used here to assign a value to the matrix from a different environment.
makeCacheMatrix <- function(x = matrix()) { 
    ## Create a special "matrix" that can cache its inverse matrix
    Invmatrix <- NULL  ## Store the inverse matrix
    set <- function(y){  ## Set a matrix to the object created by makeCacheMetrix
      x <<- y
      Invmatrix <<- NULL  ## Store the inverse matrix in cache
    }
    get <- function() x  ## Recall the original matrix
    setsolve <- function (solve) Invmatrix <<- solve  ## Set the inverse matrix
    getsolve <- function() Invmatrix  ## Recall the inverse matrix
    list (set = set, get = get,  ## Create store/recall functions in/from the cache
          setsolve = setsolve,
          getsolve = getsolve)
}

## cacheSolve calculates the inverse matrix of the original matrix created by 
## makeCacheMatrix, by first checking the availability of the calculation in 
## the cache and then recalling the results directly from the cache; otherwise 
## calculating the inverse matrix and then storing it in the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
    Invmatrix <- x$getsolve()  ## Get the inverse matrix
    if (!is.null(Invmatrix)){  ## If the inverse matrix has been calculated and
                               ## stored in the cache
    message("getting cached data")
    return(Invmatrix)  ## Return the calculated inverse matrix in cache
  }
    data <- x$get()  ## If not in cache, get the matrix created by makeCacheMatrix
    Invmatrix <- solve(data, …)  ## Invert the matrix
    x$setsolve(Invmatrix)  ## Store the inverse matrix in cache by makeCacheMatrix
    Invmatrix  
}
