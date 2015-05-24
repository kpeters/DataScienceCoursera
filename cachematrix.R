# see "https://github.com/DanieleP/PA2-clarifying_instructions" for a 
# clear explanation of each step from te example functions makeVector 
# and cacheMean provided by the R Programming course

  ## Assignment 2
    # Matrix inversion is usually a costly computation and there may be 
    # some benefit to caching the inverse of a matrix rather than compute 
    # it repeatedly
    # The functions 'makeCacheMatrix' and 'cacheSolve' create a Matrix in
    # cache and solves' the inverse of the matrix respectively.

  ## Example:
    # x <- rbind(c(1, -1/4), c(-1/4, 1)) # creates a 2x2 matrix, see below:
    #
    # 
    # x =        [,1]  [,2]
    #       [1,]  1.00 -0.25
    #       [2,] -0.25  1.00
    #
    # executing  x <- makeCacheMatrix(x) overwrites 'x' and saves the matrix 
    # into cache instead of working memory
    # 
    # executing cacheSolve(x) retrieves te matrix from cache, calculates the 
    # inverse of x and saves the inverse back to cache (x$getsolve)
    #
    # x$getsolve()
    #           [,1]      [,2]
    # [1,] 1.0666667 0.2666667
    # [2,] 0.2666667 1.0666667

#------------------------------------------------------------------------------

    # please refer to the detailed example above,
    # for further reference read: 
    # https://github.com/DanieleP/PA2-clarifying_instructions
    # it's really clarifying!
    
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

#------------------------------------------------------------------------------

    # please refer to the detailed example above,
    # for further reference read: 
    # https://github.com/DanieleP/PA2-clarifying_instructions
    # it's really clarifying!

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
