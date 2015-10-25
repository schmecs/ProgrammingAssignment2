## These two functions work together to create an object that contains a matrix
## and caches the matrix's inverse, once calculated, for later retrieval.

## The inverse is only cached once both functions have been called.

## makeCacheMatrix creates the special matrix and creates an empty object
## where the inverse may later be stored.

## cacheSolve then solves for the inverse - unless the special matrix already has
## an inverse cached in that object location. In that case, the inverse is
## pulled from the existing object value.

## Function that takes a matrix object (this_matrix)
## and caches its inverse once solved (this_matrix_inv)

  ## **I removed the "set" function from the sample code as I did not see its
  ## usefulness in what we were trying to achieve (commented out on line 25)

makeCacheMatrix <- function(data, nrows, ncols) { # takes matrix inputs
  this_matrix <- matrix(data, nrows, ncols) # creates matrix
  this_matrix_inv <- NULL # creates inverse object within function
  get <- function() this_matrix # retrieves current matrix
  setinv <- function(inv) this_matrix_inv <<- inv #sets inverse **called from other function**
  getinv <- function() this_matrix_inv # retrieves current inverse
  list(get = get, #set = set, 
       setinv = setinv,
       getinv = getinv)
}


## Function that checks to see if matrix has already cached its inverse
## and a) if it has, retrieves that cached inverted matrix
## and b) if not, calculates and returns the inverted matrix

cacheSolve <- function(solve_matrix, ...) {
  ## Return a matrix that is the inverse of the matrix contained in 'solve_matrix'
  this_matrix_inv <- solve_matrix$getinv() #brings in cached value (either null, or inverse)
  if(!is.null(this_matrix_inv)) { 
    message("getting cached data") # if cached inverse object is not null, retrieves it
    return(this_matrix_inv)
  }
  else { ## this part is only necessary if there is no cached value
    data <- solve_matrix$get() # import original matrix
    inv <- solve(data, ...) # calculate inverse of matrix
    solve_matrix$setinv(inv) # sets inverse in matrix cache for next retrieval
    return (inv) # return newly calculated inverted matrix
  }
}
