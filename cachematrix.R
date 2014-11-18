## The functions below calculate the inverse of a matrix, making use of cached data.

## The makeCachematrix-function sets the value in setmax and gets the inverse value in getmat
## It stores all data in a list.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    # Original matrix
    x <<- y
    # Inverse matix
    i <<- NULL
  }
  get <- function() x
  setmat <- function(solve) i <<- solve
  getmat <- function() i
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)

}


## This function checks whether there is already cached data.
## If this is the case, it retrieves the cached data.
## If not, it stores the data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' if there are no changes to the matrix
  i <- x$getmat()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setmat(i)
  i
}
