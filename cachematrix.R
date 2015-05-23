## These functions manage caching of inverse (solve) matrix computations

## Creates a special matrix object that can cache it's inverse (solve).
makeCacheMatrix <- function(x = matrix()) {
  solve <- NULL
  set <- function(y){
    x <<- y
    solve <<- null
  }
  get <- function() x
  setsolve <- function(i = matrix()) solve <<- i
  getsolve <- function() solve
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Computes the inverse of a matrix returned by makeCacheMatrix (above) or
## retrieves the inverse from cache if already calculated.
## Assumes that the matrix is invertible.
cacheSolve <- function(x, ...) {
        solve <- x$getsolve()
        if(!is.null(solve)){
          message("getting cached data")
          return(solve)
        }
        matrix <- x$get()
        solve <- solve(matrix)
        x$setsolve(solve)
        solve
}
