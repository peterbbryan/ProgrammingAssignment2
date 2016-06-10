## makeCacheMatrix, like the provided makeVector function, returns a list
## of functions a setter and a getter for the numeric vector and the mean
## cacheSolve returns the inverse either freshly calculated or from cache

## makeCacheMatrix, like the provided makeVector function, returns a list
## of functions a setter and a getter for the numeric vector and the mean
makeCacheMatrix <- function(x = matrix()) {
          s <- NULL
          set <- function(y) {
                  x <<- y
                  s <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) s <<- inverse
          getinverse <- function() s
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}

## cacheSolve returns the inverse either freshly calculated or from cache
cacheSolve <- function(x, ...) {
          s <- x$getinverse()
          if(!is.null(s)) {
                  message("getting cached data")
                  return(s)
          }
          data <- x$get()
          s <- solve(data, ...)
          x$setinverse(s)
          s
}

# So I don't spend an hour flipping tabs
# makeVector <- function(x = numeric()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }
# cachemean <- function(x, ...) {
#         m <- x$getmean()
#         if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m
# }