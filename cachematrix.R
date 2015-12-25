## This is for r-programming assignment #2 - this discusses the role of how a matrix (an array of 
## data works) and how values can be cached for faster computational time, this is benefical
## when working with larger datasets.

makeCacheMatrix <- function(x = matrix()) {

## function to create a special matrix object that can cache its inverse

  inv = NULL
  set = function(y) {
## assign a value to object
  x << - y
  inv <<- NULL
}

 get = function() x
 setinv = function(inverse) inv <<- inverse
 getinv = function() inv
 list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The following function calculates the mean of the special vector created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## this function computers the inverse of a special "matrix" returned by makeCacheMatrix. If the inverse
## has already been calculated (and remains unchanged) then cachesolve should retrieve the inverse from the cache
## otherwise proceed to calculate the inverse.

 inv = x$getinv()

## run if the inverse has already been calcuated
  if (!is.null(inv)){
# retrieve from cache
  message("retrieving cache data")
  return(inv)
}

##if not calculated, proceed with calculating the inverse
 mat.data = x$get()
 inv = solve(mat.data, ...)

# set the value of the inverse in the cache via the setinv function
  x$setinv(inv)

  return(inv)
}

