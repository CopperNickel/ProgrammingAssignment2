## Cached version of matrix inversion (kind of memoization): 
## http://en.wikipedia.org/wiki/Memoization
## a given matrix inverted one time only, then computed value used
##
## E.g.
## A <- matrix(c(1, 2, 3, 4), nrow=2) # standard R matrix
## Z <- makeCacheMatrix(A)            # cached-inversed version
## cacheSolve(Z)                      # explicit inversion
## cacheSolve(Z)                      # no computation: cached value is shown

## Make a inverse-cached version of standard matrix

makeCacheMatrix <- function(x = matrix()) {
  # Either inverted matrix OR error text (for, say, singular matrixes)
  inverse <- NULL
  
  set <- function(value) {
    x <<- value
    inverse <<- NULL
  }
  
  get <- function() {
    x 
  }

  set.Inverse <- function(value) {
    inverse <<- value
  }

  get.Inverse <- function() {
    inverse
  }

  list(set = set,
       get = get,
       get.Inverse = get.Inverse,
       set.Inverse = set.Inverse) 
}

## Cached matrix (see makeCacheMatrix) inversion
## first call is an explicit insversion, all the others are
## just returnings of a cached value 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  result <- x$get.Inverse();

  if (!is.null(result)) {
    ## For debug purpose only: uncomment the line below if you want check cache working
    # message("gettting from cache");
  
    if (is.character(result))
      stop(result);

    return(result); 
  }

  m <- x$get()

  # A matrix doesn't necessarily have an inversed one...
  tryCatch(x$set.Inverse(solve(m, ...)),
           error = function(e) {x$set.Inverse(as.character(e)); stop(e)})

  x$get.Inverse(); 
}

