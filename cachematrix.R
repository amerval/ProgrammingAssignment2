## The two functions makeCacheMatrix and cacheSolve below
##    define a "matrix-like" object that also contains the
##    value of the inverse of the matrix -if it has been
##    previously computed

## makeCacheMatrix define the structure of the object:
##    a list of four functions allowing to initialise 
##    and retrieve the value of the matrix and of its
##    inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialise object "inverse of matrix"
  Xinv <- NULL
  
  # define function set: initialise the value of the
  #     matrix object
  set <- function(M) {
    x <<- M
    Xinv <<- NULL
  }  
  
  # define function get: simply retrieve the value of the
  #     matrix object
  get <- function() x
  
  # define function setInv: initialise the value of the
  #     inverse matrix
  setInv <- function(XXII) Xinv <<- list(matinv=XXII, matval=x)
  
  # define function getInv: get the value of the
  #     inverse matrix -if already computed
  getInv <- function() Xinv 
  
  list(set=set,get=get,
       setInv=setInv,getInv=getInv)
}


## cacheSolve test for the existence of the value for the
##    inverse in the cache. If found, it returns it, otherwise
##    it computes it.

cacheSolve <- function(x, ...) {
  # test if an inverse has been computed already
  p_inv <- x$getInv()
  
  # if a value is stored, return it only if the value of 
  #   the matrix hasn't changed
  if (!is.null(p_inv)) {
    message("A value for the inverse is stored in cache")
    # a value is stored in the cache.
    # check that the input matrix hasn't changed since
    # computation
    xcur <- x$get()
    if (identical(xcur,p_inv$matval)) {
      # if same matrix, they output stored value
      # for inverse
      message("...retrieving value for cache")
      return(p_inv$matinv)
    } else{
      # the matrix has change, we need to compute the
      # new inverse
      message("...but the matrix has changed")
      message("...computing new inverse")
      XINV <- solve(xcur)
      # updating stored values
      x$setInv(XINV)
      XINV
    }
    
  } else {
    # no value stored for inverse
    message("No value is found stored in cache")
    message("...computing matrix inverse")
    xcur <- x$get()
    XINV <- solve(xcur)
    # store computed value              
    x$setInv(XINV)
    XINV
  }
 
}