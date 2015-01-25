## this function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function (xx = matrix ()) { ## the function returns a list of
                                               ## nested functions (set, get,
                                               ## setInv, getInv)

  Inv <- NULL       ## Inv is initialized to NULL. 
                    ## It will hold the cached inverse of matrix
  
                    ## Four functions are defined below. 
                    ## give access to xx and Inv inside a makeCacheMatrix object   
  set <- function(yy) {
    xx <<- yy       ## Use "<<-" to assign xx in higher environment! 
    Inv <<- NULL    ## while we have here a new matrix we need to reset the Inv
                    ## as the Inverse needes to be recalculated
  }
  get <- function () xx   ## return value of xx stored in makeCacheMatrix object
                          ## i.e. the original matrix
  
  setInv <- function (solve) Inv <<- solve     ## set the value of Inv
  
  getInv <- function () Inv      ## returns the value of Inv stored in 
                                 ## "makeCacheMatrix" object 
  list (set = set, get = get,    ## list of 4 nested functions "tag=value" pairs 
        setInv = setInv,
        getInv = getInv)
}
##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------
## returns the inverse of a matrix using a cached value if possible,
## otherwise calculates the matrix inverse 

cacheSolve <- function(xx,...) {   ## requires one parameter that must be a
                                   ## "makeCacheMatrix" object
    
  Inv <- xx$getInv()               ## Stores the cached Inv value
  
  if(!is.null(Inv)) {              ## if Inv not NULL then there is cached Inv
    message("getting cached data")
    return(Inv)                    ## returns cached Inv
    
  }
  data <- xx$get()                 ## using the get() function in the 
                                   ## makeCacheMatrix object, assign the matrix
                                   ## to local data
  Inv <- solve(data,...)           ## calculates the inverse of data
  xx$setInv(Inv)                   ## cache the newly calculated inverse
  Inv                              ## returns the inverse
}