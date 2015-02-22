## Two functions are provided here.
## 1. makeCacheMatrix is a function that creates a hybrid matrix object that includes a 
## list of functions that allow the user to: 
## a. Set (and cache) a matrix. Because the function is defined outside of the calling environment its local (free) 
##    varaibles are stored outside of the calling environment which allows the data to be cached.
## b. Get the matrix (if any) that is stored/cached outside the calling environment.
## c. Set (and cache) the inverse of the original matrix.
## d. Get the inverted matric (if any) that is stored/cached outside the calling environment.
##
## 2. cacheSolve is a function that takes a square/invertible matrix and creates its inverse. It uses the makeCacheMatrix function to 
##    to store/cache both the original and inverted matrices. This function only performs the actual matrix inversion once, for every subsequent 
##    call it simply returns inverted matrix from the cache

## function creates a matrix object that can store and retrieve data for two matrices.
makeCacheMatrix <- function(x = matrix()) {

    invertdMatrix <- NULL
  
    set <- function(y) {
      x <<- y           #cache the original matrix
      invertdMatrix <<- NULL    #cache the inverted matrix
    }  
    
    get <- function() x
    setInverseMatrix <- function(inverse) invertdMatrix <<- inverse
    getInverseMatrix <- function() invertdMatrix
    
    list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## function returns accepts any square invertible matrix as input and returns its inversion.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inverse <- x$getInverseMatrix()
    if(!is.null(inverse)) {
      message("Retrieving from cache.")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverseMatrix(inverse)
    inverse
    
}
