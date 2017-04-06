##Those functions are inspired by the functions about the mean proposed in the class

##The first function returns a list coitaing a function to: set the value of the vector
##get its value, set the inverse of the matrix and get its value

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){   ##set function to set the inv to null and the matrix to the imputed vmatrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x                           ##get function to get the matrix
  setInv <- function(inverse) inv <<- inverse   ##to compute the inverse
  getInv <- function() inv                      ##to get the inverse
  list(set = set, get = get,                    ##list returned
       setInv = setInv,
       getInv = getInv)
}


##Compute the inverse of the matrix, retrieve it from the cache 
##if the inverse has already been computed and the matrix has not changed
cacheSolve <- function(x, ...){
  inv <- x$getInv()             ##get the inverse
  if(!is.null(inv)){            ##if the inverse exists prints 'getting cached data' and return its value
    message('getting cached data')
    return(inv)
  }
  matrix <- x$get()             ##compute and return the inverse of the matrix
  inv <- solve(matrix,...)
  x$setInv(inv)
  inv
}
