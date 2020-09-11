## makeCacheMatrix : Defines functions to set a matrix value, get that matrix, 
## cache its inverse, and get that inverse. Returns a list containing these 
## functions.

## cacheSolve : Calculates and returns the inverse of the matrix stored in the
## list returned and caches it, provided an inverse isn't already cached. If so,
## returns the cached value.

##

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set_mat<-function(y){
    x<<-y
    inv<<-NULL
  }
  get_mat<-function() x
  set_inverse<-function(inverse) inv<<-inverse
  get_inverse<-function() inv
  list(set_mat=set_mat,get_mat=get_mat,set_inverse=set_inverse,
       get_inverse=get_inverse)
}


##

cacheSolve <- function(x, ...) {
  inv<-x$get_inverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get_mat()
  inv<-solve(mat)
  x$set_inverse(mat)
  inv
}
