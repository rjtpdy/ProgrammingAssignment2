##  If the contents of a matrix are not changing, it may make sense to cache 
##the value of the inverse so that when we need it again, it can be looked up
##in the cache rather than recomputed. In this Programming Assignment will
##take advantage of the scoping rules of the R language and how they can
##be manipulated to preserve state inside of an R object.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)

}


##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the
##inverse from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
