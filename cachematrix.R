## The first function, makeCacheMatrix, sets and stores the inverse for any matrix passed to it. The second function utilizes the makeCacheMatrix function to check if the inverse for a given matrix was already computed or not. If so, the inverse is returned with "getting cached inverse" message otherwise fresh inverse computation is done inside the second function itself and the result will be displayed accordingly.

## This functions caches the inverse for any new matrix variable passed by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(solve) inv<<- solve
  getinv<-function() inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## This function calls the makeCacheMatrix function to checks in the environment to see if the cached inverse for the given matrix was already computed. If yes, then returns the result prefixed with comment 'getting cached inverse'. Otherwise, it just returns the newly computed inverse for the given matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  inv<-x$getinv()
		  if(!is.null(inv)){
			message("getting cached inverse")
			return(inv)
		  }
		  m<-x$get()
		  inv<-solve(m, ...)
		  x$setinv(inv)
		  inv
}
