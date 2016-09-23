## makeCacheMatrix is a list of 4 functions that does the following:
##1. set value of the matrix, 2. get value of the matrix, 
##3. set value of the inverse, 4. get value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i<-NULL #initializes inverse "i" as NULL
  
  ##1. set value of the matrix
  set<-function(y) {
    x<<-y
    i<<-NULL 
  }

  ##2. get value of the matrix 
  get<-function()x

  ##3. set value of the inverse  
  setinverse<-function(solve) i<<-solve #use "solve" to find inverse of matrix
  
  ##4. get value of the inverse
  getinverse<-function()i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    i<-x$getinverse()
    if(!is.null(i)) {
      message("getting cached data") #if inverse is not null, then display message on cache
      return(i)
    }
    data<-x$get() #if inverse is null, then solve and set inverse value
    i<-solve(data, ...)
    x$setinverse(i)
    i
  
}
