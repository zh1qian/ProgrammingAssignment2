## cache the inverse of a matrix

## makeCacheMatrix is to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
s<-NULL
setmatrix<-function(y){
  x<<-y
  s<<-NULL
}
getmatrix<-function(){
  x
}
setinverse<-function(iv){
  s<<-iv
}
getinverse<-function(){
  s
}
list(setmatrix=setmatrix,getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve functions to compute the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x=matrix(), ...){
  s <- x$getinverse()
  if(!is.null(s)){
    message("caching data")
     return(s)
    } 
  data<-x$getmatrix()
  s<-solve(data,...)
  x$setinverse(s)
  s
}
