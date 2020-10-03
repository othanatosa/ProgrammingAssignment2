

## makeCacheMatrix consists of set, get, setInversa, getInversa

makeCacheMatrix <- function(x = matrix()) {
  inversa<-NULL
  set <- function(y){
    x<<-y
    inversa<<-NULL
  }
  get<-function(){x}
  setInversa<-function(inversacalculada){inversa<<-inversacalculada}
  getInversa<-function(){inversa}
  list(set = set, get = get, setInversa = setInversa, getInversa=getInversa)
}



## In this way we get the cached data

cacheSolve <- function(x, ...) {
  
  inversa<-x$getInversa()
  if (!is.null(inversa)){
    message("getting cached data") #examines if the inverse is NULL
    return(inversa)
  }
  data <-x$get()
  inversa<-solve(data,...)
  x$setInversa(inversa)
  
        ## Return a matrix that is the inverse of 'x'
}


