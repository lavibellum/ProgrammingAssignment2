makeCacheMatrix <- function(x=matrix()){
  q <- NULL
  set <- function(p){
    x <<-p
    q<-NULL
  }
  get<- function()x
  setinversematrix <- function(invmatrix)im <<- inversematrix
  getinversematrix <- function()im
  list(set=set, get=get, setinversematrix=setinversematrix, getinversematrix=getinversematrix)
  
  cachesolve <- function(x){
    q <- x$get() 
    if(!is.null(q)){
      message("getting cached data")
      return(q)
    }
    input <- x$get()
    q <- solve(input)
    x$setinversematrix(q)
    q
  }
  