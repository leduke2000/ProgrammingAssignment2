## Two functions to calculate mean of a matrix and save in 
## global environement

## A function create a list of functions that permit to 
## set and to get global values of input et output of next function

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set<-function(y)
  {
    x<<-y
    s<<-NULL
  }
  get<-function() x
  setsolve<-function(sol) s<<-sol
  getsolve<-function() s
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}

## A function to get the inverse of a square matrix, 
## input should of produced by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s<-x$getsolve()
  if(!is.null(s))
  {
    message("gettin cached data")
    return(s)
  }
  data<-x$get()
  s<-solve(data,...)
  x$setsolve(s)
  s
}
