## Creates a special matrix that stores the inverse of regular matrix 'x' 

makeCacheMatrix <- function(x = matrix()) {
  i <<- NULL
  set <- function(y)
  {
    if(!identical(x,y))
    {
      x<<-y
      i<<-NULL
    }
  } 
  get <- function()x
  setinverse <- function(inverse)i<<-inverse
  getinverse <- function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Calculates the inverse of special matrix 'x'. Uses cached
## inverse if matrix 'x' has not changed

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(is.null(i))
  {
    m<-x$get()
    if(is.na(m[1,1]))
    {
      message("No matrix to calculate the inverse for!")
    }
    else
    {
      message("Calculating new inverse")
      x$setinverse(solve(m))
    }
  }
  return(x$getinverse())
  ## Return a matrix that is the inverse of 'x'
}
