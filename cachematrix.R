## 1.makeCacheMatrix: 
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) 
{
	inv = NULL ##set default value for inv
      set = function(fn) 
	{
	# use `<<-` to assign a value to an object in an alternative environment or 'cache' the information  
      	mat <<- fn
            inv <<- NULL
      }
      get = function() mat
      setinv = function(inverse) inverse <<- inv 
      getinv = function() return(inv)
      return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}

## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve
##the inverse from the cache.

cacheSolve <- function(mat, ...) 
{
      ## if inverse is not null, check the cache and return contents
	inverse <- mat$getinv()
	if(!is.null(inv)) 
	{
      	message("retrieving cached data")
       	print(inv)
     	}
      ## if inv is NULL calculate the inverse and store in cache.
	## inverse is calculated using 'solve'
	invmat<-mat$get()
	inv<-solve(invmat,...) 
	mat$setinv(inv)
	return(inv)
}
