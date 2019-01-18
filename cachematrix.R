## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    IM <- NULL             ## Set IM (Inverse Matrix) variable to NULL to call upon later
    set1 <- function(y1){
        x <<- y1           ## Assign y1 to x variable in global environment
        IM <<- NULL        ## Keep IM as NULL to call upon later
    }        
    get1 <- function() x
    setsolve1 <- function(solve) IM <<- solve
    getsolve1 <- function() IM
    list(set1 = set1, get1 = get1, setsolve1 = setsolve1, getsolve1 = getsolve1)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  IM <- x$getsolve1()                ## Retrieve the result of x in makeCacheMatrix
  if(!is.null(IM)){                    ## If IM it's not N/A, just return the cached solve()
     message("getting cached data")  
     return(IM)
  }
   data1 <- x$get1()                  ## Otherwise, store the get1 product in data1
   IM <- solve(data1, ...)            ## Then apply the solve function to get1 product
   x$setsolve1(IM)                    ## Run through original setsolve1 function
   IM                                 ## Print the result
}
