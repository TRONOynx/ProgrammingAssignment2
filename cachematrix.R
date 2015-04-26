# The makeCacheMatrix function works like a class to create a list 
# that contains the following sub-functions:
# (a) set
# (b) get
# (c) setInv 
# (d) getInv

makeCacheMatrix <- function(x = matrix()) {

# Define cache for inversion results
  xinv <- NULL 
  
# Set a matrix to object created by the above pre-defined makeCacheMatrix function
# And initialize xinv to null
      set <- function(y) {
	  x <<- y
	  xinv <<- NULL 
      }

# Return the input matrix
      get <- function() x 
      
# Set the inversed matrix
      setInv <- function(inv) xinv <<- inv
      
# Return the inversed matrix
      getInv <- function() xinv 

# Create a list that contains the sub-functions (set > get > setInv > getInv) 
# under the parent pre-defined function makeCacheMatrix
      
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)

}

# Get the inversed matrix from object x
# Notes: 
# (a) if uncalculated, it will be returned null value as defined above in xinv function
# (b) if the inversion result is there, it will returned the calculated inversion
# (c) if the inversion result is not there, it will get the matrix object using sub-function get and
#     solve it using the function solve. Next, set it to the object using sub-function setInv before 
#     returning the solved result

cacheSolve <- function(x, ...) {
      m <- x$getInv() 
      if(!is.null(m)) {
	  message("return cached data")
	  return(m) 
      }
      data <- x$get() 
      m <- solve(data) 
      x$setInv(m) # 
      m 
}
