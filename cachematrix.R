#The purpose of this assignment is to write a pair 
#of function that caches the inverse of the matrix 

#Functions makes a special matrix that can cache 
#its inverse

makeCacheMatrix <- function(x = matrix()) {
	output <- NULL
      set <- function(y) {
		x <<- y
		output <<- NULL
      }
      get <- function() x 
	setinverse <- function(solve) output <<- solve 
	getinverse <- function()output 
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


#Calcualtes the inverse of the special martix inverse. 
#It outputs the inverse, if it is already been computed

cacheSolve <- function(x, ...) {
      output <- x$getinverse()
	if (!is.null(output)){  
		message("getting cached Inverse of Matrix")
		return(output)
	}
	in_data<- x$get()
	message("Computing the inverse of Matrix for the first time")
	output<- solve(in_data)
	x$setinverse(output)
	output

}
