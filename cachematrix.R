## Put comments here that give an overall description of what your
## functions do
## The following functions calculate the inverse of a matrix and saves it
## to the cache.The next time the users tries to calculate the inverse of this matrix this functions caches the result
##saving time for the user.
## Write a short comment describing this function
## The function makeCacheMatrix creates a special "matrix" which is a list with the functions
## get, set, getinverse and setinverse.
makeCacheMatrix <- function(x = matrix()) {
  ## initialiaze inverse property
	y<-NULL
	## set value of the matrix
	set<-function(matrix){
		x<<-matrix
		y<<-NULL
	} 
        
        ## get the value of the matrix
	get<-function(){
		x
	}
        
        ## set inverse of the matrix
	setinverse<-function(inverse){
		y<<-inverse
	}
        
        ## get inverse of the matrix
	getinverse<-function(){
		y
	}

        ## list with the above methods
	list(set = set,get=get,setinverse = setinverse,getinverse = getinverse )
}


## Write a short comment describing this function
## cacheSolve computes the iverse of a matrix.If this inverse has been calculated before
## then cacheSolve returnes the cached inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         	m<-x$getinverse()

        ## if inverse exists return the cached inverse
	if(!is.null(m)){
		message("getting cache data")
		return(m)
	}
        ## set data with the value of the matrix
	data<-x$get()
         ## calculate the inverse 
	m<-solve(data) %*% data
	
	## set the inverse to our object
	x$setinverse(m)
	## return the inverse of the matrix
	m
}
