## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {


        m <- NULL
        set <- function(y)
		{
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse =  setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.Write a short comment describing this function

cacheSolve <- function(x) {
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

#matrix1 <- makeCacheMatrix( matrix ( c(3,3.5,3.2,3.6) , 2, 2 ) )
#cacheSolve(matrix1)
