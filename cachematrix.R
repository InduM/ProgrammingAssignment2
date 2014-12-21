# 'makeCacheMatrix' function creates a matrix object
# that can cache its inverse. 'cacheSolve' function computes
# the inverse of the "matrix" returned by `makeCacheMatrix` .
# If the inverse has already been calculated (and the matrix has not 
# been changed), then `cacheSolve` retrieves the inverse from the cache


##creates a matrix object that can cache its inverse and returns a  list 
##of internal functions everytime it is invoked

makeCacheMatrix <- function(x = matrix())  
{	
        IM <- NULL						#variable 'IM' is for the inverted matrix
								#IM is NULL when makeCacheMatrix is called for the first time
      
	set <- function(y)					# function is called in cacheSolve() 
	{
                x <<- y						#saves the input matrix passed from cacheSolve()
                IM <<- NULL					#initializes the inverse matrix to NULL
        }
       
	getmatrix <- function()	
	{								
		x						#This function returns the input matrix
	}
        
	setinverse <- function(inverse) 			#this is called by cachmatrix() during the first 
	{							#cachematrix() access and stores the value using superassignment
		IM <<- inverse				
	}
        
	getinverse <- function()	 			# returns the cached value to cacheSolve() on
        {					  		# subsequent accesses
		IM
	}
       
	list(getmatrix = getmatrix,  				# a list of the internal functions is returned each time   
             setinverse =  setinverse,	 			# makeCacheMatrix is invoked so a calling function
             getinverse = getinverse)			 	# knows how to access those methods.                            
}


#'cacheSolve' function computes the inverse of 
#the "matrix" returned by `makeCacheMatrix` .
cacheSolve <- function(x,...)
{
	m <- x$getinverse()					#gets inverse form makeCacheMatrix and assigns to m
       
      	if(!is.null(m))						#checks if the inverse is null i.e. checks if the inverse
	{							#has been computed already and cached
		message("getting cached data")
         	m						# returns the cached inverse of 'x'
        }
        							
	else
	{						
								
		data <- x$getmatrix()				#gets the input matrix from makeCacheMatrix function 
								#and assigns to the variable data
        
		m <- solve(data,...)				#Finds the inverse of a matrix
       
		x$setinverse(m)					#caches the inverse matrix for future use 

		m						#Returns the inverse of 'x'
	}							
}

