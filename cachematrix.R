#week 3 programming assignment 
#save as cachematrix.R

# ----------------------------------------------------------------------------
# testing the results
# <<source: solve help files>>
# ----------------------------------------------------------------------------

# > source("cachematrix.R")
#
# create a matrix 
# > test_matrix_fx <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# > test_matrix <- test_matrix_fx(4)
# > test_matrix <- makeCacheMatrix(test_matrix)
# > cacheSolve(test_matrix)
# It should retirn the line item "Calculating the matrix"
#
# > cacheSolve(test_matrix)
# It should return the line item "Returning the cached matrix"

# confirm the matrix has been inverted
# > Round(matrix_test$getmatrix() %*% matrix_test$get() , 3)


# ----------------------------------------------------------------------------
# makeCacheMatrix & cacheSolve
# ----------------------------------------------------------------------------
#
# makeCacheMatrix purpose is to store a matrix and the inversion of the matrix 
# it has four methods associated with it.
# set: 			Allow for setting value of matrix
# get:			Allow for returning the matrix
# setmatrix:	Allow for inverting the matrix
# getmatrix:	Allow for retrieving the inverted matrix

makeCacheMatrix <- function(matrix_object = matrix()) 
{
	# reset the value of the matrix_cached object to null
	matrix_cached <- NULL
	
	# set method of the class; init the object 
	# set the cached version of the matrix to NULL
	set <- function(matrix_setter)
	{
		matrix_object <<- matrix_setter
		matrix_cached <<- NULL
	}
	
	# get method of the class; return the matrix
	get <- function() 
	{
		matrix_object
	}
	# setmatrix method of the class
	# store an inverted version of the matrix
	setmatrix <- function(invert_matrix) 
	{
		matrix_cached <<- invert_matrix	
	}
	
	#get a copy of the matrix from the cache if it exists 
	getmatrix <- function() 
	{
		matrix_cached
	}
	# create a list vector with all commands for the object
	# set: 			Allow for setting value of matrix
	# get:			Allow for returning the matrix
	# setmatrix:	Allow for inverting the matrix
	# getmatrix:	Allow for retrieving the inverted matrix
	list(set       = set
		 ,get       = get
		 ,setmatrix = setmatrix
		 ,getmatrix = getmatrix
	)
}


# cacheSolve will take a matrix and arguments for the solve fx
# it will calculate the inversion of the matrix and store it
# so it can be read from a local cache.  Potentially saving time.

cacheSolve <- function(arg_matrix, ...) 
{
	# Return a matrix that is the inverse of 'x'
	
	# call the makecachematrix::getmatrix method
	# and store the value in temp_matrix
	temp_matrix <- arg_matrix$getmatrix()
	
	
	# check if the cached matrix is NOT NULL
	# allows to return the cached matrix
	if(!is.null(temp_matrix))
	{
		message("Returning the cached matrix")
		return(temp_matrix)
	}
	else
	{
		message("Calculating the matrix")
	}
	# if the temp_matrix is NULL then we need to
	# invert the matrix & store the results in the cache
	
	local_matrix <- arg_matrix$get()
	temp_matrix <- solve(local_matrix)
	arg_matrix$setmatrix(temp_matrix)
	
	# print out the inverted matrix
	temp_matrix
}