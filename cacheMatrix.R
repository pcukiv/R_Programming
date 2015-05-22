#Coursera R Programming Assignment 2: cachematrix.R

#### \/.:| MY CODE |:.\/ ####

##\/ This first function "makeCacheMatrix" creates a list of functions which makes:
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse matrix
#4. get the value of the inverse matrix


makeCacheMatrix <- function (mtrx = matrix ()) {
	  # 1.inverse will store the cached inverse matrix
	invrs <- NULL
	set <- function (y) {
		mtrx <<- y
		invrs <<- NULL
		}
	  #2. get the value of the matrix
	get <- function () mtrx
	  #3. set the value of the inverse matrix
	setinv <- function (inverse) invrs <<- inverse
	  #4. get the value of the inverse matrix
	getinv <- function () invrs
	
	  #Return the matrix with our newly defined functions
	list(set=set, get=get, setinv=setinv, getinv=getinv)
	}


#\/ This second function "cacheSolve" calculates de inverse of the matrix.
#However, it first checks if the inverse has already been calculated.
#If so, it gets the inverse from the cache, skips the computation and prints a message.
#Otherwise, it calculates the inverse matrix as normal and sets the value in the cache.


cacheSolve <- function (mtrx, ...) {
	# If the inverse is already calculated, return it
	invrs <- mtrx$getinv ()
	if (!is.null (invrs)) {
		message ("|-| Getting cached matrix...")
		return (invrs)
		}
	data <- mtrx$get ()
	invrs <- solve (data, ...)
	mtrx$setinv (invrs)
	return (invrs)
	}


##\/ Example usage:
z <- matrix(rnorm(16), nrow = 4)	#Create a random matrix z
cz <- makeCacheMatrix(z)		#Create my special matrix
cz$get()				#Return the matrix
cacheSolve(cz)				#Return the inverse
cacheSolve(cz)				#Call the 2nd time, thus return the cached inverse
#\/


