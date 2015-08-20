## makeCacheMatrix is used to create a list of functions. It is basically used to store
## and retrieve a matrix.
## cacheSolve is used to compute the inverse of a matrix created by makeCacheMatrix
## and cache this inverse.

## This function takes a matrix as input and returns a list containing four functions:
## 1. set the value of a matrix 2. get the value of the matrix 3. set the value of the inverse "i"
## 4. get the value of the inverse "i" of the matrix

makeCacheMatrix <- function(x = matrix()) {
        #set the value of the matrix
        i <- NULL #i is initialized to NULL but if cacheSolve() is run,  i is an inverse of x 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        #set the value of the inverse
        setinverse <- function(inverse) i <<- inverse
        #get the value of the inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the matrix and returns it. If the inverse has
## already been calculated, the function prints out the message "Getting cached data" before 
## retrieving the cached inverse, "i".

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        i <<- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data) #calculate the inverse
        x$setinverse(i) #The inverse is passed on to the setinverse() function above.
        i
}
