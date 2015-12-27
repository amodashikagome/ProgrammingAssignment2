# ## This two functions together return the inversion of a square matrix, 
# and store the value. If the same matrix is input for the second time, 
# the function simply return the stored value instead of another calculation.

# 1.makeCacheMatrix: 
# # This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL             # initialized value is NULL
        set <- function(y) {
                x <<- y         # set and store the input matrix of value x, 
                # passing through the nominal variable y
                sol <<- NULL
        }
        get <- function() x     # get stored matrix value x
        setsolve <- function(solve) sol <<- solve  # store inversion from cacheSolve
        getsolve <- function() sol                 # get stored inverstion matrix                
        list(set = set, get = get,                 # functions defined within the environment
             setsolve = setsolve,                  # displayed in a list
             getsolve = getsolve)
}


# 2. cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        sol <- x$getsolve()                    # firstly check if the inversion already exists
        if(!is.null(sol)) {
                message("getting cached data") # if yes, return what is stored in x$getsolve()
                return(sol)
        }
        data <- x$get()                        # if not, calculate and store it into x$setsolve()           
        sol <- solve(data)                     # and return the inversion of 'x'
        x$setsolve(sol)
        sol
}

# # an example of how to use the functions
# 1. suppose we have two matrices, a and b.
# a <- matrix(1:4, 2, 2)
# b <- matrix(2:5, 2, 2)

# 2. get the list of functions containint matrices
# x <- makeCacheMatrix(a)
# y <- makeCacheMatrix(a)
# As shown in Environment window, they are both List of 4.

# 3. set matrix and check if inversion exists
# x$get()
# y$get()
# we will get exactly the same matrices as a and b
# x$getsolve()
# y$getsolve()
# we will get NULL because the calculations haven't been done before

# 4. calculate and cache the inversion
# call cacheSolve
# cacheSolve(x)
# cacheSolve(y)
# we get the inversion matrices
# x$getsolve()
# y$getsolve()
# the same results appear instead of NULL

# 5. call cacheSovle again
# cacheSolve(x)
# cacheSolve(y)
# This time we get the message "getting cached data"
# and the function directly print the stored results