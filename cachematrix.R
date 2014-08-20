# The following functions are used to cache the inverse of a matrix using lexical scopes.

# The function makeCacheMatrix creates a list containing the functions
# get, set, get_inverse and set_inverse

makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse <- NULL
        
        set <- function(matrix_data) {
                # Assign a new matrix
                x <<- matrix_data
                matrix_inverse <<- NULL
        }
        
        # Return the matrix that was passed into the function
        get <- function() x
        
        # Assign the inverse of the data matrix
        set_inverse <- function(inverse) matrix_inverse <<- inverse
        
        # Return the inverse of the matrix
        get_inverse <- function() matrix_inverse
        
        # Return the four embedded functions in a list so they can be manipulated
        return(list(get = get, set = set, get_inverse = get_inverse, set_inverse = set_inverse))
        
}


# The function cacheSolve returns the inverse of the matrix.

cacheSolve <- function(cache_matrix, ...) {
        
        # Check if the inverse matrix has been already calculated.
        # If not, calculate it.
        inverse <- cache_matrix$get_inverse()
        if (is.null(inverse)) {
                # Get the inverse
                print("Calculating inverse")
                inverse_matrix <- solve(cache_matrix$get())
                cache_matrix$set_inverse(inverse_matrix)
        }
        # Return the inverse
        return(cache_matrix$get_inverse())
}
