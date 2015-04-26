## This function creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
	in_matrix <- NULL
        set <- function(y) {
                x <<- y
                in_matrix <<- NULL
        }
        get <- function() x
        setin_matrix <- function(solve) in_matrix <<- solve
        getin_matrix <- function() in_matrix
        list(set = set, get = get,
             setin_matrix = setin_matrix,
             getin_matrix = getin_matrix)
}


## This function calculates the inverse of the matrix returned by the function presented above. If it's already been calculated, then this function returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        in_matrix <- x$getin_matrix()
        if(!is.null(in_matrix)) {
                message("getting cached data")
                return(in_matrix)
        }
        data <- x$get()
        in_matrix <- solve(data, ...)
        x$setin_matrix(in_matrix)
        in_matrix	## Return a matrix that is the inverse of 'x'
}
