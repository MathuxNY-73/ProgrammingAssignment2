## The two following functions create a cache to compute and store the inverse  
## of an invertible matrix. This optimize repetitive calls to matrix inversion operations. 

## Create a wrapper to cache the inverse of a given matrix. 

makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL;
    set <- function(newMatrix) {
        x <<- newMatrix;
        mInverse <<- NULL;
    };
    get <- function() x
    setInverse <- function(inverse) mInverse <<- inverse 
    getInverse <- function() mInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Retrieve the inverse of a wrapped matrix or compute it and store it in the cache wrapper. 

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse();

    # If the inverse is NULL then we need to compute it.
    if(!is.null(inverse)) {
        message("Getting cached matrix inverse");
        return(inverse);
    }

    # Retrieve the data of a wrapped matrix.
    mat <- x$get();

    # Compute the inverse of the matrix.
    inverse <- solve(mat, ...);

    # Store the computed inverse in the wrapper.
    x$setInverse(inverse);
    inverse
}
