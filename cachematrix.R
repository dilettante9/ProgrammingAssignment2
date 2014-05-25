## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() creates a data structure that consists of a cache for 
## the inverse of a matrix and four functions to get and set the value
## of the matrix and the value of the cache.
## input = a matrix; pre-assigned with an empty matrix.
## output = a list containing the four functions.

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL # variable to cache future results.
    
    set <- function(y){ # input = matrix
        x <<- y  # uses <<- operator to assign value to the x variable outside of this function. 
        cache <<- NULL # cache initiated with NULL value;
    }         # uses <<- operator to assign value to the cache variable outside of this function.
    
    get <- function() x # output = value of x, a matrix.
    
    setcache <- function(inverse) cache <<- inverse # input = inverse of matrix
            # uses <<- operator to assign value to the cache variable outside of this function.
    
    getcache <- function() cache # output = cache; the inverse of matrix x or NULL if not set.
    
    list(set=set, get=get, setcache=setcache, 
         getcache=getcache) # output = list; the return value for makeCacheMatrix()
}                           # the four functions packaged in a list. 


## cacheSolve() produces the inverse of a matrix and writes it to a cache.
## It uses the functions provided by the makeCacheMatrix() object.  
## It first interrogates the cache to see if a value for the inverse has already been produced.
## If not it gets the inverse using solve(), and caches that value.
## input = a matrix type object, as produced by makeCacheMatrix()
## output = the inverse of the matrix provided. 

cacheSolve <- function(x, ...) { # input = matrix object, as produced by makeCacheMatrix()
    inverse <- x$getcache() # calls getcache(); assigns cache value to inverse
    
    if(!is.null(inverse)){  # if inverse value (cache) is not NULL 
        message("Getting inverse from cache") # return inverse
        return(inverse)
    }
    my.matrix <- x$get() # calls get(); assigns the value of the matrix in 
                         # the makeCacheMatix() object to my.matrix
    
    inverse <- solve(my.matrix) # calls solve() which returns the inverse of my.matrix
    
    x$setcache(inverse) # calls setcache(); caches the value of inverse.
    
    inverse # returns the inverse of my.matrix.
}

