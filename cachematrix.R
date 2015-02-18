##See README.md for instructions on running the code and output from it

## This function creates a special "matrix" object that caches the calculation of its inverse

## It does this by creating the object as a list with `set` and `get` functions,
## and using the scoping rules of R

#Contains the following functions:
# * set          set the value of a matrix
# * get          get the value of a matrix
# * setinverse   get the cahced value (inverse of the matrix)
# * getinverse   get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  
         INV<- NULL ## the starting point is to create a null object called "INV"
                       
                    
        ## Store a Matrix - By setting the object, I assign the new argument to
        ## be my stored value and reset my inverse calculation
         set<-function(y){
              x<<-y
              # since the matrix is assigned a new value, flush the cache
              INV<<-NULL
              }
        ## returns the stored object (matrix)
          get<-function() x
        
        ## get and set functions for the inverse of the matrix
        
               ## cache the given argument  
        
          setinverse<-function(inverse) INV <<- inverse
        
               ## get the cached value
        
          getinverse<-function() INV
        
        # return a list. Each named element of the list is a function
               list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  
       ## Get the cached value - Return a matrix that is the inverse of 'x'
  
              INV<-x$getinverse()
          
       ## if the cached value exists, then return it
              if (!is.null(m)){
              message("getting cached data")
              return(INV)
          }
       
       # otherwise get the matrix, caclulate the inverse and store it in
       # the cache
       
             data<-x$get()
             INV<-solve(data, ...) ## calculating the inverse
             x$setinverse(INV)
       
       ## return the inverse matrix
             INV
}
