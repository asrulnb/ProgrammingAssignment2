## These pair of function will Solve the inversed of the matrix passed into it 
## and cache the result so it can be used later.


## This Function will create the construct to save the value passed into it and return
## it back when needed.

makeCacheMatrix <- function(myx = matrix()) {

     invM <- NULL #initialize the value of Inverse Matrix with NULL
     
     set <- function(y) {
          myx <<- y #overwrite the parent variable "myx" with value from "y"
          invM <<- NULL #reset the "invM" value in parent to NULL
     }
     
     get <- function() myx #pass the "myx" value to "get"
     
     setInverse <- function(inverse) invM <<- inverse #set the "invM" value to the parent "inverse" value
     
     getInverse <- function() return(invM) #pass the "invM" value to "getInverse"
     
     #return the value to parent
     return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## This function checks if there are cached Inverse Matrix answer for the one passed to it.
## It will use the cached value if present.
## It will solve the inverse matrix and store the answer in the cache when not.

cacheSolve <- function(x, ...) {
     
     ## Checks if the answer was previously created
     answer <- x$getInverse() # call from Cache value
     
     if(!is.null(answer)) { # check if the answer is Not NULL (present in Cache)
     
          message("getting cached data") # indicate to user the value is from Cache
          
          return(answer) # return the answer
          
     }
     
     message("new Calculation")
     
     data <- x$get() # retrieve the data
     
     answer <- solve(data, ...) # calculate the inverse Matrix answer
     
     x$setInverse(answer) # save the data to Cache
     
     # return the answer
     return(answer)
     
}

### [ Sample running code ] ###

## > myM <- matrix(c(1,4,3,5),nrow=2)
## > myM
##      [,1] [,2]
## [1,]    1    3
## [2,]    4    5
## > test <- makeCacheMatrix(myM)
## > cacheSolve(test)
## new Calculation
## [,1]       [,2]
## [1,] -0.7142857  0.4285714
## [2,]  0.5714286 -0.1428571
## > cacheSolve(test)
## getting cached data
## [,1]       [,2]
## [1,] -0.7142857  0.4285714
## [2,]  0.5714286 -0.1428571
