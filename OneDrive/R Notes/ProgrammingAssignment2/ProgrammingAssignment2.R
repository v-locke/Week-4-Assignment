##Assignment 2 Prep
library("tidyverse")
}

##Run first
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

##Run second
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

##Run third
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##Run fourth
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

##Run fourth again
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        i
}

##Run fifth again but received error
specialMatrix <- makeCacheMatrix(matrix(c(2, 2, 3, 3), nrow = 2))

inverse1 <- cacheSolve(specialMatrix)
print(inverse1)

inverse2 <- cacheSolve(specialMatrix)
print(inverse2)

##Run fifth
B <- matrix(c(1,2,3,4),2,2)
}

##Run sixth
solve(B)
}

##Answer for fifth and sixth step
       [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

##Run seventh and eighth step
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
}

##Answer for seventh and eighth step
      [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

##Run ninth
matrix(c(2, 2, 3, 3), nrow = 2)

##Trying again
##Run first
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()  # Get the cached inverse
        
        if (!is.null(inv)) {  # Check if the inverse is already cached
                message("getting cached data")
                return(inv)  # Return the cached inverse
        }
        
        data <- x$get()  # Get the matrix
        
##Run second
        inv <- solve(data, ...)  # Compute the inverse
        x$setInverse(inv)  # Cache the inverse
        inv  # Return the computed inverse
}

##Set working directory
setwd("C:\\Users\\elisa\\OneDrive\\R Notes")
