## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix is for getters and setters,library(MASS) used for squared an non squared matrix
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
inv<-NULL #initializing
set<-function(y){
	x<<-y
	inv<<-NULL
}
get <- function() x #similar to makevector we get x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() {
        	inve<-ginv(x)
        	inve%*%x #obtain inverse of matrix
        	
        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
##second part cache data
cacheSolve <- function(x, ...) {
   inv<-x$getinv()   
   if(!is.null(inv)){ #base cndtn
   	   message("cached data there")
   	   return(inv)
   }
   data<-x$get()
   inv<-solve(data,...)  #inverse value calculation
   x$setinv(inv)
   inv #return final matrix
}
