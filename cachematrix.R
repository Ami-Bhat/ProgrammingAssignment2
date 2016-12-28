## Set of functions that can store and recall a cached version
## of a square of a matrix and its inverse matrix 
>mat<-matrix(c(1:4),c(2,2))
>cacheMat<-makeCacheMatrix(mat)
>cacheMat$get()
>cacheSolve(cacheMat)


##Creates an object with the cache of a sqaure matrix and creates a list of functions for  interacting with the matrix

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(y){
x<<-y
i<<-NULL
}
get<-function()x
setinv<-function(inv)i<<-inv
getinv<-function()i
list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##Inverse of the cached square Matrix is given by the above function.If the inverse of the
##cached matrix has been solved returns the cached version of the inverse matrix else,solves
##the inverse matrix and stores in the cache

cacheSolve <- function(x, ...) {
i<-x$getinv()
if(!is.null(i)){
message("retrieving the cached inverse matrix")
return(i)
}
        ## Return a matrix that is the inverse of 'x'
m<-x$get()
i<-solve(m,...)
x$setinv(i)
i
}
