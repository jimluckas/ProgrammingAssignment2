## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix. If the inverse has already
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.
 
## makeCacheMatrix: 
##    set the value of the matrix
##    get the value of the matrix
##    set the inverse value of the matrix 
##    get the inverse value of the matrix
##    return a list

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinv<-function(solve) m<<- solve
    getinv<-function() m
    list(set=set, get=get,
        setinv=setinv,
        getinv=getinv)
}

## cacheSolve returns the inverse of the matrix. 
##    It checks to see if the inverse has already been done. 
##    If it has then it gets the result
##    If not, it calculates the inverse then sets the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m<-solve(data, ...)
    x$setinv(m)
    m
}
## new