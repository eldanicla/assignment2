# assignment2

makeCacheMatrix <- function(x = matrix()) {
                                  x<-matrix(sample(1:50,replace = TRUE),10,10)
                                  
                                  makeCacheMatrix<- function(x = matrix()) {
                                          m <- NULL
                                          matriz<-matrix(x,sqrt(length(x)),sqrt(length(x)))
                                          set <- function(y) {
                                                  x <<- y
                                                  m <<- NULL
                                          }
                                          get <- function() matriz
                                          setsolve <- function(solve) m <<- solve
                                          getsolve <- function() m
                                          list(set = set, get = get,
                                               setsolve = setsolve,
                                               getsolve = getsolve) 
                                          
                                          cacheSolve <- function(x, ...) {
                                                  solve(x)
                                                 
                                                  m <- x$getsolve()
                                                  if(!is.null(m)) {
                                                         
                                                          return(m)
                                                  }
                                                  data <- x$get()
                                                  m <- solve(data, ...)
                                                  x$setsolve(m)
                                                  m
