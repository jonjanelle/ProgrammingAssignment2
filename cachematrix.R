#This file contains a pair of functions that cache the inverse of a matrix.
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#cacheSolve: This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

#Sample usage of the functions makeCacheMatrix and cacheSolve
#
#> mat <- matrix(c(1,2,3,0,1,4,5,6,0), nrow = 3, ncol = 3) #Create arbitrary new invertible matrix
#> matobj <- makeCacheMatrix(mat) #create special matrix object with makeCacheMatrix
#> cacheSolve(matobj) #call cacheSolve with matobj, will cache and return the inverse on first call
#> cacheSolve(matobj) #Checks to see if inverse calculated, since it has been prints "getting cached data" and returns inverse


#makeCacheMatrix creates a special matrix that can cache its inverse.
#makeCacheMatrix returns a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix mean
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m <<- solve
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

#cacheSolve calculates the matrix inverse using the list object created 
#with the makeCacheMatrix function. However, it first checks to see if 
#the inverse has already been calculated. If so, it gets the inverse from the 
#cache and skips the computation. Otherwise, it calculates the inverse of the 
#matrix using the "solve()" function and sets the value of the inverse in the 
#cache via the setinverse function.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  m
}