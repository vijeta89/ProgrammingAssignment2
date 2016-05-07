##Assignment: Caching the Inverse of a Matrix
##Week 3: R Programming 
##Data Science Specialization
##Vijeta Bhambhani

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x=matrix()) {
  ##This is initializing the inverse property
 inv<-NULL
  
  ##This is setting the matrix
  
  set<-function(y){
    x<<-y
   inv<<-NULL
  }

  ##This is getting the matrix
  
  get<-function(){
    ##This is returning the matrix
    x
  }
  
##This is setting the inverse of the matrix
  
  setInverse<-function(inverse){
    inv<<-inverse
  }
  
##This is getting the inverse of the matrix 
  
  getInverse<-function(){
    inv
  }
  ##This returns a list of the methods
  
  list(set=set, get=get, 
       setInverse=setInverse,
       getInverse=getInverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve<-function(x,...){
  ##Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  ##Return the inverse 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##Get matrix 
  data<-x$get()
  ##Calculate inverse
  inv<-solve(data)
  ##Set inverse
  x$setInverse(inv)
  ##Return matrix
  inv
}
##> ##Example run
> x=rbind(c(1,-2/5),c(-2/5,1))
> m=makeCacheMatrix(x)
> m$get()
     [,1] [,2]
[1,]  1.0 -0.4
[2,] -0.4  1.0
> ##No cache in first run
> cacheSolve(m)
          [,1]      [,2]
[1,] 1.1904762 0.4761905
[2,] 0.4761905 1.1904762
> ##Getting from cache in second run
> cacheSolve(m)
getting cached data
          [,1]      [,2]
[1,] 1.1904762 0.4761905
[2,] 0.4761905 1.1904762
