## Put comments here that give an overall description of what your
## functions do


#################################################################################
## This function works on a OOP-like philosophy, implementing the CacheMatrix  ##
## object and its outside interface.                                           ##
## It heavily depends on R's lexical scoping rules allowing all the functions  ##
## defined inside the 'calls' to access the same environment (the one in which ##
## the function itself is defined) and therefore cross-manipulate the data.    ##
##                                                                             ##
## Inputs:                                                                     ##
## x: (o) the matrix for which we want to compute the inverse                  ##
##                                                                             ##
## Output:                                                                     ##
## Retruns a set of functions inside a 'list' object which can be used to      ##
## access and manipulate the object's variables and envirnoment.               ##                                       ##
#################################################################################
makeCacheMatrix <- function(x = matrix()) {
      
      ##Initialize Object's variables
      CacheData <- matrix()
      IsCached <-FALSE
      
      
      
      ##Creating Object's interface and properties
      ### Sets input matrix
      SetMatrix <-function(Matrix){
            x <<-Matrix
            CacheData <<- matrix() #Clean Cache data when input matrix changes
            IsCached <<-FALSE #Clean Cache Status
      }
      
      ### Returns input matrix
      GetMatrix <- function(){
            x
      }
      
      ### Returns Cache status
      CacheExists <- function(){
            IsCached
      }
      
      ### Computes, stores in cache and returns the Inverse Matrix
      ComputeInverse <- function(){
            IsCached <<- TRUE  #I'm quite optimistic: solve() may fail for some reason!
            CacheData <<- solve(x)
      }

      ### Sets Inverse Matrix from an outside input
      SetInverse <- function(InvMatrix){
            IsCached <<- TRUE  
            CacheData <<- InvMatrix
      } 
      
      ### Reads Inverse Matrix from Cache
      GetInverse <- function(){
            CacheData
      }  
      
      ### Cleans existing cache
      CleanCache <- function(){
            CacheData <<- matrix() 
            IsCached <<-FALSE 
      }
      
      
      
      ## Expose Object's properties as return of the function
      list(SetMatrix = SetMatrix, 
           GetMatrix=GetMatrix, 
           CacheExists=CacheExists, 
           ComputeInverse=ComputeInverse,
           SetInverse=SetInverse,
           GetInverse=GetInverse,
           CleanCache=CleanCache)
}


#################################################################################
## This function handles the 'Cache Matrix' object it receives as mandatory    ##
## parameter and based on the existence of cached data it 'decides' if it has  ##
## to compute the Inverse Matrix (IM) on the fly (and store it for later use)  ##
## or if the IM can be read from cache avoiding recomputing on each time the   ##
## IM is needed.                                                               ##
##                                                                             ##
## Inputs:                                                                     ##
## x: (m) a "Cache Matrix" object                                              ##
##                                                                             ##
## Output:                                                                     ##
## Retruns a 'matrix' inside a 'list' object which label indicates the data    ##
## source (computed vs read).                                                  ##
#################################################################################
cacheSolve <- function(x, ...) {

      ##Look at the cache status on the Cache Matrix object
      if (x$CacheExists()) {
            ## Inverse matrix is in cache... Let's read it
            out<-list(Read=x$GetInverse())
      } else {
            ## Inverse matrix is not in cache... Let's compute and store it
            out<-list(Computed=x$ComputeInverse())
      }
      out
}

