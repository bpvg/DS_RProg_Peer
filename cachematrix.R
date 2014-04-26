#################################################################################
## The present set of functions are intended to allow computing the inverse of ##
## a given matrix and store this result in memory for later retrieval without  ##
## new computations.                                                           ##
##                                                                             ##
## For further details, please read the comments on each function.             ##
#################################################################################


#################################################################################
## This function works on a OOP-like philosophy, implementing the CacheMatrix  ##
## 'object' and its outside interface.                                         ##
## It heavily depends on R's lexical scoping rules allowing all the functions  ##
## defined inside the 'class' to access the same environment (the one in which ##
## the function itself is defined) and therefore cross-manipulate the data.    ##
##                                                                             ##
## Inputs:                                                                     ##
## x: (o) the matrix for which we want to compute the inverse                  ##
##                                                                             ##
## Output:                                                                     ##
## Returns a set of functions inside a 'list' object which can be used to      ##
## access and manipulate the object's variables and environment.               ##
#################################################################################
makeCacheMatrix <- function(x = matrix()) {
      
      ##Initialize Object's variables
      CacheData <- matrix()
      IsCached <-FALSE
      
      
      
      ##Creating Object's interface and properties
      ### Sets input matrix
      SetMatrix <-function(Matrix){
            if (!is.matrix(Matrix)) stop("input should be a 'matrix'.")
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
            if (!is.matrix(x)) stop("input should be a 'matrix'.")
            if (min(dim(x))==0) stop("input should have at least 1 row and 1 column.")
            if (nrow(x)!=ncol(x)) stop ("input should be a square matrix.")
            if (det(x)==0) stop("input is a singular matrix - it cannot be inverted.")
            IsCached <<- TRUE  #I'm quite optimistic: solve() may fail for some reason!
            CacheData <<- solve(x)
      }

      ### Sets Inverse Matrix from an outside input
      SetInverse <- function(InvMatrix){
            if (!is.matrix(InvMatrix)) stop("input should be a 'matrix'.")
            #Tests if {A}*{A-1}=={I}.
            if(!identical(InvMatrix%*%x,diag(nrow(x)))) stop("this is not the right inverse matrix!")
            IsCached <<- TRUE  
            CacheData <<- InvMatrix
      } 
      
      ### Returns Inverse Matrix from Cache
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
## x: (m) a 'Cache Matrix' object                                              ##
##                                                                             ##
## Output:                                                                     ##
## Returns a 'matrix' inside a 'list' object which label indicates the data    ##
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



#################################################################################
## Testing Function                                                            ##
##                                                                             ##
## Inputs:                                                                     ##
## size: (o) a sizexsize matrix will be used to measure compute vs read time   ##
##                                                                             ##
## Output:                                                                     ##
## a list with time consumption statistics for compute vs read                 ##
#################################################################################
# Q: Is it that faster?
# A: Yehh! It looks it is! ;)
#
# > IsItFaster(3000)
# $Compute
# user  system elapsed 
# 148.656   0.051 149.689 
# 
# $Cache
# user  system elapsed 
# 0.033   0.013   0.048

IsItFaster <- function(size=500){
      
      # creates a size x size matrix to invert
      m <- matrix(rnorm(size*size),size,size)
      # creates 'Cache Matrix' object (could have passed 'm' as parameter 'x')
      myMatrixObject <- makeCacheMatrix()  
      # lets CM know what matrix to invert (not needed if passed before as 'x')
      myMatrixObject$SetMatrix(m)         
      # returns Inverse of 'm' by computing it...
      compute_time <-system.time(invisible(cacheSolve(myMatrixObject)))
      # ... or by reading it from cache (2nd request over the same matrix)
      cache_time <- system.time(invisible(cacheSolve(myMatrixObject)))
      
      list(Compute=compute_time,Cache=cache_time)
}
