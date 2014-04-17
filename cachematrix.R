## Put comments here that give an overall description of what your
## functions do


#################################################################################
##  This function implements the 'Cache Matrix' object and its interface. It   ##
##  returns a 'list of functions' used as interface to the object.             ##
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
      
      ### Reads Inverse Matrix from Cache
      ReadInverse <- function(){
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
           ReadInverse=ReadInverse,
           CleanCache=CleanCache)
}


#################################################################################
## This function expects to receive a 'Cache Matrix' object as parameter 'x'   ##
##  and prints: i) a string saying what is the data origin (ie, computed live  ##
##  or read from cache), and ii) the Inverse Matrix for one initial matrix     ##
## previously given to the 'Cache Matrix' via its 'SetMatrix' property.        ##
#################################################################################
cacheSolve <- function(x, ...) {

      ##Look at the cache status on the Cache Matrix object
      if (x$CacheExists()) {
            ## Inverse matrix is in cache... Let's read it
            print("Read")
            print(x$ReadInverse())
      } else {
            ## Inverse matrix is not in cache... Let's compute and store it
            print("Compute")
            print(x$ComputeInverse())
      }      
}

