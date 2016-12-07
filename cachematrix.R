## Our function makeCacheMatrix (obvoiusly ;)) creates a matrix thingy
## that can store its inverted data:

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        
        get<-function() x
        setInverted<-function(inverted) inv<<-inverted
        getInverted<-function() inv
        list(set=set,
             get=get,
             setInverted=setInverted,
             getInverted=getInverted)
}


## The function below returns inverted matrix defined by the first function.
## If we already inverted the matrix - it is stored in cache, so it won't be 
## calculated again, but instead retrieved from cache (with a special message
## about that fact).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverted()
        if(!is.null(inv)) {
                message("retrieving cached values, Yay!")
                return(inv)
        }
        
        dat<-x$get()
        inv<-solve(dat,...)
        x$setInverted(inv)
        inv
}
