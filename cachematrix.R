makeCacheMatrix<-function(x=matrix()){
        ##initialize matrix
        m<-NULL
        ##set the value of the matrix
        set <-function(y){
                x<<-y
                m<<-NULL
        }
        ##returns value of cached matrix
        get <- function() x
        ##matrix is inverted and cached
        setinverse <-function(inverse) m<<-inverse
        ##return value of cached inverted matrix
        getinverse <-function() m
        ##list of functions returned
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve<-function(x,...){
       ##does an inverted matrix already exist in the cache
       im<-x$getinverse()
       ##if it does simply return it
       if (!is.null(im)){
               message("Returning data from cache")
               return(im)
       }
       ##if not just get the cached matrix and use solve to invert
       cm<-x$get()
       ##For purposes of this assignment, assume a "proper" matrix which can be inverted with the solve() command
       ##TryCatch and non-square inversion not neccessary in this context.
       im<-solve(cm)
       ##set value of this new inverted matrix and return
       x$setinverse(im)
       im
}       