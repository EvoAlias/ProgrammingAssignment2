## makeCacheMatrix takes a matrix and initializes a list of functions along
#with storing the matrix which can be retrieved by get in cacheSolve

##cacheSolve takes makeCacheMatrix(x) and gives the inverse of x. 


## makeCacheMatrix takes a square matrix as input

makeCacheMatrix <- function(x = matrix()) {

#IN is NULL when the inverse has not been calculated
    IN<-NULL
    #after initial run don't have to reuse makeCacheMatrix. Instead set will
    #take a new matrix and this will then be available as x through get
    set<-function(y){
        x<<-y
        IN<<-NULL
    }

    ## get returns the initial matrix when called
    get<-function() x
    
    ##IN takes on the value from the cacheSolve and can be retrieved outside
    ##the setIN function
    setIN<-function(solve) IN<<-solve
    
    ##getIN returns the inverse matrix when called if it has been assigned 
    ##in cacheSolve
    
    getIN<-function() IN
    list(set=set,get=get,setIN=setIN,getIN=getIN)
}

## cacheSolve takes as input makeCacheMatrix(x) and returns a matrix that
##is the inverse of x

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
    IN<-x$getIN()
        #IN is assigned to the return of getin which is NULL if it has not been
        #calculated before. If not null it has been caclulated so the inverse matrix
        # is returned.
    
    if(!is.null(IN)){
        message ("getting cached inverse matrix")
        return(IN)
    }
    
    #If IN is Null we pull the matrix from the return of get and then use solve
    #to obtain the inverse matrix. This inverse matrix is returned. 
    mat<-x$get()
    IN<-solve(mat,...)
    x$setIN(IN)
    IN
}
