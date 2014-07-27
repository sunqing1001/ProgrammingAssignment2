## R Programming
## Assignment 2 - Lexical Scoping
## Author: Qing Sun

## Create special matrix in memory

makeCacheMatrix<-function(mat=numeric()){
    inv<-NULL
    set<-function(y){
        mat<<-y
        inv<<-NULL
    }
    get<-function() mat
    setInv <- function(a) inv<<-a
    getInv <- function() inv
    list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Solve the inverse of matrix and save it in memory
cacheSolve <- function(x,...) {
    ## Return a matrix that is the inverse of 'x'
    inv<-x$getInv()
    if(!is.null(inv)) {
        message("Getting cached data......")
        return(inv)
    }
    else {
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInv(inv)
        inv
    }
}


a<-matrix(0,3,3); diag(a) <- c(1,2,3)
a
x<-makeCacheMatrix(a)
x$get()
x$getInv()
cacheSolve(x)
x$getInv()

