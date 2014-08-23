## The following two functions cache and recall the inverse of a matrix, which is 
## calculated via the solve() function

## makeCacheMatrix produces a list of functions to recover the input matrix, to store
## that matrix's inverse, and to retreive the inverse

makeCacheMatrix <- function(x = matrix()) {
        # the inverse of the input matrix x is initialized to the NULL object. 
        inv<-NULL 
        getmat<-function(){
                # this function recovers the input matrix
                x
        }
        invstore<-function(invx){
                # this function stores the inverse of the input matrix
                inv<<-invx
        }
        getinv<-function(){
                # retrieves the currently stored inverse of the input matrix
                inv
        }
        list(getmat=getmat, invstore=invstore, getinv=getinv)
}


## cacheSolve searches the output list of makeCacheMatrix for a stored inverse of the 
## input matrix, x. If found, the cached inverse of x is returned; otherwise, the 
## inverse is calculated and stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx<-x$getinv()
        if (!is.null(invx)){
                # this loop is entered only if there is a cache of the inverse of the 
                # input matrix x, in which case it is retreived and displayed
                message("getting cached data")
                invx
        } else {
                # this loop is entered only when there is no cache of the inverse of the 
                # input matrix. The inverse is calulated from the input matrix
                mat<-x$getmat()
                # whose inverse is computed via solve()
                invx<-solve(mat)
        }
        x$invstore(invx)
        invx
}
