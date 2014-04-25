### i is the matrix inverse
### when we call set(), we set x to new matrix and set the inverse to null again
##getMatrix returns the Matrix, setInverse allows one to set inverse
## getinverse gets the inverse of the matrix
### note that we can define each internal function in one line
### all the internal functions illustrate lexical scoping

makeCacheMatrix<-function(m=matrix()){
	i<-NULL
	setMatrix<-function(x){
		m<<-x
		i<<-NULL
	}
	getMatrix<-function()m
    setinverse <- function(inverse)i<<-inverse
    getinverse<-function()i
    list(setMatrix=setMatrix,get=getMatrix,
             setInverse=setInverse,
             setInverse=setInverse)
}


## Write a short comment describing this function

cacheSolve<-function(m,...){
        i<-m$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data<-x$getMatrix()
        if(dim(data)[1]==dim(data)[2]){
        	i<-solve(data,...)
        	x$setinverse(i)
        	return(m)
        }else{
        	message("Matrix not Square! Returning NULL")
			return(NULL)
		}
}