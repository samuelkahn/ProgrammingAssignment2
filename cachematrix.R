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


## takes matrix as arguement
## first checks if we have already cahced the matrix inverse
## if we have not then we set the data variable to matrix
## we then check if matrix is square (only square matrices are invertible)
## if matrix is invertible then we store matrix inverse in i and return i
## if matrix is not invertible we say matrix is not square and return NULL

cacheSolve<-function(m,...){
        i<-m$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data<-m$getMatrix()
        if(dim(data)[1]==dim(data)[2]){
        	i<-solve(data,...)
        	m$setinverse(i)
        	return(i)
        }else{
        	message("Matrix not Square! Returning NULL")
			return(NULL)
		}
}