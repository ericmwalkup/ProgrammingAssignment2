## Put comments here that give an overall description of what your
## functions do

## Creates a matrix, creates functions, makes a list of the functions

MakeCacheMatrix<-function(x,nr,nc) 
{
     minv<-NULL                          # set inverse to null in function env
     mtx<-matrix(c(x),nr,nc)             # create matrix from input 1xN vector

     getmtx<-function() mtx             # function to return the matrix 
     getinv<-function() minv            # function to return the inverse
     
     setinv<-function(inv) minv<<-inv   # function to set inverse
     
     # create a list with function item names
     # and the functions items
     list(getmtx=getmtx, getinv=getinv, setinv=setinv)
}


## Checks to see if inverse exists


CacheSolve<-function(xl)
{
     minv<-xl$getinv()                    # use input list item named getinv to get inverse
   
     if(!is.null(minv))                   # if the inverse is NOT null cache it
          # return skips remaining code (no need to recalc inv)
     {
          message("getting cached data")
          return(minv)
     }
     
     mtx<-xl$getmtx()                     # if minv is NULL, get the matrix from MakeCacheList            
     minv<-solve(mtx)                     # compute the inverse
     xl$setinv(minv)                      # set the inverse to minv
     minv                                 # function returns minv 
}



##x<-c(2, 4, 3, 1, 5, 7, 6, 5, 4)
##listfunc<-MakeCacheMatrix(x,3,3)
##CacheSolve(listfunc)
##CacheSolve(listfunc)