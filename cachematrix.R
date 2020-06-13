## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function stores a matrix and in cache
# Inverse matrix can be stored of accessed with method setinv() or getinv()

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m_inv <<- inv
        getinv <- function() m_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
# This function verify if the inverse matrix of x has been calculated, if so it returns its value
# if no calculation, it is checked wether the matrix is 2x2 or 3x3 which can be calculated using basic operations
# For matrix with dimensions different to 2x2 or 3x3 the function inv() from package matlib is used
# x must be an object created by fucntion makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        mat_inv <- x$getinv()
        if(!is.null(mat_inv)) {
                message("getting cached inverse")
                return(mat_inv)
        }
        data <- x$get()
        
        mat_inv<-matrix(
                rep(1,length(data)), 
                    nrow = nrow(data),
                    ncol= ncol(data)
                        )
                           

        #Square matrix?
        if(nrow(data)==2 & ncol(data)==2){
                #If so, swap [1,1] with [2,2] and negative of [1,2] andd [2,1]
                #https://en.wikipedia.org/wiki/Invertible_matrix#Inversion_of_2_%C3%97_2_matrices
                a1<-data[1,1]
                b1<-data[1,2]
                c1<-data[2,1]
                d1<-data[2,2]
                
                det_m<-a1*d1 - b1*c1
                mat_inv[1,1]<-d1/det_m
                mat_inv[2,2]<-a1/det_m
                mat_inv[1,2]<-(-1)*b1/det_m
                mat_inv[2,1]<-(-1)*c1/det_m
                
        } else {
                if(nrow(data)==3 & ncol(data)==3){
                        #https://en.wikipedia.org/wiki/Invertible_matrix#Inversion_of_3_%C3%97_3_matrices
                        a1<-data[1,1]
                        b1<-data[1,2]
                        c1<-data[1,3]
                        d1<-data[2,1]
                        e1<-data[2,2]
                        f1<-data[2,3]
                        g1<-data[3,1]
                        h1<-data[3,2]
                        i1<-data[3,3]
                        
                        A1<- e1*i1-f1*h1
                        B1<- (-1)*(d1*i1 - f1*g1)
                        C1<- d1*h1 - e1*g1
                        D1<- (-1)*(b1*i1 - c1*h1)
                        E1<- a1*i1 - c1*g1
                        F1<- (-1)*(a1*h1 - b1*g1)
                        G1<- b1*f1 - C1*e1
                        H1<- (-1)*(a1*f1 - c1*d1)
                        I1<- a1*e1 - b1*d1
                        det_m1<-a1*A1+b1*B1+c1*C1
                        mat_inv<-matrix(c(A1, D1, G1,
                                        B1, E1, H1,
                                        C1, F1, I1), 3,3)/det_m1
                } else {
                        # no square, nor 3x3...then use inv() from matlib
                        mat_inv<-inv(data)
                }
                
        }
        x$setinv(mat_inv)
        mat_inv

}
