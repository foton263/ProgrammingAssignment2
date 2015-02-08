## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
# We define the cachedMatrix object and the set,get,set_inverse,
# get_inverse methods that are stored in a list
# We use the <<- operator to persistent store in global env of 
# x which is the original matrix and inv_matrix which is the inverted 
# matrix. 
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(initmatrix) { x <<- initmatrix; inv_matrix <<- NULL }
  get <- function() x
  set_inverse <- function(inverted_matrix) inv_matrix <<- inverted_matrix
  get_inverse <- function() inv_matrix
  list(set = set, 
       get = get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse
       )
  }

## Write a short comment describing this function
## first check if the inverse matrix exists and if exists it is returned
## otherwise we calculate the inverse matrix using the solve() function 
## and assign it with the set_inverse method to the chache matrix object 
## x is an cached matrix object 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix<- x$get_inverse()
  if(!is.null(inverse_matrix)) { return(inverse_matrix) }
  cached_matrix <- x$get()
  inverse_matrix<- solve(cached_matrix) 
  x$set_inverse(inverse_matrix)
  return(inverse_matrix)
}
# some function testing...
# define the ordinary matrix
m<-m<-matrix(rnorm(n = 25,mean=11,sd = 1),nrow = 5,ncol = 5)
# construct the matrix object 
A<-makeCacheMatrix(m)
# call the get method to show the matrix elements
A$get()
# compute the inverse and store it persistently in  global env
cacheSolve(A)
# use the get_inverse method to show the stored inverse of the matrix m
A$get_inverse()

# may the R be with you...
