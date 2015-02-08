## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
# We define the cachedMatrix object and the set,get,set_inverse,
# get_inverse methods that are stored in a list
# We use the <<- operator to persistent store in global env of 
# x which is the original matrix and inv_matrix which is the inverted 
# matrix. 
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inv_matrix to null
  inv_matrix <- NULL
  # set method for assigning matrix elements to matrix and inv matrix to 
  # null persistently.
  set <- function(initmatrix) { x <<- initmatrix; inv_matrix <<- NULL }
  # get method returning the matrix elements
  get <- function() x
  # setting inverse matrix method in the cached object
  set_inverse <- function(inverted_matrix) inv_matrix <<- inverted_matrix
  # getting the inverse matrix (we must first calculate it with cacheSolve)
  get_inverse <- function() inv_matrix
  # wraping the objects methods in a list for be able yo call them by $
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
## first we read the contents of the inverse of the object    
  inverse_matrix<- x$get_inverse()
##  (if its not empty return the inverse of the matrix and exit)
  if(!is.null(inverse_matrix)) { return(inverse_matrix) }
## get the original matrix elements
  cached_matrix <- x$get()
## compute the inverse matrix
  inverse_matrix<- solve(cached_matrix) 
## set it back to persistent object memory via set_inverse method
  x$set_inverse(inverse_matrix)
## you may returnnverse 
  return(inverse_matrix)
}


# finally some function testing...
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
