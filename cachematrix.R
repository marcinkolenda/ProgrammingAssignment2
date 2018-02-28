

# make function 

makeCacheMatrix <- function(orginal_matrix = matrix()) {
  inverted_matrix <- NULL
  ## passing orginal/ inverted  null matrix
  set <- function(pass_arg) {
    orginal_matrix <<- pass_arg
    inverted_matrix <<- NULL
  }
  ## passing computed inverted matrix
  set_inverted_matrix <- function(pass_arg2) inverted_matrix <<- pass_arg2
  ## getting orginal matrix
  get <- function() orginal_matrix
  ## getting  inverted matrix 
  get_inverted_matrix <- function() inverted_matrix
  ## passing all functions as a list
  list(set = set, get = get,
       setim = set_inverted_matrix,
       getim = get_inverted_matrix)
}

# comuting or geting cashed inverted matrix function

cacheSolve <- function(orginal_matrix) {
## takeing inverted matrix for orginal_matrix 
  inverted_matrix <- orginal_matrix$getim()
  ## if inverted_matrix exists than taking it from "cache"
  if(!is.null(inverted_matrix)) {
    message("getting cached data")
    return(inverted_matrix)
  }
  message("no cached data - need to compute :(")
  data <- orginal_matrix$get()
  inverted_matrix <- solve(data)
  orginal_matrix$setim(inverted_matrix)
  inverted_matrix

}

#  testing function
c_i_m_testing <-function() {
  aM<-matrix(c(2,1,5,-1,-1,-2,1,2,2), nrow=3, ncol=3)
  aMatrix <-makeCacheMatrix(aM)
  c1 <-cacheSolve(aMatrix)
  c2 <-cacheSolve(aMatrix)
  list(aM,c1, c2)}

