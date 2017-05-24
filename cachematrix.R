# makeCacheMatrix function creates a list object that will be input into a complementary function cacheSolve
# the input of makeCacheMatrix is a square matrix (n_columns x n_rows) 
# the list object is stored in a environment other than the global environment so that it is ('cached') using the <<- operator
# the list object contains four elements, which are 'set', 'get', 'setInverse', and 'getInverse'
# set stores empty variables x and inverse
# get retrieves the input matrix
# setInverse caches the computed inverse matrix from the companion function, cacheSolve 
# getInverse will be assigned after cacheSolve has been run once, and will be retrievable after that

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # assigning empty vector to variable inverse 
        set <- function(y){
                x <<- y # caching the set as variable x
                inverse <<- NULL # default inverse is NULL
        }
        get <- function() x # the input matrix will be set to get
        setInverse <- function(inverse.matrix) inverse <<- inverse.matrix # cache the inverse matrix to the variable inverse
        getInverse <- function() inverse # stores the inverse matrix as variable getInverse which will be input into a list
        list(set=set, # first element of the list is the input matrix called 'set'
             get=get, # second element of list is called 'get' which retrieves the input matrix
             setInverse=setInverse, # third element of list is called 'setInverse' which will cache the inverted matrix that is calculated
             getInverse=getInverse) # fourth element of the list is called 'getInverse' which will be assigned after a matrix cacheSolve has been previously run
}

# cacheSolve will take a list object created using the makeCacheMatrix function as an input
# cacheSolve will then check to see if the inverse matrix was already calculated -- and if it is, it will report it
# if it is not ... it will compute the inverse of a square matrix (the input of the makeCacheMatrix) using the solve function in R
# it will print out the inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse() # assign prevously stored inverse matrix to the variable 'inverse'
        if(!is.null(inverse)){ # initiate if loop that says if inverse is NOT NULL, write the following message:
                message("getting cached data") # write the message that the cached inverse matrix is being retrieved
                return(inverse) # returns the cached inverse matrix which is the variable 'inverse'
        }
        data <- x$get() # assigns the input matrix as the variable 'data'
        inverse <- solve(data) # computes the inverse of a square matrix (the input data) and assigns to the variable 'inverse'
        x$setInverse(inverse) # assign the inverse matrix to the setInverse element of makeCacheMatrix
        inverse # print the inverse matrix
}

# creating a test matrix that 2 rows, 2 columns
test.matrix <- matrix(1:4, nrow=2, ncol=2)
test.matrix # print matrix just to see it

inverse.test <- makeCacheMatrix(test.matrix)

# just to prove that it starts out with NULL
inverse.test$getInverse() # run first time and you will see that its NULL

### HERE WE ACTUALLY TEST THE FUNCTION ###
cacheSolve(inverse.test) # calculate and cache the matrix

# NOW WE SEE THAT GET INVERSE IS NO LONGER NULL
inverse.test$getInverse() # we can now retrieve the cached matrix using getInverse, so we see that our function worked, it assigned the inverse matrix to getInverse

##  run it one more time
cacheSolve(inverse.test) # this time you will see that it says 'getting cached data' because the inverse matrix is no longer NULL, and furthermore it prints the inverse