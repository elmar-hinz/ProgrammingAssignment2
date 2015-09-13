######################################################################
#                                                                    #
#    Copyright (c) 2015 Elmar Hinz (github.com/elmar-hinz)           #
#                                                                    #
#    This script solves the programming assignment 2                 #
#    of coursera rprog-032.                                          #
#    It ships with a suite of unit tests.                            #
#                                                                    #
#    License: MIT (see LICENSE.txt)                                  #
#                                                                    #
######################################################################

######################################################################
# A few words to my reviewers:
#
# On the first glance you may think, why so many lines for so little
# code. I simply want to show how professional code looks like, as
# a am a professiol coder. Professional code is well documented,
# verbose and tested.
#
# If you have experiences in object orientated programming, you will
# observe, that my code is orginized in two levels in the spirit of
# classes and methods. You will quickly feel at home.
#
# The course didn't cover a special commenting style, so I applied
# rules that are common within Java and PHP. For each method there
# should at least be a title and the description of the @return
# value. If there are parameters, there are descriptions as well.
#
# The lower half of the file is a simple unit test suite to prove
# that the expected behaviour is provided.
#
# Runnig the tests is as simple as this:
#
#          source("cachematrix.R")
#          unitTests()
#
# Keep up the good work!
######################################################################

######################################################################
# Factory method of: Caching matrix object with inverse matrix
#
# @param: matrix - initialisation with given matrix, optional
# @return: list - list of objects access methods
##
makeCacheMatrix <- function(mtrx = matrix()) {

    inverse <- NULL

    ###################################################
    # Set the matrix
    #
    # @param: matrix - the matrix
    # @return: NULL
    ##
    setMatrix <- function(mtrx) {
            mtrx <<- mtrx
            inverse <<- NULL
    }

    ###################################################
    # Get the matrix.
    #
    #   @return: matrix - the matrix
    ##
    getMatrix <- function() {
        mtrx
    }

    ###################################################
    # Set the inverse
    #
    # @param: matrix - the inverse
    # @return: NULL
    ##
    setInverse <- function(inverse) {
            inverse <<- inverse
            NULL
    }

    ###################################################
    # Get the inverse
    #
    #  @return: matrix - the inverse
    ##
    getInverse <- function() {
        inverse
    }

    ###################################################
    # Check if matrix is solved
    #
    # The matrix is solved if there is an inverse.
    #
    #  @return: logical - TRUE if changed else FALSE
    ##
    isSolved <- function() {
        !identical(inverse, NULL)
    }

    ###################################################
    # Setup the object
    #
    # @return: NULL
    ##
    construct <- function() {
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse,
             isSolved = isSolved)
    }

    # construct and return the object
    construct()
}

######################################################################
# Solve a caching matrix and use it to cache the invers
#
# The caching matrix mtrx is an object created by makeCacheMatrix().
# This function checks if mtrx was already solved.
# If not it calculates the invers and stores it into mtrx.
#
# @param: list - a caching matrix object
# @param: ellipses - more parameters to directly handle to solve()
# @return: matrix - the invers
##
cacheSolve <- function(mtrx, ...) {
    if(!mtrx$isSolved()) {
        mtrx$setInverse(solve(mtrx$getMatrix(), ...))
    }
    mtrx$getInverse()
}

######################################################################
# Unit test suite for the methods of this file
#
# Run unit tests and report results.
#
# Usage: unitTests()
#
# @return NULL
##
unitTests <- function() {

    tests <- list()
    failed <- 0
    count <- 0

    ##################################################
    # Add test
    #
    # @param: string - name of test
    # @param: function - the test itself
    # @return: NULL
    ##
    addTest <- function(name, definition) {
        tests[[name]] <<- definition
    }

    ##################################################
    # All tests
    #
    # @return NULL
    ##
    addTest('The constructor works.', function() {
        candidate <- makeCacheMatrix()
        assertTrue(class(candidate) == "list")
        assertTrue(length(candidate) == 5)
        NULL
    })

    addTest('Initialisation by default parameter works.', function() {
        candidate <- makeCacheMatrix()
        assertTrue(dim(candidate$getMatrix()) == c(1,1))
        assertTrue(is.na(candidate$getMatrix()[1,1]))
        NULL
    })

    addTest('Initialisation with given matrix works.', function() {
        mtrx <- makeInvertibleMatrix(2)
        candidate <- makeCacheMatrix(mtrx)
        assertTrue(identical(candidate$getMatrix(), mtrx))
        NULL
    })

    addTest('Set-get cycle of matrix works.', function() {
        mtrx <- makeInvertibleMatrix(2)
        candidate <- makeCacheMatrix()
        candidate$setMatrix(mtrx)
        assertTrue(identical(candidate$getMatrix(), mtrx))
        NULL
    })

    addTest('Set-get cycle of inverse works.', function() {
        inverse <- makeInvertibleMatrix(2)
        candidate <- makeCacheMatrix()
        candidate$setInverse(inverse)
        assertTrue(identical(candidate$getInverse(), inverse))
        NULL
    })

    addTest('Inverse is initially NULL', function() {
        candidate <- makeCacheMatrix()
        assertTrue(!identical(candidate$getMatrix(), NULL))
        assertTrue(identical(candidate$getInverse(), NULL))
        assertTrue(!candidate$isSolved())
        NULL
    })

    addTest('Setting matrix nulls the invers.', function() {
        mtrx <- makeInvertibleMatrix(3)
        candidate <- makeCacheMatrix()
        candidate$setInverse(mtrx)
        assertTrue(!identical(candidate$getInverse(), NULL))
        assertTrue(candidate$isSolved())
        candidate$setMatrix(mtrx)
        assertTrue(identical(candidate$getInverse(), NULL))
        assertTrue(!candidate$isSolved())
        NULL
    })

    addTest('A matrix can be solved and the result is cached.', function() {
        mtrx <- makeInvertibleMatrix(3)
        inverse <- solve(mtrx)
        cache <- makeCacheMatrix(mtrx)
        assertTrue(!cache$isSolved())
        result <- cacheSolve(cache)
        assertTrue(identical(result, inverse))
        assertTrue(cache$isSolved())
        NULL
    })

    addTest('Changing the matrix clears and resolves.', function() {
        mtrx1 <- makeInvertibleMatrix(3)
        mtrx2 <- makeInvertibleMatrix(4)
        cache <- makeCacheMatrix(mtrx1)
        cacheSolve(cache)
        assertTrue(cache$isSolved())
        cache$setMatrix(mtrx2)
        assertTrue(!cache$isSolved())
        cacheSolve(cache)
        assertTrue(cache$isSolved())
        NULL
    })

    ##################################################
    # Create a random invertible matrix of dimension d * d
    #
    # @param d: numeric - dimension of the matrix, default to 4
    # @return: matrix - invertible matrix
    ##
    makeInvertibleMatrix <- function(d = 4) {
        mtrx <- sample(1:(d * d))
        dim(mtrx) <- c(d, d)
        # recurse until we have got an invertible matrix
        if(det(mtrx) == 0) mtrx <- makeInvertibleMatrix()
        mtrx
    }

    ##################################################
    # Check the test result and do statistics
    #
    # @return NULL
    ##
    assertTrue <- function(result) {
        count <<- count + 1
        if(!is.na(result) && is.logical(result) && result == TRUE) {
            print("+ ")
        } else {
            print("- ")
            failed <<- failed + 1
        }
        NULL
    }

    ##################################################
    # Run all tests
    #
    # @return NULL
    ##
    runTests <- function() {
        nms  <- names(tests)
        for(i in 1:length(tests)) {
            print(nms[i])
            tests[[i]]()
        }
        NULL
    }

    ##################################################
    # Report success count
    #
    # @return NULL
    ##
    report <- function() {
        print(paste(failed, "tests of", count, "failed."))
        NULL
    }

    # Run tests and report
    runTests()
    report()
    NULL
}

