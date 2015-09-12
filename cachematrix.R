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
# Factory method of: Caching matrix object with inverse matrix
#
# @param: matrix - initialisation with given matrix, optional
# @return: list - list of objects access methods
##
makeCacheMatrix <- function(matrix = matrix()) {

        inverse <- NULL

        ###################################################
        # Setup the object
        #
        # @return: NULL
        ##
        construct <- function() {
            list(setMatrix <- setMatrix,
                 getMatrix <- getMatrix,
                 setInverse <- setInverse,
                 getInverse <- getInverse)
        }

        ###################################################
        # Set the matrix
        #
        # @param: matrix - the matrix
        # @return: NULL
        ##
        setMatrix <- function(matrix) {
                matrix <<- matrix
                inverse <- NULL
        }

        ###################################################
        # Get the matrix.
        #
        #   @return: matrix - the matrix
        ##
        getMatrix <- function() {
            matrix
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
        # Check if matrix was changed
        #
        #  @return: logical - TRUE if changed else FALSE
        ##
        wasChanged <- function() {
            NULL
        }

        # construct and return the object
        construct()
}

######################################################################
# Solve a Matrix and cache the result
#
# @param matrix: matrix - the matrix to inverse
# @return:       matrix - the reversed matrix
##
cacheSolve <- function(matrix, ...) {

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

    failed <- 0
    count <- 0

    ##################################################
    # Test methods
    #
    # All methodes starting with the prefix "test" contain tests.
    #
    # @return NULL (b/c assertTrue() returns NULL)
    ##

    testTheConstructor <- function() {
        candidate <- makeCacheMatrix()
        assertTrue(class(candidate) == "list")
        assertTrue(length(candidate) == 4)
    }

    ##################################################
    # Create a random invertible matrix of dimension d * d
    #
    # A test helper method
    #
    # @param d: numeric - dimension of the matrix, default to 4
    # @return: matrix - invertible matrix
    ##
    makeInvertibleMatrix <- function(d = 4) {
        matrix <- sample(1:(d * d))
        dim(matrix) <- c(d, d)
        # recurse until we have got an invertible matrix
        if(det(matrix) == 0) matrix <- makeInvertibleMatrix()
        matrix
    }

    ##################################################
    # Check the test result and do statistics
    #
    # @return NULL
    ##
    assertTrue <- function(result) {
        count <<- count + 1
        if(result == TRUE) {
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
        testTheConstructor()
        NULL
    }

    ##################################################
    # Check the test result and do statistics
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

