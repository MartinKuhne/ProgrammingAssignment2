## reviewer note: I've called makeCacheMatrix makeProxy for a more correct
## and descriptive name

# makeProxy - returns a list of functions to get and set an object
# and get and set an associated object
# Parameters - object: object you want to decorate
# Returns - list of functions: getObject, setObject, getMeta, setMeta
# Notes: The associated object is called Meta here, alluding to "Metadata"
# This is a generalized and cleaned up version of the makeVector/makeCacheMatrix () provided

makeProxy <- function(object)
{
    cachedMeta <- NULL
    
    # return the encapsulated object
    getObject <- function()
    {
        object
    }
    
    # setting the encapsulated object clears the Meta
    setObject <- function(newObject)
    {
        object <<- newObject
        cachedMeta <<- NULL
    }
    
    # retrieve the Meta
    getMeta <- function()
    {
        cachedMeta
    }
    
    # update the Meta
    setMeta <- function(newMeta)
    {
        cachedMeta <<- newMeta
    }
    
    list(getObject = getObject,
         setObject = setObject,
         getMeta = getMeta,
         setMeta = setMeta)
}

# cacheSolve - returns the inverse of a matrix, caching the result for repeat calls
# Parameters - proxy: proxy obtained from makeProxy()
# Returns - inverted matrix

cacheSolve <- function(proxy)
{
    cachedValue <- proxy$getMeta()
    if(!is.null(cachedValue)) {
        message("getting cached data")
        return(cachedValue)
    }
    
    data <- proxy$getObject()
    computedValue <- solve(data)
    proxy$setMeta(computedValue)
    computedValue
}

