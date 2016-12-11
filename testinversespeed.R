testInverseSpeed <-function(x=numeric()){
    
    # Load functions:
    source("makeCacheMatrix.R")
    source("cacheSolve.R")
    
    # Make an invertible matrix from function input:
    n<-x
    nn<-x^2
    m<-matrix(runif(nn,min = 1,max = 10),n)
    m[lower.tri(m)]=t(m)[lower.tri(m)]
    
    # Run script to initialize "special" vector:
    x<-makeCacheMatrix(m)
    
    # Run script to calculate matrix inverse without the cached solution and time it:
    tic<-proc.time()
    y<-cachesolve(x)
    toc<-proc.time()-tic
    message(c("Time to invert matrix:",toc[[3]]*1000,"ms"))
    
    # Run script again to call cached matrix inverse and time it:
    tic<-proc.time()
    y<-cachesolve(x)
    toc<-proc.time()-tic
    message(c("Time to call cached matrix inverse:",toc[[3]]*1000,"ms"))
    return(y)
}
