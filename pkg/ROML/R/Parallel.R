## TODO!

load_parallel <- function() {
    if ( any(grepl(":parallel", search())) ) 
        return( TRUE )
    ## loaded <- require("parallel", quietly=TRUE)
    return( FALSE )
}

run_scenarios <- function(model, scenarios, cl=1L) {
    if ( is.numeric(cl) ) {
        if ( !load_parallel() ) stop("the 'parallel' package couldn't be found!")
        solution <- NULL
    } else if ( inherits(cl, "cluster") ) {

    } else {
        stop("'cl' must be 'integer' or object of type 'cluster'!")
    }
}
