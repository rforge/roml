##' @noRd
##' @S3method variable.names Vector.Variable
variable.names.Vector.Variable <- function(object, ...) {
    names(object$value)    
}

fill_zero <- function(x, nrow=NULL, ncol=NULL) UseMethod( "fill_zero" )

fill_zero.simple_triplet_matrix <- function(x, nrow=NULL, ncol=NULL) {
    if ( is.null(nrow) & is.null(ncol) ) return(x)
    if ( !is.null(ncol) ) {
        if ( ncol < x$ncol ) stop("'ncol' must be greater than ncol(x)")
        x$ncol <- ncol
    }
    if ( !is.null(nrow) ) {
        if ( nrow < x$nrow ) stop("'nrow' must be greater than nrow(x)")
        x$nrow <- nrow
    }
    return(x)
}

fill_zero.L_objective <- function(x, nrow=NULL, ncol=NULL) {
    if ( !is.null(nrow) ) warning("'nrow' provided but will be ignored")
    x$L <- fill_zero(x$L, nrow=NULL, ncol=ncol)
    attr(x, "nobj") <- ncol
    return(x)
}

fill_zero.Q_objective <- function(x, nrow=NULL, ncol=NULL) {
    if ( !is.null(nrow) ) warning("'nrow' provided but will be ignored")
    if ( !is.null(x$L) )
        x$L <- fill_zero(x$L, nrow=NULL, ncol=ncol)
    x$Q <- fill_zero(x$Q, nrow=ncol, ncol=ncol)
    attr(x, "nobj") <- ncol
    return(x)
}

zero_L_constraint <- function(ncol, names) {
    L_constraint(L=simple_triplet_zero_matrix(nrow=0, ncol=ncol), 
        dir=character(0), rhs=integer(0), names=names)
}

##' @noRd
##' @S3method print environment
print.environment <- function(x, ...) {
    print(as.list(x))
}
