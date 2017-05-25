##
## types:
##  - logical 
##  - integer
##  - double
##
##
##
##

ROML_Variable <- R6Class(
    "Variable",
    public = list(
        portable=FALSE,
        name=NULL,
        container=NULL,
        type=NULL,
        length=NULL,
        dim=NULL,
        value=NULL,
        data=NULL,
        initialize = function(name, container=NULL, type=NULL, dim=NULL, 
                              length=NULL,  data=NULL) {
            self$name <- name
            self$container <- container
            self$type <- type
            self$dim <- dim
            if ( is.numeric(length) ) {
                self$length <- length
                self$value <- setNames(rep.int(1L, length), 
                                       sprintf("%s$%i", name, seq_len(length)))
            }
            self$data <- data
        }
    )
)

##' @noRd
##' @S3method length Variable
length.Variable <- function(x) x$length

Variable <- function(name, container=NULL, type=NULL, dim=NULL, length=NULL, data=NULL) {
    if ( container == "Vector" )
        return( Vector.Variable(name=name, type=type, length=length, data=data) )
    if ( container == "Matrix" )
        return( Matrix.Variable(name=name, type=type, dim=dim, data=data) )
    if ( container == "Psd.Matrix" )
        return( Psd.Matrix.Variable(name=name, type=type, dim=dim, data=data) )
    stop("unknown Varibable container!")
}

Promise.Variable <- function(name, container, type) {
    v <- Variable(name, container=container, type=type)
    class(v) <- c("Promise", class(v))
    v
}

##' @noRd
##' @export
Vector.Variable <- function(name, type="double", length, data=NULL) {
    v <- ROML_Variable$new(name=name, container="Vector", type=type, length=length, 
                           data=data)
    class(v) <- c("Vector.Variable", class(v))
    v
}

Promise.Vector.Variable <- function(name, type="double") {
    v <- Vector.Variable(name=name, type=type, length=NULL)
    class(v) <- c("Promise", class(v))
    v
}

Matrix.Variable <- function(name, type="double", dim, data=NULL) {
    v <- ROML_Variable$new(name=name, container="Matrix", type=type, dim=dim, data=data)
    class(v) <- c("Matrix.Variable", class(v))
    v
}

Promise.Matrix.Variable <- function(name, type="double") {
    v <- Matrix.Variable(name=name, type=type, dim=NULL)
    class(v) <- c("Promise", class(v))
    v
}

Psd.Matrix.Variable <- function(name, type="double", dim, data=NULL) {
    v <- ROML_Variable$new(name=name, container="Psd.Matrix", type=type, dim=dim, data=data)
    class(v) <- c("Psd.Matrix.Variable", class(v))
    v
}

Promise.Psd.Matrix.Variable <- function(name, type="double") {
    v <- Psd.Matrix.Variable(name=name, type=type, dim=NULL)
    class(v) <- c("Promise", class(v))
    v
}

length.Vector.Variable <- function(x) length(x$value)

is.Promise <- function(x) inherits(x, "Promise")
is.Vector.Variable <- function(x)  inherits(x, "Vector.Variable")
is.Matrix.Variable <- function(x)  inherits(x, "Matrix.Variable")
is.Psd.Matrix.Variable <- function(x)  inherits(x, "Psd.Matrix.Variable")
