
## Vector
x <- ROML:::Variable(name="x", container="Vector", type="double", 
                     dim=NULL, length=3L)

y <- ROML:::Variable(name="y", container="Vector", type="double", 
                     dim=NULL, length=2L)

str(2 * x + 3 * y)

class(x)

str(x - y)

str(2 * x - 3 * y)

attach(getNamespace("ROML"))

str(as.list(as.Expression(x)))

class(as.Expression(x))
as.Expression(x) + as.Expression(y)

`%*%.default` <- .Primitive("%*%")

`%*%.numeric` <- function(x, y) {
	if ( is.Vector.Variable(y) )
		return( vector_multiplication(x, y) )
	.Primitive("%*%")(x, y)
}

`%*%` <- function(x,...){ #make S3
	UseMethod("%*%",x)
}

##
vector_multiplication <- function(x, y) {
    stopifnot(is.numeric(x) | is.numeric(y))
    if ( is.numeric(x) ) {
        return( A_Expression(container="Vector", value=(x * y$value), data=y$data) )
    }
    return( A_Expression(container="Vector", value=(y * x$value), data=x$data) )
}

`%*%.Vector.Variable` <- vector_multiplication

registerS3method("%*%", "Vector.Variable", vector_multiplication)

c(1, 2, 3) * x

c(1, 2, 3) %*% x
x %*% c(1, 2, 3)

length(x)

traceback()

## NOTE: We should add a length parameter which can be 
##       checked at the end (where it has to be 1L)!

