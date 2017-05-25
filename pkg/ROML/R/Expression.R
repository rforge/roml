
ROML_Expression <- R6Class(
    "Expression",
    public = list(
        portable=FALSE,
        container=NULL,
        value=NULL,
        data=NULL,
        A=NULL,
        Q=NULL,
        initialize = function(container=NULL, value=NULL, data=NULL, A=NULL, Q=NULL) {
            self$container <- container
            self$value <- value
            self$data <- data
            self$A <- A
            self$Q <- Q
        }
    )
)

## A expressions are linear expressions
A_Expression <- function(container=NULL, value=NULL, data=NULL, A=NULL) {
    expr <- ROML_Expression$new(container=container, value=value, data=data, A=A)
    class(expr) <- c("A_Expression", class(expr))
    expr
}

is.A_Expression <- function(x) inherits(x, "A_Expression")

## Q expressions are quadratic expressions
Q_Expression <- function(container=NULL, value=NULL, data=NULL, Q=NULL) {
    expr <- ROML_Expression$new(container=container, value=value, data=data, Q=Q)
    class(expr) <- c("Q_Expression", class(expr))
    expr
}

is.Q_Expression <- function(x) inherits(x, "Q_Expression")

as.Expression <- function(x) UseMethod("as.Expression")
as.Expression.Expression <- identity
as.Expression.NULL <- NULL
as.Expression.numeric <- function(x) x
as.Expression.Vector.Variable <- function(x) {
    A_Expression(container=x$container, value=x$value, data=x$data, A=x$A)
}

as.A_Expression <- function(x) UseMethod("as.A_Expression")
as.A_Expression.NULL <- function(x) x

as.Q_Expression <- function(x) UseMethod("as.Q_Expression")
as.Q_Expression.NULL <- function(x) Q_Expression()

