
`[.Vector.Variable` <- function(x, i) {
    A_Expression(x$container, x$value[i], x$data)
}

`[.Expression` <- `[.Vector.Variable`

#' @export
#' @noRd
`*.Vector.Variable` <- function(x, y) return( as.Expression(x) * as.Expression(y) )

#' @export
#' @noRd
`*.A_Expression` <- function(x, y) {
    if ( is.A_Expression(x) & is.A_Expression(y) ) stop("TODO: is.A_Expression(x) & is.A_Expression(y)")
    if ( is.numeric(y) ) {
        x$value <- y * x$value
        return(x)
    }
    if ( is.numeric(x) ) {
        y$value <- x * y$value
        return(y)
    }
}

add_expressions <- function(x, y) {
    x <- as.Expression(x)
    y <- as.Expression(y)
    if ( is.A_Expression(x) & is.A_Expression(y) ) {
        if ( !all(c(x$container, y$container) %in% "Vector") ) {
            stop("TODO: NOT all Vector container")
        }
        return( A_Expression(container="Vector", value=c(x$value, y$value), 
                             data=cbind(x$data, y$data)) )
    }
    if ( is.numeric(x) ) 
        return( A_Expression(container=y$container, value=(x + y$value), data=y$data) )
    if ( is.numeric(y) ) 
        return( A_Expression(container=x$container, value=(y + x$value), data=x$data) )
    stop("error")
}

substract_expressions <- function(x, y) {
    x <- as.Expression(x)
    y <- as.Expression(y)
    if ( is.A_Expression(x) & is.A_Expression(y) ) {
        if ( !all(c(x$container, y$container) %in% "Vector")  ) 
            stop("TODO: NOT all Vector container")
        return( A_Expression(container="Vector", value=c(x$value, -y$value), 
                             data=cbind(x$data, y$data)) )
    }
    if ( is.numeric(x) ) 
        return( A_Expression(container=y$container, value=( x - y$value), data=y$data) )
    if ( is.numeric(y) ) 
        return( A_Expression(container=x$container, value=(-y + x$value), data=x$data) )
    stop("error")
}

#' @export
#' @noRd
`/.Vector.Variable` <- function(x, y) {
    if ( is.numeric(x) ) 
        return( A_Expression(container="Vector", value=(x / y$value), data=y$data) )
    if ( is.numeric(y) ) 
        return( A_Expression(container="Vector", value=(y / x$value), data=x$data) )
    stop(sprintf('"%s / %s" is not applicable!', shQuote(class(x)[1]), shQuote(class(y)[1])))
}

#' @export
#' @noRd
`%*%.Vector.Variable` <- function(x, y) {
    stopifnot(is.numeric(x) | is.numeric(y))
    if ( is.numeric(x) ) {
        return( A_Expression(container="Vector", value=(x / y$value), data=y$data) )
    }
    return( A_Expression(container="Vector", value=(y / x$value), data=x$data) )
}

## Note:
##   Due to registration issues + and - Vector.Variable is registered in zzz.R
## 
## # @export
## # @noRd
## `+.Vector.Variable` <- function(x, y) {
##     if ( is.Vector.Variable(x) & is.Vector.Variable(y) )
##         return( A_Expression(container="Vector", value=c(x$value, y$value), 
##                              data=cbind(x$data, y$data)) )
##     if ( is.numeric(x) ) 
##         return( A_Expression(container="Vector", value=x + y$value, data=y$data) )
##     if ( is.numeric(y) ) 
##         return( A_Expression(container="Vector", value=y + x$value, data=x$data) )
##     stop("error")
## }

## # @export
## # @noRd
## `-.Vector.Variable` <- function(x, y) {
##     if ( is.Vector.Variable(x) & is.Vector.Variable(y) )
##         return( A_Expression(container="Vector", value=c(x$value, -y$value), 
##                              data=cbind(x$data, y$data)) )
##     if ( is.numeric(x) ) 
##         return( A_Expression(container="Vector", value=( x - y$value), data=y$data) )
##     if ( is.numeric(y) ) 
##         return( A_Expression(container="Vector", value=(-y + x$value), data=x$data) )
##     stop("error")
## }

`<=.Variable` <- function(lhs, rhs) as.Expression(lhs) <= as.Expression(rhs)
`>=.Variable` <- function(lhs, rhs) as.Expression(lhs) >= as.Expression(rhs)
`<.Variable`  <- function(lhs, rhs) as.Expression(lhs) <  as.Expression(rhs)
`>.Variable`  <- function(lhs, rhs) as.Expression(lhs) >  as.Expression(rhs)
`==.Variable` <- function(lhs, rhs) as.Expression(lhs) == as.Expression(rhs)

`<=.Expression` <- function(lhs, rhs) {
    if ( inherits(lhs, "Expression") & inherits(rhs, "Expression") ) {
        return( (lhs - rhs) <= 0  )
    }
    if ( is.numeric(lhs) ) return( rhs >= lhs )
    if ( is.numeric(rhs) ) {
        if( is.A_Expression(lhs) ) {
            ## TODO: What happens when A is defined?
            return( L_constraint(L=ROI::as.L_term(lhs$value), 
                                 dir="<=", rhs=rhs, names=names(lhs$value)) )
        }
    }
    stop(sprintf("TODO: `<=.Expression('', '')`", class(lhs)[1], class(lhs)[2]))
}

`>=.Expression` <- function(lhs, rhs) (rhs <= lhs)

`<.Expression` <- function(lhs, rhs) {
    if ( inherits(lhs, "Expression") & inherits(rhs, "Expression") ) {
        return( (lhs - rhs) < 0  )
    }
    if ( is.numeric(lhs) ) return( rhs > lhs )
    if ( is.numeric(rhs) ) {
        if( is.A_Expression(lhs) ) {
            ## TODO: What happens when A is defined?
            return( L_constraint(L=ROI::as.L_term(lhs$value), 
                                 dir="<", rhs=rhs, names=names(lhs$value)) )
        }
    }
    stop(sprintf("TODO: `<.Expression('', '')`", class(lhs)[1], class(lhs)[2]))
}

`>.Expression` <- function(lhs, rhs) (rhs < lhs)

`==.Expression` <- function(lhs, rhs) {
    if ( inherits(lhs, "Expression") & inherits(rhs, "Expression") ) {
        return( (lhs - rhs) == 0  )
    }
    if ( is.numeric(lhs) ) return( rhs == lhs )
    if ( is.numeric(rhs) ) {
        if( is.A_Expression(lhs) ) {
            ## TODO: What happens when A is defined?
            return( L_constraint(L=ROI::as.L_term(lhs$value), 
                                 dir="==", rhs=rhs, names=names(lhs$value)) )
        }
    }
    stop(sprintf("TODO: `<.Expression('', '')`", class(lhs)[1], class(lhs)[2]))
}

as.Fixed_constraint <- function(x, ...) UseMethod( "as.Fixed_constraint" )
as.Fixed_constraint.FIXED_constraint <- identity
as.Fixed_constraint.constraint <-function(x, ...) {
    class(x) <- c("FIXED_constraint", class(x))
    return(x)
}

rm_Fixed_constraint <- function(x) {
    class(x) <- class(x)[which("FIXED_constraint" != class(x))]
    x
}

is.Fixed_constraint <- function(x) inherits(x, what="FIXED_constraint")

## ---------------------------------------------------------
## Constraints
## ===========
## ---------------------------------------------------------
#' @export
#' @noRd
`>=.constraint` <- function(lhs, rhs) {
    dir <- ">="
    if ( inherits(lhs, "constraint") & inherits(rhs, "constraint") ) {
        stop(">=.constraint TODO: #1")
    } 
    if ( inherits(rhs, "constraint") ) {
        if ( is.Fixed_constraint(rhs) ) return( rm_Fixed_constraint(rhs) )
        tmp <- lhs
        lhs <- rhs
        rhs <- tmp
        dir <- "<="
    }
    if ( inherits(lhs, "constraint") ) {
        if ( is.Fixed_constraint(lhs) ) return( rm_Fixed_constraint(lhs) )
        lhs$dir <- rep(dir, length(lhs$dir))
        if ( is.numeric(rhs) ) {
            if (length(rhs) == 1L) {
                lhs$rhs <- rep(rhs, length(lhs$rhs))
            } else {
                lhs$rhs <- rhs
            }
            return(lhs)
        } else {
            stop("not constraint nor numeric")
        }
    }
    stop(">=.constraint TODO: #2")
}

#' @export
#' @noRd
`>.constraint` <- function(lhs, rhs) {
    dir <- ">"
    if ( inherits(lhs, "constraint") & inherits(rhs, "constraint") ) {
        stop(">.constraint TODO: #1")
    } 
    if ( inherits(rhs, "constraint") ) {
        if ( is.Fixed_constraint(rhs) ) return( rm_Fixed_constraint(rhs) )
        tmp <- lhs
        lhs <- rhs
        rhs <- tmp
        dir <- "<"
    }
    if ( inherits(lhs, "constraint") ) {
        if ( is.Fixed_constraint(lhs) ) return( rm_Fixed_constraint(lhs) )
        lhs$dir <- rep(dir, length(lhs$dir))
        if ( is.numeric(rhs) ) {
            if (length(rhs) == 1L) {
                lhs$rhs <- rep(rhs, length(lhs$rhs))
            } else {
                lhs$rhs <- rhs
            }
            return(lhs)
        } else {
            stop("not constraint nor numeric")
        }
    }
    stop(">.constraint TODO: #2")}

#' @export
#' @noRd
`<=.constraint` <- function(lhs, rhs) {
    dir <- "<="
    if ( inherits(lhs, "constraint") & inherits(rhs, "constraint") ) {
        stop("<=.constraint TODO: #1")
    } 
    if ( inherits(rhs, "constraint") ) {
        if ( is.Fixed_constraint(rhs) ) return( rm_Fixed_constraint(rhs) )
        tmp <- lhs
        lhs <- rhs
        rhs <- tmp
        dir <- ">="
    }
    if ( inherits(lhs, "constraint") ) {
        if ( is.Fixed_constraint(lhs) ) return( rm_Fixed_constraint(lhs) )
        lhs$dir <- rep(dir, length(lhs$dir))
        if ( is.numeric(rhs) ) {
            if (length(rhs) == 1L) {
                lhs$rhs <- rep(rhs, length(lhs$rhs))
            } else {
                lhs$rhs <- rhs
            }
            return(lhs)
        } else {
            stop("not constraint nor numeric")
        }
    }
    stop("<=.constraint TODO: #2")
}

#' @export
#' @noRd
`<.constraint` <- function(lhs, rhs) {
    dir <- "<"
    if ( inherits(lhs, "constraint") & inherits(rhs, "constraint") ) {
        stop("<.constraint TODO: #1")
    } 
    if ( inherits(rhs, "constraint") ) {
        if ( is.Fixed_constraint(rhs) ) return( rm_Fixed_constraint(rhs) )
        tmp <- lhs
        lhs <- rhs
        rhs <- tmp
        dir <- ">"
    }
    if ( inherits(lhs, "constraint") ) {
        if ( is.Fixed_constraint(lhs) ) return( rm_Fixed_constraint(lhs) )
        lhs$dir <- rep(dir, length(lhs$dir))
        if ( is.numeric(rhs) ) {
            if (length(rhs) == 1L) {
                lhs$rhs <- rep(rhs, length(lhs$rhs))
            } else {
                lhs$rhs <- rhs
            }
            return(lhs)
        } else {
            stop("not constraint nor numeric")
        }
    }
    stop("<.constraint TODO: #2")
}

#' @export
#' @noRd
`==.constraint` <- function(lhs, rhs) {
    dir <- "=="
    if ( inherits(lhs, "constraint") & inherits(rhs, "constraint") ) {
        stop("==.constraint TODO: #1")
    } 
    if ( inherits(rhs, "constraint") ) {
        if ( is.Fixed_constraint(rhs) ) return( rm_Fixed_constraint(rhs) )
        tmp <- lhs
        lhs <- rhs
        rhs <- tmp
    }
    if ( inherits(lhs, "constraint") ) {
        if ( is.Fixed_constraint(lhs) ) return( rm_Fixed_constraint(lhs) )
        lhs$dir <- rep(dir, length(lhs$dir))
        if ( is.numeric(rhs) ) {
            if (length(rhs) == 1L) {
                lhs$rhs <- rep(rhs, length(lhs$rhs))
            } else {
                lhs$rhs <- rhs
            }
            return(lhs)
        } else {
            stop("not constraint nor numeric")
        }
    }
    stop(".constraint TODO: #2")
}

