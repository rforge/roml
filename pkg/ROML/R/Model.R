##  -----------------------------------------------------------
##  model
##  =====
##' @title Create a New Optimization Model
##
##' @description A constructor of an object of type \code{"Optimization.Model"}.
##' @details Each document has the following methods:
##
##' @examples
##' m <- model()
##' @export
## -----------------------------------------------------------
model <- function(){
    Optimization.Model$new()
}

is_zero <- function(x) {
    if ( !is.numeric(x) ) return(FALSE)
    if ( length(x) != 1 ) return(FALSE)
    abs(x) < 1e-12
}

is_inf <- function(x) {
    if ( !is.numeric(x) ) return(FALSE)
    if ( length(x) != 1 ) return(FALSE)
    x == Inf
}

sanitize_lower_bounds <- function(lb, len) {
    xb <- new.env(parent=emptyenv(), size=2L)
    if ( !is_zero(lb) ) {
        xb$is_default_bound <- FALSE
        if ( !is.numeric(lb) | (!length(lb)) ) 
            stop("type missmatch, lower bounds 'lb' need to be numeric!")
        if ( length(lb) == 1L ) {
            xb$bound <- rep.int(lb, len)
        } else {
            xb$bound <- lb
        }
    } else {
        xb$is_default_bound <- TRUE
    }
    return(xb)
}

sanitize_upper_bounds <- function(ub, len) {
    xb <- new.env(parent=emptyenv(), size=2L)
    if ( !is_inf(ub) ){
        xb$is_default_bound <- FALSE
        if ( !is.numeric(ub) | (!length(ub)) )
            stop("type missmatch, upper bounds 'ub' need to be numeric!")
        if ( length(ub) == 1L ) {
            xb$bound <- rep.int(ub, len)
        } else {
            xb$bound <- ub
        }
    } else {
        xb$is_default_bound <- TRUE
    }
    return(xb)
}

sanitize_bounds <- function(variable_name, lb, ub, cb, len) {
    if ( is_zero(lb) & is_inf(ub) ) return(NULL)
    xb <- new.env(parent=emptyenv(), size=4L)
    if ( is.null(len) ) {
        xb$variable_name <- variable_name
        xb$lower <- lb
        xb$upper <- ub
        xb$conic <- "TODO!"
        class(xb) <- c("Promise", "ROML_Bound")
        return( xb )
    }
    xb$variable_name <- variable_name
    xb$lower <- sanitize_lower_bounds(lb, len)
    xb$upper <- sanitize_upper_bounds(ub, len)
    xb$conic <- "TODO!"
    class(xb) <- "ROML_Bound"
    return( xb )
}

## TODO: add a specific variable for expressions!
##' @rdname model
##' @export Optimization.Model
##' @exportClass Optimization.Model
Optimization.Model <- R6Class(
    "Optimization.Model",
    public=list(
        data=NULL,
        portable=TRUE,
        parameters=NULL,
        variables=list(),
        objective=NULL,
        constraints=list(),
        current_constraint=NULL,
        bounds=list(),
        maximum=NULL, ## c(TRUE, FALSE)
        solver=NULL,
        type=NULL, ## TODO: define model classes + short cuts (we define a list of alias) LP, QP, ...?
        objective.parse.data=NULL,
        objective.type=NULL,
        meta=list(length=NULL),
        export = function(to="ROI") to, ## TODO: write a export function.
        maximize = function(x) {
            self$objective <- substitute(x)
            self$objective.parse.data <- getParseTreeFromText(deparse(self$objective))
            ## m$objective.parse.data$has.function_call()
            self$maximum <- TRUE
        },
        minimize = function(x) {
            self$objective <- substitute(x)
            self$objective.parse.data <- getParseTreeFromText(deparse(self$objective))
            ## m$objective.parse.data$has.function_call()
            self$maximum <- FALSE
        },
        bound = function(x) {
            print("The definition of bounds via expressions is not implemented jet!")
            ## self$add_bound( substitute(x) )
        },
        variable = function(x, lb=0L, ub=Inf, cone=NULL, length=NULL, dim=NULL, type="double") {
            container <- "Vector"
            if ( !is.null(dim) ) {
                container <- "Matrix"
                length <- NULL
            }
            ## create name
            is_name <- tryCatch(is.character(x), error=function(e) FALSE)
            if (!is_name) vname <- as.character(substitute(x))
            else vname <- x
            if ( (nchar(vname) == 0) ) stop("variable name not valid")

            ## chreate bounds
            ## #TODO: Model PsdMatrices
            self$add_bound( sanitize_bounds(vname, lb, ub, cone, length), 
                            variable_name = vname )
            
            ## create variable based on length an dimension
            if ( is.null(length) ) {
                self$add_variable( Promise.Variable(vname, container=container, type=type) )
            } else {
                self$add_variable( Variable(name=vname, container=container, type=type, 
                                            dim=dim, length=length) )
            }
        },
        parameter = function(x) {
            self$parameters <- sort(union(self$parameters, paste(substitute(x))))
        },
        subject_to = function(x) {
            self$constraints <- append(self$constraints, substitute(x))
        },
        add_variable = function(x) {
            self$variables[[x$name]] <- x
        },
        add_constraint = function(x) {
            self$constraints[[length(self$constraints) + 1L]] <- x
        },
        add_bound = function(x, variable_name=NULL) {
            if ( !is.null(x) ) {
                if ( is.null(variable_name) )
                    self$bounds[[length(self$bounds) + 1L]] <- x
                else
                    self$bounds[[variable_name]] <- x
            }
        },
        get_id = function() sprintf("VARIABLE_%03i", length(self$variables) + 1L),
        update_variables=function(x) {
            if ( (!is.list(x) | is.null(names(x)) | any(nchar(LETTERS) == 0) ) ) {
                stop("update_variables expects a named list!")
            }
            for (n in names(x)) {
                self$variables[[n]] <- x[[n]]
            }
        },
        initialize = function(parameters, variables, objective, constraints, bounds) {
            if (!missing(parameters)) self$parameters <- parameters
            if (!missing(variables)) self$variables <- variables
            if (!missing(objective)) self$objective <- objective
            if (!missing(constraints)) self$constraints <- constraints
            if (!missing(bounds)) self$bounds <- bounds
        },
        print = function() {
            cat("Optimization.Model\n")
        }
    )
)

##' @noRd
##' @export
is.Optimization.Model <- function(x) {
    if ( is.null(x) ) return(FALSE)
    return( inherits(x, "Optimization.Model") )
}

