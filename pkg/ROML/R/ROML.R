##'
##' ROML - R Optimization Modeling Language
##'

## Imports
#' @import ROI
#' @import R6
#' @import slam
#' @importFrom "stats" "variable.names"
#' @importFrom "stats" "setNames"
#' @importFrom "stats" "na.omit"
#' @importFrom "stats" "terms"
#' @importFrom "graphics" "plot"

ROML <- new.env(hash=TRUE, parent=emptyenv(), size=30)
ROML$Objective_Functions  <- new.env(hash=TRUE, parent=emptyenv(), size=30)
ROML$Constraint_Functions <- new.env(hash=TRUE, parent=emptyenv(), size=30)
ROML$Objective_Data_Update_Functions <- new.env(hash=TRUE, parent=emptyenv(), size=30)
ROML$Constraint_Data_Update_Functions <- new.env(hash=TRUE, parent=emptyenv(), size=30)

ROML_assign <- function(x, value) {
    assign(x, value, envir=base::getNamespace("ROML"))
}

#  -----------------------------------------------------------
#  ROML_get
#  ========
#' @title Register Special Function
#' @param name a character string giving the name of the function
#' @param method a method to be registered
#' @noRd
#  -----------------------------------------------------------
ROML_get <- function(x=NULL) {
    if ( is.null(x) ) return( base::getNamespace("ROML") )
    return( get(x, envir=base::getNamespace("ROML")) )
}

##  -----------------------------------------------------------
##  ROML_register_objective_function
##  ================================
##' @title Register Objective Function
##' @description \code{"ROML_register_objective_function"} allows to register
##'   objective functions.
##' @param name a character string giving the name of the function
##' @param method a method to be registered
##' @export
##  -----------------------------------------------------------
ROML_register_objective_function <- function( name, method ) {
    assign(name, method, envir=ROML_get_objective_function())
}

##  -----------------------------------------------------------
##  ROML_get_objective_function
##  ===========================
##' @title Get Objective Transformation Function
##' @description Get an objective function by name.
##' @param x a character string giving the name of the function
##' @export
##  -----------------------------------------------------------
ROML_get_objective_function <- function(x=NULL) {
    if ( is.null(x) ) return( ROML_get("ROML")$Objective_Functions )
    return( ROML_get("ROML")$Objective_Functions[[x]] )
}

#  -----------------------------------------------------------
#  ROML_register_constraint_function
#  =================================
#' @title Register Special Function
#' @description TODO
#' @param name a character string giving the name of the function
#' @param method a method to be registered
#' @export
#  -----------------------------------------------------------
ROML_register_constraint_function <- function( name, method ) {
    assign(name, method, envir=ROML_get_constraint_function())
}

#  -----------------------------------------------------------
#  ROML_get_constraint_function
#  ============================
#' @title Get Objective Transformation Funtion
#' @description TODO
#' @param x a character string giving the name of the funtion
#' @details TODO
#' @examples
#' ROML_get_constraint_function("sum")
#' @export
#  -----------------------------------------------------------
ROML_get_constraint_function <- function(x=NULL) {
    if ( is.null(x) ) return( ROML_get("ROML")$Constraint_Functions )
    return( ROML_get("ROML")$Constraint_Functions[[x]] )    
}

#  -----------------------------------------------------------
#  ROML_register_objective_data_update_function
#  ============================================
#' @title Register a Generator Function
#' @description TODO
#' @param name a character string giving the name of the function
#' @param method a method to be registered
#' @export
#  -----------------------------------------------------------
ROML_register_objective_data_update_function <- function( name, method ) {
    assign(name, method, envir=ROML_get_objective_data_update_function())
}

#  -----------------------------------------------------------
#  ROML_get_objective_data_update_function
#  =======================================
#' @title Get Objective Variable Generator Funtion
#' @description TODO
#' @param x a character string giving the name of the funtion
#' @details TODO
#' @export
#  -----------------------------------------------------------
ROML_get_objective_data_update_function <- function(x=NULL) {
    if ( is.null(x) ) return( ROML_get("ROML")$Objective_Data_Update_Functions )
    return( ROML_get("ROML")$Objective_Data_Update_Functions[[x]] )    
}

#  -----------------------------------------------------------
#  ROML_get_model
#  ==============
#' @title Get the model
#' @description TODO
#' @details TODO
#' @export
ROML_get_model <- function() {
    for (i in seq(2, 10)) {
        if ( is.Optimization.Model(parent.frame(i)$ROML_MODEL) ) {
            return(parent.frame(i)$ROML_MODEL)
        }
    }
}
