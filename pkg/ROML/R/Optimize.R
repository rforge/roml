
model_get_objective_functions <- function(model) {
    if ( !model$objective.parse.data$has.function_call() ) return(NULL)
    b <- model$objective.parse.data$data$token.num %in% 296L
    funs <- model$objective.parse.data$data$text[b]
    ## check if all defined
    b <- funs %in% ls(ROML_get_objective_function())
    if ( !all(b) ) {
        stop("objective function ", paste(shQuote(funs[!b]), collapse=", "), " not found")
    }

    list(functions=setNames(lapply(funs, ROML_get_objective_function), funs),
         update_data=setNames(lapply(funs, ROML_get_objective_data_update_function), funs) )
}

## #TODO: put this into the model!
model_get_constraint_functions <- function(constraint) {
    get.parse_data <- function(x) get_parse_data(getParseDataFromText(deparse(x)))
    parse_data <- do.call(rbind, lapply(constraint, get.parse_data))
    b <- parse_data$token.num %in% 296L
    funs <- parse_data$text[b]
    b <- funs %in% ls(ROML_get_constraint_function())
    if ( !all(b) ) {
        stop("constraint function ", paste(shQuote(funs[!b]), collapse=", "), " not found")
    }
    setNames(lapply(funs, ROML_get_constraint_function), funs)
}

get.parse_data <- function(x) get_parse_data(getParseDataFromText(deparse(x)))

build_constraints <- function(model) {
    if (!length(model$constraints)) return(NULL)
    m_constraints <- model$constraints
    model$constraints <- list()
    con_fun <- as.list(ROML_get_constraint_function())
    ##model_expr <- lapply(model$variables, as.expr)
    for (i in seq_along(m_constraints)) {
        if ( inherits(m_constraints[[i]], "constraint") ) {
            model$add_constraint(m_constraints[[i]])
        } else if ( is.call(m_constraints[[i]]) ) {
            model$current_constraint <- m_constraints[[i]]
            constr <- eval(m_constraints[[i]], c(model$variables, con_fun))
            model$current_constraint <- NULL
            if ( inherits(constr, "constraint") ) {
                model$add_constraint(constr)
            } else {
                stop("build_constraints TODO!")
            }
        } else {
            stop("wrong data type")
        }
    }
    return(NULL)
}

type_mapping <- setNames(c("B", "I", "C", "B", "I", "C"), 
                         c("B", "I", "C", "logical", "integer", "double"))

build_types <- function(model, var_names) {
    types <- list()
    for (i in seq_along(model$variables)) {
        type <- model$variables[[i]]$type
        if ( !is.null(type) ) {
            if ( !all(type %in% names(type_mapping)) )
                stop("given type is not valid!")
            len <- length(model$variables[[i]]$value)
            ## one type for the entire variable
            if ( length(type) == 1 ) {
                types <- append(types, 
                    list(setNames(rep.int(unname(type_mapping[type]), len), 
                                  names(model$variables[[i]]$value))))
            } else if ( length(type) == len ) {
                types <- append(types, 
                    list(setNames(unname(type_mapping[type]), 
                                  names(model$variables[[i]]$value))))
            } else {
                stop("length of types has to match the variable length!")
            }
        }
    }
    types <- unlist(types)[var_names]
    types[is.na(types)] <- "C"
    types
}

build_bounds <- function(model) {
    if ( !length(model$bounds) ) return(NULL)
    bo <- list()
    bo$li <- integer()
    bo$ui <- integer()
    bo$lb <- numeric()
    bo$ub <- numeric()
    cum_len <- 0L
    for (i in seq_along( model$variables )) {
        vname <- model$variables[[i]]$name
        len <- model$variables[[i]]$length
        if ( inherits(model$bounds[[vname]], "ROML_Bound") ) {
            mb <- model$bounds[[vname]]
            if ( is.Promise( mb ) ) {
                mb <- sanitize_bounds(vname, mb$lower, mb$upper, mb$conic, len)
            }
            if ( !mb$lower$is_default_bound ) {
                li <- cum_len + seq_along(mb$lower$bound)
                bo$li <- c(bo$li, li)
                bo$lb <- c(bo$lb, mb$lower$bound)
            }
            if ( !mb$upper$is_default_bound ) {
                ui <- cum_len + seq_along(mb[[i]]$upper$bound)
                bo$li <- c(bo$ui, ui)
                bo$lb <- c(bo$ub, mb$upper$bound)
            }
        }
        cum_len <- cum_len + len
    }
    return( V_bound(li=bo$li, ui=bo$ui, lb=bo$lb, ub=bo$ub, cum_len) )
}

build_names <- function(x) sprintf("%s$%i", x$name, seq_len(x$length))

## this returns
## # TODO: write tests for this!
get_update_data_expressions <- function(parse_data, update_names) {
    expr <- list()
    pd <- parse_data$data
    pd <- pd[pd$text != "",]
    i <- 1L
    for (i in seq_along(update_names)) {
        for (j in which((pd$text == update_names[i]) & 
                        (pd$token == "SYMBOL_FUNCTION_CALL")) ) {
            pd$counter <- 0L
            pd$counter[pd$text == "("] <-  1L
            pd$counter[pd$text == ")"] <- -1L
            pd$counter[ seq_along(pd$counter) < j ] <- 0
            cs <- cumsum(pd$counter)
            w <- which(seq_along(pd$counter) > j)
            k <- w[min(which(cs[w] == 0L))]
            txt <- paste(pd$text[seq.int(from=j, to=k)], collapse="")
            expr <- append(expr, list(parse(text=txt)))
        }
    }
    return(expr)
}

#  -----------------------------------------------------------
#  optimize
#  ========
#' @title optimize
#'
#' @description optimize a given model.
#' @param model an object of type \code{"Optimization.Model"}.
#' @param solver a character string giving the name of the solver.
#' @param data a list giving the data used in the the model.
#' @export
# -----------------------------------------------------------
optimize <- function(model, solver="", data=list()) {
    ROML_MODEL <- model$clone()
    ROML_MODEL$data <- data
    obj <- model_get_objective_functions(ROML_MODEL)

    ## update variables
    if ( !is.null(unlist(obj$update_data, use.names=FALSE)) ) {
        update_expr <- get_update_data_expressions(ROML_MODEL$objective.parse.data, 
                                                   names(obj$update_data))
        for ( i in seq_along(update_expr) ) {
            upd_var <- eval(update_expr[[i]], envir=c(obj$update_data, data))
            ROML_MODEL$update_variables(upd_var)
        }
    }
    i <- which(!names(data) %in% names(ROML_MODEL$variables))

    ## check if a varibale is still a promise if so throw error!
    if ( any(sapply(ROML_MODEL$variables, inherits, what="Promise")) )
        stop("unevaluated promise!")

    ## build objective
    objective <- eval(ROML_MODEL$objective, 
                      envir=c(obj$functions, ROML_MODEL$variables, data[i]))
    ## build constraints    
    build_constraints(ROML_MODEL)

    var_names <- unlist(lapply(ROML_MODEL$variables, variable.names), use.names=FALSE)
    if ( length(ROML_MODEL$constraints) ) {
        ##constraints <- c(list(zero_L_constraint(length(var_names), var_names)), constraints)
        constraints <- c(list(zero_L_constraint(length(var_names), var_names)), ROML_MODEL$constraints)
        constraints <- ROI::rbind.constraint(constraints, use.names=TRUE, recursive=TRUE)
    }
    
    ## build types   
    types <- build_types(ROML_MODEL, var_names)

    if ( !inherits(objective, "objective") ) { 
        if ( !all(names(objective$value) %in% var_names) ) stop("not all var_names registered!")
        obj <- setNames(numeric(length(var_names)), var_names)
        obj[names(objective$value)] <- unname(objective$value)
        objective <- L_objective(obj)
    } 
    ## fill objective with zeros if necessary
    if ( !is.null(constraints) & (length(objective) < NCOL(constraints)) & inherits(objective, "Q_objective") ) {
        ## TODO: extend for F_constraints
        objective <- fill_zero(objective, ncol=ncol(constraints))
    }
    
    bounds <- build_bounds(ROML_MODEL)

    ## FIXME:
    ## - add bounds
    ## build ROI-Model

    roi_model <- ROI::OP(objective = objective, constraints = constraints,
                         types = types, bounds = bounds, maximum=ROML_MODEL$maximum)
    ## if (only_model) return(roi_model)
    solution <- ROI_solve(x=roi_model, solver=solver)
    attributes(solution$solution)$names <- var_names
    attributes(solution)$model <- roi_model
    return( solution )
}
