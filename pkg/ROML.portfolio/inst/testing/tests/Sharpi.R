q("no")
Rdevel

library(testthat)
library(slam)
library(ROI)
library(ROML)
library(ROML.plugin.portfolio)
set.seed(20160512)

parse_data_has_function <- function(parse_data, function_name) {
    function_name %in% parse_data$text[parse_data$token == "SYMBOL_FUNCTION_CALL"]
}

## usage example
## parse_data_has_function(self$objective.parse.data$data, "sharpe")
## parse_data_has_function(self$objective.parse.data$data, c("sharpe", "marko"))

marko3_constraint <- function(portfolioVar) {
    self <- ROML_get_model()
    has_sharpi_objective <- parse_data_has_function(self$objective.parse.data$data, "sharpe")
    print(as.character(self$current_constraint))
    if ( !has_sharpi_objective ) {
        print("no sharpi")
        ## do default
        return(L_constraint(L=0, dir="==", -1)) ## some dummy constraint
    } else {
        ## do something different
        print("sharpi")
        constr <- L_constraint(L=0, dir="==", -2)  ## some dummy constraint
        ## fixiere den constraint! So dass er nicht mehr Verändert wird von Operatoren
        ## Dann ändere ich die Operatoren dass wenn ein fixed Constraint kommt ich nix 
        ## überschreibe!
        class(constr) <- c("FIXED_constraint", class(constr))
        return(constr)
    }

}

ROML_register_constraint_function("marko3", marko3_constraint)

dat <- matrix(c(#  publicly available data from Markowitz (1959). Eppen, Gould and Schmidt (1991)    
                1.259       ,        1.300        ,     1.225        ,     1.149 ,                   
                1.198       ,        1.103        ,     1.290        ,     1.260 ,                   
                1.364       ,        1.216        ,     1.216        ,     1.419 ,                   
                0.919       ,        0.954        ,     0.728        ,     0.922 ,                   
                1.057       ,        0.929        ,     1.144        ,     1.169 ,                   
                1.055       ,        1.056        ,     1.107        ,     0.965 ,                   
                1.188       ,        1.038        ,     1.321        ,     1.133 ,                                       
                1.317       ,        1.089        ,     1.305        ,     1.732 ,                   
                1.240       ,        1.090        ,     1.195        ,     1.021 ,                   
                1.184       ,        1.083        ,     1.390        ,     1.131 ,                   
                0.990       ,        1.035        ,     0.928        ,     1.006 ,                   
                1.526       ,        1.176        ,     1.715        ,     1.908 ) , 
              ncol = 4, byrow = T)

## taken from p. 390, http://home.anadolu.edu.tr/~nila/PROGRAM/KILAVUZ/optimization%20modelling%20with%20lingo/Chapter13.pdf
R <- dat[, 2 : 4] - 1
m <- model()
m$variable(portfolio)
m$maximize( sharpe(portfolio, rf) )
m$subject_to( marko3(portfolio) <= 100 )
## solution <- optimize(m, solver="quadprog", data=list(portfolio = R, rf = .05))  

## xopt <- solution$solution[1:ncol(R)]/solution$solution[ncol(R) + 1]
## sd <- sqrt(drop(xopt %*% cov(R) %*% xopt))
## mu <- colMeans(R)
## obj <- (drop(mu %*% xopt) - 0.05)/sd
## expect_true(ROI::equal(xopt , c(0.1319260, 0.6503984, 0.2176757), tol=1e-02))
## expect_true(ROI::equal(obj, 0.6933179, tol=1e-02))

## Simulate Model Construction
solver <- "quadprog"
data <- list(portfolio = R, rf = 0.05)

ROML_MODEL <- m$clone()
ROML_MODEL$data <- data
obj <- ROML:::model_get_objective_functions(ROML_MODEL)

## update variables
if ( !is.null(unlist(obj$update_data, use.names=FALSE)) ) {
    update_expr <- ROML:::get_update_data_expressions(ROML_MODEL$objective.parse.data, 
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
ROML:::build_constraints(ROML_MODEL)

ROML_MODEL$constraints[[1]]$dir
ROML_MODEL$constraints[[1]]$rhs
str(ROML_MODEL$constraints)
## bis hier geht das 1 Beispiel unten siehst du dann wie man auf die constraints 
## zugreifen kann

self <- ROML_MODEL
self$objective.parse.data$has.function_call()
self$current_constraint
sapply(self$constraints, class)
CONSTRAINT_CALL <- self$constraints[[1L]]
as.character(self$constraints[[1L]])
as.list(CONSTRAINT_CALL)
as.list(CONSTRAINT_CALL)[[2]]
as.list(CONSTRAINT_CALL)[[3]]

marko3_constraint(0)

e <- expression(x + y <=  z + t)
e[[1]][[1]]
e[[1]][[2]]
e[[1]][[3]]

length(e)
