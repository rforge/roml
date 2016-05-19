##  ----------------------------------------------------------------------------
##
##  Constraints
##  ===========
##
##  #NOTE: dir and rhs is the default value and will be overwritten later if 
##         necessary.
##         e.g.
##            m$subject_to(full_investment)      --> L = rep(1, NCOL(R$data)), dir = "==",  rhs = 1)
##            m$subject_to(full_investment <= 3) --> L = rep(1, NCOL(R$data)), dir = "<=",  rhs = 3)
##
##  ----------------------------------------------------------------------------

parse_data_has_function <- function(parse_data, function_name) {
    ## function to check objective
    function_name %in% parse_data$text[parse_data$token == "SYMBOL_FUNCTION_CALL"]
}

##  --------------------------
##  Return/reward of portfolio
##  ==========================
##  --------------------------
reward_constraint <- function(portfolioVar) {
    self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    has_sharpe_objective <- parse_data_has_function(self$objective.parse.data$data, "sharpe")
    if (!has_sharpe_objective){
        return(L_constraint(portfolioVar$data$mu,  dir=">", rhs=0, 
        names=names(portfolioVar$value)))
    } else {
        extra.constr <- (as.character(self$current_constraint))
        extra.constr.dir <- extra.constr[1]
        extra.constr.rhs <- as.numeric(extra.constr[3])
        constr <- L_constraint(
                            L = c(portfolioVar$data$mu,  - extra.constr.rhs ),
                                   dir = extra.constr.dir, rhs = 0, 
                                   names=c(names(self$variables[[vname]]$value), 
                                           names(self$variables[["kappa_sharpe"]]$value)))
        class(constr) <- c("FIXED_constraint", class(constr))
        return(constr)
    }
}

##  --------------
##  Markowitz
##  =========
##  --------------
## needs cplex or mosek for Q constraints 
markowitz_constraint <- function(portfolioVar) {
    self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    N <- NCOL(portfolioVar$data$Sigma)
    has_sharpe_objective <- parse_data_has_function(self$objective.parse.data$data, "sharpe")
    has_omega_objective <- parse_data_has_function(self$objective.parse.data$data, "omega")
    if (!has_omega_objective & !has_sharpe_objective){
        return(Q_constraint(Q = 2 * portfolioVar$data$Sigma, 
        L = slam::simple_triplet_zero_matrix(nrow=1, 
            ncol=NCOL(portfolioVar$data$Sigma)), 
        dir = "<", rhs = 1000, names=names(portfolioVar$value)))
    } else {
        extra.constr <- (as.character(self$current_constraint))
        extra.constr.dir <- extra.constr[1]
        extra.constr.rhs <- as.numeric(extra.constr[3])
        Q1 <- simple_triplet_matrix(N + 1, N + 1, extra.constr.rhs)
        Q1[1:N, 1:N] <- portfolioVar$data$Sigma
        if (has_sharpe_objective){
            constr <- Q_constraint(Q = 2 * Q1, 
                                   L = slam::simple_triplet_zero_matrix(nrow=1, 
                                                          ncol=NCOL(portfolioVar$data$Sigma) + 1),
                                   dir = extra.constr.dir, rhs = 0, 
                                   names=c(names(self$variables[[vname]]$value), 
                                           names(self$variables[["kappa_sharpe"]]$value)))
        }
        if (has_omega_objective){
            constr <- Q_constraint(Q = 2 * Q1, 
                                   L = slam::simple_triplet_zero_matrix(nrow=1, 
                                                          ncol=NCOL(portfolioVar$data$Sigma) + 1),
                                   dir = extra.constr.dir, rhs = 0, 
                                   names=c(names(self$variables[[vname]]$value), 
                                           names(self$variables[["z_omega"]]$value)))
        }
        class(constr) <- c("FIXED_constraint", class(constr))
        return(constr)
    }

}

##  --------------------------
##  sum of weights
##  ===============
##  --------------------------
sum_1_constraint_fixed <- function(portfolioVar) {
    constr <- L_constraint(L = rep(1, NCOL(portfolioVar$data[[1]])), 
        dir = "==",  rhs = 1, 
    	names=names(portfolioVar$value))
    class(constr) <- c("FIXED_constraint", class(constr))
    return(constr)
}
sum_constraint <- function(portfolioVar) {
    L_constraint(L = rep(1, NCOL(portfolioVar$data[[1]])), 
        dir = "==",  rhs = 1, 
        names=names(portfolioVar$value))
}
##  --------------------------
##  cardinality
##  ===============
##  --------------------------
cardinality_constraint <- function(portfolioVar) {
	self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    N <- NCOL(portfolioVar$data[[1]])
	vname2 <- self$get_id()
	self$variable(vname2, length = N, type = "logical")
    Amat <- cbind(diag(N), -diag(N))
    self$add_constraint( L_constraint(L=Amat, dir=rep("<=", N), rhs=rep(0, N), 
                                      names=c(names(portfolioVar$value), 
                                      names(self$variables[[vname2]]$value))) )
    L_constraint(L = c(rep(0, N), rep(1, N)), 
        dir = "==",  rhs = N , 
    	names=c(names(portfolioVar$value), 
                names(self$variables[[vname2]]$value)))
}
#cardinality_long_constraint <- function(portfolioVar) {
#    self <- ROML_get_model()
#    vname <- as.character(substitute(portfolioVar))
#    N <- NCOL(portfolioVar$data[[1]])
#    vname2 <- self$get_id()
#    self$variable(vname2, length = N, type = "logical")
#    Amat <- cbind(diag(N), -diag(N))
#    self$add_constraint( L_constraint(L=Amat, dir=rep("<=", N), rhs=rep(0, N), 
#                                      names=c(names(portfolioVar$value), 
#                                      names(self$variables[[vname2]]$value))) )
#    L_constraint(L = c(rep(0, N), rep(1, N)), 
#        dir = "==",  rhs = N , 
#        names=c(names(portfolioVar$value), 
#                names(self$variables[[vname2]]$value)))
#}
#cardinality_short_constraint <- function(portfolioVar) {
#    self <- ROML_get_model()
#    vname <- as.character(substitute(portfolioVar))
#    N <- NCOL(portfolioVar$data[[1]])
#    vname2 <- self$get_id()
#    self$variable(vname2, length = N, type = "logical")
#    Amat <- cbind(diag(N), -diag(N))
#    self$add_constraint( L_constraint(L=Amat, dir=rep("<=", N), rhs=rep(0, N), 
#                                      names=c(names(portfolioVar$value), 
#                                      names(self$variables[[vname2]]$value))) )
#    L_constraint(L = c(rep(0, N), rep(1, N)), 
#        dir = "==",  rhs = N , 
#       names=c(names(portfolioVar$value), 
#                names(self$variables[[vname2]]$value)))
#}
##  --------------------------
##  CVaR
##  ===============
##  --------------------------
cvar_constraint <- function(portfolioVar, alpha, probs = NULL) {
    self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    N <- NCOL(portfolioVar$data$returns)
    S <- NROW(portfolioVar$data$returns)
    if (alpha < 0.5) alpha <- 1 - alpha
    if (is.null(probs)) probs <- rep(1/S, S)
    Amat <- cbind(portfolioVar$data$returns, diag(S), 1)
    vname2 <- self$get_id()
    self$variable(vname2, length= S + 1)
    bnds <- ROI::V_bound(li = c(N + S + 1), lb = c( -Inf),
                         ui = c(N + S + 1), ub = c(  Inf))
    self$add_bound(bnds)
    self$add_constraint(L_constraint(L=Amat, dir=rep(">=", S), rhs=rep(0, S), 
                                      names=c(names(portfolioVar$value), 
                                        names(self$variables[[vname2]]$value))) )

    has_omega_objective <- parse_data_has_function(self$objective.parse.data$data, "omega")
    has_sharpe_objective <- parse_data_has_function(self$objective.parse.data$data, "sharpe")
    if (!has_omega_objective & !has_sharpe_objective){
        return(L_constraint(c(rep(0, N), rep(1 / ((1 - alpha) * S), S), 1), 
                            dir = "<", rhs = 100, 
                            names=c(names(portfolioVar$value), 
                                    names(self$variables[[vname2]]$value))))
    } else {
        extra.constr <- (as.character(self$current_constraint))
        extra.constr.dir <- extra.constr[1]
        extra.constr.rhs <- as.numeric(extra.constr[3])
        if (has_omega_objective){
            constr <- L_constraint(c(rep(0, N), rep(1 / ((1 - alpha) * S), S), 1, 
                                   - extra.constr.rhs), 
                            dir = extra.constr.dir, rhs = 0, 
                            names=c(names(self$variables[[vname]]$value), 
                                    names(self$variables[[vname2]]$value), 
                                    names(self$variables[["z_omega"]]$value)))
        } 
        if (has_sharpe_objective){
            constr <- L_constraint(c(rep(0, N), rep(1 / ((1 - alpha) * S), S), 1, 
                                   - extra.constr.rhs), 
                            dir = extra.constr.dir, rhs = 0, 
                            names=c(names(self$variables[[vname]]$value), 
                                    names(self$variables[[vname2]]$value), 
                                    names(self$variables[["kappa_sharpe"]]$value)))
        } 
        class(constr) <- c("FIXED_constraint", class(constr))
        return(constr)
    }
}

##  --------------------------
##  Value at risk
##  ===============
##  --------------------------
#value_at_risk_constraint <- function(portfolioVar, alpha) {
#    self <- ROML_get_model()
#    vname <- as.character(substitute(portfolioVar))
#    N <- ncol(portfolioVar$data)
#    S <- nrow(portfolioVar$data)
#    vname2 <- self$get_id()
#    self$variable(vname2, length=S + 1)
#    bnds <- ROI::V_bound(li = c(N + S + 1), lb = c( -Inf),
#                         ui = c(N + S + 1), ub = c(  Inf))
#    L_constraint(c(rep(0, N), rep(0, S), 1), dir = "<", rhs = 100, 
#        names=c(names(portfolioVar$value), 
#                                        names(self$variables[[vname2]]$value)))
#}
##  --------------------------
##  Turnover
##  ===============
##  --------------------------
## sum_i |x_i - x_i0| <= turnover target
## y_i^+ - y_i^- = x_i - x_i0
## y_i^+, y_i^ >=0
## sum(y_i^+ + y_i^-) <= turnover target
## variables x_1..x_N, y+_1, .. y+_N, y-_1,..., y-_N
turnover_constraint <- function(portfolioVar, x0 = NULL) {
    ## x0 is a vector of initial weights
    self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    N <- NCOL(portfolioVar$data[[1]])
    if (is.null(x0)) x0 <- rep(1/N, N)
    vname2 <- self$get_id() 
    self$variable(vname2, length= 2 * N)
    Amat <- cbind(diag(N), - diag(N), diag(N))
    has_omega_objective <- parse_data_has_function(self$objective.parse.data$data, "omega")
    if (!has_omega_objective){
        self$add_constraint( L_constraint(L=Amat, dir=rep("==", N), rhs=x0, 
                            names=c(names(portfolioVar$value), 
                                    names(self$variables[[vname2]]$value))) )
        return(L_constraint(c(rep(0, N), rep(1, N), rep(1, N) ), dir = "<=", rhs = 1, 
                                names=c(names(portfolioVar$value), 
                                        names(self$variables[[vname2]]$value))))   


    } else {
        ## TODO: if constraint turnover(portfolio) <= 0.5 + portfolio[1]
        ## for now extra.constr[3] must be rhs
        extra.constr <- (as.character(self$current_constraint))
        extra.constr.dir <- extra.constr[1]
        extra.constr.rhs <- as.numeric(extra.constr[3])
        self$add_constraint( L_constraint(L=cbind(Amat, -x0), 
                                          dir=rep("==", N), rhs=rep(0,N), 
                                          names=c(names(portfolioVar$value), 
                                                  names(self$variables[[vname2]]$value),
                                                  names(self$variables[["z_omega"]]$value))) )
        constr <- L_constraint(
            L = c(rep(0, N), rep(1, N), rep(1, N), -extra.constr.rhs),
            dir = extra.constr.dir,
            rhs = 0,
            names=c(names(portfolioVar$value), 
                    names(self$variables[[vname2]]$value),
                    names(self$variables[["z_omega"]]$value)))
        class(constr) <- c("FIXED_constraint", class(constr))
        return(constr)
    }
}

##  --------------------------------
##  TODO : Proportional transaction costs
##  ==============================
##  --------------------------------
