## Imports
#' @import ROI
#' @import ROML

##  ----------------------------------------------------------------------------
##
##  Objectives
##  ==========
##
##  ----------------------------------------------------------------------------

##  --------------------------
##  Return of investment of portfolioVar
##  ====================
##  --------------------------
reward_objective <- function( portfolioVar ) {
    L_objective(portfolioVar$data$mu)
}

##  --------------------------
##  Markowitz
##  =========
##  -------------------------- 
markowitz_objective <- function( portfolioVar ) {
    self <- ROML_get_model()
    Q_objective(Q = 2 * portfolioVar$data$Sigma, 
        L = slam::simple_triplet_zero_matrix(nrow=1, 
            ncol=ncol(portfolioVar$data$Sigma)))
}
##  --------------------------
##  Downside SD
##  =========
##  -------------------------- 
downside_var_objective <- function( portfolioVar, tau = NULL ) {
    self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    N <- NCOL(portfolioVar$data$returns)
    S <- NROW(portfolioVar$data$returns)
    if (is.null(tau)) tau <- portfolioVar$data$mu
    mu <- portfolioVar$data$mu
    Amat <-  cbind(sweep(portfolioVar$data$returns, 2, mu), 
        diag(S))
    vname2 <- self$get_id()
    self$variable(vname2, length=S)
    self$add_constraint(L_constraint(L=Amat, 
        dir=rep(">=", S), 
        rhs=rep(0, S), 
        names=c(names(portfolioVar$value), 
                names(self$variables[[vname2]]$value))))
    ## TODO: slow
    Q_objective(
             Q = 2 * simple_triplet_diag_matrix(c(rep(1e-05, N), 
                rep(1/S, S))),
             L = rep(0, N + S))
}
##  --------------------------
##  MAD
##  ===
##  --------------------------
## see "Optimization Methods in Finance" p. 154
## (Gerald Curnuejols and Reha Tutuncu) 
## minimize_{x,y,z} 1/S sum(y_s + z_s)
## st.
## y_s - z_s = sum(r_{it}-\bar r_i)x_i, i=1,..,N
## 0<=x_i<=m_i (Box constraints)
## y_s >= 0, z_s>= 0
## variables x_1, ... x_N, y_1,..y_S, z_1,..z_S
mad_objective <- function( portfolioVar ){
    self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    N <- ncol(portfolioVar$data$returns)
    S <- nrow(portfolioVar$data$returns)
    mu <- portfolioVar$data$mu
    Amat <-  cbind(sweep(portfolioVar$data$returns, 2,  mu), 
        - diag(S), diag(S))
    vname2 <- self$get_id()
    self$variable(vname2, length=S)
    vname3 <- self$get_id()
    self$variable(vname3, length=S)
    self$add_constraint(L_constraint(L=Amat, dir=rep("==", S), rhs=rep(0, S), 
                                      names=c(names(portfolioVar$value), 
                                        names(self$variables[[vname2]]$value),
                                        names(self$variables[[vname3]]$value))) )
    L_objective( L = c(rep(0, N), rep(1/S, 2 * S)))
      
}
##  --------------------------
##  Downside MAD
##  ============
##  --------------------------
downside_mad_objective <- function( portfolioVar){
    self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    N <- NCOL(portfolioVar$data$returns)
    S <- NROW(portfolioVar$data$returns)
    mu <- portfolioVar$data$mu
    Amat <-  cbind(sweep(portfolioVar$data$returns, 2, mu), diag(S))
    vname2 <- self$get_id()
    self$variable(vname2, length=S)
    self$add_constraint(L_constraint(L=Amat, dir=rep(">=", S), rhs=rep(0, S), 
                                      names=c(names(portfolioVar$value), 
                                        names(self$variables[[vname2]]$value))))
    L_objective( L = c(rep(0, N), rep(1/S,  S)))
      
}
##  --------------------------
##  CVaR
##  ====
##  --------------------------
## following specification in OIMF (p.275)
## objective is minimize: 1/(1 - alpha) * sum(p_s * z_s) + gamma, where p_s is 1/S 
## z_s = (f(x, y_s) - gamma)^+
## with f(x, y_s) = - sum(r_is * x_i)
## constraints
## z_s >= 0, gamma in R
## z_s >= f(x, y_s) - gamma =>   z_s + gamma + sum(r_is * x_i) >= 0
## variables are (x_1, ..., x_N, z_1, ..., z_S, gamma)
cvar_objective <- function( portfolioVar, alpha, probs = NULL) {
    self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    N <- NCOL(portfolioVar$data$returns)
    S <- NROW(portfolioVar$data$returns)
    if (is.null(probs)) probs <- rep(1/S, S)
    if (alpha < 0.5) alpha <- 1 - alpha
    Amat <- cbind(portfolioVar$data$returns, diag(S), 1)
    vname2 <- self$get_id()
    self$variable(vname2, length= S )
    self$variable("gamma", length=1 )
    bnds <- ROI::V_bound(li = c(N + S + 1), lb = c( -Inf),
                         ui = c(N + S + 1), ub = c(  Inf))
    self$add_constraint( L_constraint(L=Amat, dir=rep(">=", S), rhs=rep(0, S), 
                                      names=c(names(portfolioVar$value), 
                                        names(self$variables[[vname2]]$value),
                                        names(self$variables[["gamma"]]$value))))
    self$add_bound(bnds)
    L_objective(c(rep(0, N), probs/(1 - alpha), 1))
}
##  --------------------------
##  Minimax Young
##  ==============
##  --------------------------
## maximizes the minimum gain
## 
## max_(M_p, x) M_p
## with M_p = min_t r_pt, r_pt = sum_{i=1}^N (x_i * r_it)
## s.t. sum_{i=1}^N(x_i * r_it) - M_p >= 0
## sum_{i=1}^N x_i  \bar r_i >= G
## sum_{i=1}^N x_i <= W
## x_i >= 0
## 
## Equivalent formulation:
## 
##  max_x E = sum_{i=1}^N x_i \bar r_i
## s.t. sum_{i=1}^N (x_i * r_it) >= H
##  sum_{i=1}^N x_i <= W
##  x_i >= 0
##
minimax_young_objective <- function( portfolioVar ){
     self <- ROML_get_model()
     vname <- as.character(substitute(portfolioVar))
     mp <- self$get_id()
     self$variable(mp, length = 1)
     N <- NCOL(portfolioVar$data$returns)
     S <- NROW(portfolioVar$data$returns) 
     self$add_constraint(L_constraint(
        L= cbind(portfolioVar$data$returns, -1), 
        dir=rep(">=", S), 
        rhs=rep(0, S), 
        names=c(names(portfolioVar$value), 
                names(self$variables[[mp]]$value))))
     bnds <- ROI::V_bound(li = c(N + 1), lb = c( -Inf),
                          ui = c(N + 1), ub = c(  Inf))
     self$add_bound(bnds)
     L_objective(c(rep(0, N), 1))
}

##  --------------------------
##  Quadratic Utility
##  =================
##  --------------------------
## max x * mu - lambda/2 x^T Q x, 
## lambda risk aversion parameter
quadratic_utility_objective <- function( portfolioVar, lambda ){
    self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    Q_objective(Q = - lambda * portfolioVar$data$Sigma, 
        L = portfolioVar$data$mu)
}
##  --------------------------
##  Sharpe Ratio
##  =================
##  --------------------------
sharpe_objective <- function(portfolioVar, rf = 0){
    ## variables y_1, ... y_N, kappa
    self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    N <- NCOL(portfolioVar$data[[1]])
    S <- NROW(portfolioVar$data[[1]])
    mu <- portfolioVar$data$mu 
    self$variable("kappa_sharpe", length = 1)
    Amat <- rbind(c(mu - rf, 0),
                  c(rep(0, N), 1),
                  c(rep(1, N), -1)
        )
    self$add_constraint( L_constraint(L=Amat, 
        dir=c("==", ">", "=="), rhs=c(1,0,0), 
        names=c(names(portfolioVar$value), 
                names(self$variables[["kappa_sharpe"]]$value))) )
    mat <- simple_triplet_zero_matrix(ncol = N + 1, nrow = N + 1)
    mat[1:N, 1:N] <- 2 * portfolioVar$data$Sigma
    mat[N + 1, N + 1] <-  1e-05
    Q_objective(Q = - mat, 
                L = c(rep(0, N), 0))
    ## x* <- y*/(sum(y*))
}

##  --------------------------
##  Omega
##  =================
##  --------------------------
omega_objective <- function(portfolioVar, tau = 0){
    ## variables y_1, ... y_N, u_1, .., u_S, z
    self <- ROML_get_model()
    vname <- as.character(substitute(portfolioVar))
    N <- NCOL(portfolioVar$data$returns)
    S <- NROW(portfolioVar$data$returns)
    mu <- portfolioVar$data$mu 
    vname2 <- self$get_id() 
    self$variable(vname2, length = S) # u
    self$variable("z_omega", length = 1)
    Amat <- rbind(cbind(portfolioVar$data$returns, diag(S), 0),  # u_s >=  tau - r_s' y
                  c(rep(0, N), rep(1, S), 0), # sum(u) = 1
                  c(rep(1, N), rep(0, S), -1), # sum(y) = z
                  c(mu, rep(0,S), - tau),
                  c(rep(0, N), rep(0, S), 1)) # bar r'y  >= tau * z
    self$add_constraint( L_constraint(L=Amat, 
        dir=c(rep(">=", S), "==", "==", ">=", ">"), 
        rhs=c(rep(tau, S), 1, 0, 0, 1e-05), 
        names=c(names(portfolioVar$value), 
                names(self$variables[[vname2]]$value),
                names(self$variables[["z_omega"]]$value))) )
    L_objective(L = c(mu, rep(0, S), -tau))
    ## x* <- y*/z*
}
