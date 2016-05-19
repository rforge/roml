##  ----------------------------------------------------------------------------
##
##  Data Update Functions
##  =====================
##
##  The data update is done in the optimize function (of ROML). It is executed
##  before the the objective and the constraint function.
##
##  # TODO: 
##    - Export the Vector.Variable in ROML.
##
##  ----------------------------------------------------------------------------

##  --------------------------
##  Markowitz
##  =========
##  --------------------------
markowitz_update_data <- function( R) {
    vname <- as.character(substitute(R))
    self <- ROML_get_model()
    R <- self$data
    if (is.null(R$Sigma) & is.null(R$returns)) stop("Needs covariance matrix ...")
    if (!is.null(R$mu)) R$mu <- matrix(R$mu, nrow = 1)
    if (is.null(R$mu) & !is.null(R$returns)) 
        R$mu <- matrix(colMeans(R$returns), nrow = 1)
    if (is.null(R$Sigma) & !is.null(R$returns)) 
        R$Sigma <- cov(R$returns)
    setNames(list(ROML::Vector.Variable(vname, 
        type="double", length=NCOL(R$Sigma), data=R)), vname)
    setNames(list(ROML::Vector.Variable(vname, 
        type="double", length=NCOL(R[[1]]), data=R)), vname)
}
##  --------------------------
##  Downside variance
##  =================
##  --------------------------
downside_var_update_data <- function( R) {
    vname <- as.character(substitute(R))
    self <- ROML_get_model()
    R <- self$data
    if (is.null(R$returns)) stop("Needs returns (scenario set). Maybe sample one? ...")
    if (!is.null(R$mu)) R$mu <- matrix(R$mu, nrow = 1)
    else  R$mu <- matrix(colMeans(R$returns), nrow = 1)
    if (is.null(R$Sigma) & !is.null(R$returns)) 
        R$Sigma <- cov(R$returns)
    setNames(list(ROML::Vector.Variable(vname, type="double", 
        length=NCOL(R[[1]]), data=R)), vname)
}
##  ----------------------------
##  Return / reward of portfolio
##  ============================
##  ----------------------------
reward_update_data <- function( R ) {
    vname <- as.character(substitute(R))
    self <- ROML_get_model()
    R <- self$data
    if (is.null(R$mu) & is.null(R$returns)) stop("Needs mean vector ...")
    if (!is.null(R$mu)) R$mu <- matrix(R$mu, nrow = 1)
    if (is.null(R$mu) & !is.null(R$returns)) 
        R$mu <- matrix(colMeans(R$returns), nrow = 1)
    if (is.null(R$Sigma) & !is.null(R$returns)) 
        R$Sigma <- cov(R$returns)
    setNames(list(ROML::Vector.Variable(vname, 
        type="double", length=NCOL(R$mu), data=R)), vname)
    #vname <- as.character(substitute(R))
    #setNames(list(ROML::Vector.Variable(vname, type="double", length=ncol(R), data=R)), vname)
}

##  --------------------------
##  CVaR
##  ====
##  --------------------------
cvar_update_data <- function( R, alpha) {
    vname <- as.character(substitute(R))
    self <- ROML_get_model()
    R <- self$data
    if (is.null(R$returns)) stop("Needs returns (scenario set). Maybe sample one? ...")
    if (!is.null(R$mu)) R$mu <- matrix(R$mu, nrow = 1)
    if (is.null(R$mu) & !is.null(R$returns)) 
        R$mu <- matrix(colMeans(R$returns), nrow = 1)
    if (is.null(R$Sigma) & !is.null(R$returns)) 
        R$Sigma <- cov(R$returns)
    setNames(list(ROML::Vector.Variable(vname, 
        type="double", length=NCOL(R$mu), data=R)), vname)
}
##  --------------------------
##  MAD
##  ===
##  --------------------------
mad_update_data <- function( R ) {
    vname <- as.character(substitute(R))
    self <- ROML_get_model()
    R <- self$data
    if (is.null(R$returns)) stop("Needs returns (scenario set). Maybe sample one?...")
    if (!is.null(R$mu)) R$mu <- matrix(R$mu, nrow = 1)
    else  R$mu <- matrix(colMeans(R$returns), nrow = 1)
    if (is.null(R$Sigma)) R$Sigma <- cov(R$returns)
    setNames(list(ROML::Vector.Variable(vname, 
        type="double", length=NCOL(R$mu), data=R)), vname)
}
##  --------------------------
##  Downside MAD
##  ============
##  --------------------------
downside_mad_update_data <- function( R ) {
    vname <- as.character(substitute(R))
    self <- ROML_get_model()
    R <- self$data
    if (is.null(R$returns)) stop("Needs returns (scenario set). Maybe sample one? ...")
    if (!is.null(R$mu)) R$mu <- matrix(R$mu, nrow = 1)
    else  R$mu <- matrix(colMeans(R$returns), nrow = 1)
    if (is.null(R$Sigma)) R$Sigma <- cov(R$returns)
    setNames(list(ROML::Vector.Variable(vname, 
        type="double", length=NCOL(R$mu), data=R)), vname)
}
##  --------------------------
##  Minimax Young
##  =============
##  --------------------------
minimax_young_update_data <- function( R ) {
    vname <- as.character(substitute(R))
    self <- ROML_get_model()
    R <- self$data
    if (is.null(R$returns)) stop("Needs scenario set. Maybe sample one? ...")
    if (!is.null(R$mu)) R$mu <- matrix(R$mu, nrow = 1)
    else  R$mu <- matrix(colMeans(R$returns), nrow = 1)
    if (is.null(R$Sigma)) R$Sigma <- cov(R$returns)
    setNames(list(ROML::Vector.Variable(vname, 
        type="double", length=NCOL(R$mu), data=R)), vname)
}

##  --------------------------
##  Quadratic Utility
##  =============
##  --------------------------
quadratic_utility_update_data <- function( R , lambda) {
    vname <- as.character(substitute(R))
    self <- ROML_get_model()
    R <- self$data
    if (is.null(R$Sigma) & is.null(R$returns)) 
        stop("Need scenario set or covariance matrix...")
    if (is.null(R$mu) & is.null(R$returns)) 
        stop("Need scenario set or mean vector...")
    if (!is.null(R$mu)) R$mu <- matrix(R$mu, nrow = 1)
    else  R$mu <- matrix(colMeans(R$returns), nrow = 1)
    if (is.null(R$Sigma)) R$Sigma <- cov(R$returns)
    setNames(list(ROML::Vector.Variable(vname, 
        type="double", length=NCOL(R$mu), data=R)), vname)
}
##  --------------------------
##  Sharpe ratio
##  =============
##  --------------------------
sharpe_update_data <- function( R , rf = 0) {
    vname <- as.character(substitute(R))
    self <- ROML_get_model()
    R <- self$data
    if (is.null(R$Sigma) & is.null(R$returns)) 
        stop("Need scenario set or covariance matrix...")
    if (is.null(R$mu) & is.null(R$returns)) 
        stop("Need scenario set or mean vector...")
    if (!is.null(R$mu)) R$mu <- matrix(R$mu, nrow = 1)
    else  R$mu <- matrix(colMeans(R$returns), nrow = 1)
    if (is.null(R$Sigma)) R$Sigma <- cov(R$returns)
    setNames(list(ROML::Vector.Variable(vname, 
        type="double", length=NCOL(R$mu), data=R)), vname)
}
##  --------------------------
##  Omega ratio
##  =============
##  --------------------------
omega_update_data <- function( R , tau = 0) {
    vname <- as.character(substitute(R))
    self <- ROML_get_model()
    R <- self$data
    if (is.null(R$returns)) stop("Needs scenario set. Maybe sample one?  ...")
    if (!is.null(R$mu)) R$mu <- matrix(R$mu, nrow = 1)
    else  R$mu <- matrix(colMeans(R$returns), nrow = 1)
    if (is.null(R$Sigma)) R$Sigma <- cov(R$returns)
    setNames(list(ROML::Vector.Variable(vname, 
        type="double", length=NCOL(R$mu), data=R)), vname)
}