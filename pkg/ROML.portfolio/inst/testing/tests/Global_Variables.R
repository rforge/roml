q("no")
Rdevel

## install.packages("testthat")
library(testthat)
library(ROI)
library(ROML)
library(ROML.plugin.portfolio)
set.seed(20160512)

marko2_update_data <- function( R ) {
    vname <- as.character(substitute(R))
    self <- ROML_get_model()
    R <- self$data$returns
    setNames(list(ROML:::Vector.Variable(vname, type="double", length=ncol(R), data=R)), vname)
}

ROML_register_objective_data_update_function("marko2", marko2_update_data)

marko2_objective <- function( portfolioVar ) {
    self <- ROML_get_model()
    Q_objective(Q = 2 * cov(portfolioVar$data), 
        L = slam:::simple_triplet_zero_matrix(nrow=1, ncol=ncol(portfolioVar$data)))
}

ROML_register_objective_function("marko2", marko2_objective)
ls(ROML_get_objective_function())

## Example:   Minimum variance portfolio
##'  Consider two assets A and B with 
##'  E(r_A) = 8 % and E(r_B) =  13 % 
##'  sigma_A = 12 %, sigma_B =  20 %, rho_{AB} = 0.3
##'  
##' optimal weights for minimum variance portfolio are x_B = 0.18 x_A = 0.82
##'  Objective is sigma^2_P = 0.013104 (sigma_P =  0.1144727)
eps <- 1e-03
dat <- MASS:::mvrnorm(1000, mu = c(0.08, 0.13), 
    Sigma = matrix(c(0.12^2, 0.3 * 0.12 * 0.2, 0.3 * 0.12 * 0.2, 0.20^2), nrow = 2, ncol = 2))

m <- model()
m$variable(portfolio)
m$minimize( marko2(portfolio) )
m$subject_to( full_investment(portfolio) )
solution <- optimize(m, solver="quadprog", data=list(returns=dat))
expect_true(ROI::equal(solution$solution, c(0.82, 0.18), tol=1e-02))
expect_true(ROI::equal(solution$objval, 0.013104, tol=1e-02))


DEBUG_ROML_MODEL

obj <- ROML:::model_get_objective_functions(m)
