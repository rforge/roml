context("Markowitz")

test_that("Markowitz", {
    q("no")
    R
    library(ROI)
    library(ROML)
    library(ROML.plugin.portfolio)

    library("PerformanceAnalytics")
    library("PortfolioAnalytics")
    data(edhec)
    portfolio <- coredata(edhec[, 1:4])

    ##' --------------------------
    ##' Test
    ##' ====
    ##' --------------------------
    ## solution: c(0.224193409436734, 0.341973545792181, 0.433833044771086, 0)
    ## objval:   0.000192166773520566
    m <- model()
    m$variable(portfolio)
    m$maximize(reward(portfolio))
    m$subject_to( markowitz(portfolio) < 0.03)    
    solution <- optimize(m, solver="quadprog", 
        data=list(portfolio=portfolio))



    portfolio <- coredata(edhec[, 1:2])
    m <- model()
    m$variable(portfolio)
    m$minimize( markowitz(portfolio) )
    m$subject_to( reward(portfolio) >= (0.03/12) )
    m$subject_to( full_investment(portfolio) )
    solution <- optimize(m, solver="quadprog", 
        data=list(portfolio=portfolio))
    solution$solution
    solution$objval

    ##'  Consider two assets A and B with 
    ##'  E(r_A) = 8 % and E(r_B) =  13 % 
    ##'  sigma_A = 12 %, sigma_B =  20 %, rho_{AB} = 0.3
    ##'  
    ##' optimal weights for minimum variance portfolio are x_A = 0.18 x_B = 0.82
    ##'  Objective is sigma^2_P = 0.013104 (sigma_P =  0.1144727)
    

    R <- MASS:::mvrnorm(100000, mu = c(0.08, 0.13), Sigma = matrix(c(0.12^2, 0.3 * 0.12 * 0.2, 
        0.3 * 0.12 * 0.2, 0.20^2), nrow = 2, ncol = 2))
    ## minimum variance portfolio
    m <- model()
    m$variable(portfolio)
    m$minimize( markowitz(portfolio) )
    m$subject_to( full_investment(portfolio) )
    solution <- optimize(m, solver="quadprog", 
        data=list(portfolio=R))
    solution$solution
    solution$objval    
    ## maximize expected return
    m <- model()
    m$variable(portfolio)
    m$maximize( reward(portfolio) )
    m$subject_to( full_investment(portfolio) )
    solution <- optimize(m, solver="glpk", 
        data=list(portfolio=R))
    solution$solution
    solution$objval    
} )
