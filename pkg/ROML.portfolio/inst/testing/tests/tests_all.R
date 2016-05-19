library(testthat)

context("markowitz")

test_that("markowitz", {
    ## Example:   Minimum variance portfolio
    ##'  Consider two assets A and B with 
    ##'  E(r_A) = 8 % and E(r_B) =  13 % 
    ##'  sigma_A = 12 %, sigma_B =  20 %, rho_{AB} = 0.3
    ##'  
    ##' optimal weights for minimum variance portfolio are x_B = 0.18 x_A = 0.82
    ##'  Objective is sigma^2_P = 0.013104 (sigma_P =  0.1144727)
    library(ROI)
    library(ROML)
    library(ROML.plugin.portfolio)
    set.seed(20160512)
    dat <- MASS:::mvrnorm(1000, mu = c(0.08, 0.13), 
        Sigma = matrix(c(0.12^2, 0.3 * 0.12 * 0.2, 
                         0.3 * 0.12 * 0.2, 0.20^2), nrow = 2, ncol = 2))
    m <- model()
    m$variable(portfolio)
    m$minimize( markowitz(portfolio) )
    m$subject_to( sum(portfolio) == 1)
    solution <- optimize(m, solver="", 
        data=list(returns = dat))
    ## or 
    solution <- optimize(m, solver="", 
        data=list(Sigma = matrix(c(0.12^2, 0.3 * 0.12 * 0.2, 
                         0.3 * 0.12 * 0.2, 0.20^2), nrow = 2, ncol = 2)))

    expect_true(ROI::equal(solution$solution, c(0.82, 0.18), tol=1e-02))
    expect_true(ROI::equal(solution$objval, 0.013104, tol=1e-02))
    ## Example: minimize sd s.t. return >= target + full constraint
    ## taken from http://home.anadolu.edu.tr/~nila/PROGRAM/KILAVUZ/optimization%20modelling%20with%20lingo/Chapter13.pdf
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
    colnames(dat) <- c("SP500", "ATT", "GMC", "USX")
    R <- dat[, -1] - 1
    m <- model()
    m$variable(portfolio)
    m$minimize( markowitz(portfolio) )
    m$subject_to( budget_norm(portfolio))
    m$subject_to(reward(portfolio) >= 0.15)
    solution <- optimize(m, solver="", 
        data=list(returns = R ))  
    ## or 
    solution <- optimize(m, solver="", 
        data=list(Sigma = cov(R), mu = colMeans(R) ))

    expect_true(ROI::equal(solution$solution, c(0.53, 0.36, 0.11), tol=1e-02))
    expect_true(ROI::equal(solution$objval, 0.0224138, tol=1e-02)) 
} )

context("downside_var")

test_that("downside_var", {
    library(ROI)
    library(ROML)
    library(ROML.plugin.portfolio)

    library(slam)
    ## Example: minimize downside_var s.t. return >= target + full constraint
    ## taken from http://home.anadolu.edu.tr/~nila/PROGRAM/KILAVUZ/optimization%20modelling%20with%20lingo/Chapter13.pdf
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
    colnames(dat) <- c("SP500", "ATT", "GMC", "USX")
    R <- dat[, -1] - 1
    
    m <- model()
    m$variable(portfolio)
    m$minimize( downside_var(portfolio) )
    m$subject_to( budget_norm(portfolio))
    m$subject_to(reward(portfolio) >= 0.15)
    solution <- optimize(m, solver="", 
        data=list(returns = R ))  
    expect_true(ROI::equal(solution$solution[1:ncol(R)], c(0.5757791, 0.3858243E-01, 0.3856385), tol=1e-02))
    expect_true(ROI::equal(solution$objval, 0.8917110E-02 , tol=1e-02)) 
})

context("minimax_young")

test_that("minimax_young", {
    ## taken from p. 390, http://home.anadolu.edu.tr/~nila/PROGRAM/KILAVUZ/optimization%20modelling%20with%20lingo/Chapter13.pdf
    m <- model()
    m$variable(portfolio)
    m$maximize( minimax_young(portfolio) )
    m$subject_to( budget_norm(portfolio))
    dat <- matrix(c(1, 1.5, 1.2, 0.7), ncol = 2)
    dat1 <- matrix(c(1, 1.5, 1.3, 0.7), ncol = 2)
    solution <- optimize(m, solver="", 
        data=list(returns = dat ))  
    solution1 <- optimize(m, solver="", 
        data=list(returns = dat1 ))  
    expect_true(ROI::equal(solution$solution[1:ncol(dat)] , c(0.5, 0.5), tol=1e-02))

    expect_true(ROI::equal(solution1$solution[1:ncol(dat1)] ,  c(0.5454545, 0.4545455), tol=1e-02))

    expect_true(ROI::equal(solution$objval, 1.1 , tol=1e-02)) 
    
    expect_true(ROI::equal(solution1$objval, 1.136364 , tol=1e-02)) 
})

context("sharpe")
test_that("sharpe", {
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
    m$maximize( sharpe(portfolio, rf))
    solution <- optimize(m, solver="quadprog", 
        data=list(returns = R, rf = .05))  
    ## OR

    solution <- optimize(m, solver="quadprog", 
        data=list(mu = colMeans(R), Sigma = cov(R), rf = .05)) 
    xopt <- solution$solution[1:ncol(R)]/solution$solution[ncol(R) + 1]
    sd <- sqrt(drop(xopt %*% cov(R) %*% xopt))
    mu <- colMeans(R)
    obj <- (drop(mu %*% xopt) - 0.05)/sd
    expect_true(ROI::equal(xopt , c(0.1319260, 0.6503984, 0.2176757), tol=1e-02))
    expect_true(ROI::equal(obj, 0.6933179, tol=1e-02))

})

context("cvar")
test_that("cvar", {
    ## Minimize CVaR s.t. sum(weights) = 1 and weights >= 0
    library("PerformanceAnalytics")
    library("PortfolioAnalytics")
    data(edhec)
    dat <- edhec[, 1:10]
    # try with PortfolioAnalytics
    pspec <- portfolio.spec(assets=colnames(dat))
    pspec <- add.constraint(portfolio=pspec, type="long_only")
    pspec <- add.constraint(portfolio=pspec, type="full_investment")
    pspec <- add.objective( portfolio = pspec , type="risk",name="CVaR",
                            arguments=list(p=0.95))
    out <- optimize.portfolio(R=dat, portfolio=pspec,
                          optimize_method="ROI",
                          trace=TRUE)
    ## with ROML
    m <- model()
    m$variable(portfolio)
    m$minimize( cvar(portfolio, 0.95) )
    m$subject_to( budget_norm(portfolio) )
    solution <- optimize(m, solver="", 
        data=list(returns = coredata(dat)))  
    expect_true(ROI::equal(solution$solution[1:ncol(dat)], out$weight, tol=1e-02))
    expect_true(ROI::equal(solution$objval, out$objective_measures[[1]], tol=1e-02))
})

context("cvar_card")
test_that("cvar_card", {
    ## Minimize CVaR s.t. sum(weights) = 1 and weights >= 0 and cardinality <= 2
    ## try with PortfolioAnalytics
    library("PerformanceAnalytics")
    library("PortfolioAnalytics")
    data(edhec)
    dat <- edhec[, 1:10]
    # try with PortfolioAnalytics
    pspec <- portfolio.spec(assets=colnames(dat))
    pspec <- add.constraint(portfolio=pspec, type="long_only")
    pspec <- add.constraint(portfolio=pspec, type="full_investment")
    pspec <- add.objective( portfolio = pspec , type="risk",name="CVaR",
                            arguments=list(p=0.95))
    pspec <- add.constraint(portfolio=pspec, type="position_limit",
                       max_pos=2) 
    out <- optimize.portfolio(R=dat, portfolio=pspec,
                          optimize_method="ROI",
                          trace=TRUE)
    ## with ROML
    m <- model()
    m$variable(portfolio, lb = 0)
    m$minimize( cvar(portfolio, 0.95) )
    m$subject_to( budget_norm(portfolio) )
    m$subject_to( cardinality(portfolio) <= 2)
    solution <- optimize(m, solver="glpk", 
        data=list(returns = coredata(dat)))  
    expect_true(ROI::equal(solution$solution[1:ncol(dat)], out$weight, tol=1e-02))
    expect_true(ROI::equal(solution$objval, out$objective_measures[[1]], tol=1e-02))
})


context("reward_cvar")
test_that("reward_cvar", {
    ## Minimize reward s.t. sum(weights) = 1 and weights >= 0 and cvar <= 0.04 and weights[1] + weights[2] == 0.5
    library("PerformanceAnalytics")
    library("PortfolioAnalytics")
    data(edhec)
    dat <- edhec[, 1:3]
    ## TODO: compute true solution
    ## with ROML
    m <- model()
    m$variable(portfolio)
    m$maximize( reward(portfolio) )
    m$subject_to( budget_norm(portfolio))
    m$subject_to( cvar(portfolio, 0.95) <= 0.04 )
    m$subject_to( portfolio[1] + portfolio[2] == 0.5)
    solution <- optimize(m, solver="glpk", 
        data=list(returns = coredata(dat)))  
    solution
    solution$solution[1:ncol(R)]
})

## context("omega")
## test_that("omega", {
##    # TODO: find a better example
##    # example from Kane et.al 2007 - as non-linear program, solution is 0.45, 0.55  Omega = 2.56
##    dat <- read.csv("~/Work/Projects/RFinance/stocks3.csv")
##    m <- model()
##    m$variable(portfolio)
##    m$maximize( omega(portfolio, tau))
##    solution1 <- optimize(m, solver="", 
##            data=list(portfolio=as.matrix(dat), tau = 0.01))
##    solution1
##    xopt <-(solution1$solution[1:ncol(dat)]/(solution1$solution[ncol(dat) + nrow(dat) + 1]))
##    obj <- mean(pmax(as.matrix(dat) %*% xopt - 0.01, 0))/mean(pmax(0.01 - as.matrix(dat) %*% xopt, 0))
##    expect_true(ROI::equal(xopt, c(0.45, 0.55, 0), tol=1.5e-02))
##    expect_true(ROI::equal(obj, 2.56, tol=1.5e-02))
##})

context("cvar_turnover")
test_that("cvar_turnover", {
    library("PerformanceAnalytics")
    library("PortfolioAnalytics")
    data(edhec)
    R <- coredata(edhec[, 1:10])   
    m <- model()
    m$variable(portfolio, lb = -Inf)
    m$minimize(cvar(portfolio, 0.95))
    m$subject_to( budget_norm(portfolio))
    m$subject_to( turnover(portfolio) <= 0.5)
    solution <- optimize(m, solver = "glpk", 
                         data = list(returns = R))
    solution
    solution$solution[1:ncol(R)]
    # TODO: true solution
    expect_true(ROI::equal(sum(abs(solution$solution[1:ncol(R)] - 1/ncol(R))), 0.5))

})


context("mad")
test_that("mad", {
    ##  Consider two assets A and B with 
    ##  E(r_A) = 8 % and E(r_B) =  13 % 
    ##  sigma_A = 12 %, sigma_B =  20 %, rho_{AB} = 0.3
    ##  
    ## optimal weights for minimum variance portfolio are x_B = 0.18 x_A = 0.82
    ##  Objective is sigma^2_P = 0.013104 (sigma_P =  0.1144727)
    ##
    ## MAD should deliver same weights as  the variance for normally distributed data
    library(ROI)
    library(ROML)
    library(ROML.plugin.portfolio)
    set.seed(20160512)
    dat <- MASS:::mvrnorm(1000, mu = c(0.08, 0.13), 
        Sigma = matrix(c(0.12^2, 0.3 * 0.12 * 0.2, 
                         0.3 * 0.12 * 0.2, 0.20^2), nrow = 2, ncol = 2))
    m <- model()
    m$variable(portfolio)
    m$minimize( mad(portfolio) )
    m$subject_to(budget_norm(portfolio) )
    solution <- optimize(m, solver="", 
        data=list(returns=dat))
    expect_true(ROI::equal(solution$solution[1:ncol(dat)], c(0.82, 0.18), tol=1e-02))
})
context("qu")
test_that("qu", {
   ## TODO: Check if the solutions are correct!!
    library("PerformanceAnalytics")
    library("PortfolioAnalytics")
    data(edhec)
    R <- edhec[, 1:10]
    funds <- colnames(R)
    Wcons <- portfolio.spec( assets = funds )
    Wcons <- add.constraint( portfolio=Wcons, type='box', 
        min = -10, max = Inf)

    Wcons <- add.constraint( portfolio=Wcons, type="full_investment")
    Wcons <- add.objective(portfolio=Wcons, type="return", name="mean")
    Wcons <- add.objective(portfolio=Wcons, type='risk',
                           name='var', risk_aversion = 1)

    out <- optimize.portfolio(R=R, portfolio=Wcons,   
                            optimize_method="ROI", 
                            trace=TRUE)
 
    m<-model()
    m$variable(portfolio, lb = -10)
    m$maximize( quadratic_utility(portfolio, lambda) ) 
    m$subject_to(budget_norm(portfolio))
    solution <- optimize(m, solver="", 
        data=list(returns=coredata(R), lambda = 1*2)) 
    expect_true(ROI::equal(solution$solution, out$weights, tol=1e-02))
    expect_true(ROI::equal(out$objective[["mean"]] - 2/2 * out$objective[["StdDev"]]^2,
                           solution$objval))

})