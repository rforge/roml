
context("CVaR")

test_that("CVaR", {
    ## 
    q("no")
    R
    ## TODO: Check if the solutions are correct!!
    library("PerformanceAnalytics")
    library("PortfolioAnalytics")
    data(edhec)
    R <- edhec[, 4:8]
    funds <- colnames(R)
    Wcons <- portfolio.spec( assets = funds )
    Wcons <- add.constraint( portfolio=Wcons, type='box', min = 0, max = Inf)
    Wcons <- add.constraint( portfolio=Wcons, type="full_investment")
    ObjSpec <- add.objective( portfolio = Wcons , type="risk",name="CVaR",
                             arguments=list(p=0.95), enabled=TRUE)
    opt <- optimize.portfolio(R=R, portfolio=ObjSpec,   
                            optimize_method="ROI", trace=TRUE)
    opt
    ##############################
    library(ROI)
    library(ROML)
    library(ROML.plugin.portfolio)
    m <- model()
    m$variable(portfolio)
    m$minimize( CVaR(portfolio, 0.95) )
    m$subject_to( full_investment(portfolio) )
    solution <- optimize(m, solver="glpk", 
        data=list(portfolio=coredata(R)))
    solution$solution
    solution$objval
    ##############################
    R <- edhec[, 4:8]
    funds <- colnames(R)
    Wcons <- portfolio.spec( assets = funds )
    Wcons <- add.constraint( portfolio=Wcons, type='box', min = 0, max = Inf)
    Wcons <- add.constraint( portfolio=Wcons, type="full_investment")
    Wcons <- add.constraint( portfolio=Wcons, type="return", return_target=0.007)
    ObjSpec <- add.objective( portfolio = Wcons , type="risk",name="CVaR",
                             arguments=list(p=0.95), enabled=TRUE)
    opt1 <- optimize.portfolio(R=R, portfolio=ObjSpec,   
                            optimize_method="ROI", trace=TRUE)
    opt1
    #################
    library(ROI)
    library(ROML)
    library(ROML.plugin.portfolio)
    m <- model()
    m$variable(portfolio)
    m$minimize( CVaR(portfolio, 0.95) )
    m$subject_to( reward(portfolio) >= 0.007 )
    m$subject_to( full_investment(portfolio) )
    
    R <- edhec[, 4:8]  
    solution <- optimize(m, solver="glpk", 
        data=list(portfolio=coredata(R)))
    solution$solution
    solution$objval




    R1 <- edhec[, 1:4]  
    solution1 <- optimize(m, solver="glpk", 
        data=list(portfolio=coredata(R1)))
    solution1$solution
    solution1$objval

    ########################
    # MAD ###
    #################
    library(ROI)
    library(ROML)
    library(ROML.plugin.portfolio)
    m <- model()
    m$variable(portfolio)
    m$minimize( MAD(portfolio) )
    m$subject_to( full_investment(portfolio) )
    solution <- optimize(m, solver="glpk", 
        data=list(portfolio=coredata(R)))
    solution$solution
    solution$objval

    ########################
    ### maximize return with cardinality
       q("no")
    
    R
    R <- read.csv("~/Work/Projects/RFinance/poptmod-models/data/sp100-2011-2015.csv", sep = ";")
    head(R)
    dim(R)
    R <- R[, 1:10]

    library("PerformanceAnalytics")
    library("PortfolioAnalytics")
    data(edhec)
    R <- edhec[, 1:10]
    
    ## does not wwork :-((( )))
    pspec <- portfolio.spec(assets=colnames(R))
    pspec <- add.constraint(portfolio=pspec, type="long_only")
    pspec <- add.constraint(portfolio=pspec, type="full_investment")
    
    pspec <- add.constraint(portfolio=pspec, type="position_limit",
                        min_pos=3) 
    pspec <- add.objective(portfolio=pspec, type='risk',
                          name='var')

    out <- optimize.portfolio(R=R, portfolio=pspec,
                          optimize_method="ROI",
                          trace=TRUE)
    out

    q("no")
    Rdevel
    library("PerformanceAnalytics")
    library("PortfolioAnalytics")
    data(edhec)
    R <- coredata(edhec[, 1:10])
    
    library(ROI)
    library(ROML)
    library(ROML.plugin.portfolio)
    m <- model()
    m$variable(portfolio)
    m$maximize( reward(portfolio) )
    # m$minimize( CVaR(portfolio, 0.95) )
    m$subject_to( full_investment(portfolio) )
    m$subject_to( cardinality(portfolio) >= 2 )
    solution <- optimize(m, solver="",  data=list(portfolio=R))
    solution$solution
   
    roim <- attr(solution, "model")
    names(roim)
    as.matrix(roim$objective$L)
    as.matrix(roim$constraint$L)
    as.matrix(roim$constraint$dir)
    as.matrix(roim$constraint$rhs)

    model <- m
    solver=""
    data=list(portfolio=R)
    attach(getNamespace("ROML"))

    fun <- function(x) print(class(x))
    eval(expression(fun(po)), envir=list(fun=fun, po=1:3))
    eval(expression(fun(po)), envir=list(fun=fun, po=list()))

    str(m$objective.parse.data)
    m$objective.parse.data$tokens
    m$objective.parse.data$tree
    m$objective[1][1]

    x + reward(portfolio)

    traceback()
    solution$solution
    sqrt(solution$objval)

    ROML_get_objective_function("reward")

    q("no")
    R CMD INSTALL ROML.plugin.portfolio
    R
    library(ROI)
    library(ROML)
    library(ROML.plugin.portfolio)
    library(slam)
    R <- read.csv("~/Work/Projects/RFinance/poptmod-models/data/sp100-2011-2015.csv", sep = ";")
    head(R)
    dim(R)
    R <- R[, 1:10]

    m <- model()
    m$variable(portfolio, lb= -1)
#    m$minimize(downside_MAD(portfolio) )
#    m$minimize(downside_sd(portfolio) )
#    m$minimize( CVaR(portfolio, 0.95) )
#    m$maximize( minimax_young(portfolio) )
#    m$subject_to( full_investment(portfolio) )
    m$maximize( quadratic_utility(portfolio, lambda))
    m$subject_to( reward(portfolio)>=-0.01)
    solution <- optimize(m, solver="", 
        data=list(portfolio=as.matrix(R), lambda = 2))
    solution
    solution$solution[1 : ncol(R)]
    solution1 <- optimize(m, solver="", 
            data=list(portfolio=as.matrix(R), lambda = 1))
    solution1
    solution1$obj
    ## TODO: minimax not working :-( no solution found)

    ## minimiax young
    #q("no")
    #R
    ## TODO: Check if the solutions are correct!!
    #library("PerformanceAnalytics")
    #library("PortfolioAnalytics")
    #data(edhec)
    #R <- edhec[, 4:8]
    #library(ROI)
    #library(ROML)
    #library(ROML.plugin.portfolio)
    #m <- model()
    #m$variable(portfolio)
    #m$maximize(minimax_young(portfolio, 0.001) )
    #m$subject_to( full_investment(portfolio) )
    #solution <- optimize(m, solver="glpk", 
    #    data=list(portfolio=coredata(R)))
    #solution$solution

    
} )
