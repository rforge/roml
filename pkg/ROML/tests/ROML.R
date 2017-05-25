
q("no")
Rdevel

Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
library(ROI)
library(ROI.plugin.glpk)
library(ROML)

attach(getNamespace("ROML"))


check <- function(domain, condition, level=1, message="", call=sys.call(-1L)) {
    if ( isTRUE(condition) ) return(invisible(NULL))
    msg <- sprintf("in %s", domain)
    if ( all(nchar(message) > 0) ) msg <- sprintf("%s\n\t%s", msg, message)
    stop(msg)
    return(invisible(NULL))
}

## ---------------------------
## Types
## --------------------------- 
## boolean
test.types.logical_1 <- function() {
    m <- model()
    m$variable(x, length = 3, type = "logical")
    ## FIXME this should not be interpreted as.
    ##       sum(x)
    m$subject_to( x == 1)
    m$maximize(x)
    sol <- optimize(m)
    max(sol$solution) == 1
    sol$solution
}

m <- model()
m$variable(x, length=3, type="logical")
m$subject_to( x == 2 )
m$maximize(x)
sol <- optimize(m, "glpk")
max(sol$solution) == 1
sol$solution

traceback()

## ---------------------------
## Bounds
## --------------------------- 
test.lower.bounds_1 <- function() {
    m <- model()
    m$variable(x, lb = 3, length = 1L)
    m$variable(y, lb = 4, length = 1L)
    m$minimize(x + y)
    sol <- optimize(m, solver = "glpk")
    check("test.lower.bounds_1", equal(sol$solution, c(3, 4)))
}

test.upper.bounds_1 <- function() {
    m <- model()
    m$variable(x, ub = 4, length = 1L)
    m$variable(y, ub = 3, length = 1L)
    m$maximize(x + y)
    sol <- optimize(m, solver = "glpk")
    check("test.upper.bounds_1", equal(sol$solution, c(4, 3)))
}

## ---------------------------
## Examples
## --------------------------- 
test.LP_1 <- function(solver) {
##' LP - Example
##' Solver: Rglpk
##'
##' maximize:   
##'   2 x + 4 y + 3 z
##' subject to:    
##'   3 x + 4 y + 2 z <= 60
##'   2 x +   y + 2 z <= 40
##'     x + 3 y + 2 z <= 80
##'   x, y, z are non-negative real numbers
##'
##' Solution:  0.000000  6.666667 16.666667
    m <- model()
    m$variable(x, length=1L)
    m$variable(y, length=1L)
    m$variable(z, length=1L)
    m$maximize(2 * x + 4 * y + 3 * z)
    m$subject_to(3 * x + 4 * y + 2 * z <= 60)
    m$subject_to(2 * x +     y + 2 * z <= 40)
    m$subject_to(    x + 3 * y + 2 * z <= 80)

    solution <- optimize(m, solver = solver)
    sol <- c(0, 6.66666666666667, 16.6666666666667)
    check("test.LP_1", equal(solution$solution, sol))
}



solver <- "glpk"

model <- m
data <- list()


test.LP_1("glpk")

m <- model()
m$variable(x, length=3L)
A <- 
m$maximize(2 * x + 4 * y + 3 * z)
m$subject_to(3 * x + 4 * y + 2 * z <= 60)
m$subject_to(2 * x +     y + 2 * z <= 40)
m$subject_to(    x + 3 * y + 2 * z <= 80)

solution <- optimize(m, solver = solver)
sol <- c(0, 6.66666666666667, 16.6666666666667)

print("Start Testing!")

local({test.lower.bounds_1()})
local({test.upper.bounds_1()})

local({test.LP_1("glpk")})

m$objective
m$maximize

str(m)

str(ROML_MODEL)


## ---------------------------
## Examples (koberstein)
## --------------------------- 

test.PP_simple <- function(solver) {
##' LP - Example
##' Solver: Rglpk
##'
##' maximize:   
##'   25 xb + 30 xc
##' subject to:    
##'   1/200 xb + 1/140 xc <= 40
##'         xb            <= 6000
##'                    xc <= 4000
##'   xb, xc  are non-negative real numbers
##'
##' Solution:  6000 1400
    m <- model()
    m$variable(XB, length = 1L, ub = 6000)
    m$variable(XC, length = 1L, ub = 4000)
    m$maximize(25 * XB + 30 * XC)
    m$subject_to(1/200 * XB + 1/140 * XC  <= 40)
    solution <- optimize(m, solver = "auto")
    sol <- c(6000, 1400)
    check("test.PP_simple", equal(solution$solution, sol))
}

test.PP_simple("glpk")

test.PP_general <- function(solver) {
##' LP - Example
##' Solver: Rglpk
##'
##' maximize:   
##'   25 xb + 30 xc
##' subject to:    
##'   1/200 xb + 1/140 xc <= 40
##'         xb            <= 6000
##'                    xc <= 4000
##'   xb, xc  are non-negative real numbers
##'
##' Solution:  6000 1400
    P <- c("bands", "coils")
    C <- c(25, 30)
    a <- c(200, 140)
    b <- 40
    u <- c(6000, 4000)
    m <- model()
    m$variable(X, length = length(P), ub = u)
    m$maximize(C %*% X)

m$subject_to(1/a %*% X <= b )

sol <- optimize(m)
sol$solution
m <- model()
    m$variable(XB, length = 1L, ub = 6000)
    m$variable(XC, length = 1L, ub = 4000)
    m$maximize(25 * XB + 30 * XC)
    m$subject_to(1/200 * XB + 1/140 * XC  <= 40)
    solution <- optimize(m, solver = "auto")
    sol <- c(6000, 1400)
    check("test.PP_simple", equal(solution$solution, sol))
}
