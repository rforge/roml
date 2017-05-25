
q("no")
Rdevel

library(ROI)
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






