.onLoad <- function( libname, pkgname ) {
    registerS3method("+", "Vector.Variable", add_expressions)
    registerS3method("-", "Vector.Variable", substract_expressions)
    registerS3method("+", "Expression", add_expressions)
    registerS3method("-", "Expression", substract_expressions)
}
