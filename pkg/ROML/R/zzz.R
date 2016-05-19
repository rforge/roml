.onLoad <- function( libname, pkgname ) {
    registerS3method("+", "Vector.Variable", add_expressions)
    registerS3method("+", "Expression", add_expressions)
}
