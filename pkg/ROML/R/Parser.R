
##library(R6)

getParseDataFromText <- function(text) {
    sf <- srcfile("")
    try(parse(text = text, srcfile = sf, keep.source = FALSE))
    sf$parseData
}

get_parse_data <- function(data) {
    tokens <- attr(data, "tokens")
    data <- t(unclass(data))
    colnames(data) <- c("line1", "col1", "line2", "col2", "terminal",
                        "token.num", "id", "parent" )
    data <- data.frame(data[,, drop = FALSE], token = tokens,
                       text = attr(data, "text"),
                       stringsAsFactors = FALSE)
    data$terminal = as.logical(data$terminal)
    o <- order(data[,1], data[,2], -data[,3], -data[,4])
    data <- data[o,]
    return(data)
}

getParseTreeFromText <- function(src) {
    x <- getParseDataFromText(src)
    data <- get_parse_data(x)
    parse_tree <- as.Parse.Tree(data)
    Parse.Tree.Data$new(src, parse_tree, data, attributes(x)$tokens, attributes(x)$text)
}

Parse.Tree.Data <- R6Class(
    "Parse.Tree.Data",
    public=list(
        portable=FALSE,
        src=NULL,
        tree=NULL,
        data=NULL,
        tokens=NULL,
        text=NULL,
        has.function_call = function() {
            return( any(c(296L) %in% self$data$token.num) )
        },
        initialize = function(src, tree, data, tokens, text) {
            self$src <- src
            self$tree <- tree
            self$data <- data
            self$tokens <- tokens
            self$text <- text
            return( NULL )
        },
        print = function() {
            cat("  src: ", self$src, "\n")
            cat("  data:\n")
            ## since we parse only single lines the line is not important
            print(self$data[nchar(self$data$text) > 0,-c(1,3)])
            cat("  text-tokens:\n")
            print(rbind(self$text, self$tokens)[,nchar(self$text) > 0])
        }
    )
)

## #NOTE: "expr" play the role of container!
as.Parse.Tree <- function(x) {
    ..ID.. <- 1L
    .as.Parse.Tree <- function(value) {
        i <- which(x$id == value)
        children <- x$id[x$parent == value]
        if ( length(children) == 1 ) {
            if ( "expr" %in% x$token[i] ) {
                return( .as.Parse.Tree(children) )
            }
        }
        if ( length(children) == 0 ) {
            node <- list(value=..ID.., children=list(), token.id=x$token.num[i], 
                         token=x$token[i], text=x$text[i])
            ..ID.. <<- ..ID.. + 1L
            return( node )
        } else {
            children <- lapply(children, .as.Parse.Tree)
            uchildren <- unlist(lapply(children, "[[", "text"))
            text <- paste(uchildren, collapse="")
            node <- list(value=..ID.., children=children, token.id=x$token.num[i], 
                         token=x$token[i], text=text)
            ..ID.. <<- ..ID.. + 1L
            return( node )
        }
    }
    x <- .as.Parse.Tree(0)
    x$token <- "root"
    x$value <- 0L
    x
}
