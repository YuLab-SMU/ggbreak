##' @method print ggbreak
##' @export
print.ggbreak <- function(x,...) {
    suppressWarnings(grid.draw.ggbreak(x, ...))
}

##' @method print ggwrap
##' @export
print.ggwrap <- function(x, ...){
    suppressWarnings(grid.draw.ggwrap(x, ...))
}

##' @method print ggcut
##' @export
print.ggcut <- function(x, ...){
    suppressWarnings(grid.draw.ggcut(x, ...))
}
