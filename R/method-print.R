##' @method print ggbreak
##' @export
print.ggbreak <- function(x, ...) {
    grid.draw.ggbreak(x)
}

##' @method print ggwrap
##' @export
print.ggwrap <- function(x, ...){
    grid.draw.ggwrap(x)
}

##' @method print ggcut
##' @export
print.ggcut <- function(x, ...){
    grid.draw.ggcut(x)
}
