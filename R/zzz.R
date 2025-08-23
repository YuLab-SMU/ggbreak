#' @importFrom utils packageDescription
#' @importFrom yulab.utils yulab_msg
.onAttach <- function(libname, pkgname) {
    packageStartupMessage(yulab_msg(pkgname))

    citation <- paste0(paste0("If you use ", 
                              pkgname, 
                              " in published research, please cite the following paper:\n\n"
                              ),
                       ggbreak_citation(), collapse = "\n")

    packageStartupMessage(paste0(strwrap(pillar::style_subtle(citation)), collapse="\n"))
}


ggbreak_citation <- function() {
    paste0("S Xu, M Chen, T Feng, L Zhan, L Zhou, G Yu. ",
           "Use ggbreak to effectively utilize plotting space to deal with large datasets and outliers. ",
           "Frontiers in Genetics. 2021, 12:774846. doi: 10.3389/fgene.2021.774846\n"
           )
}


.ggbreak <- yulab.utils::get_cache()
.ggbreak$scale_break <- function(axis, breaks, scales, ticklabels=NULL, expand=TRUE, space = .1) {
    call_stack <- sys.calls()
    for (call in call_stack) {
        if (is.function(call[[1]]) && is.null(attr(call[[1]], "name"))) {
            stop("invalid called.")
        }
    }

    structure(list(axis = axis, breaks = breaks, scales=scales, 
                   ticklabels=ticklabels, expand = expand, space = space),
              class = "ggbreak_params")
}


