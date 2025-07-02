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

