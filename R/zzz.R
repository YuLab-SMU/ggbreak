##' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname) {
    pkgVersion <- packageDescription(pkgname, fields="Version")
    msg <- paste0(pkgname, " v", pkgVersion, "\n\n")

    citation <- paste0("If you use ", pkgname,
                       " in published research, please cite the following paper:\n\n",
                       ggbreak_citation(), "\n")

    packageStartupMessage(paste0(msg, citation))
}


ggbreak_citation <- function() {
    paste("S Xu, M Chen, T Feng, L Zhan, L Zhou, G Yu.",
           "Use ggbreak to effectively utilize plotting space to deal with large datasets and outliers.",
           "Frontiers in Genetics. 2021, 12:774846. doi: 10.3389/fgene.2021.774846\n"
           )
}

