## function taken from ggtree
##' @importFrom ggplot2 ggplot_build
ggrange2 <- function (plot, var) {
    var <- paste0("panel_scales_", var)
    axis_range <- ggplot_build(plot)$layout[[var]][[1]]$range$range
    flagrev <- ggplot_build(plot)$layout[[var]][[1]]$trans$name
    list(axis_range=axis_range, flagrev=flagrev)
}

#' @importFrom ggplot2 labs
set_label <- function(p, totallabs, p2 = NULL) {
    p <- p + 
         do.call(labs, totallabs) +
         theme(axis.title = element_text())

    if (is.null(p2)) {
        has_theme <- FALSE
    } else {
        has_theme <- length(p2$theme) != 0
    }

    if (has_theme) {
        xlab_param <- if(length(p2$theme$axis.title.x)>0){p2$theme$axis.title.x}else{defte$axis.title.x}
        ylab_param <- if(length(p2$theme$axis.title.y)>0){p2$theme$axis.title.y}else{defte$axis.title.y}
        title_param <- if(length(p2$theme$plot.title)>0){p2$theme$plot.title}else{defte$plot.title}
        title_pos_param <- if(length(p2$theme$plot.title.position)>0){p2$theme$plot.title.position}else{defte$plot.title.position}
        subtitle_param <- if(length(p2$theme$plot.subtitle)>0){p2$theme$plot.subtitle}else{defte$plot.subtitle}
        cap_param <- if(length(p2$theme$plot.caption)>0){p2$theme$plot.caption}else{defte$plot.caption}
        cap_pos_param <- if(length(p2$theme$plot.caption.position)>0){p2$theme$plot.caption.position}else{defte$plot.caption.position}
        tag_param <- if(length(p2$theme$plot.tag)>0){p2$theme$plot.tag}else{defte$plot.tag}
        tag_pos_param <- if(length(p2$theme$plot.tag.position)>0){p2$theme$plot.tag.position}else{defte$plot.tag.position}
        
        p <- p + theme(axis.title.x = xlab_param,
                       axis.title.y = ylab_param,
                       plot.title = title_param,
                       plot.title.position = title_pos_param, 
                       plot.subtitle = subtitle_param,
                       plot.caption = cap_param,
                       plot.caption.position = cap_pos_param,
                       plot.tag = tag_param,
                       plot.tag.position = tag_pos_param
                      )
    } else {
        p <- p + defte #theme(axis.title.x = element_text(vjust = 1),
                 #      axis.title.y = element_text(angle = 90, vjust = 1)) 
    }
    return(p)
}

extract_totallabs <- function(plot){
    alllabs <- plot$labels
    totallabs <- alllabs[names(alllabs) %in% c("x", "y", "title", "subtitle", "caption", "tag")]
    totallabs
}


combine_range <- function(breaks, rangeres){
    if (rangeres$flagrev=="reverse"){
        rangeres$axis_range <- sort(abs(rangeres$axis_range))
    }
    newbreaks <- merge_intervals(breaks)
    newbreaks <- c(rangeres$axis_range[1], unlist(newbreaks), rangeres$axis_range[2])
    newbreaks <- lapply(data.frame(matrix(newbreaks, nrow=2)), function(i)i)
    if (rangeres$flagrev=="reverse"){
        newbreaks <- lapply(newbreaks, function(i)rev(i))
        return(rev(newbreaks))
    }
    return(newbreaks)
}

merge_intervals <- function(breaks){
    if (!inherits(breaks, "list")){
        breaks <- list(breaks)
    }
    out <- list()
    breaks <- lapply(breaks, function(i) sort(i))
    breaks <- breaks[rank(unlist(lapply(breaks, function(i)i[[1]])))]
    for (i in breaks){
        if (length(out) > 1 && i[1] <= out[[length(out)]][2]){
            out[[length(out)]][2] <- max(out[[length(out)]][2], i[2])
        }else{
            out <- c(out, list(i))
        }
    }
    return(out)
}

extract_axis_break <- function(object){
    if (inherits(object, "ggbreak_params")){
        axis <- object$axis
        breaks <- object$breaks
        scales <- object$scales
    }else{
        axis <- object[[1]]$axis
        breaks <- lapply(object, function(i)i$breaks)
        scales <- lapply(object, function(i)i$scales) 
    }
    return(list(axis=axis, breaks=breaks, scales=scales))
}

theme_no_margin <- getFromNamespace("theme_no_margin", "aplot")

#' @importFrom ggplot2 theme_gray
defte <- ggplot2::theme_gray()
