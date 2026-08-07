library(grid)
library(ggplot2)

collect_axis_text_paths <- function(grob, path = '') {
  if (inherits(grob, 'text')) {
    return(if (grepl('/axis-[tb]-', path)) path else character())
  }
  if (!inherits(grob, 'gTree') || length(grob$children) == 0) {
    return(character())
  }
  unlist(lapply(seq_along(grob$children), function(i) {
    child_name <- names(grob$children)[i]
    collect_axis_text_paths(grob$children[[i]], paste0(path, '/', child_name))
  }), use.names = FALSE)
}

test_that('datetime x axis is drawn only on the bottom y-break panel (#83)', {
  dat <- data.frame(
    x = Sys.time() + (1:10) * 1000,
    y = c(1:9, 60)
  )
  plots <- list(
    ggplot(dat, aes(x, y)) + geom_point() + scale_y_break(c(20, 50)),
    ggplot(dat, aes(x, y)) + geom_point() +
      scale_x_datetime() + scale_y_break(c(20, 50))
  )

  for (p in plots) {
    rendered <- grid.draw(p, recording = FALSE)
    grob <- grid.force(ggplotGrob(rendered))
    axis_text <- collect_axis_text_paths(grob)

    expect_length(grep('/axis-t-', axis_text), 0)
    expect_length(grep('/axis-b-', axis_text), 1)
  }
})
