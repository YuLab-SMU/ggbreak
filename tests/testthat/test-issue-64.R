library(grid)
library(ggplot2)

## collect the paths of all axis text grobs of a rendered plot
collect_axis_paths_64 <- function(grob, path = '') {
  if (inherits(grob, 'text')) {
    return(if (grepl('/axis-[a-z]+-', path)) path else character())
  }
  if (!inherits(grob, 'gTree') || length(grob$children) == 0) {
    return(character())
  }
  unlist(lapply(seq_along(grob$children), function(i) {
    child_name <- names(grob$children)[i]
    collect_axis_paths_64(grob$children[[i]], paste0(path, '/', child_name))
  }), use.names = FALSE)
}

## number of axis text grobs on a given side ('t', 'b', 'l', 'r')
n_axis_64 <- function(plot, side) {
  rendered <- suppressWarnings(grid.draw(plot, recording = FALSE))
  axis_text <- collect_axis_paths_64(grid.force(ggplotGrob(rendered)))
  length(grep(paste0('/axis-', side, '-'), axis_text))
}

test_that('no spurious secondary axis is drawn for scales without `sec.axis` (#64)', {
  base <- ggplot(datasets::ToothGrowth, aes(x = supp, y = len)) +
    geom_bar(stat = 'summary', fun = 'mean') +
    theme_classic()

  ## a scale that does not define `sec.axis` must not add a secondary axis,
  ## no matter whether it is a continuous or a discrete position scale
  plots <- list(
    base + scale_y_continuous() + scale_y_break(c(5, 15)),
    base + ylim(0, 25) + scale_y_break(c(5, 15)),
    base + scale_x_discrete() + scale_y_break(c(5, 15)),
    base + scale_x_discrete() + scale_y_continuous() + scale_y_break(c(5, 15))
  )

  for (p in plots) {
    expect_equal(n_axis_64(p, 'r'), 0)
    expect_equal(n_axis_64(p, 't'), 0)
    expect_equal(n_axis_64(p, 'b'), 1)
  }
})

test_that('a real secondary axis is still drawn (#64)', {
  base <- ggplot(datasets::ToothGrowth, aes(x = supp, y = len)) +
    geom_bar(stat = 'summary', fun = 'mean') +
    theme_classic()

  p <- base + scale_y_continuous(sec.axis = dup_axis()) + scale_y_break(c(5, 15))
  expect_equal(n_axis_64(p, 'r'), 2)
})
