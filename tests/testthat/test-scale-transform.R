library(grid)
library(ggplot2)

## A transformed axis, e.g. `scale_y_log10()`, stores its breaks in transformed
## units, so the ends of the subplot intervals have to be mapped back one by one.
## `grid.draw.ggcut()` applied the inverse of the transform to the whole list of
## intervals instead, which failed with
## "non-numeric argument to binary operator" as soon as a cut was combined with a
## transform.  `scale_x_break()` and `scale_wrap()` always did it element-wise.

set.seed(1)
dat <- data.frame(x = rnorm(200, 10, 3), y = rnorm(200, 10, 3))

## the labels of one axis may live in a single text grob that carries the whole
## vector, so collect `grob$label` as a vector rather than one element
axis_tick_labels <- function(plot, side = 'l') {
  g <- grid.force(ggplotGrob(suppressMessages(grid.draw(plot, recording = FALSE))))
  out <- character()
  walk <- function(grob, path = '') {
    if (inherits(grob, 'text') && grepl(paste0('/axis-', side, '-'), path) &&
        !is.null(grob$label)) {
      out <<- c(out, as.character(grob$label))
    }
    if (inherits(grob, 'gTree') && length(grob$children) > 0) {
      for (i in seq_along(grob$children)) {
        walk(grob$children[[i]], paste0(path, '/', names(grob$children)[i]))
      }
    }
  }
  walk(g)
  out[nzchar(out)]
}

draw <- function(plot) suppressMessages(grid.draw(plot, recording = FALSE))

test_that('a cut works on a transformed axis', {
  log_y <- ggplot(dat, aes(x, y)) + geom_point() + scale_y_log10()
  log_x <- ggplot(dat, aes(x, y)) + geom_point() + scale_x_log10()

  expect_silent(draw(log_y + scale_y_cut(8)))
  expect_silent(draw(log_x + scale_x_cut(10)))
  expect_silent(draw(log_y + scale_y_cut(c(4, 8), which = 1:3,
                                         scales = c(2, 1, 2))))
  expect_silent(draw(log_x + scale_x_cut(c(8, 12), which = 2)))
})

test_that('the cut of a transformed axis keeps the ticks of the transform', {
  log_y <- ggplot(dat, aes(x, y)) + geom_point() + scale_y_log10()

  ## the panel of a cut ends exactly at the cut point, so the log axis of the
  ## subplot carries it as a tick; before the fix nothing was drawn at all
  labels <- axis_tick_labels(log_y + scale_y_cut(8))
  expect_true('8' %in% labels)
  expect_true(length(labels) >= 4)
  expect_false(any(grepl('^[0-9]+[.][0-9]+$', labels)))
})

test_that('breaks and wraps still work on a transformed axis', {
  log_y <- ggplot(dat, aes(x, y)) + geom_point() + scale_y_log10()
  log_x <- ggplot(dat, aes(x, y)) + geom_point() + scale_x_log10()

  expect_silent(draw(log_y + scale_y_break(c(7, 13))))
  expect_silent(draw(log_x + scale_x_break(c(8, 12))))
  expect_silent(draw(log_y + scale_wrap(2)))
  expect_silent(draw(log_x + scale_wrap(2)))
})
