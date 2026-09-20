library(grid)
library(ggplot2)

## #43: the subplot ranges are built by splicing the break points into the axis
## range, which assumes that the break points are inside that range.  A break
## interval outside of it used to produce a subplot range whose limits are in
## the wrong order (e.g. (20, 6) for an axis ranging from 4 to 6) and thus a
## silently flipped axis.

test_that('a break interval outside the plot range is reported (#43)', {
  p <- ggplot(data.frame(x = 1:3, y = 4:6), aes(x, y)) + geom_point()

  ## completely above, completely below and only partly outside
  for (breaks in list(c(10, 20), c(0, 1), c(-5, -1), c(5.5, 10))) {
    expect_error(grid.draw(p + scale_y_break(breaks), recording = FALSE),
                 'not in the plot range')
  }

  ## the same for the x axis
  px <- ggplot(data.frame(x = 1:3, y = 4:6), aes(x, y)) + geom_point()
  expect_error(grid.draw(px + scale_x_break(c(10, 20)), recording = FALSE),
               'not in the plot range')
})

test_that('the axis range is the range of every panel, not of the first (#43)', {
  ## `facet_grid(scales = "free_y")` gives every panel a scale of its own, so the
  ## first one holds the range of the first panel only.  Reading the axis range
  ## off it built the windows from that range, and every point outside them was
  ## dropped: the facet whose data sits high came out empty, and a break that was
  ## inside the first panel but outside the axis of the others was accepted while
  ## one inside the others was rejected as "not in the plot range".
  dat <- data.frame(x = 1:30,
                    y = c(2 + (1:10)/10, 29 + (1:10)/10, 3 + (1:10)/10),
                    row = rep(c('A', 'B', 'C'), each = 10))
  p <- ggplot(dat, aes(x, y)) + geom_point() +
         facet_grid(row ~ ., scales = 'free_y')

  expect_equal(ggbreak:::ggrange2(p, 'y')$axis_range, range(dat$y))

  ## a break outside the first panel and inside the axis is the reported case,
  ## and it has to be accepted now
  expect_silent(grid.draw(p + scale_y_break(c(6, 25)), recording = FALSE))

  ## while a break outside the whole axis is still reported
  expect_error(grid.draw(p + scale_y_break(c(0.1, 1)), recording = FALSE),
               'not in the plot range')

  ## a plot that is not faceted has a single scale, and its range is unchanged
  pf <- ggplot(dat, aes(x, y)) + geom_point() + facet_grid(row ~ .)
  expect_equal(ggbreak:::ggrange2(pf, 'y')$axis_range, range(dat$y))
})

test_that('the y range of `geom_signif` is not mistaken for a valid range (#43)', {
  ## in the report the plot has a single y value, so the y range is degenerate
  ## and the break at c(10, 20) is outside of it
  p <- suppressWarnings(
    ggplot() +
      ggsignif::geom_signif(
        mapping = aes(xmin = 'A', xmax = 'B', y_position = 50,
                      annotations = 'Hello, world!'),
        data = data.frame(), manual = TRUE)
  )

  expect_error(grid.draw(p + scale_y_break(c(10, 20)), recording = FALSE),
               'not in the plot range')
})

test_that('breaks inside the plot range still work (#43)', {
  dat <- data.frame(x = 1:10, y = c(1:9, 60))
  p <- ggplot(dat, aes(x, y)) + geom_point()

  ## a single break, several breaks, a break on the other axis and breaks
  ## combined with a reversed, a transformed and a secondary axis
  plots <- list(
    p + scale_y_break(c(20, 50)),
    p + scale_y_break(c(20, 30)) + scale_y_break(c(40, 55)),
    p + scale_x_break(c(5, 7)),
    p + scale_y_reverse() + scale_y_break(c(20, 50)),
    p + scale_y_continuous(sec.axis = dup_axis()) + scale_y_break(c(20, 50))
  )

  for (pl in plots) {
    expect_silent(grid.draw(pl, recording = FALSE))
  }

  ## a break at the boundary of the range is fine, it just yields an empty panel
  expect_silent(grid.draw(p + scale_y_break(c(1, 5)), recording = FALSE))
})
