library(grid)
library(ggplot2)

## #35: `geom_text_repel()` lays its labels out in the coordinate system of the
## panel and keeps them inside it, so a label whose point lies outside the window
## of a subplot is moved to the edge of that subplot instead of being clipped
## away.  ggbreak draws the same plot once per window, so every label used to be
## drawn in every subplot, piled up along the edge of the ones it does not
## belong to.

## Read the labels that ggrepel draws, grouped by subplot, in coordinates
## relative to the panel they are drawn in.  Every subplot has its own repel
## grob, which is the parent of its labels, so the parent path identifies the
## subplot, and normalising by the scale of the enclosing viewport keeps the
## check independent of whether a panel reports data units or `[0, 1]`.
collect_repel_labels <- function(grob, path = '', scale = NULL, out = list()) {
  if (!is.null(grob$vp) && !is.null(grob$vp$yscale) && !is.null(grob$vp$xscale)) {
    scale <- list(x = grob$vp$xscale, y = grob$vp$yscale)
  }
  if (inherits(grob, 'text') && grepl('/textrepelgrob', path) && !is.null(scale)) {
    ## the position of a label is a compound unit, whose `as.numeric()` is not
    ## its value, so read it in the `native` unit of the panel instead
    pos <- vapply(c('x', 'y'), function(a) {
      v <- if (a == 'x') {
        grid::convertX(grob$x, 'native', valueOnly = TRUE)
      } else {
        grid::convertY(grob$y, 'native', valueOnly = TRUE)
      }
      s <- scale[[a]]
      if (length(v) == 1 && length(s) == 2 && diff(s) != 0) (v - s[1]) / diff(s) else NA_real_
    }, numeric(1))
    key <- sub('/[^/]+$', '', path)
    out[[key]] <- rbind(out[[key]],
                        data.frame(label = grob$label, x = pos[['x']], y = pos[['y']],
                                   stringsAsFactors = FALSE))
  }
  if (inherits(grob, 'gTree') && length(grob$children) > 0) {
    for (i in seq_along(grob$children)) {
      out <- collect_repel_labels(grob$children[[i]],
                                  paste0(path, '/', names(grob$children)[i]), scale, out)
    }
  }
  out
}

## The labels a panel really shows are the ones ggrepel left inside it, as one
## string per panel so that the panels can be compared as a set.  The small
## tolerance covers a label at the very edge of the panel, whose box is then
## partly clipped, while a label that is pushed out of the panel is a whole
## panel height away.
shown_panels <- function(plot, axis) {
  labels <- collect_repel_labels(grid.force(ggplotGrob(grid.draw(plot, recording = FALSE))))
  unname(vapply(labels, function(d) {
    pos <- d[[axis]]
    paste(sort(d$label[pos >= -0.05 & pos <= 1.05]), collapse = '')
  }, character(1)))
}

test_that('a repel label is drawn only in the subplot that holds its point (#35)', {
  skip_if_not_installed('ggrepel')

  dat <- data.frame(x = 1:6, y = c(1, 2, 3, 12, 13, 14), l = letters[1:6])
  base <- ggplot(dat, aes(x, y)) + geom_point() +
          ggrepel::geom_text_repel(aes(label = l), seed = 1)

  ## `a`, `b` and `c` sit in the lower panel, `d`, `e` and `f` in the upper one;
  ## without the fix both panels show all six labels
  expect_equal(sort(shown_panels(base + scale_y_break(c(5, 11)), 'y')),
               c('abc', 'def'))

  ## the same has to hold for an x break: only `a` and `b` are on the left of
  ## the break and only `f` is on the right of it, `c` to `e` sit in the gap
  expect_equal(sort(shown_panels(base + scale_x_break(c(2.5, 5.5)), 'x')),
               c('ab', 'f'))

  ## and for `geom_label_repel()`, which shares the geometry
  expect_equal(sort(shown_panels(ggplot(dat, aes(x, y)) + geom_point() +
                                 ggrepel::geom_label_repel(aes(label = l), seed = 1) +
                                 scale_y_break(c(5, 11)), 'y')),
               c('abc', 'def'))
})

test_that('a repel label is released from the panel limits of the subplots (#35)', {
  skip_if_not_installed('ggrepel')

  dat <- data.frame(x = 1:6, y = c(1, 2, 3, 12, 13, 14), l = letters[1:6])
  repel <- function(...) {
    ggplot(dat, aes(x, y)) + geom_point() +
      ggrepel::geom_text_repel(aes(label = l), seed = 1, ...)
  }
  release <- ggbreak:::release_repel_labels

  ## the broken axis is opened up, the other one keeps the default
  released <- release(repel(), 'y')
  expect_equal(released$layers[[2]]$geom_params$ylim, c(-Inf, Inf))
  expect_length(released$layers[[2]]$geom_params$xlim[!is.na(released$layers[[2]]$geom_params$xlim)], 0)

  ## the plot of the user must not be modified by drawing it
  p <- repel()
  invisible(grid.draw(p + scale_y_break(c(5, 11)), recording = FALSE))
  expect_true(all(is.na(p$layers[[2]]$geom_params$ylim)))

  ## an explicit `ylim` of the user keeps its meaning
  expect_equal(release(repel(ylim = c(0, 20)), 'y')$layers[[2]]$geom_params$ylim, c(0, 20))

  ## a layer that is not a repel one is left alone
  plain <- ggplot(dat, aes(x, y)) + geom_point() + geom_text(aes(label = l))
  expect_identical(release(plain, 'y'), plain)
})
