## theme(plot.margin = ...) is honoured, #71.
##
## The outer ggplot that holds the annotation_custom used to keep the
## `theme_void()` from `ggplotify::as.ggplot()` and so dropped the user's
## `plot.margin`; the inner subplots had `plot.margin = 0` and the user
## margin never made it to the final draw.
##
## `mtcars$cyl` is numeric, so `aes(x = cyl)` leaves ggplot2 unable to tell which
## aesthetic the boxplot runs along: it warns ("Orientation is not uniquely
## specified ... did you forget `aes(group = ...)`?") and then draws one box
## spanning x = 4 to x = 8 instead of one per cylinder.  The fixtures below name
## the group, which is what the warning asks for; the assertion is on the theme,
## so nothing about it depends on the boxes.

user_margin <- margin(t = 2, l = 3, r = 2, b = 2, "cm")

test_that("an explicit plot.margin reaches the outer ggplot", {
    p <- ggplot(mtcars, aes(x = cyl, y = mpg, group = cyl)) + geom_boxplot() +
         theme(plot.margin = user_margin) + scale_y_break(c(15, 20))
    g <- grid::grid.draw(p, recording = FALSE)
    expect_equal(g$theme$plot.margin, user_margin)
})

test_that("the default ggplot2 plot.margin is left alone", {
    ## `theme_gray()`'s default is `margin(c(5.5, 5.5, 5.5, 5.5), "pt")`.
    ## Keeping it identical avoids shrinking the panel by ~7px on each
    ## side for users who never set `plot.margin`.
    p <- ggplot(mtcars, aes(x = cyl, y = mpg, group = cyl)) + geom_boxplot() +
         scale_y_break(c(15, 20))
    g <- grid::grid.draw(p, recording = FALSE)
    expect_equal(g$theme$plot.margin, ggplot2::theme_gray()$plot.margin)
})

test_that("the same applies to scale_y_cut", {
    p <- ggplot(mtcars, aes(x = cyl, y = mpg, group = cyl)) + geom_boxplot() +
         theme(plot.margin = user_margin) + scale_y_cut(15)
    g <- grid::grid.draw(p, recording = FALSE)
    expect_equal(g$theme$plot.margin, user_margin)
})

test_that("the same applies to scale_wrap", {
    p <- ggplot(mtcars, aes(x = mpg, y = cyl)) + geom_point() +
         theme(plot.margin = user_margin) + scale_wrap(3)
    g <- grid::grid.draw(p, recording = FALSE)
    expect_equal(g$theme$plot.margin, user_margin)
})