library(ggplot2)
library(ggbreak)

## ---------------------------------------------------------------------------
## A global theme draws like the same inline theme
## ---------------------------------------------------------------------------

## #57: a theme that is set globally (`theme_set()`) used to be applied to the
## *outer* ggplot as well.  That outer plot is only the container of the
## assembled subplots -- it is the panel that carries the `patchwork` figure --
## so a global `theme_bw()` drew a second panel border around the whole figure
## and a global `theme_classic()` a second pair of axis lines outside the plot.
## The panels, their borders and their axis lines belong to the subplots, and
## they have to look the same however the theme reaches them.
##
## The invariant these tests pin is that a theme applied with `theme_set()`
## renders exactly like the same theme applied to the plot: the figure is drawn
## from the same grobs either way.  Before the fix the global theme added a
## border, a background and axis lines at the outer level, so the two signatures
## differed.

## Names and classes of every grob the figure really draws, with the running
## numbers that `ggplot2` puts in a grob name stripped, so that two figures can
## be compared without depending on the order in which the grobs were created.
## Blank elements are skipped: they are the ones this issue is about.  Gtables
## are skipped too -- they are containers, and `ggplot2` numbers them by
## position.
grob_signature <- function(plot) {
    gt <- grid::grid.force(
        ggplot2::ggplotGrob(grid::grid.draw(plot, recording = FALSE))
    )
    out <- character(0)
    walk <- function(z) {
        if (!inherits(z, "zeroGrob") && !inherits(z, "gtable")) {
            nm <- sub("\\.[0-9]+$", "", if (is.null(z$name)) "" else z$name)
            out <<- c(out, paste(class(z)[1], nm))
        }
        if (inherits(z, "gtable")) {
            for (i in seq_along(z$grobs)) walk(z$grobs[[i]])
        } else if (inherits(z, "gTree") && length(z$children) > 0) {
            for (i in seq_along(z$children)) walk(z$children[[i]])
        }
        invisible(NULL)
    }
    walk(gt)
    sort(out)
}

## Draw the plot once with `th` set globally and once with the very same theme
## added to the plot, and return both signatures.  The global theme is put back
## to the default before the inline figure is drawn, otherwise it would reach
## the outer container of that figure as well and the two would agree whatever
## the package does.
signature_set_and_inline <- function(p, th) {
    theme_set(th)
    set_sig <- grob_signature(p)
    theme_set(theme_grey())
    inline_sig <- grob_signature(p + th)
    list(set = set_sig, inline = inline_sig)
}

set.seed(42)
d <- data.frame(x = 1:20, y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22))
base <- ggplot(d, aes(x, y)) + geom_point()

themes <- list(bw = theme_bw(), classic = theme_classic(),
               minimal = theme_minimal(), linedraw = theme_linedraw())

test_that("a globally set theme draws the same figure as the same inline theme, #57", {
    on.exit(theme_set(theme_grey()), add = TRUE)
    for (nm in names(themes)) {
        s <- signature_set_and_inline(base + scale_y_break(c(7, 17)), themes[[nm]])
        expect_identical(s$set, s$inline, info = nm)
    }
})

test_that("the invariant holds for every break flavour, #57", {
    on.exit(theme_set(theme_grey()), add = TRUE)
    broken <- list(
        y_break = base + scale_y_break(c(7, 17)),
        x_break = base + scale_x_break(c(7, 17)),
        y_cut = base + scale_y_cut(c(7, 17)),
        x_cut = base + scale_x_cut(c(7, 17)),
        wrap = base + scale_wrap(n = 3),
        dual = base + scale_x_break(c(7, 17)) + scale_y_break(c(7, 17)),
        flip = base + coord_flip() + scale_y_break(c(7, 17))
    )
    for (nm in names(broken)) {
        s <- signature_set_and_inline(broken[[nm]], themes$bw)
        expect_identical(s$set, s$inline, info = nm)
    }
})

test_that("an unbroken plot is left alone, #57", {
    on.exit(theme_set(theme_grey()), add = TRUE)
    s <- signature_set_and_inline(base, themes$classic)
    expect_identical(s$set, s$inline)
})

test_that("only the subplots carry a panel border, never the container, #57", {
    on.exit(theme_set(theme_grey()), add = TRUE)
    theme_set(theme_bw())
    sig <- grob_signature(base + scale_y_break(c(7, 17)))
    ## two subplots, each with the border of the global theme, and no third one
    expect_identical(sum(sig == "rect panel.border..rect"), 2L)
})

test_that("the default theme is untouched, #57", {
    on.exit(theme_set(theme_grey()), add = TRUE)
    sig <- grob_signature(base + scale_y_break(c(7, 17)))
    ## the default theme has no panel border to begin with
    expect_identical(sum(sig == "rect panel.border..rect"), 0L)
    ## and the figure still has an opaque background of its own
    expect_identical(sum(sig == "rect plot.background..rect"), 1L)
})

## ---------------------------------------------------------------------------
## The figure paints no background of its own
## ---------------------------------------------------------------------------

## #52: a broken plot could not be superimposed on another plot.
##
## `patchwork` gives the assembled figure a background of its own, taken from the
## default theme, and that rectangle is opaque.  Two plots stacked with
## `annotation_custom()` therefore hid each other: the `plot.background` the user
## set on the plot never reached the figure, because the background of the figure
## was painted by the assembly instead of by the outer ggplot that holds it.
##
## The contract these tests pin: the assembled figure paints no background of its
## own, and the background of the figure is the one the user asked for.

drawn_gtable <- function(plot) {
    grid::grid.force(
        ggplot2::ggplotGrob(grid::grid.draw(plot, recording = FALSE))
    )
}

## The gtable that `patchwork` assembles the subplots into.  It is the one that
## carries the `panel-area` entry, and it is nested inside the panel of the outer
## ggplot.
patch_gtable <- function(plot) {
    found <- NULL
    walk <- function(z) {
        if (!is.null(found)) return(invisible(NULL))
        if (inherits(z, "gtable")) {
            if ("panel-area" %in% z$layout$name) {
                found <<- z
                return(invisible(NULL))
            }
            for (i in seq_along(z$grobs)) walk(z$grobs[[i]])
        } else if (inherits(z, "gTree") && length(z$children) > 0) {
            for (i in seq_along(z$children)) walk(z$children[[i]])
        }
        invisible(NULL)
    }
    walk(drawn_gtable(plot))
    found
}

## The fill of every grob that paints a background and really paints something.
## A transparent fill is what the user asked for in #52, so it does not count.
opaque_background_fills <- function(plot) {
    out <- character(0)
    walk <- function(z) {
        nm <- if (is.null(z$name)) "" else z$name
        if (inherits(z, "rect") && grepl("background", nm)) {
            fill <- z$gp$fill
            if (!is.null(fill) && !anyNA(fill) &&
                any(grDevices::col2rgb(fill, alpha = TRUE)[4, ] > 0)) {
                out <<- c(out, fill)
            }
        }
        if (inherits(z, "gtable")) {
            for (i in seq_along(z$grobs)) walk(z$grobs[[i]])
        } else if (inherits(z, "gTree") && length(z$children) > 0) {
            for (i in seq_along(z$children)) walk(z$children[[i]])
        }
        invisible(NULL)
    }
    walk(drawn_gtable(plot))
    out
}

set.seed(2022 - 8 - 23)
d <- data.frame(x = 1:20,
                y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22))
base <- ggplot(d, aes(x, y)) + geom_point()

transparent <- theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
)

test_that("the assembled figure paints no background of its own, #52", {
    p <- base + scale_y_break(c(7, 17))
    pg <- patch_gtable(p)
    expect_false(is.null(pg))
    i <- which(pg$layout$name == "background")
    expect_length(i, 1L)
    expect_s3_class(pg$grobs[[i]], "zeroGrob")
})

test_that("a transparent plot.background really makes the figure transparent, #52", {
    p <- base + transparent + scale_y_break(c(7, 17))
    expect_length(opaque_background_fills(p), 0L)
})

test_that("the same holds for every break flavour, #52", {
    broken <- list(
        y_break = base + scale_y_break(c(7, 17)),
        x_break = base + scale_x_break(c(7, 17)),
        y_cut = base + scale_y_cut(c(7, 17)),
        wrap = base + scale_wrap(n = 3)
    )
    for (nm in names(broken)) {
        p <- broken[[nm]] + transparent
        expect_length(opaque_background_fills(p), 0L)
    }
})

test_that("a figure that asks for no background keeps a white one by default, #52", {
    p <- base + scale_y_break(c(7, 17))
    fills <- opaque_background_fills(p)
    ## the outer ggplot paints the figure white, as `ggplot2` does
    expect_true("white" %in% fills)
})

## ---------------------------------------------------------------------------
## plot.margin reaches the figure
## ---------------------------------------------------------------------------

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
