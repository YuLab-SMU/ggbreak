library(ggplot2)
library(ggbreak)
library(grid)

# Test-1: check that the new storage model for axis breaks is working as intended
# - This is a regression test to ensure that the new storage model (using plot attributes)
#   is correctly implemented and does not break existing functionalities.
# - It also checks that the new dual-axis break feature is working as expected.
#   This checks that the new storage model can handle multiple axis breaks

# ===== OLD: check consistency with existing functionalities
cat("=== Test 1: single x-break (regression) ===\n")
p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + scale_x_break(c(3, 4))
cat("  axis_break_x present:", !is.null(attr(p1, "axis_break_x")), "\n")
cat("  axis_break_y present:", !is.null(attr(p1, "axis_break_y")), "\n")
invisible(grid.draw(p1, recording = FALSE))
cat("  Rendered OK\n")

cat("\n=== Test 2: single y-break (regression) ===\n")
p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point() + scale_y_break(c(25, 35))
cat("  axis_break_x present:", !is.null(attr(p2, "axis_break_x")), "\n")
cat("  axis_break_y present:", !is.null(attr(p2, "axis_break_y")), "\n")
invisible(grid.draw(p2, recording = FALSE))
cat("  Rendered OK\n")

cat("\n=== Test 3: multiple x-breaks on same axis (regression) ===\n")
p3 <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
    scale_x_break(c(3, 4)) + scale_x_break(c(5, 6))
ab3 <- attr(p3, "axis_break_x")
cat("  axis_break_x class:", class(ab3), "\n")
cat("  axis_break_x length:", length(ab3), "\n")
invisible(grid.draw(p3, recording = FALSE))
cat("  Rendered OK\n")

# ===== NEW: dual-axis breaks 
cat("\n=== Test 4: DUAL AXIS - x + y break (THE NEW FEATURE) ===\n")
p4 <- tryCatch({
    ggplot(mpg, aes(displ, hwy)) + geom_point() +
        scale_x_break(c(3, 4)) + scale_y_break(c(25, 35))
}, error = function(e) {
    cat("  ERROR creating plot:", e[["message"]], "\n")
    NULL
})
if (!is.null(p4)) {
    cat("  Plot created successfully!\n")
    cat("  axis_break_x present:", !is.null(attr(p4, "axis_break_x")), "\n")
    cat("  axis_break_y present:", !is.null(attr(p4, "axis_break_y")), "\n")
    cat("  class:", paste(class(p4), collapse = ", "), "\n")
    tryCatch(
        invisible(grid.draw(p4, recording = FALSE)),
        error = function(e) cat("  Render error:", e[["message"]], "\n")
    )
    cat("  Render completed\n")
}

cat("\n=== Test 5: y + x order (symmetric) ===\n")
p5 <- tryCatch({
    ggplot(mpg, aes(displ, hwy)) + geom_point() +
        scale_y_break(c(25, 35)) + scale_x_break(c(3, 4))
}, error = function(e) {
    cat("  ERROR:", e[["message"]], "\n")
    NULL
})
if (!is.null(p5)) {
    cat("  Plot created successfully!\n")
    cat("  axis_break_x present:", !is.null(attr(p5, "axis_break_x")), "\n")
    cat("  axis_break_y present:", !is.null(attr(p5, "axis_break_y")), "\n")
}

cat("\n=== Test 6: adding layers to dual-axis break plot ===\n")
p6 <- tryCatch({
    ggplot(mpg, aes(displ, hwy)) + geom_point() +
        scale_x_break(c(3, 4)) + scale_y_break(c(25, 35)) +
        labs(title = "Dual break") + theme_minimal()
}, error = function(e) {
    cat("  ERROR:", e[["message"]], "\n")
    NULL
})
if (!is.null(p6)) {
    cat("  Layers added to dual-break plot OK\n")
}

cat("\n=== All Phase 1 tests completed ===\n")
