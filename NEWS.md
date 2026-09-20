# ggbreak 0.1.7.020

+ `scale_x_break()` and `scale_y_break()` gained a `bridge` argument, off by default; with `bridge = TRUE` the piece of a line that the break hides is drawn in the space between the windows, so a line that crosses the break reads as one line instead of two. A line grob keeps the geometry of the whole line and is only clipped when it is drawn, so where a line leaves one window and enters the next can be read off the grob, and the space `space` opens is a cell of the assembled figure that spans exactly those two edges. The lines of `geom_line()`, `geom_path()` and `geom_step()` are bridged, and only for a single break on a continuous axis; a break on a discrete axis and a plot that is faceted along the broken axis are left alone. The default is unchanged, so nothing moves unless it is asked for (2026-09-20, Sun, #33)

+ a broken plot is no longer misaligned with the other plots of a `patchwork` or `cowplot` layout; the layout aligns the plots by their `panel` cell, and the assembled figure is one panel that also held the axis of its windows, so a plot drawn beside it was aligned against a panel that already carried the axis labels and came out wider than the plot area it sits next to -- 44 pixels on a 6 by 3 inch figure, and 0 to 1 pixel now, for the break, the cut and the wrap scales, with `coord_flip()`, with a dual break, with a legend and with facets. The axis of the assembled figure is moved out of the panel into the cell the layout expects an axis in, and the room it took inside the figure is kept, so nothing is drawn differently; this is applied to the gtable `ggplotGrob()` hands out only, and `print()` and `ggsave()` are byte-for-byte unchanged (2026-09-20, Sun, #46)
+ a broken plot no longer loses its break when it is combined with other plots; `patchwork` and `cowplot` do not draw a plot the way `print()` does, they call `ggplotGrob()`, and that builds the plot from `ggplot_build()` without ever reading the break, so `plt2 + plt1` came out as two plain plots -- the break was dropped silently, without a warning, and the same happened to `plot_grid()`, to `scale_wrap()` and to the cut scales. `ggplot_build()` and `ggplot_gtable()` are generics even though `ggplotGrob()` is not, so a broken plot now tags the object it is built into and is drawn as a whole from there; a regression test covers the break, the wrapping and the cutting, and the drawing is left to `print()` unchanged (2026-09-20, Sun, #46)
+ the axis range of a plot is no longer read off the first panel alone; with `facet_grid(scales = "free_y")` every panel has a scale of its own and the first one holds the range of the first panel only, so the windows were built from that range and every point outside it was dropped (the facet whose data sat high came out empty), while a break that was inside the first panel but outside the axis of the others was accepted and one inside the others was rejected as "not in the plot range". A broken axis is shared by the facets, so the windows are drawn on the axis of all the panels together; a regression test covers both the range and the two breaks (2026-09-20, Sun)
+ added regression tests that a slice of a facet grid keeps the axis that belongs to its own facet row (column), that a slice claims a share of the figure in one direction only, that the assembled figure keeps the share of the figure each window is given, and that a `facet_wrap()` is left alone (2026-09-20, Sun)
+ fixed the nesting of a facet grid; the first attempt dropped the x axis of a facet column, so a `scale_x_break()` on a faceted plot came out with no tick labels at all, sized the panels against every facet column instead of against their own and left them too small, and did not keep the share of the figure that each window is given. The pieces are now bound into a single gtable instead of being handed to `patchwork` as opaque panels, and a `facet_wrap()` is left alone because it has no facet rows or columns to nest into and puts a strip above every panel (2026-09-20, Sun, #55, #17)
+ a broken facet grid no longer separates a facet row from its other windows; the subplot of a faceted plot carries the whole facet grid, so the windows used to be stacked *outside* the facets and the rows came out A B A B instead of A A B B, for `facet_grid()`, and with `coord_flip()` the facet columns are kept together instead, which is the case reported in #55 and #17 (2026-09-20, Sun, #55, #17)
+ added regression tests that the windows of a facet row (column) stay together, that a plot which is not faceted along the broken axis is assembled as before, and that the legend is still drawn exactly once when the pieces are reassembled (2026-09-20, Sun)
+ a theme that is set with `theme_set()` no longer draws a second panel border, a second pair of axis lines or a panel background around the whole figure; those elements belong to the subplots, and the outer ggplot that carries the assembled figure was taking them from the global theme by accident, so `theme_set(theme_bw())` drew a rectangle around the figure and `theme_set(theme_classic())` two extra lines outside the plot (2026-09-20, Sun, #57)
+ the assembled figure no longer paints an opaque background of its own; `patchwork` took that background from the default theme, so a broken plot could not be superimposed on another one with `annotation_custom()` (it hid whatever was drawn below it) and the `plot.background` set on the plot never reached the figure (2026-09-20, Sun, #52)
+ added regression tests that a globally set theme renders like the same theme applied to the plot, and that the assembled figure paints no background of its own (2026-09-20, Sun)
+ added a regression test that the axis title of a broken plot stays above the collected legend with `theme(legend.position = "bottom")`, for `scale_x_break()`, `scale_y_break()`, a dual break, `coord_flip()`, `scale_wrap()` and the cut scales; the report in #85 is the same defect that #53 fixed, the reporter was on an earlier version (2026-09-19, Sat, #85)
+ drawn the axis title of a broken plot above the legend when `theme(legend.position = "bottom")` is used; `patchwork` collects the legend at the bottom of the assembled figure, which is the panel of the outer ggplot that carries the axis title, so the legend used to be drawn on top of the title instead of underneath it (2026-09-18, Fri, #53)
+ `patchwork` and `gtable` are now declared in `Imports`; both were already needed at run time to assemble the subplots (2026-09-18, Fri)
+ document how to make the ticks line up across the subplots: the `breaks` argument of `scale_x_break()`/`scale_y_break()` is the place where the axis is cut, and the ticks are set with `breaks` in the continuous scale, which every subplot then filters to its own range (2026-09-18, Fri, #66)
+ fixed adding a coord, theme, scale, facet or layer to a plot that uses `scale_wrap()` or `scale_x_cut()`/`scale_y_cut()`; `ggplot_add()` re-dispatched to itself for ever and R died with a C stack overflow (2026-09-17, Thu)
+ kept the limits that `coord_cartesian()` or `coord_flip()` set on the axis that is *not* broken, so `scale_y_break()` no longer discards `coord_cartesian(xlim = ...)`, and `scale_x_break()`, `scale_wrap()` and `scale_x_cut()`/`scale_y_cut()` no longer discard the limits of the other axis (2026-09-17, Thu, #59)
+ honoured `theme(plot.margin = ...)` when the plot is wrapped for the final draw; previously the `theme_void()` from `ggplotify::as.ggplot()` swallowed the user margin so `plot.margin` had no effect on a broken plot (2026-09-17, Thu, #71)
+ fixed an extra blank page when a broken plot is saved with `pdf()` or `ggsave()`; `ggbreak` now starts the page before it builds the subplots instead of leaving the page that building opens behind (2026-09-17, Thu, #73)
+ document the breaks and cuts on a discrete axis, on a `Date` or datetime axis and the use of `ggrepel` in the vignette (2026-09-17, Thu)
+ fixed the axis losing its tick labels when `scale_wrap()`, `scale_x_cut()` or `scale_y_cut()` is used together with `coord_flip()` (2026-09-17, Thu)
+ a `Date` or datetime axis is no longer drawn as numbers when the plot relies on the scale that `ggplot2` creates from the data instead of calling `scale_x_date()`, `scale_x_datetime()` or their `y` counterparts itself (2026-09-17, Thu, #84)
+ fixed `scale_x_cut()` and `scale_y_cut()` failing with "non-numeric argument to binary operator" when the axis uses a transformed scale such as `scale_y_log10()` (2026-09-17, Thu)
+ fixed the labels of `ggrepel::geom_text_repel()` and `geom_label_repel()` being repeated in every subplot; a label is now only drawn in the subplot that holds its point (2026-09-17, Thu, #35)
+ support `scale_x_break()`, `scale_y_break()`, `scale_x_cut()` and `scale_y_cut()` on a discrete axis; the break points are level names and a break between two adjacent levels only inserts a gap (2026-09-16, Wed, #68)
+ `scale_x_break()`, `scale_y_break()`, `scale_x_cut()` and `scale_y_cut()` accept break points of a datetime axis given as `Date`, character or numeric, in addition to `POSIXct` (2026-09-16, Wed, #84)
+ report an error instead of silently flipping the axis of a subplot when a break interval is not in the plot range (2026-09-16, Wed, #43)
+ fixed extra lines drawn outside the plot and a duplicated axis when the plot uses a scale without `sec.axis` (2026-09-16, Wed, #64)
+ fixed duplicated datetime x-axis labels when using `scale_y_break()` (2026-08-07, Fri, #83)

# ggbreak 0.1.7

+ add test unit (2026-03-18, Wed)
+ support axis break symbols (2026-03-18, Wed)
+ support multiple axis break (#81, thanks @abertran3)

# ggbreak 0.1.6

+ compatible with ggplot2 v=4.0.0 (2025-08-24, Sun, #80)

# ggbreak 0.1.3

+ use `yulab_msg()` for startup message (2025-01-08, Wed)

# ggbreak 0.1.2

+ update CITATION file to use `c()` and `bibentry()` instead of old-style `personList()` and `citEntry()` (2023-06-26, Mon) 
+ move `theme_fp()` to the 'ggfun' package (2023-06-24, Sat)

# ggbreak 0.1.1

+ update vignette with example to place legend at any position (2022-10-15, Sat)

# ggbreak 0.1.0

+ fixed a bug in drawing dual axis (2022-06-01, Wed, #48, #49)

# ggbreak 0.0.9

+ `scale_wrap()` supports using categorical variable as x-axis (2022-01-16, Sun, #41)
+ support double axes (2022-01-16, Sun, #40)

# ggbreak 0.0.8

+ introduce `space` argument (2022-01-11, Tue)

# ggbreak 0.0.7

+ introduce `expand` argument (2021-10-22, Fri, #30)
+ add citation (2021-10-13, Wed)

# ggbreak 0.0.6

+ remove xy intercept label and compatible with `xlim` and `ylim` (2021-09-24, Fri, #29)
+ import `is.ggbreak` and `ggbreak2ggplot` from the ggfun package (2021-09-16, Thu)
+ fixed x and y labels and introduced `scale_xy_expand` option (2021-09-15, Wed; #26)
+ apply text and title setting from pre-break graph to breaked graph (2021-09-09, Thu)

# ggbreak 0.0.5

+ change according to `aplot::plot_list` (2021-09-03, Fri)

# ggbreak 0.0.4

+ set `expand=FALSE` (2021-08-14, Sat) 
+ support setting e.g. `legend.position = "bottom"` via `theme()` (2021-05-31, Mon)
    - manual legend position (e.g. `legend.position = c(.1, .2)`) is not supported currently
+ if passing `recording = FALSE` to `grid.draw` method, only graph object return without plotting the figure (2021-05-24, Mon)
 
# ggbreak 0.0.3

+ compatible with `patchwork` for axis break plots + gg plot (2021-05-21, Fri)
+ supports setting axis tick labels (2021-05-20, Tue)
+ `scale_x_cut` and `scale_y_cut` functions (2021-05-18, Tue)
+ `scale_wrap` function and support of using date object as axis (2021-05-15, Sat)
+ add vignette (2021-05-14, Fri)
+ support `coord_flip`, `facet_grid` and `facet_wrap` (2021-05-14, Fri)
+ support `legend` and all labels (x, y, title, subtitle, caption and tag)

# ggbreak 0.0.2

+ compatible with `patchwork`, now p1 + p2 (or other operations) will align two ggbreak plot (2021-05-11, Tue)  
+ support multiple `scale_x_break` or `scale_y_break` and compatible with reverse axis 

# ggbreak 0.0.1

+ initial version of ggbreak that provide `scale_x_break` and `scale_y_break` (2021-05-10, Mon)

