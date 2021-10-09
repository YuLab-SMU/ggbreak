# ggbreak 0.0.6

+ remove xy intercept label and compatible with `xlim` and `ylim` (2021-09-24, Fri, #29)
+ import `is.ggbreak` and `ggbreak2ggplot` from the ggfun package (2021-09-16, Thu)
+ fixed x and y labels and introduced `scale_xy_expand` option (2021-09-15, Wed; #26)
+ applying text and title setting from pre-break graph to breaked graph (2021-09-09, Thu)

# ggbreak 0.0.5

+ change according to `aplot::plot_list` (2021-09-03, Fri)

# ggbreak 0.0.4

+ set `expand=FALSE` (2021-08-14, Sat) 
+ support setting e.g. `legend.position = "bottom"` via `theme()` (2021-05-31, Mon)
    - manual legend position (e.g. `legend.positon = c(.1, .2)`) is not supported currently
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

