# ------------------------------------------------------------------------------
# Figure 5: fleet characteristics (A), fleet efficiency (B),
#           environmental indicators (C) and control and safety (D)
# One script, no post-editing in Inkscape needed.
# ------------------------------------------------------------------------------

library(ggplot2)
library(patchwork)

# ---- colours ------------------------------------------------------------------
regions <- c("Alaska", "Northwest Europe", "Northwest Atlantic")
col_bor <- c("red", "black", "blue")
col_in  <- c("#FF000026", "#fff7bc", "#0000FF26")
names(col_bor) <- names(col_in) <- regions

# ---- data (Table 2) -------------------------------------------------------------
#                  Alaska   NW Europe  NW Atlantic
shelf      <- c(   1160,     1421,      375)     # shelf area (1000 km2) used for per-km2 values

landings   <- c(   2525,     3876,     1281)     # 1000 t per year
kwdays     <- c(    128,      285,      123)     # million KW sea days
seadays    <- c(   0.46,     1.25,     0.22)     # million sea days
vessels    <- c(   9420,    15115,     6626)     # number of vessels
fuel       <- c(    393,      990,      452)     # million liter

value      <- c(  2.363,    5.021,    4.412)     # billion US$
land_fuel  <- c(    6.4,      3.9,      2.8)     # kg landings per liter fuel
land_kw    <- c(     20,       14,       10)     # kg landings per KW sea day
val_fuel   <- c(    6.8,      6.5,       16)     # value of landings / fuel cost
income     <- c(     36,       30,       24)     # 1000 US$ per year

discards   <- c(     76,      301,      110)     # 1000 t per year
disc_pct   <- discards / (discards + landings) * 100   # % of total catch
mammals    <- c(    0.7,      0.1,      0.6)     # t per km2
demersal   <- c(     84,       15,       15)     # t per km2
trawled    <- c(    4.3,     28.6,       17)     # % of area < 1000 m bottom trawled

fatal      <- c(    0.3,      1.8,      0.3)     # fatal injuries per 1000 harvesters per year
observer   <- c(39932 / (0.46e6) * 100, NA, 5907 / (0.22e6) * 100)  # observer days per 100 sea days

# ---- helper: segment ("rose") plot in Cartesian coordinates ---------------------
# values: matrix (categories x regions); each category is scaled to its maximum
rose_plot <- function(values, labels, title, divider_col = "grey85",
                      dotted_vertical = FALSE, label_r = 1.2, label_size = 4.2) {
  n     <- nrow(values)
  width <- 360 / n                         # degrees per category
  bar_w <- width * 0.22                    # angular width of one wedge
  offs  <- c(-0.27, 0, 0.27) * width       # wedge positions within a category (clockwise)
  
  scaled <- sweep(values, 1, apply(values, 1, max, na.rm = TRUE), "/")
  
  wedges <- do.call(rbind, lapply(seq_len(n), function(i) {
    do.call(rbind, lapply(1:3, function(j) {
      r  <- scaled[i, j]
      th <- seq((i - 0.5) * width + offs[j] - bar_w / 2,
                (i - 0.5) * width + offs[j] + bar_w / 2, length.out = 30) * pi / 180
      data.frame(x = c(0, r * sin(th), 0), y = c(0, r * cos(th), 0),
                 id = paste(i, j), region = regions[j])
    }))
  }))
  wedges$region <- factor(wedges$region, levels = regions)
  
  # divider lines between categories
  th_div <- (0:(n - 1)) * width * pi / 180
  divs   <- data.frame(x = 0, y = 0, xend = 1.08 * sin(th_div), yend = 1.08 * cos(th_div))
  
  # labels around the plot
  th_lab <- ((1:n) - 0.5) * width * pi / 180
  labs_df <- data.frame(x = label_r * sin(th_lab), y = label_r * cos(th_lab), lab = labels,
                        hjust = ifelse(sin(th_lab) > 0.25, 0, ifelse(sin(th_lab) < -0.25, 1, 0.5)))
  
  p <- ggplot() +
    geom_segment(data = divs, aes(x = x, y = y, xend = xend, yend = yend),
                 colour = divider_col, linewidth = 0.4) +
    geom_polygon(data = wedges, aes(x = x, y = y, group = id, fill = region, colour = region),
                 linewidth = 0.4) +
    scale_fill_manual(values = col_in, guide = "none") +
    scale_colour_manual(values = col_bor, guide = "none") +
    coord_fixed(clip = "off") +
    theme_void() +
    labs(title = title) +
    theme(plot.title = element_text(size = 14, hjust = 0),
          plot.margin = margin(5, 25, 5, 25))
  
  # labels: plain text, or plotmath when the label starts with "expr:"
  for (k in seq_len(n)) {
    is_expr <- startsWith(labs_df$lab[k], "expr:")
    p <- p + annotate("text", x = labs_df$x[k], y = labs_df$y[k],
                      label = if (is_expr) sub("expr:", "", labs_df$lab[k]) else labs_df$lab[k],
                      parse = is_expr, hjust = labs_df$hjust[k], vjust = 0.5,
                      size = label_size, lineheight = 0.9)
  }
  
  if (dotted_vertical) {
    p <- p +
      annotate("segment", x = 0, xend = 0, y = -1.4, yend = 1.12,
               linetype = "dotted", linewidth = 0.5) +
      annotate("segment", x = 0.05, xend = 0.55, y = -1.4, yend = -1.4,
               linetype = "dotted", arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
      annotate("segment", x = -0.05, xend = -0.55, y = -1.4, yend = -1.4,
               linetype = "dotted", arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
      annotate("text", x = 0.5, y = -1.58, label = "total", size = label_size) +
      annotate("text", x = -0.5, y = -1.58, label = "per~km^2", parse = TRUE, size = label_size)
  }
  p + scale_x_continuous(limits = c(-1.5, 1.5)) +
    scale_y_continuous(limits = c(-1.7, 1.4))
}

# ---- panel A: fleet characteristics ---------------------------------------------
# clockwise from the top: totals (right half), then per km2 (left half)
totals <- rbind(landings, kwdays, seadays, vessels, fuel)
perkm2 <- sweep(totals, 2, shelf, "/")
A_vals <- rbind(totals, perkm2)
A_labs <- rep(c("Landings", "KW sea\ndays", "Sea\ndays", "Fleet size", "Fuel use"), 2)

pA <- rose_plot(A_vals, A_labs, "A   Fleet characteristics", dotted_vertical = TRUE)

# ---- panel B: fleet efficiency -----------------------------------------------------
B_vals <- rbind(value, land_fuel, land_kw, val_fuel, income, value / shelf)
B_labs <- c("Landing value",
            "Landings\nper liter fuel",
            "Landings\nper KW sea day",
            "expr:frac('Landing value', 'fuel costs')",
            "Income",
            "expr:atop('Landing value', '(per '*km^2*')')")

pB <- rose_plot(B_vals, B_labs, "B   Fleet efficiency", divider_col = "grey80")

# ---- helper: bar plot for panels C and D -----------------------------------------
bar_plot <- function(vals, title, ylab, ymax, na_label = FALSE) {
  df <- data.frame(region = factor(regions, levels = regions), val = vals)
  p <- ggplot(df, aes(x = region, y = val, fill = region, colour = region)) +
    geom_col(width = 0.95, linewidth = 0.5, na.rm = TRUE) +
    scale_fill_manual(values = col_in, guide = "none") +
    scale_colour_manual(values = col_bor, guide = "none") +
    scale_y_continuous(limits = c(0, ymax), breaks = c(0, ymax / 2, ymax),
                       labels = function(x) format(x, drop0trailing = TRUE),
                       expand = c(0, 0)) +
    labs(title = title, y = ylab, x = NULL) +
    theme_classic(base_size = 11) +
    theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 11, colour = "black"),
          axis.title.y = element_text(size = 10),
          plot.title = element_text(size = 11, hjust = 0.5),
          plot.margin = margin(5, 8, 5, 5))
  if (na_label) {
    p <- p + annotate("text", x = which(is.na(vals)), y = ymax * 0.03, label = "NA",
                      size = 3, vjust = 0)
  }
  p
}

c1 <- bar_plot(disc_pct, "Discards",       "% of catch",                      10)
c2 <- bar_plot(mammals,  "Marine mammals", expression(Biomass~(t~km^{-2})),   1)
c3 <- bar_plot(demersal, "Demersal fish",  expression(Biomass~(t~km^{-2})),   90)
c4 <- bar_plot(trawled,  "Bottom trawled", "% of area < 1000 m",              30)

d1 <- bar_plot(fatal,    "Fatal injuries", expression(paste("Per 1000 harvesters ", y^{-1})), 2)
d2 <- bar_plot(observer, "Observer days",  "Per 100 sea days",                10, na_label = TRUE)

boxed <- function(p, title) {
  g <- grid::grobTree(patchworkGrob(p),
                      grid::rectGrob(gp = grid::gpar(fill = NA, col = "black", lwd = 1)))
  wrap_elements(panel = g) + labs(title = title) +
    theme(plot.title = element_text(size = 14, hjust = 0),
          plot.background = element_blank(), panel.background = element_blank())
}

pC <- boxed(c1 | c2 | c3 | c4, "C   Environmental indicators")
pD <- boxed(d1 | d2,           "D   Control and safety")

# ---- legend -------------------------------------------------------------------------
leg_df <- data.frame(x = c(0, 1.25, 2.9), region = factor(regions, levels = regions))
pLeg <- ggplot(leg_df) +
  geom_rect(aes(xmin = x, xmax = x + 0.25, ymin = 0, ymax = 1, fill = region, colour = region),
            linewidth = 0.5) +
  geom_text(aes(x = x + 0.33, y = 0.5, label = region), hjust = 0, size = 3.6) +
  scale_fill_manual(values = col_in, guide = "none") +
  scale_colour_manual(values = col_bor, guide = "none") +
  coord_cartesian(xlim = c(-2.5, 4.6), ylim = c(-0.5, 1.5)) +
  theme_void()

# ---- combine ------------------------------------------------------------------------
fig5 <- (pA | pB) / (pC | pD) / pLeg +
  plot_layout(heights = c(1.35, 0.62, 0.05))
fig5[[2]] <- fig5[[2]] + plot_layout(widths = c(2, 1.1))

pdf("Output/Figure_4.pdf", width = 11, height = 9.5)
print(fig5)
dev.off()