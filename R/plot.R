## =============================================================================
##  plot.R -- drawing only
## =============================================================================
##  This file computes no statistics. Everything it prints arrives in `stats`
##  (from sel_stats()), so labels are analytic and cannot drift away from the
##  utility readout or the drawn lines.
##
##  validity_panel()   one bare panel
##  marginal_x/_y()    the marginal densities, separately
##  add_marginals()    convenience wrapper for a single panel
##  validity_legend()  the hand-built key strip
##  panel_letter()     the A / B / C tag, as its own narrow plot
##
##  Keeping these apart is what lets the triplet compose three identical panels,
##  share one top marginal, and slot the tag between panel and marginal.
##
##  Sizing is driven by `base_size` in points, used for the theme and the box
##  labels alike, so there is only one unit system to reason about.
## =============================================================================

# --- palette -----------------------------------------------------------------

#' Okabe-Ito, colour-blind safe. Point identity is doubled by size and alpha so
#' the selected/rejected split never rests on hue alone.
validity_palette <- function() {
  list(
    unselected = "gray60",
    selected   = "#E69F00",   # orange
    correct    = "#009E73",   # bluish green   (box fill, low alpha)
    incorrect  = "#CC79A7",   # reddish purple (box fill, low alpha)
    validity   = "#56B4E9",   # sky blue       -- the rho line
    cut_x      = "#E69F00",   # orange, dashed -- cutoff on the diagnosis
    cut_y      = "#009E73",   # green,  dashed -- cutoff on the actual value
    mean       = "#000000",   # solid black    -- means of the selected
    sample     = "#0072B2",   # blue,   dashed -- observed means (app only)
    box_alpha  = 0.20
  )
}

#' Fall back silently when a font is not installed
safe_family <- function(family = "Helvetica") {
  ok <- tryCatch({
    if (requireNamespace("systemfonts", quietly = TRUE))
      any(grepl(family, systemfonts::system_fonts()$family, ignore.case = TRUE))
    else family %in% names(grDevices::postscriptFonts())
  }, error = function(e) FALSE)
  if (isTRUE(ok)) family else ""
}

#' Manuscript theme: minimal, no grid, no panel border, bold axis titles
theme_validity <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      legend.position     = "none",
      panel.grid          = ggplot2::element_blank(),
      panel.border        = ggplot2::element_blank(),
      axis.line           = ggplot2::element_line(colour = "black", linewidth = 0.3),
      axis.ticks          = ggplot2::element_line(colour = "black", linewidth = 0.3),
      axis.title          = ggplot2::element_text(size = base_size, face = "bold",
                                                  colour = "black"),
      axis.text           = ggplot2::element_text(size = base_size * 0.95,
                                                  colour = "black"),
      plot.title          = ggtext::element_markdown(size = base_size * 1.1,
                                                     face = "bold", hjust = 0),
      plot.subtitle       = ggtext::element_markdown(size = base_size * 0.95,
                                                     colour = "grey25", hjust = 0),
      plot.title.position = "panel",
      plot.margin         = ggplot2::margin(3, 3, 3, 3)
    )
}


# --- notation ----------------------------------------------------------------
# The manuscript writes the mean of the selected as z-bar with a subscript, not
# as lambda. These two helpers keep that spelling in one place.

#' Markdown form, for ggtext elements and Shiny HTML
zbar_md <- function(sub = "") {
  if (nzchar(sub)) sprintf("z\u0304<sub>%s</sub>", sub) else "z\u0304"
}
#' Plain form, for console output
zbar_txt <- function(sub = "") {
  if (nzchar(sub)) sprintf("z\u0304_%s", sub) else "z\u0304"
}


# --- box labels --------------------------------------------------------------

box_label <- function(cell, stats, labels = c("percent", "count")) {
  nm  <- cell_names()[as.character(cell)]
  out <- sprintf("<b>%s</b>", nm)
  if ("percent" %in% labels)
    out <- paste0(out, "<br>", sprintf("%.1f%%", stats$percent[as.character(cell)]))
  if ("count" %in% labels)
    out <- paste0(out, "<br>", sprintf("(%.1f)", stats$counts[as.character(cell)]))
  out
}


# --- the panel ---------------------------------------------------------------

#' One scatter panel
#'
#' @param sim     output of simulate_panel()
#' @param stats   output of sel_stats()
#' @param layers  any of "boxes", "contour", "points", "cut_x", "cut_y",
#'                "cuts" (both), "line", "means"
#' @param labels  what goes in the boxes: "percent", "count", both, or none
#' @param show_sample_means add the observed selected-group means as dashed
#'        lines beside the analytic ones. For the app: press Resample and the
#'        vertical line barely moves while the horizontal one jumps.
#' @param lims    fixed axis limits. Fixed rather than data-driven, so panels
#'        are comparable and the box areas mean something.
#' @param fixed_aspect keep the panel square. A non-square panel rescales the
#'        apparent slope of the validity line by height/width, so leave this
#'        TRUE unless the layout genuinely cannot accommodate it.
validity_panel <- function(sim, stats,
                           layers = c("points", "cuts", "boxes"),
                           labels = c("percent", "count"),
                           show_sample_means = FALSE,
                           lims = c(-3.8, 3.8),
                           base_size = 11, base_family = "",
                           fixed_aspect = TRUE,
                           show_xlab = TRUE, show_ylab = TRUE,
                           xlab = "Diagnosis (z)", ylab = "Actual value (z)",
                           title = NULL, subtitle = NULL,
                           pal = validity_palette()) {

  d  <- sim$data
  cx <- unname(sim$cuts[["x"]])
  cy <- unname(sim$cuts[["y"]])
  sz <- base_size / 11
  box_layer <- NULL          # built below, added last so it sits above points

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y))

  ## background: the 2x2 ------------------------------------------------------
  if ("boxes" %in% layers) {
    rects <- data.frame(
      cell = factor(cell_levels(), levels = cell_levels()),
      xmin = c(cx, cx, lims[1], lims[1]),
      xmax = c(lims[2], lims[2], cx, cx),
      ymin = c(cy, lims[1], cy, lims[1]),
      ymax = c(lims[2], cy, lims[2], cy)
    )
    rects$correct <- rects$cell %in% c("hit", "correct_rejection")
    rects$xlab <- (rects$xmin + rects$xmax) / 2
    rects$ylab <- (rects$ymin + rects$ymax) / 2

    p <- p + ggplot2::geom_rect(
      data = rects, inherit.aes = FALSE,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = .data$ymin, ymax = .data$ymax, fill = .data$correct),
      alpha = pal$box_alpha) +
      ggplot2::scale_fill_manual(values = c(`TRUE`  = pal$correct,
                                            `FALSE` = pal$incorrect))

    if (length(labels)) {
      rects$text <- vapply(rects$cell, box_label, character(1),
                           stats = stats, labels = labels)
      box_layer <- ggtext::geom_richtext(
        data = rects, inherit.aes = FALSE,
        ggplot2::aes(x = .data$xlab, y = .data$ylab, label = .data$text),
        size = 0.85 * base_size / ggplot2::.pt,        # one unit system
        family = base_family, lineheight = 1.15, label.colour = NA,
        fill = grDevices::adjustcolor("white", alpha.f = 0.75),
        label.padding = grid::unit(rep(0.15 * sz, 4), "lines"),
        label.r = grid::unit(0.1, "lines"), colour = "grey10")
    }
  }

  ## density contours ---------------------------------------------------------
  if ("contour" %in% layers)
    p <- p + ggplot2::stat_density_2d(colour = "grey35",
                                      linewidth = 0.25 * sz, bins = 6)

  ## points -------------------------------------------------------------------
  if ("points" %in% layers)
    p <- p + ggplot2::geom_point(
      ggplot2::aes(colour = .data$selected, size = .data$selected,
                   alpha = .data$selected), shape = 16) +
      ggplot2::scale_colour_manual(values = c(`FALSE` = pal$unselected,
                                              `TRUE`  = pal$selected)) +
      ggplot2::scale_size_manual(values  = c(`FALSE` = 0.75 * sz,
                                             `TRUE`  = 1.50 * sz)) +
      ggplot2::scale_alpha_manual(values = c(`FALSE` = 0.55, `TRUE` = 0.95))

  ## cutoffs ------------------------------------------------------------------
  ## "cuts" draws both; "cut_x" / "cut_y" draw one. A figure about the means of
  ## the selected has no use for the cutoff on the actual value.
  if (any(c("cuts", "cut_x") %in% layers))
    p <- p + ggplot2::geom_vline(xintercept = cx, colour = pal$cut_x,
                                 linetype = "dashed", linewidth = 0.55 * sz)
  if (any(c("cuts", "cut_y") %in% layers))
    p <- p + ggplot2::geom_hline(yintercept = cy, colour = pal$cut_y,
                                 linetype = "dashed", linewidth = 0.55 * sz)

  ## validity line: the population regression, slope = rho, no confidence band -
  if ("line" %in% layers)
    p <- p + ggplot2::geom_abline(intercept = 0, slope = sim$rho,
                                  colour = pal$validity, linewidth = 0.7 * sz)

  ## analytic means of the selected group -------------------------------------
  if ("means" %in% layers) {
    p <- p +
      ggplot2::geom_vline(xintercept = stats$zbar_x, colour = pal$mean,
                          linewidth = 0.5 * sz) +
      ggplot2::geom_hline(yintercept = stats$zbar_y, colour = pal$mean,
                          linewidth = 0.5 * sz) +
        ggplot2::scale_x_continuous(breaks = c(-2, 0, stats$zbar_x, 2),
                           labels = c("-2", "0", expression(bar(z)[x]), "2"))+
        ggplot2::scale_y_continuous(breaks = c(-2, 0, stats$zbar_y, 2),
                                    labels = c("-2", "0", expression(bar(z)[y]), "2"))
    if (isTRUE(show_sample_means)) {
      ss <- sample_stats(sim)
      p <- p +
        ggplot2::geom_vline(xintercept = ss$mean_x_sel, colour = pal$sample,
                            linewidth = 0.5 * sz, linetype = "22") +
        ggplot2::geom_hline(yintercept = ss$mean_y_sel, colour = pal$sample,
                            linewidth = 0.5 * sz, linetype = "22")
    }
  }

  ## box labels last, so their backing covers the point cloud ------------------
  if (!is.null(box_layer)) p <- p + box_layer

  ## frame --------------------------------------------------------------------
  # coord_*, never xlim()/ylim(): the scale functions DROP out-of-range points,
  # which would desynchronise the visible dots from anything computed on them.
  p <- p + if (fixed_aspect) {
    ggplot2::coord_fixed(ratio = 1, xlim = lims, ylim = lims, expand = FALSE)
  } else {
    ggplot2::coord_cartesian(xlim = lims, ylim = lims, expand = FALSE)
  }

  p +
    ggplot2::labs(x = if (show_xlab) xlab else NULL,
                  y = if (show_ylab) ylab else NULL,
                  title = title, subtitle = subtitle) +
    theme_validity(base_size, base_family)
}


# --- marginal densities ------------------------------------------------------

#' Density on a fixed grid, with the cut point interpolated in
density_curve <- function(v, lims, cut = NULL, n = 512) {
  if (length(v) < 2L) return(data.frame(v = numeric(0), dens = numeric(0)))
  dd  <- stats::density(v, from = lims[1], to = lims[2], n = n)
  out <- data.frame(v = dd$x, dens = dd$y)
  if (!is.null(cut) && cut > lims[1] && cut < lims[2]) {
    out <- rbind(out, data.frame(v = cut,
                                 dens = stats::approx(dd$x, dd$y, xout = cut)$y))
    out <- out[order(out$v), ]
  }
  out
}

#' Marginal density of the diagnosis, with the selected tail shaded
#'
#' @param align_ylab when the marginal sits above a panel in a multi-row grid,
#'        pass the panel's y-axis title here. The marginal then reserves an
#'        identically sized (but invisible) left gutter, which is what keeps its
#'        cutoff line vertically above the panel's.
marginal_x <- function(sim, stats, lims = c(-3.8, 3.8), base_size = 11,
                       show_mean = TRUE, show_cut = TRUE,
                       align_ylab = NULL, align_tick = "-2",
                       base_family = "", pal = validity_palette()) {
  d <- sim$data; cx <- unname(sim$cuts[["x"]]); sz <- base_size / 11
  dx <- density_curve(d$x, lims, cut = cx)
  p <- ggplot2::ggplot(dx, ggplot2::aes(.data$v, .data$dens)) +
    ggplot2::geom_area(data = dx[dx$v >= cx, ], fill = pal$selected, alpha = 0.45) +
    ggplot2::geom_line(colour = "grey25", linewidth = 0.4 * sz)
  if (show_cut)
    p <- p + ggplot2::geom_vline(xintercept = cx, colour = pal$cut_x,
                                 linetype = "dashed", linewidth = 0.55 * sz)
  if (show_mean)
    p <- p + ggplot2::geom_vline(xintercept = stats$zbar_x, colour = pal$mean,
                                 linewidth = 0.5 * sz)
  p <- p + ggplot2::coord_cartesian(xlim = lims, expand = FALSE)

  if (is.null(align_ylab)) {
    p + ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none",
                     plot.margin = ggplot2::margin(0, 0, 0, 0))
  } else {
    p +
      ggplot2::scale_y_continuous(breaks = mean(range(dx$dens)),
                                  labels = align_tick) +
      ggplot2::labs(y = align_ylab) +
      theme_validity(base_size, base_family) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_text(colour = NA, face = "bold",
                                             size = base_size),
        axis.text.y  = ggplot2::element_text(colour = NA,
                                             size = base_size * 0.95),
        axis.title.x = ggplot2::element_blank(),
        axis.text.x  = ggplot2::element_blank(),
        axis.ticks   = ggplot2::element_blank(),
        axis.line    = ggplot2::element_blank(),
        plot.margin  = ggplot2::margin(0, 3, 0, 3))
  }
}

#' Marginal density of the actual value
#'
#' @param split "selected_vs_rest" contrasts the two groups, so the panels of a
#'        triplet visibly pull apart as validity rises. "selected_vs_all"
#'        contrasts the selected against the whole pool, which is the
#'        Brogden-Cronbach-Gleser reading. Both curves are normalised to unit
#'        area; with a small selected group, count-scaled curves are invisible.
marginal_y <- function(sim, stats, lims = c(-3.8, 3.8), base_size = 11,
                       split = c("selected_vs_rest", "selected_vs_all"),
                       show_mean = TRUE, show_cut = FALSE,
                       pal = validity_palette()) {
  split <- match.arg(split)
  d  <- sim$data; sz <- base_size / 11
  cy <- unname(sim$cuts[["y"]])

  ref   <- if (split == "selected_vs_all") d$y else d$y[!d$selected]
  d_ref <- density_curve(ref, lims)
  d_sel <- density_curve(d$y[d$selected], lims)

  p <- ggplot2::ggplot() +
    ggplot2::geom_area(data = d_ref, ggplot2::aes(.data$v, .data$dens),
                       fill = pal$unselected, alpha = 0.30) +
    ggplot2::geom_area(data = d_sel, ggplot2::aes(.data$v, .data$dens),
                       fill = pal$selected, alpha = 0.35) +
    ggplot2::geom_line(data = d_ref, ggplot2::aes(.data$v, .data$dens),
                       colour = "grey35", linewidth = 0.4 * sz) +
    ggplot2::geom_line(data = d_sel, ggplot2::aes(.data$v, .data$dens),
                       colour = pal$selected, linewidth = 0.5 * sz)
  if (show_cut)
    p <- p + ggplot2::geom_vline(xintercept = cy, colour = pal$cut_y,
                                 linetype = "dashed", linewidth = 0.55 * sz)
  if (show_mean)
    p <- p +
      ggplot2::geom_vline(xintercept = 0, colour = "grey45",
                          linewidth = 0.4 * sz, linetype = "22") +
      ggplot2::geom_vline(xintercept = stats$zbar_y, colour = pal$mean,
                          linewidth = 0.5 * sz)
  p + ggplot2::coord_flip(xlim = lims, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::margin(0, 0, 0, 0))
}

#' Wrap one panel with marginal densities
#'
add_marginals <- function(panel, sim, stats, which = c("top", "right"),
                          lims = c(-3.8, 3.8), base_size = 11,
                          split = "selected_vs_all",
                          show_means = TRUE, title = NULL, subtitle = NULL,
                          base_family = "", pal = validity_palette()) {

    top <- if ("top" %in% which)
        marginal_x(sim, stats, lims, base_size, show_mean = show_means, pal = pal)
    right <- if ("right" %in% which)
        marginal_y(sim, stats, lims, base_size, split = split,
                   show_mean = show_means, pal = pal)

    sp  <- patchwork::plot_spacer()
    out <- if (!is.null(top) && !is.null(right)) {
        patchwork::wrap_plots(top, sp, panel, right, ncol = 2,
                              widths = c(5, 1), heights = c(1, 4))
    } else if (!is.null(top)) {
        patchwork::wrap_plots(top, panel, ncol = 1, heights = c(1, 4))
    } else if (!is.null(right)) {
        patchwork::wrap_plots(panel, right, ncol = 2, widths = c(5, 1))
    } else panel

  if (!is.null(title) || !is.null(subtitle))
    out <- out + patchwork::plot_annotation(
      title = title, subtitle = subtitle,
      theme = ggplot2::theme(
        text          = ggplot2::element_text(family = base_family),
        plot.title    = ggtext::element_markdown(size = base_size * 1.1,
                                                 face = "bold"),
        plot.subtitle = ggtext::element_markdown(size = base_size * 0.95,
                                                 colour = "grey25")))
  out
}


#' Title block for an assembled figure
#'
#' Apply this at the OUTERMOST patchwork level. plot_annotation() attached to an
#' inner patchwork is silently dropped when that object is nested again.
fig_annotation <- function(title = NULL, subtitle = NULL, caption = NULL,
                           base_size = 11, base_family = "") {
  patchwork::plot_annotation(
    title = title, subtitle = subtitle, caption = caption,
    theme = ggplot2::theme(
      text          = ggplot2::element_text(family = base_family),
      plot.title    = ggtext::element_markdown(size = base_size * 1.15,
                                               face = "bold"),
      plot.subtitle = ggtext::element_markdown(size = base_size,
                                               colour = "grey25"),
      plot.caption  = ggtext::element_markdown(size = base_size * 0.85,
                                               colour = "grey35", hjust = 0),
      plot.margin   = ggplot2::margin(4, 4, 4, 4)))
}


# --- panel tag ---------------------------------------------------------------

#' The A / B / C tag as its own narrow plot, so it can sit between a panel and
#' its marginal rather than inside either.
panel_letter <- function(label, base_size = 11, base_family = "", at = 0.97) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = at, label = label,
                      size = 1.5 * base_size / ggplot2::.pt,
                      fontface = "bold", family = base_family,
                      hjust = 0.5, vjust = 1) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
}


# --- legend ------------------------------------------------------------------

#' Hand-built key strip
#'
#' ggplot's own guides cannot combine rectangles, lines and points drawn from
#' different scales into a single row, which is why this is assembled by hand.
#'
#' @param items any of "correct", "incorrect", "validity", "cut_y", "cut_x",
#'        "means", "sample", "selected", "unselected", in the order given
#' @param ncol  keys per row. Defaults to one row up to four keys, two rows
#'        beyond that. Size the enclosing patchwork slot with legend_rows().
validity_legend <- function(items = c("correct", "incorrect", "validity",
                                      "cut_y", "cut_x", "selected", "unselected"),
                            base_size = 11, base_family = "", ncol = NULL,
                            pal = validity_palette()) {

  spec <- list(
    correct    = list(kind = "rect",  col = pal$correct,    lty = NA,
                      lab = "Correct<br>classification"),
    incorrect  = list(kind = "rect",  col = pal$incorrect,  lty = NA,
                      lab = "Incorrect<br>classification"),
    validity   = list(kind = "line",  col = pal$validity,   lty = "solid",
                      lab = "Validity"),
    cut_y      = list(kind = "line",  col = pal$cut_y,      lty = "dashed",
                      lab = "Cutoff<br>actual value"),
    cut_x      = list(kind = "line",  col = pal$cut_x,      lty = "dashed",
                      lab = "Cutoff<br>diagnosis"),
    means      = list(kind = "line",  col = pal$mean,       lty = "solid",
                      lab = paste0("Mean of<br>selected (", zbar_md(), ")")),
    sample     = list(kind = "line",  col = pal$sample,     lty = "22",
                      lab = "Observed<br>sample mean"),
    selected   = list(kind = "point", col = pal$selected,   lty = NA,
                      lab = "Selected<br>proposal"),
    unselected = list(kind = "point", col = pal$unselected, lty = NA,
                      lab = "Non-selected<br>proposal")
  )
  items <- items[items %in% names(spec)]
  n <- length(items)
  if (n == 0L) return(patchwork::plot_spacer())

  # key on the left, label beside it. Stacking the label under the key needs
  # roughly twice the height and collides as soon as the legend wraps.
  if (is.null(ncol)) ncol <- if (n <= 4L) n else ceiling(n / 2)
  ncol  <- max(1L, min(ncol, n))
  nrow  <- ceiling(n / ncol)
  col_i <- ((seq_len(n) - 1L) %% ncol) + 1L
  row_i <- ((seq_len(n) - 1L) %/% ncol) + 1L
  yc    <- nrow - row_i + 0.5                 # vertical centre of the cell

  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, ncol), ylim = c(0, nrow), expand = FALSE)

  for (i in seq_len(n)) {
    s <- spec[[items[i]]]; x0 <- col_i[i] - 1; y <- yc[i]
    p <- p + switch(
      s$kind,
      rect  = ggplot2::annotate("rect", xmin = x0 + 0.04, xmax = x0 + 0.22,
                                ymin = y - 0.11, ymax = y + 0.11, fill = s$col,
                                alpha = pal$box_alpha * 3),
      line  = ggplot2::annotate("segment", x = x0 + 0.04, xend = x0 + 0.22,
                                y = y, yend = y, colour = s$col,
                                linetype = s$lty, linewidth = 0.7 * base_size / 11),
      point = ggplot2::annotate("point", x = x0 + 0.13, y = y, colour = s$col,
                                size = 0.26 * base_size)
    )
  }

  labs <- data.frame(x = col_i - 1 + 0.27, y = yc,
                     lab = vapply(items, function(k) spec[[k]]$lab, character(1)))
  p +
    ggtext::geom_richtext(data = labs,
                          ggplot2::aes(.data$x, .data$y, label = .data$lab),
                          size = 0.72 * base_size / ggplot2::.pt,
                          family = base_family, lineheight = 1.1,
                          label.colour = NA, fill = NA, colour = "grey10",
                          hjust = 0, vjust = 0.5,
                          label.padding = grid::unit(rep(0, 4), "pt")) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2))
}

#' Rows the legend will occupy, so callers can size the patchwork slot
legend_rows <- function(items, ncol = NULL) {
  n <- length(items)
  if (n == 0L) return(0L)
  if (is.null(ncol)) ncol <- if (n <= 4L) n else ceiling(n / 2)
  as.integer(ceiling(n / max(1L, min(ncol, n))))
}
