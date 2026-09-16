## =============================================================================
##  Figure: how the 2x2 changes with validity
## =============================================================================
##  Three panels stacked in one column.
##
##    [  shared marginal: diagnosis  ]
##    [ panel A ] [A] [ marginal: actual value ]
##    [ panel B ] [B] [ marginal: actual value ]
##    [ panel C ] [C] [ marginal: actual value ]
##    [          legend              ]
##
##  The diagnosis marginal appears once at the top because it is identical in
##  all three panels; only the actual value changes with validity. The panel
##  tags sit between the plot and the right marginal so the marginals stay true
##  marginals of the panel beside them.
##
##  Boxes are labelled with EXPECTED counts and percentages from the bivariate
##  normal, not with tallies of the plotted points, so the numbers are
##  properties of rho rather than of the seed.
##
##  All three panels share one construction (same diagnoses, same noise), so the
##  cloud rotates as validity rises instead of reshuffling, and individual
##  proposals can be watched migrating across the cutoff.
## =============================================================================

source(here::here("setup.R"))
library(ggplot2)
library(patchwork)

## --- SPEC --------------------------------------------------------------------
## The alternative is kept in view on purpose. The decoupled version is what
## Taylor-Russell tables are built for; the linked version isolates validity.

SR   <- 0.04          # selection ratio: the top 20% of diagnoses are funded
BR   <- .2            # cutoff on the actual value linked to it, so both sit at
                      # the same percentile. The two error cells are then pinned
                      # equal and only the hit cell moves with rho.
# SR <- 0.04; BR <- 0.20   # <- decoupled alternative

RHOS   <- c(0, 0.20, 0.40)
TAGS   <- c("A", "B", "C")
N      <- 1000L
SEED   <- 20250914L
LIMS   <- c(-3.5, 3.5)
BASE   <- 10                        # point size
FAMILY <- safe_family("Helvetica") # falls back silently if not installed
W <- 6.4; H <- 8 # W <- 6.4; H <- 13.6                # inches; chosen so the panels come out square
## -----------------------------------------------------------------------------

params <- sel_params(rho = RHOS[1], sr = SR, br = BR, n = N, seed = SEED,
                     exact = TRUE, cuts = "theoretical")
print(params)

## One construction serves all three panels: the residualisation depends on the
## selection (which happens on the diagnosis alone) and not on rho.
xz <- make_xz(params)

sims  <- lapply(RHOS, function(r) simulate_panel(xz, params, rho = r))
stats <- lapply(RHOS, function(r)
  sel_stats(sel_params(rho = r, sr = SR, br = BR, n = N, seed = SEED)))

panels <- Map(function(sim, st, tag, i) {
  validity_panel(
    sim, st,
    layers      = c("boxes", "points", "cuts", "line"),
    labels      = c("percent", "count"),
    lims        = LIMS, base_size = BASE, base_family = FAMILY,
    fixed_aspect = FALSE,          # the column layout sets the aspect instead
    xlab = "Standardized predictor score (x)", ylab = "Standardized actual project net benefit (y)",
    show_xlab   = i == length(RHOS),
    show_ylab   = TRUE,
    subtitle    = sprintf(
      "**Validity r<sub>xy</sub> = %.2f** \u00b7 success ratio %.1f%% (base rate %.1f%%)",
      sim$rho, 100 * st$success_ratio, 100 * st$base_rate))
}, sims, stats, TAGS, seq_along(RHOS))

## marginals: one for the diagnosis at the very top, one per panel on the right
top_marg <- marginal_x(sims[[1]], stats[[1]], lims = LIMS, base_size = BASE,
                       show_mean = FALSE, base_family = FAMILY,
                       align_ylab = "Actual value (z)")   # reserves the gutter
side <- Map(function(sim, st) marginal_y(sim, st, lims = LIMS, base_size = BASE,
                                         split = "selected_vs_rest",
                                         show_mean = FALSE, show_cut = TRUE),
            sims, stats)
tags <- lapply(TAGS, panel_letter, base_size = BASE, base_family = FAMILY)

sp   <- plot_spacer()
grid <- wrap_plots(
    sp,      top_marg,    sp,
  tags[[1]], panels[[1]], side[[1]],
  tags[[2]], panels[[2]], side[[2]],
  tags[[3]], panels[[3]], side[[3]],
  ncol = 3, widths = c(0.5, 6, 1), heights = c(1, 4, 4, 4), axes = "collect")

LEG <- c("correct", "incorrect", "validity", "cut_y", "cut_x",
         "selected", "unselected")
legend <- validity_legend(LEG, base_size = BASE + 2, base_family = FAMILY)

fig <- wrap_plots(grid, legend, ncol = 1,
                  heights = c(13, 0.7 * legend_rows(LEG)))

out <- here::here("figures",
                  sprintf("fig_triplet_sr%02d_br%02d.svg", round(100 * SR),
                          round(100 * BR)))
ggsave(out, fig, width = W, height = H, device = svglite::svglite)
ggsave(sub("[.]svg$", ".png", out), fig, width = W, height = H, dpi = 300)
message("wrote ", out)

## --- numbers for the caption -------------------------------------------------
tab <- do.call(rbind, Map(function(r, st)
  data.frame(rho = r, t(round(st$percent, 1)),
             success_ratio = round(100 * st$success_ratio, 1),
             base_rate     = round(100 * st$base_rate, 1)),
  RHOS, stats))
print(tab, row.names = FALSE)

## Caption skeleton:
##   Expected cell frequencies under a bivariate normal model with the stated
##   validity, a selection ratio of SR% and a base rate of BR%. Percentages are
##   of all N proposals; counts are expected frequencies and need not be
##   integers. The plotted points are a single illustrative configuration of
##   n = N, constructed so that the sample reproduces the stated validity and
##   margins exactly; all three panels use the same proposals and the same
##   noise, so the cloud rotates rather than being redrawn. The marginal density
##   of the diagnosis (top) is shared by all three panels; the marginal
##   densities of the actual value (right) contrast selected against
##   non-selected proposals and pull apart as validity rises.
