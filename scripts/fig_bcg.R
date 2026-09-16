## =============================================================================
##  Figure: the Brogden-Cronbach-Gleser picture
## =============================================================================
##  One panel. The 2x2 is not shown; what matters here are the two mean lines.
##
##    vertical    z-bar_x  = phi(z_c)/SR   = mean diagnosis of the selected
##    horizontal  z-bar_y  = rho * z-bar_x = mean actual value of the selected
##
##  Their ratio is the validity, and that is the whole story: you funded
##  proposals scoring z-bar_x SDs above average on the diagnosis, and they turn
##  out z-bar_y SDs above average in actual value.
##
##  Both lines are analytic. The points are constructed (see simulate.R) so that
##  the plotted cloud reproduces them exactly rather than approximately, which
##  matters because the mean actual value of a small selected group is noisy:
##  with a 4% selection ratio out of 1000, its sampling SD is about 0.15 against
##  a value of 0.86.
## =============================================================================

source(here::here("setup.R"))
library(ggplot2)
library(patchwork)

## --- SPEC --------------------------------------------------------------------
RHO    <- 0.40
SR     <- 0.04        # selection ratio: the top 4% of diagnoses are funded
BR     <- SR          # linked. Decouple by setting BR explicitly, e.g. BR <- 0.20
N      <- 1000L
SEED   <- 20250914L
LIMS   <- c(-3.5, 3.5)
BASE   <- 12
FAMILY <- safe_family("Helvetica")
W <- 6.4; H <- 7.2

SD_Y <- 50000         # SD of net project benefit, in currency units
COST <- 1750          # cost per proposal assessed
## -----------------------------------------------------------------------------

params <- sel_params(rho = RHO, sr = SR, br = BR, n = N, seed = SEED,
                     exact = TRUE, cuts = "theoretical")
print(params)

st  <- sel_stats(params)
sim <- simulate_panel(make_xz(params), params)

panel <- validity_panel(
  sim, st,
  layers       = c("points", "cuts", "line", "means"),
  labels       = character(0),            # no boxes in this figure
  lims         = LIMS, base_size = BASE, base_family = FAMILY,
  show_sample_means = F,
  xlab = "Standardized predictor score (x)", ylab = "Standardized actual project net benefit (y)",
  fixed_aspect = FALSE                    # the layout sets the aspect instead
)

with_marg <- add_marginals(
  panel, sim, st, which = c("top", "right"),
  lims = LIMS, base_size = BASE, base_family = FAMILY,
  split = "selected_vs_all")

LEG    <- c("validity", "cut_x", "cut_y", "means", "selected", "unselected")
legend <- validity_legend(LEG, base_size = BASE + 2, base_family = FAMILY)

## the title block goes on the OUTERMOST patchwork, or nesting swallows it
fig <- wrap_plots(with_marg, legend, ncol = 1,
                  heights = c(7, 0.7 * legend_rows(LEG))) +
  fig_annotation(
    title = sprintf("Validity r<sub>xy</sub> = %.2f, selection ratio %.0f%%", RHO, 100 * SR),
    subtitle = sprintf("Mean of the selected: %s = %.2f, %s = r<sub>xy</sub> \u00d7 %s = %.2f",
                       zbar_md("x"), st$zbar_x, zbar_md("y"), zbar_md("x"), st$zbar_y),
    base_size = BASE, base_family = FAMILY)

out <- here::here("figures", sprintf("fig_bcg_sr%02d_br%02d.svg",
                                     round(100 * SR), round(100 * BR)))
ggsave(out, fig, width = W, height = H, device = svglite::svglite)
ggsave(sub("[.]svg$", ".png", out), fig, width = W, height = H, dpi = 200)
message("wrote ", out)

## --- numbers for the text ----------------------------------------------------
u <- bcg_utility(params, sd_y = SD_Y, cost = COST)

cat("\n--- analytic quantities -----------------------------------------\n")
cat(sprintf("  cutoff z                  %10.3f  (top %.0f%%)\n", st$z_x, 100 * SR))
cat(sprintf("  %-24s  %10.3f\n", paste0(zbar_txt("x"), " (diagnosis)"), st$zbar_x))
cat(sprintf("  %-24s  %10.3f\n", paste0(zbar_txt("y"), " (actual value)"), st$zbar_y))
cat(sprintf("  success ratio             %9.1f%%  (base rate %.1f%%)\n",
            100 * st$success_ratio, 100 * st$base_rate))
cat("\n--- Brogden-Cronbach-Gleser -------------------------------------\n")
cat(sprintf("  selected                  %10.0f of %d\n", u$n_selected, u$n_applicants))
cat(sprintf("  benefit                   %10.0f\n", u$benefit))
cat(sprintf("  cost                      %10.0f\n", u$cost))
cat(sprintf("  delta U (net benefit)     %10.0f   (%.0f per selected proposal)\n",
            u$net_benefit, u$per_selectee))

## --- what the construction bought us -----------------------------------------
ss <- sample_stats(sim)
cat("\n--- plotted points vs analytic ----------------------------------\n")
cat(sprintf("  r                  %.6f  vs  %.6f\n", ss$r, RHO))
cat(sprintf("  mean diagnosis     %.6f  vs  %.6f\n", ss$mean_x_sel, st$zbar_x))
cat(sprintf("  mean actual value  %.6f  vs  %.6f\n", ss$mean_y_sel, st$zbar_y))

## Caption skeleton:
##   Illustration of the Brogden-Cronbach-Gleser model at a validity of RHO and
##   a selection ratio of SR%. The vertical line marks the expected diagnosis of
##   the selected proposals, z-bar_x = phi(z)/SR; the horizontal line marks their
##   expected actual value, z-bar_y = rho * z-bar_x. Marginal densities show the
##   diagnosis with the selected tail shaded (top) and the actual value of all
##   proposals against that of the selected (right, each normalised to unit
##   area). The n = N points are a single illustrative configuration,
##   constructed so that the plotted sample reproduces the stated validity and
##   both means exactly.
