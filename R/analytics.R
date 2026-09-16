## =============================================================================
##  analytics.R -- exact results for the bivariate-normal selection model
## =============================================================================
##  Everything in this file is a pure function of the model parameters: the
##  validity (rho), the selection ratio (sr) and the base rate (br). No random
##  number generation, no data frames, no plotting. Base R only, so this file
##  can be sourced and tested on its own.
##
##  Terminology
##    selection ratio (sr)  proportion of applicants accepted. The vertical
##                          cutoff. A property of your decision.
##    base rate       (br)  proportion of applicants who would succeed if all
##                          were hired. The horizontal cutoff. A property of
##                          the criterion and of where "success" is set.
##    success ratio         hits / selected. Of those you took, how many were
##                          good. Read it against the base rate, which is what
##                          you would get by selecting at random.
##
##  Identity worth remembering (holds for every rho):
##      false_alarm - miss == sr - br
##  Validity moves mass between the two correct cells and the two error cells, but can
##  never change the difference between the error cells. That difference is
##  fixed entirely by where the two cutoffs sit.
## =============================================================================


# --- parameters --------------------------------------------------------------

#' Build and validate a parameter set
#'
#' @param rho   validity, |rho| < 1
#' @param sr    selection ratio, in (0, 1)
#' @param br    base rate, in (0, 1). Defaults to `sr`, i.e. both cutoffs at the
#'              same percentile. Pass it explicitly to decouple the thresholds.
#' @param n     number of illustrative points
#' @param seed  RNG seed
#' @param exact TRUE  construct a configuration reproducing the analytic values
#'                    exactly (figures); FALSE draw an ordinary random sample
#'                    (app, where sampling noise is the point)
#' @param cuts  "theoretical" cutoffs at qnorm(1 - p), where the analytic labels
#'              say they are; "empirical" cutoffs placed so that exactly
#'              round(n * p) points fall above them
sel_params <- function(rho, sr, br = sr, n = 1000L, seed = 123L,
                       exact = TRUE, cuts = c("theoretical", "empirical")) {

  cuts <- match.arg(cuts)
  num1 <- function(x, nm) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x))
      stop(sprintf("`%s` must be a single finite number.", nm), call. = FALSE)
  }
  num1(rho, "rho"); num1(sr, "sr"); num1(br, "br"); num1(n, "n"); num1(seed, "seed")

  if (abs(rho) >= 1)
    stop("`rho` must satisfy |rho| < 1. At |rho| = 1 the criterion is a ",
         "deterministic function of the predictor and the model degenerates.",
         call. = FALSE)
  if (sr <= 0 || sr >= 1) stop("`sr` must lie strictly inside (0, 1).", call. = FALSE)
  if (br <= 0 || br >= 1) stop("`br` must lie strictly inside (0, 1).", call. = FALSE)

  n <- as.integer(round(n))
  if (n < 10L) stop("`n` must be at least 10.", call. = FALSE)

  # guarantee all four cells can be populated, which is what stops empty-factor
  # levels turning into NA labels downstream
  for (nm in c("sr", "br")) {
    p <- get(nm)
    if (n * p < 1 || n * (1 - p) < 1)
      stop(sprintf("n = %d with %s = %.4f leaves a group with fewer than one ",
                   n, nm, p), "point. Raise n or move the threshold.", call. = FALSE)
  }

  structure(list(rho = rho, sr = sr, br = br, n = n,
                 seed = as.integer(seed), exact = isTRUE(exact), cuts = cuts),
            class = "sel_params")
}

#' @export
print.sel_params <- function(x, ...) {
  cat("<sel_params>\n")
  cat(sprintf("  validity (rho)   %.3f\n", x$rho))
  cat(sprintf("  selection ratio  %.1f%%   (cutoff z = %.3f)\n",
              100 * x$sr, z_cut(x$sr)))
  cat(sprintf("  base rate        %.1f%%   (cutoff z = %.3f)%s\n",
              100 * x$br, z_cut(x$br),
              if (isTRUE(all.equal(x$sr, x$br))) "   [linked to selection ratio]" else ""))
  cat(sprintf("  n = %d, seed = %d, exact = %s, cuts = %s\n",
              x$n, x$seed, x$exact, x$cuts))
  invisible(x)
}


# --- univariate pieces -------------------------------------------------------

#' Standard-normal cutoff for the top proportion `p`
z_cut <- function(p) stats::qnorm(1 - p)

#' E[Z | Z > z_cut(p)], the mean of the selected on a standardised variable.
#' This is the `lambda` of the Brogden-Cronbach-Gleser model.
mean_z_selected <- function(p) stats::dnorm(z_cut(p)) / p


# --- bivariate normal CDF ----------------------------------------------------

#' P(X <= h, Y <= k) for a standard bivariate normal with correlation `rho`
#'
#' One-dimensional integral, so `stats::integrate` is accurate and this file
#' stays dependency-free. Verified against mvtnorm and pbivnorm in tests/.
p_bivnorm <- function(h, k, rho) {
  if (is.infinite(h) && h < 0) return(0)
  if (is.infinite(k) && k < 0) return(0)
  if (is.infinite(h) && h > 0) return(stats::pnorm(k))
  if (is.infinite(k) && k > 0) return(stats::pnorm(h))
  if (abs(rho) < 1e-12)  return(stats::pnorm(h) * stats::pnorm(k))
  if (rho >=  1 - 1e-12) return(min(stats::pnorm(h), stats::pnorm(k)))
  if (rho <= -1 + 1e-12) return(max(0, stats::pnorm(h) + stats::pnorm(k) - 1))

  s <- sqrt(1 - rho^2)
  f <- function(u) stats::dnorm(u) * stats::pnorm((k - rho * u) / s)
  stats::integrate(f, lower = -Inf, upper = h,
                   rel.tol = .Machine$double.eps^0.6)$value
}


# --- the 2x2 -----------------------------------------------------------------

cell_levels <- function() c("hit", "false_alarm", "miss", "correct_rejection")

cell_names <- function() c(hit               = "Hit",
                           false_alarm       = "False alarm",
                           miss              = "Miss",
                           correct_rejection = "Correct rejection")

#' Exact cell probabilities. Sums to 1.
cell_probs <- function(rho, sr, br = sr) {
  cr  <- p_bivnorm(z_cut(sr), z_cut(br), rho)          # both below both cutoffs
  hit <- 1 - (1 - sr) - (1 - br) + cr
  c(hit               = hit,
    false_alarm       = sr - hit,
    miss              = br - hit,
    correct_rejection = cr)
}

#' Round a vector so the rounded values sum exactly to `total`
#'
#' Independent rounding of four analytic percentages can land on 99.9 or 100.1.
#' Largest-remainder allocation fixes that.
largest_remainder <- function(x, total = sum(x), digits = 0L) {
  mult   <- 10^digits
  y      <- x * mult
  target <- round(total * mult)
  out    <- floor(y)
  gap    <- as.integer(round(target - sum(out)))
  if (gap != 0L) {
    ord <- order(y - floor(y), decreasing = gap > 0L)
    idx <- ord[seq_len(min(abs(gap), length(x)))]
    out[idx] <- out[idx] + sign(gap)
  }
  stats::setNames(out / mult, names(x))
}

round_2x2 <- function(p, sr, br, total, digits = 0L) {
    m <- 10^digits
    N <- round(total * m); S <- round(total * sr * m); B <- round(total * br * m)
    exact <- total * m * p[c("hit", "false_alarm", "miss", "correct_rejection")]
    hs    <- unique(c(floor(exact[["hit"]]), ceiling(exact[["hit"]])))
    cells <- lapply(hs, function(h) c(h, S - h, B - h, N - S - B + h))
    worst <- vapply(cells, function(x) max(abs(x - exact)), numeric(1))
    stats::setNames(cells[[which.min(worst)]] / m, names(exact))
}


# --- the single object every label reads from --------------------------------

#' All analytic quantities for one parameter set
#'
#' Boxes, cutoff lines, mean lines, panel subtitles and the utility readout all
#' pull from here, so they cannot disagree with each other.
sel_stats <- function(params) {
  p  <- cell_probs(params$rho, params$sr, params$br)
  n  <- params$n
  hit <- unname(p[["hit"]])

  list(
    params        = params,
    cells         = p,
    counts        = n * p,                                   # unrounded
    counts_int    = round_2x2(p, params$sr, params$br, n),
    percent       = round_2x2(p, params$sr, params$br, 100, digits = 1L),
    n_selected    = round(n * params$sr),
    n_successful  = round(n * params$br),
    base_rate     = params$br,
    success_ratio = hit / params$sr,                         # hits / selected
    sensitivity   = hit / params$br,                         # hits / successful
    specificity   = unname(p[["correct_rejection"]]) / (1 - params$br),
    npv           = unname(p[["correct_rejection"]]) / (1 - params$sr),
    zbar_x        = mean_z_selected(params$sr),                       # E[X | selected]
    zbar_y        = params$rho * mean_z_selected(params$sr),          # E[Y | selected]
    z_x           = z_cut(params$sr),
    z_y           = z_cut(params$br)
  )
}


# --- Brogden-Cronbach-Gleser -------------------------------------------------

#' Expected utility of using the predictor compared to random selection
#'
#'   benefit      = n_selected * SD_y * rho * zbar_x
#'   cost         = n_applicants * cost per applicant
#'   net benefit  = benefit - cost                      (reported as delta U)
#'
#' Uses the analytic zbar_x, not the realised mean of the selected points, so
#' the number under the figure cannot drift away from the line drawn in it.
bcg_utility <- function(params, sd_y, cost = 0, n_applicants = params$n) {
  zb    <- mean_z_selected(params$sr)
  n_sel <- n_applicants * params$sr
  benefit <- n_sel * sd_y * params$rho * zb
  total_cost <- n_applicants * cost
  list(n_applicants = n_applicants,
       n_selected   = n_sel,
       zbar_x       = zb,
       benefit      = benefit,
       cost         = total_cost,
       net_benefit  = benefit - total_cost,   # delta U
       per_selectee = (benefit - total_cost) / n_sel)
}
