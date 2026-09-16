## =============================================================================
##  simulate.R -- generating and classifying the illustrative points
## =============================================================================
##  Depends on analytics.R. No plotting here.
##
##  Two modes, chosen by `params$exact`:
##
##  exact = TRUE   x is the standardised normal quantile grid and the noise is
##                 residualised on (1, x, selected). The result reproduces the
##                 analytic values exactly: r = rho, mean 0, sd 1, and
##                 mean(y | selected) = rho * mean(x | selected). The cloud is a
##                 constructed configuration, not a random sample, and captions
##                 should say so. Use for manuscript figures.
##
##  exact = FALSE  ordinary rnorm draws. Everything wobbles. Use in the app,
##                 where seeing the wobble is the point.
##
##  Why the construction is rho-free: selection happens on x only, so the
##  residualisation does not involve rho. One make_xz() call therefore serves
##  every validity, which is what lets the triplet share a single construction
##  across its three panels.
## =============================================================================


#' Cutoff value for the top proportion `p` of a vector
#'
#' Returns one number used both for classification and for the drawn line, so
#' the two cannot come apart. With cuts = "empirical" the value sits midway
#' between the k-th and (k+1)-th order statistics, so exactly round(n * p)
#' points fall above it.
cut_value <- function(v, p, how = c("theoretical", "empirical")) {
  how <- match.arg(how)
  if (how == "theoretical") return(z_cut(p))
  n <- length(v)
  k <- round(n * p)
  if (k <= 0L)  return(max(v) + 1e-9)
  if (k >= n)   return(min(v) - 1e-9)
  s <- sort(v, decreasing = TRUE)
  mean(s[c(k, k + 1L)])
}


#' Draw the predictor and the noise component
#'
#' Returns a data frame with columns `x` and `z`. Depends on n, sr, seed, exact
#' and cuts, but *not* on rho, so the same result can be reused across
#' validities.
make_xz <- function(params) {
  stopifnot(inherits(params, "sel_params"))
  n <- params$n

  withr::with_seed(params$seed, {
    if (!params$exact) {
      return(data.frame(x = stats::rnorm(n), z = stats::rnorm(n)))
    }

    # exact normal margin: midpoints of n equal-probability intervals
    x <- stats::qnorm((seq_len(n) - 0.5) / n)
    x <- as.vector(scale(x))                       # mean 0, sd 1 exactly

    sel <- x >= cut_value(x, params$sr, params$cuts)

    # sweep out the three chance alignments that would otherwise spoil the
    # analytic values: a non-zero overall mean, accidental correlation with x,
    # and a non-zero noise average within the selected group
    design <- cbind(1, x, as.numeric(sel))
    z <- qr.resid(qr(design), stats::rnorm(n))
    z <- as.vector(scale(z))

    data.frame(x = x, z = z)
  })
}


#' Build one panel's worth of data at a given validity
#'
#' @return a list with
#'   `data`  tibble/data.frame with fixed columns x, y, selected, successful, cell
#'   `cuts`  named vector c(x = , y = ) -- the values that were both used to
#'           classify and should be drawn
#'   `rho`   the validity used
simulate_panel <- function(xz, params, rho = params$rho) {
  stopifnot(inherits(params, "sel_params"))
  if (abs(rho) >= 1) stop("`rho` must satisfy |rho| < 1.", call. = FALSE)

  x <- xz$x
  y <- rho * x + sqrt(1 - rho^2) * xz$z

  cx <- cut_value(x, params$sr, params$cuts)
  cy <- cut_value(y, params$br, params$cuts)

  selected   <- x >= cx
  successful <- y >= cy

  cell <- factor(
    ifelse(selected  & successful, "hit",
    ifelse(selected  & !successful, "false_alarm",
    ifelse(!selected & successful, "miss", "correct_rejection"))),
    levels = cell_levels()                # all four levels always present, so
  )                                       # empty cells never become NA labels

  list(data = data.frame(x = x, y = y, selected = selected,
                         successful = successful, cell = cell),
       cuts = c(x = cx, y = cy),
       rho  = rho,
       params = params)
}


#' Observed counterparts of the analytic quantities
#'
#' For the app's "show sample means" layer, and for checking a construction.
#' Never used to label the static figures.
sample_stats <- function(sim) {
  d <- sim$data
  tab <- table(d$cell)
  sel <- d$selected
  list(r            = stats::cor(d$x, d$y),
       counts       = stats::setNames(as.numeric(tab), names(tab)),
       percent      = stats::setNames(100 * as.numeric(tab) / nrow(d), names(tab)),
       n_selected   = sum(sel),
       mean_x_sel   = if (any(sel)) mean(d$x[sel]) else NA_real_,
       mean_y_sel   = if (any(sel)) mean(d$y[sel]) else NA_real_,
       success_ratio = if (any(sel)) mean(d$successful[sel]) else NA_real_)
}
