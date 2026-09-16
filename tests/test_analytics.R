## =============================================================================
##  Run from the project root:  Rscript tests/test_analytics.R
## =============================================================================
source(here::here("setup.R"))

ok <- 0L; bad <- character()
chk <- function(label, cond) {
  if (isTRUE(cond)) ok <<- ok + 1L else bad <<- c(bad, label)
}

grid <- expand.grid(rho = c(0, .1, .25, .4, .6, .85),
                    sr  = c(.02, .04, .10, .20, .35, .60),
                    br  = c(.02, .04, .10, .20, .35, .60))

## 1. cell probabilities form a distribution
for (i in seq_len(nrow(grid))) {
  p <- cell_probs(grid$rho[i], grid$sr[i], grid$br[i])
  chk("cells sum to 1",  abs(sum(p) - 1) < 1e-9)
  chk("cells non-negative", all(p > -1e-12))
}

## 2. the identity: false_alarm - miss == sr - br, for every rho
for (i in seq_len(nrow(grid))) {
  p <- cell_probs(grid$rho[i], grid$sr[i], grid$br[i])
  chk("FA - miss == sr - br",
      abs((p[["false_alarm"]] - p[["miss"]]) - (grid$sr[i] - grid$br[i])) < 1e-9)
}

## 3. success ratio == sensitivity whenever the cutoffs are linked,
##    and zero validity reproduces the base rate exactly
for (rho in c(0, .2, .4, .6)) for (p in c(.04, .10, .20)) {
  s <- sel_stats(sel_params(rho = rho, sr = p, n = 1000))
  chk("success ratio == sensitivity", abs(s$success_ratio - s$sensitivity) < 1e-12)
  chk("rho = 0 gives the base rate", rho != 0 || abs(s$success_ratio - p) < 1e-9)
}

## 4. the hand-rolled bivariate CDF against two independent implementations
if (requireNamespace("mvtnorm", quietly = TRUE)) {
  set.seed(1)
  for (i in 1:200) {
    h <- runif(1, -3, 3); k <- runif(1, -3, 3); r <- runif(1, -.95, .95)
    ref <- mvtnorm::pmvnorm(upper = c(h, k), corr = matrix(c(1, r, r, 1), 2))[1]
    chk("p_bivnorm vs mvtnorm", abs(p_bivnorm(h, k, r) - ref) < 1e-8)
  }
}
if (requireNamespace("pbivnorm", quietly = TRUE)) {
  set.seed(2)
  for (i in 1:100) {
    h <- runif(1, -3, 3); k <- runif(1, -3, 3); r <- runif(1, -.95, .95)
    chk("p_bivnorm vs pbivnorm",
        abs(p_bivnorm(h, k, r) - pbivnorm::pbivnorm(h, k, rho = r)) < 1e-7)
  }
}

## 5. rounded percentages and counts sum exactly
for (i in seq_len(nrow(grid))) {
  s <- sel_stats(sel_params(rho = grid$rho[i], sr = grid$sr[i],
                            br = grid$br[i], n = 1000))
  chk("percentages sum to 100", abs(sum(s$percent) - 100) < 1e-9)
  chk("counts sum to n",        sum(s$counts_int) == 1000)
}

## 6. zbar (the mean of the selected) against a brute-force truncated mean
for (p in c(.01, .04, .10, .20, .50)) {
  num <- integrate(function(z) z * dnorm(z), z_cut(p), Inf)$value / p
  chk("zbar_x", abs(lambda(p) - num) < 1e-8)
  st <- sel_stats(sel_params(rho = .4, sr = p, n = 1000))
  chk("zbar_y == rho * zbar_x", abs(st$zbar_y - .4 * st$zbar_x) < 1e-12)
}

## 6b. utility decomposition
u <- bcg_utility(sel_params(rho = .4, sr = .04, n = 1000), sd_y = 50000, cost = 1750)
chk("net benefit = benefit - cost", abs(u$net_benefit - (u$benefit - u$cost)) < 1e-9)
chk("benefit uses analytic zbar",
    abs(u$benefit - 40 * 50000 * .4 * lambda(.04)) < 1e-6)

## 7. validation rejects bad input
chk("rejects rho = 1", inherits(try(sel_params(1,  .1), silent = TRUE), "try-error"))
chk("rejects sr = 0",  inherits(try(sel_params(.4,  0), silent = TRUE), "try-error"))
chk("rejects tiny n",  inherits(try(sel_params(.4, .001, n = 100), silent = TRUE),
                                "try-error"))

## 8. the exact construction delivers what it promises
for (rho in c(0, .2, .4, .6)) for (p in c(.04, .20)) {
  pr  <- sel_params(rho = rho, sr = p, n = 1000, seed = 42, exact = TRUE)
  d   <- simulate_panel(make_xz(pr), pr)$data
  chk("exact r",             abs(cor(d$x, d$y) - rho) < 1e-10)
  chk("exact mean of x",     abs(mean(d$x)) < 1e-10)
  chk("exact sd of y",       abs(sd(d$y) - 1) < 1e-10)
  chk("exact conditional mean",
      abs(mean(d$y[d$selected]) - rho * mean(d$x[d$selected])) < 1e-10)
}

## 9. one make_xz() serves every validity (this is what the triplet relies on)
pr <- sel_params(rho = .4, sr = .20, n = 1000, seed = 7)
xz <- make_xz(pr)
for (rho in c(0, .2, .4, .6, .8)) {
  d <- simulate_panel(xz, pr, rho = rho)$data
  chk("shared construction, exact r", abs(cor(d$x, d$y) - rho) < 1e-10)
}

## 10. empirical cuts select exactly round(n * p)
pr <- sel_params(rho = .4, sr = .04, n = 1000, seed = 3,
                 exact = FALSE, cuts = "empirical")
sim <- simulate_panel(make_xz(pr), pr)
chk("empirical cut selects exactly k", sum(sim$data$selected) == 40)
chk("empirical cut on y too",          sum(sim$data$successful) == 40)

cat(sprintf("\n%d checks passed", ok))
if (length(bad)) {
  cat(sprintf(", %d FAILED:\n  %s\n", length(bad), paste(unique(bad), collapse = "\n  ")))
  quit(status = 1)
} else cat(", 0 failed.\n")
