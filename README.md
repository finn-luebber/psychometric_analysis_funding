# A Psychometric Analysis and Primer for Decision Making in Competitive Public Research Funding Allocation - Repository

This repository holds the Shiny app and the figure scripts accompanying the
manuscript (preprint: <https://osf.io/preprints/undefined/mp8y2_v3>).


**The app is currently also hosted at: https://finn-luebber.shinyapps.io/bcg_model/**


The app is an interactive version of Figures 3 and 4 from the main text. You set
the validity, the selection ratio and the cutoff on the actual value, and it
shows the resulting scatter, the 2×2 classification table, and the utility of
the selection procedure under the Brogden–Cronbach–Gleser model — the benefit,
the cost, and the net benefit ΔU.



## Contents

```
app.R                    the Shiny application
setup.R                  sources everything in R/; the first line of every entry point
R/analytics.R            the model: cell probabilities, z-bar, BCG utility
R/simulate.R             generating and classifying the illustrative proposals
R/plot.R                 theme, panel, marginals, legend
scripts/fig_triplet.R    Figure 3: three validities, 2×2 boxes
scripts/fig_bcg.R        Figure 4: one panel, means of the selected
tests/test_analytics.R   assertions on the model layer
renv.lock                pinned package versions
*.Rproj                  RStudio project file, and the here::here() root anchor
```

## Reproducing the environment

This project uses [`renv`](https://rstudio.github.io/renv/) to pin package
versions.

```r
install.packages("renv")   # if not already installed
renv::restore()            # installs the exact versions from renv.lock
```

Packages used: ggplot2, ggtext, patchwork, svglite, withr, here, and for the app
shiny and shinyjs. `R/analytics.R` deliberately has no dependencies beyond base
R, so the model layer can be read, run and checked on its own.

## Running the app

Open the `.Rproj` file in RStudio (this sets the project root), then:

```r
shiny::runApp()
```

Every entry point sources its helpers via `here::here()`, which locates the
project root from the `.Rproj` file, so the working directory at launch does not
matter and `app.R` can be moved into a subfolder without anything breaking.

## Reproducing the figures

```r
source(here::here("scripts", "fig_triplet.R"))
source(here::here("scripts", "fig_bcg.R"))
```

Each writes an `.svg` and a `.png` into `figures/`, and prints the numbers that
appear in the figure captions to the console.

Both scripts open with a short `SPEC` block holding every choice the figure
makes — validity, selection ratio, base rate, n, seed, size:

```r
SR <- 0.20          # selection ratio
BR <- SR            # cutoff on the actual value, linked to the selection ratio
```

`BR` defaults to `SR`, which puts both cutoffs at the same percentile. Setting it
explicitly decouples them (e.g. `SR <- 0.04; BR <- 0.20` funds the top 4% while
counting the top 20% of outcomes as successes). The thresholds appear in the
output filename, so the two variants cannot be mistaken for one another.

## Running the checks

```r
source(here::here("tests", "test_analytics.R"))
```

or from a shell, `Rscript tests/test_analytics.R`. This is a plain script rather
than a `testthat` suite, because the repository is a set of analysis scripts and
not an R package; it needs no test framework and prints a pass/fail count.

It checks that the cell probabilities form a distribution, that the identity
`false alarms − misses == selection ratio − base rate` holds at every validity,
that the success ratio equals sensitivity when the cutoffs are linked, that the
bivariate normal routine agrees with `mvtnorm` and `pbivnorm`, and that the
constructed illustrative data reproduces the stated validity and means exactly.
Worth running after changing anything in `R/`.

## A note on the plotted points

The figures show a constructed configuration, not a random sample. The
proposals are arranged so that the plotted cloud reproduces the analytic
validity and the analytic means of the selected group exactly, rather than
approximately. This matters because the mean outcome of a small selected group
is noisy: selecting the top 4% of 1000 leaves only 40 proposals driving it, with
a sampling SD of about 0.15 against a value of 0.86. Figure captions should say
that the points are illustrative and constructed.

The app does the opposite, drawing ordinary random samples, because there the
gap between a sample and the model is the thing worth seeing. Tick "show
observed sample means" and press Resample: the vertical line barely moves while
the horizontal one jumps.

## Notes

Contact: Finn Luebber, f.luebber2@uni-luebeck.de
Coded with heavy assistance of Claude Opus 5
