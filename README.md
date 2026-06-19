# Shiny app for: A Psychometric Analysis and Primer for Decision Making in Public Research Funding Decisions
The shiny app provides an interactive tool to explore figures 2 and 4 from the main manuscript
(preprint: https://osf.io/preprints/undefined/mp8y2_v3). It also outputs the delta U value,
i.e. the benefit (or loss) of using the simulated selection procedure.
The app is currently also hosted at: https://finn-luebber.shinyapps.io/bcg_model/

## Contents

- `bcg/app.R` — the Shiny application
- `create_output_plot.R` — helper function(s) sourced by the app
- `renv.lock` — pinned package versions for reproducibility
- `.Rproj` — RStudio project file (also serves as the `here::here()` root anchor)

## Reproducing the environment

This project uses [`renv`](https://rstudio.github.io/renv/) to pin package versions.

```r
install.packages("renv")   # if not already installed
renv::restore()            # installs exact versions from renv.lock
```

## Running the app

Open the `.Rproj` file in RStudio (this sets the project root), then:

```r
shiny::runApp("bcg")
```

The app sources its helper function via `here::here()`, which locates the
project root using the `.Rproj` file — so the working directory at launch
doesn't matter, as long as you open the project.

## Notes

Contact: Finn Luebber, University of Luebeck, f.luebber2@uni-luebeck.de
[Data sources, the live shinyapps.io URL if public, license, contact.]
