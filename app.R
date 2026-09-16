## =============================================================================
##  Interactive explorer -- same functions as the manuscript figures
## =============================================================================
##  Differences from the static scripts, all deliberate:
##
##  exact = FALSE     the app draws ordinary random samples. Seeing the sample
##                    miss the analytic values is the point here, which is what
##                    "show sample means" is for: press Resample and the
##                    vertical line barely moves while the horizontal one jumps.
##                    At a 4% selection ratio out of 1000 only 40 proposals
##                    drive the mean actual value, so its sampling SD is about
##                    0.15 against a value of 0.86.
##
##  cuts = empirical  exactly round(n * SR) proposals fall above the cutoff,
##                    which matches the interactive framing ("we fund 40").
##
##  The reactive graph is arranged so that only the seed regenerates the cloud.
##  Moving rho rotates it, moving a threshold slides a line. Nothing reshuffles
##  unless you ask it to.
##
##  The panel aspect is always fixed, so toggling the marginal densities adds
##  the margins without stretching the scatter underneath them. A non-square
##  panel would rescale the apparent slope of the validity line by height/width.
## =============================================================================

library(shiny)
library(shinyjs)
library(ggplot2)
library(patchwork)
source(here::here("setup.R"))

FAMILY <- safe_family("Helvetica")
BASE   <- 14                           # screen, not print

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Validity, selection and utility"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("rho", "Validity r_xy (diagnosis\u2013actual value correlation)",
                  min = 0, max = 0.95, value = 0.40, step = 0.05),
      hr(),
      sliderInput("sr", "Selection ratio: fund the top % on the diagnosis",
                  min = 1, max = 99, value = 4, step = 1, post = "%"),
      checkboxInput("link", "Cutoff on the actual value linked to it",
                    value = TRUE),
      sliderInput("br", "Base rate: top % on the actual value counted a success",
                  min = 1, max = 99, value = 4, step = 1, post = "%"),
      helpText(HTML(paste0(
        "The two thresholds are independent. Note that <b>false alarms minus ",
        "misses always equals the selection ratio minus the base rate</b>, ",
        "whatever the validity."))),
      hr(),
      numericInput("N", "Illustrative proposals", value = 1000, min = 50,
                   max = 20000, step = 50),
      numericInput("seed", "Seed", value = 123, step = 1),
      actionButton("resample", "Resample", icon = icon("dice")),
      hr(),
      h5("Display"),
      checkboxGroupInput(
        "layers", NULL,
        choices  = c("Proposals" = "points", "2\u00d72 boxes" = "boxes",
                     "Cutoff: diagnosis" = "cut_x",
                     "Cutoff: actual value" = "cut_y",
                     "Validity line" = "line",
                     "Mean of selected" = "means",
                     "Density contours" = "contour"),
        selected = c("points", "cut_x", "cut_y", "boxes", "line")),
      checkboxInput("sample_means", "Also show observed sample means",
                    value = FALSE),
      checkboxInput("marginals", "Marginal densities", value = TRUE),
      checkboxInput("legend", "Legend", value = TRUE),
      hr(),
      h5("Utility (Brogden\u2013Cronbach\u2013Gleser)"),
      numericInput("sd_y", "SD of net benefit (SD_y)", value = 50000, min = 0,
                   step = 1000),
      numericInput("cost", "Cost per proposal assessed", value = 1750,
                   min = 0, step = 50)
    ),

    mainPanel(
      width = 9,
      fluidRow(
        # the plot keeps a square panel, so it is given a column it roughly
        # fills rather than the full width with empty margins either side
        column(7, plotOutput("plot", height = "660px")),
        column(
          5,
          h4("2\u00d72 table"),
          helpText(HTML(paste0(
            "Expected values come from the bivariate normal model; observed ",
            "values are tallies of the plotted proposals."))),
          tableOutput("cells"),
          h4("Selected proposals"),
          tableOutput("means"),
          h4("Utility"),
          tableOutput("utility")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  ## --- thresholds ------------------------------------------------------------
  ## Computed directly rather than round-tripped through updateSliderInput, so
  ## there is no browser lag and no chance of a feedback loop. The slider is
  ## disabled purely as a visual cue.
  observe({ if (isTRUE(input$link)) disable("br") else enable("br") })

  sr_val <- reactive(input$sr / 100)
  br_val <- reactive(if (isTRUE(input$link)) sr_val() else input$br / 100)

  ## --- seed ------------------------------------------------------------------
  ## Resample nudges the seed. Everything else stays live, so moving rho cannot
  ## change the labels while leaving the cloud at the old validity.
  bump <- reactiveVal(0L)
  observeEvent(input$resample, bump(bump() + 1L))
  seed_used <- reactive(as.integer(input$seed) + bump())

  ## --- parameters ------------------------------------------------------------
  params <- reactive({
    req(input$N, input$seed)
    tryCatch(
      sel_params(rho = input$rho, sr = sr_val(), br = br_val(),
                 n = input$N, seed = seed_used(),
                 exact = FALSE, cuts = "empirical"),
      error = function(e) { validate(need(FALSE, conditionMessage(e))); NULL })
  })

  ## The draw depends only on n and seed. rho and sr here are placeholders:
  ## with exact = FALSE, make_xz() ignores them. This is what stops the cloud
  ## reshuffling every time a threshold slider moves.
  xz <- reactive({
    req(input$N)
    make_xz(sel_params(rho = 0, sr = 0.5, n = input$N,
                       seed = seed_used(), exact = FALSE))
  })

  sim   <- reactive(simulate_panel(xz(), params(), rho = input$rho))
  stats <- reactive(sel_stats(params()))

  ## --- plot ------------------------------------------------------------------
  output$plot <- renderPlot({
    st <- stats(); sm <- sim()

    p <- validity_panel(
        sm, st,
        layers            = input$layers,
        labels            = c("percent", "count"),
        show_sample_means = isTRUE(input$sample_means),
        base_size         = BASE, base_family = FAMILY,
        fixed_aspect      = !isTRUE(input$marginals))

    if (isTRUE(input$marginals))
      p <- add_marginals(p, sm, st, which = c("top", "right"),
                         base_size = BASE, base_family = FAMILY,
                         split = "selected_vs_all")

    if (isTRUE(input$legend)) {
      items <- c(if ("boxes" %in% input$layers) c("correct", "incorrect"),
                 if ("line"  %in% input$layers) "validity",
                 if (any(c("cut_y") %in% input$layers)) "cut_y",
                 if (any(c("cut_x") %in% input$layers)) "cut_x",
                 if ("means" %in% input$layers) "means",
                 if ("means" %in% input$layers && isTRUE(input$sample_means)) "sample",
                 if ("points" %in% input$layers) c("selected", "unselected"))
      if (length(items)) {
        rows <- legend_rows(items)
        p <- wrap_plots(p, validity_legend(items, base_size = BASE + 4,
                                           base_family = FAMILY),
                        ncol = 1, heights = c(8, 1.6 * rows))
      }
    }

    ## title block at the outermost level, or nesting swallows it
    p + fig_annotation(
      title = sprintf("r<sub>xy</sub> = %.2f \u00b7 selection ratio %.0f%% \u00b7 base rate %.0f%%",
                      input$rho, 100 * sr_val(), 100 * br_val()),
      subtitle = sprintf(
        "Success ratio %.1f%% (base rate %.1f%%) \u00b7 %s = %.2f, %s = %.2f",
        100 * st$success_ratio, 100 * st$base_rate,
        zbar_md("x"), st$zbar_x, zbar_md("y"), st$zbar_y),
      base_size = BASE, base_family = FAMILY)
  })

  ## --- tables ----------------------------------------------------------------
  html_table <- function(df, align = "lrr") {
      expr <- substitute(df)
      env  <- parent.frame()
      renderTable(expr, quoted = TRUE, env = env,
                  striped = TRUE, spacing = "xs", align = align,
                  sanitize.text.function = identity)
  }

  output$cells <- html_table({
    st <- stats(); ss <- sample_stats(sim()); lv <- cell_levels()
    data.frame(
      Cell       = unname(cell_names()[lv]),
      Expected   = sprintf("%.1f%% (%.1f)", st$percent[lv], st$counts[lv]),
      Observed   = sprintf("%.1f%% (%d)", ss$percent[lv],
                           as.integer(ss$counts[lv])),
      check.names = FALSE)
  })

  output$means <- html_table({
    st <- stats(); ss <- sample_stats(sim())
    data.frame(
      Quantity = c(paste0("Mean diagnosis ", zbar_md("x")),
                   paste0("Mean actual value ", zbar_md("y")),
                   "Correlation", "Number selected"),
      Expected = c(sprintf("%.3f", st$zbar_x), sprintf("%.3f", st$zbar_y),
                   sprintf("%.3f", params()$rho), sprintf("%d", st$n_selected)),
      Observed = c(sprintf("%.3f", ss$mean_x_sel), sprintf("%.3f", ss$mean_y_sel),
                   sprintf("%.3f", ss$r), sprintf("%d", ss$n_selected)),
      check.names = FALSE)
  })

  ## Analytic zbar_x, so this can never drift away from the line in the plot.
  output$utility <- html_table({
    u <- bcg_utility(params(), sd_y = input$sd_y, cost = input$cost)
    fmt <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
    data.frame(
      Quantity = c("Proposals selected", paste0("Mean diagnosis ", zbar_md("x")),
                   "Benefit", "Cost", "<b>&Delta;U (net benefit)</b>",
                   "Per selected proposal"),
      Value = c(sprintf("%d of %d", round(u$n_selected), u$n_applicants),
                sprintf("%.3f", u$zbar_x), fmt(u$benefit), fmt(u$cost),
                paste0("<b>", fmt(u$net_benefit), "</b>"), fmt(u$per_selectee)),
      check.names = FALSE)
  }, align = "lr")
}

shinyApp(ui, server)
