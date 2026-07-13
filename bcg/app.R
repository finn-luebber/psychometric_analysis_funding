library(shiny)
library(shinyjs)
library(bslib)
library(ggplot2)
library(ggExtra)
library(ggtext)
library(dplyr)
library(patchwork)

source("create_output_plot.R")

ui <- fluidPage(
    useShinyjs(),
    titlePanel("BCG model illustration"),

    page_sidebar(
        theme = bs_theme(version = 5, bootswatch = "united"),  # or superhero?

        sidebar = sidebar(

            numericInput("N", "Number of applications:", value = 1000, min = 100, max = 1e6, step = 100),
            sliderInput("costs", "Cost of individual application", min = 0, max = 100000, value = 1750, step = 50),
            numericInput("SD_y", "Standard Deviation of net project benefits (SD_y)", value = 50000, min = 0),
            # numericInput("mu", "Mean (predictions):", value = 0, step = 0.1),
            # numericInput("sigma", "SD (predictions):", value = 1, min = 0.1, step = 0.1),
            sliderInput("rho", "Predictor–truth correlation:", min = 0, max = 1, value = 0.4, step = 0.05),
            numericInput("seed", label = "Seed", value = 123),
            actionButton("go", "Resample"),
            textOutput("BCG_result")


        ),


        fluidRow(
            column(10,
                   card(
                       card_header("Benefit of selected proposals"),
                       card_body(
                           layout_column_wrap(
                               width = 1/2,
                               sliderInput("topPerc", "Cutoff: Select Top % of predictions (X):", min = 1, max = 100, value = 4, step = 1),
                               sliderInput("topPercY", "Goal: Select Top % of truth (Y):", min = 1, max = 100, value = 4, step = 1),
                           ),
                           checkboxInput("connect_cutoffs", "Y cutoff should equal X cutoff", value = F),
                           plotOutput("scatterPlot", height = 600),
                           layout_column_wrap(
                               width = 1/4,
                               checkboxInput("showBoxes", "Show 2x2 table", value = FALSE),
                               checkboxInput("showScatter", "Show scatter", value = TRUE),
                               checkboxInput("showContour", "Show contours", value = FALSE),
                               checkboxInput("showLine", "Show best fit line", value = TRUE),
                           )
                       )
                   )
            )
        )
    )
)

server <- function(input, output, session) {


    unselected_color = "gray60"
    selected_color = "darkgoldenrod2"


    my_theme <- theme_minimal(base_family = "Helvetica") +
        theme(
            legend.title = element_blank(),
            legend.text = element_text(size = 20),
            axis.title = element_text(size = 20, face = "bold", color = "black"),
            axis.text.x = element_text(size = 16, color = "black"),
            axis.text.y = element_text(size = 16, color = "black"),
            plot.title = element_blank(),
            plot.caption = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
            # plot.margin = unit(c(0, 0, 0, 0), "cm")
        )

    cut_off <- reactiveValues(prediction = NULL, truth = NULL)

    observe({
        if(input$connect_cutoffs){
            updateSliderInput(session = session, inputId = "topPercY", value = input$topPerc)
            disable("topPercY")
        } else{
            enable("topPercY")

        }
    })


    # Generate predictions and truths on resample
    data_gen <- eventReactive(input$go, {
        set.seed(input$seed)
        N <- input$N
        mu <- 0 # input$mu
        sigma <- 1 # input$sigma
        rho <- input$rho

        X <- rnorm(N, mean = mu, sd = sigma)
        Z <- rnorm(N)
        T <- rho * (X - mu)/sigma + sqrt(1 - rho^2) * Z
        T <- mu + sigma * T

        data.frame(prediction = X, truth = T)
    }, ignoreNULL = FALSE)

    observe({
        df <- data_gen()
        perc <- input$topPerc
        cut_off$prediction <- quantile(df$prediction, probs = 1 - perc/100)

        if(input$connect_cutoffs){
            cut_off$truth <- quantile(df$truth, probs = 1 - perc/100)
        } else {
            perc_y <- input$topPercY
            cut_off$truth <- quantile(df$truth, probs = 1 - perc_y/100)
        }
    })

    data_selected <- reactive({
        df <- data_gen()

        # Top % cutoff
        pred_cut <- cut_off$prediction

        # Selection based on prediction
        df$selected <- df$prediction >= pred_cut

        df
    })

    update_output_plot(input, output, data_selected, "truth", cut_off, my_theme, unselected_color, selected_color)


    # Confusion matrix table
    output$confMatrix <- renderTable({
        df <- data_gen()
        N <- nrow(df)

        # SAME CUTOFF ALWAYS
        perc <- input$topPerc
        pred_cut <- quantile(df$prediction, probs = 1 - perc/100)
        truth_cut <- quantile(df$truth, probs = 1 - perc/100)

        df <- df %>%
            mutate(category = case_when(
                prediction >= pred_cut & truth >= truth_cut ~ "Hit",
                prediction >= pred_cut & truth < truth_cut ~ "False alarm",
                prediction < pred_cut & truth >= truth_cut ~ "Miss",
                TRUE ~ "Correct rejection"
            ))

        tbl <- table(df$category)
        data.frame(
            Category = names(tbl),
            Count = as.integer(tbl),
            Percent = round(100 * as.integer(tbl)/N, 1)
        )
    })

    output$BCG_result <- renderText({
        number_selected <- input$topPerc/100 * input$N
        mean_selected <- {
            preds <- data_selected()$prediction
            sel   <- data_selected()$selected
            z     <- (preds - mean(preds)) / sd(preds)
            if (any(sel)) mean(z[sel]) else NA
        }
        delta_U <- number_selected * input$rho * input$SD_y * mean_selected - input$N * input$costs


        paste0("delta U = ",
               format(delta_U, big.mark = ",", decimal.mark = ".", nsmall = 2, scientific = FALSE)
        )
    })

}

shinyApp(ui, server)
