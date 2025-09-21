library(shiny)
library(shinyjs)
library(bslib)
library(ggplot2)
library(ggExtra)
library(dplyr)
library(patchwork)


ui <- fluidPage(
    useShinyjs(),
    titlePanel("BCG model illustration"),

    page_sidebar(
        theme = bs_theme(version = 5, bootswatch = "united"),  # or superhero?

        sidebar = sidebar(

            numericInput("N", "Number of applications:", value = 1000, min = 100, max = 1e6, step = 100),
            sliderInput("costs", "Cost of individual application", min = 0, max = 100000, value = 1000, step = 100),
            numericInput("mu", "Mean (predictions):", value = 0, step = 0.1),
            numericInput("sigma", "SD (predictions):", value = 1, min = 0.1, step = 0.1),
            sliderInput("rho", "Predictor–truth correlation:", min = 0, max = 1, value = 0.5, step = 0.05),
            actionButton("go", "Resample")


        ),

        fluidRow(

            fluidRow(
                column(10,
                       card(
                           card_header("Benefit of selected proposals"),
                           card_body(
                               layout_column_wrap(
                                   width = 1/2,
                                   sliderInput("topPerc", "Select Top % of predictions (X):", min = 1, max = 100, value = 20, step = 1),
                                   sliderInput("topPercY", "Select Top % of truth (Y):", min = 1, max = 100, value = 50, step = 1),
                               ),
                               checkboxInput("connect_cutoffs", "Y cutoff should equal X cutoff", value = F),
                               plotOutput("scatterPlot", height = 600),
                               layout_column_wrap(
                                   width = 1/4,
                                   checkboxInput("showBoxes", "Show 2x2 table", value = TRUE),
                                   checkboxInput("showScatter", "Show scatter", value = TRUE),
                                   checkboxInput("showContour", "Show contours", value = TRUE),
                                   checkboxInput("showLine", "Show best fit line", value = TRUE),
                               )
                           )
                       ),
                       br(),
                       tableOutput("confMatrix")
                )
            ),
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

    observeEvent(input$connect_cutoffs, {
        if(input$connect_cutoffs){
            updateSliderInput(session = session, inputId = "topPercY", value = input$topPerc)
            disable("topPercY")
        } else{
            enable("topPercY")

        }
    })


    # Generate predictions and truths on resample
    data_gen <- eventReactive(input$go, {
        set.seed(020522)
        N <- input$N
        mu <- input$mu
        sigma <- input$sigma
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
        truth_cut <- cut_off$truth

        # Selection based on prediction
        df$selected <- df$prediction >= pred_cut

        df
    })

    output$scatterPlot <- renderPlot({
        df <- data_selected()

        pred_cut <- cut_off$prediction
        truth_cut <- cut_off$truth

        mean_selected <- if (any(df$selected)) mean(df$prediction[df$selected]) else NA

        # Finite boundaries
        x_min <- min(df$prediction)
        x_max <- max(df$prediction)
        y_min <- min(df$truth)
        y_max <- max(df$truth)

        # Classify each point
        df <- df %>%
            mutate(category = case_when(
                prediction >= pred_cut & truth >= truth_cut ~ "Hit",
                prediction >= pred_cut & truth < truth_cut ~ "False alarm",
                prediction < pred_cut & truth >= truth_cut ~ "Miss",
                TRUE ~ "Correct rejection"
            ))

        # Confusion matrix percentages
        N <- nrow(df)
        percent_df <- df %>%
            count(category, name = "Count") %>%
            mutate(Percent = round(100 * Count / N, 1))

        # Rectangle positions + merge with percentages
        rects <- data.frame(
            label = c("Hit", "False alarm", "Miss", "Correct rejection"),
            xmin  = c(pred_cut, pred_cut, x_min, x_min),
            xmax  = c(x_max, x_max, pred_cut, pred_cut),
            ymin  = c(truth_cut, y_min, truth_cut, y_min),
            ymax  = c(y_max, truth_cut, y_max, truth_cut),
            # Put text in the center of each rect
            x_text = c((pred_cut + x_max)/2,
                       (pred_cut + x_max)/2,
                       (x_min + pred_cut)/2,
                       (x_min + pred_cut)/2),
            y_text = c((truth_cut + y_max)/2,
                       (y_min + truth_cut)/2,
                       (truth_cut + y_max)/2,
                       (y_min + truth_cut)/2)
        ) %>%
            left_join(percent_df, by = c("label" = "category")) %>%
            mutate(
                type = ifelse(label %in% c("Hit", "Correct rejection"), "Correct", "Incorrect")
            )

        xlims <- range(df$prediction)
        ylims <- range(df$truth)



        # Extract contour paths
        contours <- ggplot_build(
            ggplot(df, aes(prediction, truth)) +
                stat_density_2d(geom = "path", bins = 6) # pick number of contours
        )$data[[1]]

        # Add color depending on x threshold
        threshold <- 0
        contours <- contours %>%
            mutate(color_side = ifelse(x < pred_cut, "FALSE", "TRUE"))


        # Base scatterplot with two colors
        p <- ggplot(df, aes(x = prediction, y = truth))

        if (input$showScatter) {
            p <- p + geom_point(aes(color = selected), alpha = 0.6, size = 0.8)
        }

        if(input$showContour){
            p <- p + geom_path(
                data = contours,
                aes(x = x, y = y, group = interaction(level, piece), color = color_side),
                linewidth = 1
            )
        }
        if(input$showLine){
            p <- p + geom_smooth(method = "lm")
        }

        p <- p +
            scale_color_manual(values = c("FALSE" = unselected_color, "TRUE" = selected_color)) +
            labs(x = "Predicted goodness", y = "True goodness", color = "Selected") +
            xlim(xlims) +
            ylim(ylims) +
            my_theme +
            theme(legend.position = "none")

        # Add grey rectangles and internal labels if toggled
        if (input$showBoxes) {
            p <- p +
                geom_rect(
                    data = rects,
                    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                        fill = type, alpha = Percent),
                    inherit.aes = FALSE, color = "black"
                ) +
                geom_text(
                    data = rects,
                    aes(x = x_text, y = y_text,
                        label = paste0(label, "\n", Percent, "%")),
                    inherit.aes = FALSE,
                    size = 5, fontface = "bold", color = "black"
                ) +
                scale_fill_manual(values = c("Correct" = "#377EB8", "Incorrect" = "#E6550D")) +
                scale_alpha(range = c(0.2, 0.9), guide = "none")
        }


        # Add threshold line and mean selected line
        p <- p +
            geom_vline(xintercept = pred_cut, linetype = "dashed", color = "dodgerblue3", linewidth = 1) +
            geom_hline(yintercept = truth_cut, linetype = "dashed", color = "green4", linewidth = 1)

        if (!is.na(mean_selected)) {
            p <- p + geom_vline(xintercept = mean_selected, color = selected_color, linewidth = 1) +
                annotate("text", label = "Mean selected", x = mean_selected + 0.02*x_max, y = y_min + 0.02 * y_max, hjust = 0, vjust = 0, size = 5, color = "darkgoldenrod2")
        }


        # Add marginal plots

        dens <- density(df$prediction, n = 1024)
        dens_df <- data.frame(x = dens$x, y = dens$y)

        dens_left  <- subset(dens_df, x < pred_cut)
        dens_right <- subset(dens_df, x >= pred_cut)

        p_top <- ggplot() +
            geom_area(data = dens_left, aes(x, y), fill = unselected_color, alpha = 0.6) +
            geom_area(data = dens_right, aes(x, y), fill = selected_color, alpha = 0.6) +
            xlim(xlims) +
            theme_void()

        p_right <- ggplot(df, aes(truth, fill = selected)) +
            stat_density(geom = "area", position = "identity", alpha = 0.5, trim = TRUE) +
            xlim(ylims) + # xlim not ylim because of axis flipping
            coord_flip() +
            scale_fill_manual(values = c("FALSE" = unselected_color, "TRUE" = selected_color)) +
            theme_void() +
            theme(legend.position = "none")

        # p_top / p + plot_layout(heights = c(1, 4))

        # --- layout design ---
        # layout <- "
        # A
        # CB
        # "

        # p_top + p_right + p +
        #     plot_layout(design = layout, heights = c(1, 4), widths = c(4, 1))

        p_top + plot_spacer() + p + p_right +
            plot_layout(heights = c(1, 4), widths = c(4, 1))
    })

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

}

shinyApp(ui, server)
