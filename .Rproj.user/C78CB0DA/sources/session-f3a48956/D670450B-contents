library(shiny)
library(ggplot2)
library(ggExtra)
library(dplyr)

ui <- fluidPage(
    titlePanel("Idea Quality Simulation with Selection Rectangles"),

    sidebarLayout(
        sidebarPanel(
            numericInput("N", "Number of ideas:", value = 5000, min = 1000, max = 1e6, step = 1000),
            numericInput("mu", "Mean (predictions):", value = 0, step = 0.1),
            numericInput("sigma", "SD (predictions):", value = 1, min = 0.1, step = 0.1),
            sliderInput("rho", "Predictor–truth correlation:", min = 0, max = 1, value = 0.7, step = 0.05),
            sliderInput("topPerc", "Select Top % of predictions:", min = 1, max = 100, value = 20, step = 1),
            checkboxInput("showBoxes", "Show category rectangles", value = TRUE),
            actionButton("go", "Resample")
        ),

        mainPanel(
            plotOutput("scatterPlot", height = 600),
            br(),
            tableOutput("confMatrix")
        )
    )
)

server <- function(input, output) {

    # Generate predictions and truths on resample
    data_gen <- eventReactive(input$go, {
        set.seed(Sys.time())
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

    output$scatterPlot <- renderPlot({
        df <- data_gen()

        # Top % cutoff
        perc <- input$topPerc
        pred_cut <- quantile(df$prediction, probs = 1 - perc/100)
        truth_cut <- quantile(df$truth, probs = 1 - perc/100)

        # Selection based on prediction
        df$selected <- df$prediction >= pred_cut
        mean_selected <- if (any(df$selected)) mean(df$prediction[df$selected]) else NA

        # Finite boundaries
        x_min <- min(df$prediction)
        x_max <- max(df$prediction)
        y_min <- min(df$truth)
        y_max <- max(df$truth)

        rects <- data.frame(
            xmin = c(pred_cut, pred_cut, x_min, x_min),
            xmax = c(x_max, x_max, pred_cut, pred_cut),
            ymin = c(truth_cut, y_min, truth_cut, y_min),
            ymax = c(y_max, truth_cut, y_max, truth_cut),
            label = c("Hit", "False alarm", "Miss", "Correct rejection"),
            # Text positions inside rectangles
            x_text = c(x_max, x_max, x_min, x_min),
            y_text = c(y_max, y_min, y_max, y_min),
            hjust = c(1,1,0,0),  # right-aligned for right rectangles, left-aligned for left
            vjust = c(1,0,1,0)   # top for top rectangles, bottom for bottom
        )


        # Base scatterplot with two colors
        p <- ggplot(df, aes(x = prediction, y = truth, color = selected)) +
            geom_point(alpha = 0.6, size = 0.8) +
            scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "darkgoldenrod2")) +
            labs(x = "Predicted goodness", y = "True goodness", color = "Selected") +
            theme_minimal(base_size = 20)+
            theme(legend.position = "none")

        # Add grey rectangles and internal labels if toggled
        if (input$showBoxes) {
            p <- p +
                geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                          inherit.aes = FALSE, fill = "grey80", alpha = 0.2) +
                geom_text(data = rects, aes(x = x_text, y = y_text, label = label, hjust = hjust, vjust = vjust),
                          inherit.aes = FALSE, size = 6, fontface = "bold")
        }

        # Add threshold line and mean selected line
        p <- p +
            geom_vline(xintercept = pred_cut, linetype = "dashed", color = "dodgerblue3", linewidth = 1) +
            geom_hline(yintercept = truth_cut, linetype = "dashed", color = "green4", linewidth = 1)

        if (!is.na(mean_selected)) {
            p <- p + geom_vline(xintercept = mean_selected, color = "darkgoldenrod2", linewidth = 1) +
            annotate("text", label = "Mean selected", x = mean_selected + 0.02*x_max, y = y_min + 0.02 * y_max, hjust = 0, vjust = 0, size = 5, color = "darkgoldenrod2")
        }

        ggMarginal(p, type = "density", margins = "y", groupColour = TRUE, groupFill = TRUE)
    })

    # Confusion matrix table
    output$confMatrix <- renderTable({
        df <- data_gen()
        N <- nrow(df)

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
