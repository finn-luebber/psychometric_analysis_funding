create_output_plot <- function(df, truth_col, pred_cut, truth_cut, my_theme, show_xlab, show_ylab, unselected_color, selected_color, showScatter = T, showContour = F, showLine = T, showBoxes = T, marginal_y = TRUE){
    # Turn string/column name into symbol
    truth_sym <- sym(truth_col)

    mean_selected <- if (any(df$selected)) mean(df$prediction[df$selected]) else NA
    mean_truth <- if (any(df$selected)) mean(df[[truth_col]][df$selected]) else NA

    # Finite boundaries
    x_min <- min(df$prediction)
    x_max <- max(df$prediction)
    y_min <- min(df[[truth_col]])
    y_max <- max(df[[truth_col]])

    xlims <- range(df$prediction)
    ylims <- range(df[[truth_col]])

    # Base scatterplot with two colors
    p <- ggplot(df, aes(x = prediction, y = !!truth_sym))

    ## SCATTER
    if (showScatter) {
        p <- p + geom_point(aes(color = selected), alpha = 0.6, size = 0.8)
    }

    ## CONTOUR LINES
    if(showContour){

        # Extract contour paths
        contours <- ggplot_build(
            ggplot(df, aes(prediction, !!truth_sym)) +
                stat_density_2d(geom = "path", bins = 6) # pick number of contours
        )$data[[1]]

        # Add color depending on x threshold
        contours <- contours %>%
            mutate(color_side = ifelse(x < pred_cut, "FALSE", "TRUE"))

        p <- p + geom_path(
            data = contours,
            aes(x = x, y = y, group = interaction(level, piece), color = color_side),
            linewidth = 1
        )
    }

    ## REGRESSION LINE
    if(showLine){
        p <- p + geom_smooth(method = "lm", color = "purple")
    }

    ## GENERAL SETTINGS
    p <- p +
        scale_color_manual(values = c("FALSE" = unselected_color, "TRUE" = selected_color)) +
        # labs(x = "Predictor score (Z)", y = "Project net benefit (Z)", color = "Selected") +
        xlim(xlims) +
        ylim(ylims) +
        my_theme +
        theme(legend.position = "none")

    if(show_xlab){
        p <- p + labs(x = "Predictor score (Z)")
    } else{
        p <- p + labs(x = "")+
            theme(
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            )

    }

    if(show_ylab){
        p <- p + labs(y = "Project net benefit (Z)")

    } else{
        p <- p + labs(y = "")

    }


    ## 2x2 TABLE
    if (showBoxes) {

        # Classify each point
        df <- df %>%
            mutate(category = case_when(
                prediction >= pred_cut & (!!truth_sym) >= truth_cut ~ "Hit",
                prediction >= pred_cut & (!!truth_sym) <  truth_cut ~ "False alarm",
                prediction <  pred_cut & (!!truth_sym) >= truth_cut ~ "Miss",
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




        p <- p +
            geom_rect(
                data = rects,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                    fill = type),
                alpha = .5,
                inherit.aes = FALSE, color = NA
            ) +
            geom_richtext(
                data = rects,
                aes(
                    x = x_text,
                    y = y_text,
                    label = paste0(
                        "<span style='font-size:22pt; color:#403d39; font-weight:bold;'>", label, "</span><br>",
                        Percent, "%<br>",
                        Count
                    )
                ),
                inherit.aes = FALSE,
                size = 5,        # this sets the default size for the non-styled text
                color = "#0d1b2a",
                fill = NA,       # remove background box
                label.color = NA # remove border
            )+
            scale_fill_manual(values = c("Correct" = "#377EB8", "Incorrect" = "#E6550D")) #+
    }


    ## ADD LINES

    # X cutoff label (anchored at bottom of plot)
    x_lab_y <- ylims[1]   # bottom of plot
    x_side_offset <- 0.05 * diff(xlims)  # 5% offset
    x_lab_x <- if (pred_cut > mean(xlims)) pred_cut - x_side_offset else pred_cut + x_side_offset

    # Y cutoff label (anchored at left of plot)
    y_lab_x <- xlims[1]   # left of plot
    y_side_offset <- 0.05 * diff(ylims)
    y_lab_y <- if (truth_cut > mean(ylims)) truth_cut - y_side_offset else truth_cut + y_side_offset


    p <- p +
        geom_vline(xintercept = pred_cut, linetype = "dashed", color = "dodgerblue3", linewidth = 1) +
        geom_hline(yintercept = truth_cut, linetype = "dashed", color = "green4", linewidth = 1)+
        annotate(
            "text",
            x = x_lab_x, y = x_lab_y,
            label = "cutoff",
            vjust = 1, color = "dodgerblue3"
        ) +
        annotate(
            "text",
            x = y_lab_x, y = y_lab_y,
            label = "cutoff (goal)",
            hjust = 0, color = "green4"
        )


    if (!is.na(mean_selected)) {
        p <- p + geom_vline(xintercept = mean_selected, color = selected_color, linewidth = 1) +
            annotate("text", label = paste0(expression(paste(over(,"Z"))), "[x]"), parse = TRUE, x = mean_selected + 0.02*x_max, y = y_min, hjust = 0, vjust = .8, size = 5, color = selected_color)+
            geom_hline(yintercept = mean_truth, color = "darkred", linewidth = 1) +
            annotate("text", label = paste0(expression(paste(over(,"Z"))), "[y]"), parse = TRUE, y = mean_truth + 0.02*y_max, x = x_min + 0.02 * x_max, hjust = 0, vjust = 0, size = 5, color = "darkred")
    }


    ## MARGINAL PLOTS

    dens <- density(df$prediction, n = 1024)
    dens_df <- data.frame(x = dens$x, y = dens$y)

    dens_left  <- subset(dens_df, x < pred_cut)
    dens_right <- subset(dens_df, x >= pred_cut)

    if (marginal_y){
        p_top <- ggplot() +
            geom_area(data = dens_left, aes(x, y), fill = unselected_color, alpha = 0.6) +
            geom_area(data = dens_right, aes(x, y), fill = selected_color, alpha = 0.6) +
            xlim(xlims) +
            theme_void()
    }

    p_right <- ggplot(df, aes(!!truth_sym, fill = selected)) +
        stat_density(geom = "area", position = "identity", alpha = 0.5, trim = TRUE) +
        xlim(ylims) + # xlim not ylim because of axis flipping
        coord_flip() +
        scale_fill_manual(values = c("FALSE" = unselected_color, "TRUE" = selected_color)) +
        theme_void() +
        theme(legend.position = "none")

    # plot layout
    if(marginal_y){
        p_top + plot_spacer() + p + p_right +
            plot_layout(heights = c(1, 4), widths = c(4, 1))
    } else {
        p + p_right +
            plot_layout(widths = c(4, 1))
    }
}


update_output_plot <- function(input, output, data_selected, truth_col, cut_off, my_theme, unselected_color, selected_color, marginal_y = TRUE){
    output$scatterPlot <- renderPlot({
        df <- data_selected()

        pred_cut <- cut_off$prediction
        truth_cut <- cut_off$truth

        create_output_plot(df, truth_col, pred_cut, truth_cut, my_theme, show_xlab = T, show_ylab = T, unselected_color, selected_color, showScatter = input$showScatter, showContour = input$showContour, showLine = input$showLine, showBoxes = input$showBoxes, marginal_y = TRUE)



    })
}
