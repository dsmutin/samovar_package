#' Visualize composition
#'
#' @param data data.frame with dimensions of species * samples, or samovar objects: samovar_data, samovar_base, samovar_run or GMrepo_run (with data)
#' @param top integer, number of top-represented taxa to show, or FALSE to show all
#' @param reord_samples character, fpc, fpc_scaled, hcl, amount, tsne, or none reorder of samples on plot
#' @param reord_species character, same for reord_samples
#' @param interactive logical. ggplot or plotly object to return
#' @param ggplot_add functions to add to ggplot object, or FALSE.
#' @param bottom_legend vector length of samples to show on plot as a color legend, or FALSE
#'
#' @example R/examples/data2viz.R
#' @export

viz_composition <- function(data,
                            reord_samples = "fpc",
                            reord_species = "amount",
                            top = 15, interactive = F,
                            ggplot_add = F, bottom_legend = F) {
  #configure
  data <- switch(class(data),
                 "data.frame" = data,
                 "tibble" = data,
                 "matrix" = data,
                 "samovar_run" = data$data,
                 "samovar_data" = data$data,  #%>% data$reverse_normalize_df,
                 "samovar_base" = data$samovar_data$data) %>%  #%>% data$samovar_data$reverse_normalize_df) %>%
    data.frame

  data[is.na(data)] <- 0

  data[,apply(data, 2, sum) > 1] <- data[,apply(data, 2, sum) > 1] /
    apply(data[,apply(data, 2, sum) > 1], 2, sum)

  data["unclassified",] <- apply(data, 2, function(x) 1 - sum(x))

  #legend
  if(!isFALSE(bottom_legend)) {
    legend <- data.frame(samples = colnames(data), col = bottom_legend)
  }

  #reorder and subset
  if(!isFALSE(top)) {
    sumdata <- data %>% apply(1, sum) %>% order(decreasing = T)
    if (top < length(sumdata)) {
      data <- data[sumdata <= top,]
    }
  }
  data["other",] <- data %>% apply(2, function(x) 1 - sum(x))

  data <- data %>%
    reorder_df(reord_samples, dim = 2) %>%
    reorder_df(reord_species, dim = 1) %>%
    rownames_to_column("sp") %>%
    pivot_longer(names_to = "samples", values_to = "value", cols = -1) %>%
    mutate(sp = fct_inorder(sp),
           samples = fct_inorder(samples))

  if(!isFALSE(bottom_legend)) data <- left_join(data, legend, by = "samples")

  #!!!!! add adjusting colors to other taxa

  gg <- data %>%
    ggplot(aes(x = samples)) +
    geom_col(aes(y = value, fill = sp)) +
    theme_minimal() +
    xlab("") + ylab("") +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          axis.text.y = element_text(),
          axis.text.x = element_text(angle = 90)) +
    scale_fill_viridis_d()


  if(!isFALSE(bottom_legend)) {
    gg <- gg +
      geom_rug(aes(y = -0.01, col = col))
  }

  if(!isFALSE(ggplot_add)) {
    gg <- gg + ggplot_add
  }

  if(interactive) {
    gg <- gg +
      theme(legend.position = "none")

    plotly::ggplotly(gg)%>%
      plotly::layout(title="Composition",
                     xaxis = list(title = '', showgrid = F),
                     yaxis = list(title = '', showgrid = F))

  } else {
    return(gg)
  }
}
