
#' @param df dataframe
#' @param x variable name to be used on x-axis, character string
#' @param y variables to be plotted, character vector
#' @param errorbars variable names to be used for error bars, character vector, defaults to NULL
#' 
plot_rebound <- function(df, x, y, labs = NULL, theme = NULL) {
  
  quo_x  <- dplyr::sym(x)
  quo_y <- dplyr::syms(y)
  
  lines <- purrr::map(quo_y, ~ggplot2::geom_line(ggplot2::aes_(x = quo_x, y = .x, group = 1, color = rlang::quo_text(.x))))
  points <- purrr::map(quo_y, ~ggplot2::geom_point(ggplot2::aes_(x = quo_x, y = .x, color = rlang::quo_text(.x))))
  
  p <- ggplot2::ggplot(data = rb)
  p <- ggpmisc::append_layers(ggpmisc::append_layers(p, lines), points)
  p <- p +
    ggplot2::scale_color_brewer(palette = "Set1") + 
    labs + 
    theme +
    ggplot2::theme(legend.title = element_blank(),
                   legend.position = c(0.75, 0.75))
  pg <- ggplot2::ggplotGrob(p)
  
  #' table grob
  df_sideways <- dplyr::as_data_frame(t(select(rb, !!!quo_y)))
  df_sw <- dplyr::bind_cols(dplyr::data_frame(at_risk = c("Viral rebound", "Person years")), df_sideways)
  tbl <- gridExtra::tableGrob(df_sw, cols = NULL, rows = NULL)
  df_coln <- ncol(df_sw)
  tbl$widths <- ggplot2::unit(rep(1/df_coln, df_coln), "npc")
  
  #' other bits and pices
  rect <- grid::rectGrob(gp = grid::gpar(fill = 0, col = 0))
  title <- matrix(list(firstcol.title, rect), nrow = 1, byrow = TRUE)
  
  gtitle <- gtable::gtable_matrix(name = "title", grobs = title, 
                             widths = unit(c((1 / df_coln), (1 / df_coln) * (df_coln - 1)), "npc"), 
                             heights = unit(c(1/3, 2/3), c("npc")))
  
  g <- gtable::gtable_matrix(name = "plot", grobs = mat, 
                     widths = unit(c(1/df_coln, (df_coln - 1) / df_coln), "npc"), 
                     heights = unit(c(9/10, 1/10), c("npc")))
  
  gg <- gtable::gtable_matrix(name = "tab", grobs = matrix(list(g, tbl), nrow = 2), 
                      widths = unit(1, "npc"),
                      heights = unit(c(9/10, 1/10), "npc"))
  
  grid::grid.draw(gg)
}

plot_rebound(rb, x = "year", 
             y = c("rep per y", "inter per y"),
             labs = labs(x = "Year", y = "Viral load")) 
