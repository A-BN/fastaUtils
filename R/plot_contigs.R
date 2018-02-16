#' Calculate assembly statistics N50, L50, N90, L90
#' @param contigs_df, a contigs data.frame
#'   with three cols, name, size and cum_size
#' @return a plot of the contig cumulative size as a function
#'   of the log10 of the number of contigs and Nx, Lx metrics
#'   and dashed lines representing N50 / L50 and N90 / L90 values

plot_contigs <- function(contig_df, metrics_df){
  # add Nx vertial line plot
  # use cowplot
  ggplot2::ggplot(data = contig_df)+
    ggplot2::geom_area(mapping = ggplot2::aes(x = contig_n, y = cum_size),
                       colour = "black", size = 1,
                       fill = ggplot2::alpha("blue2", 0.3))+
    ggplot2::geom_segment(data = metrics_df,
                        mapping = ggplot2::aes(x = l,
                                               y = 0,
                                               xend = l,
                                               yend = cum_Nx), linetype = 3, alpha = 0.3)+
    ggplot2::geom_segment(data = metrics_df,
                          mapping = ggplot2::aes(x = 0,
                                                 y = cum_Nx,
                                                 xend = l,
                                                 yend = cum_Nx), linetype = 3, alpha = 0.3)+
    ggplot2::xlab("Contig number")+
    ggplot2::ylab("cumulative\nsize")+
    ggplot2::scale_x_log10()+
    ggplot2::annotation_logticks(base = 10, sides = "b", scaled = TRUE)+
    ggplot2::theme_classic()
}
