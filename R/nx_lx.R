#' Calculate assembly statistics N50, L50, N90, L90
#' @param contigs_df, a contigs data.frame with two cols, name and size
#' @return a data.frame containing N50, L50, N90, L90 and
#' the input data.frame with cols cum_size and contig_n added
nx_lx <- function(contig_df) {
  tot_contig_size <- sum(contig_df$size)
  contig_df$cum_size <- cumsum(contig_df$size)
  contig_df$contig_n <- 1:nrow(contig_df)
  N50 <-
    contig_df$size[sum((contig_df$cum_size) <= (tot_contig_size * 0.5)) + 1]
  L50 <-
    sum((contig_df$cum_size) <= (tot_contig_size * 0.5)) + 1
  N90 <-
    contig_df$size[sum((contig_df$cum_size) <= (tot_contig_size * 0.9)) + 1]
  L90 <-
    sum((contig_df$cum_size) <= (tot_contig_size * 0.9)) + 1
  metrics_df <-
    data.frame(x = c(50, 90), n = c(N50, N90), l = c(L50, L90))
  metrics_df$cum_Nx <-
    contig_df$cum_size[c(L50, L90)]
  return(list(contig_df, metrics_df))
}
