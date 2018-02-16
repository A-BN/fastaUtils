#' fastanalize() gives information about a fasta file
#' @description it counts the number of contigs, their size and the total size.
#'   It also calculates the N50 / L50, the N90 / L90 and prints a synthetic plot
#'   suming up all these results
#' @param fasta path of the fasta file
#' @param metrics
#' @param plot
#' @param verbose boolean, indicates whether each contig name and size be
#'   reported. default: TRUE
#' @return
#' @export
fastanalize <-
  function(fasta_file, metrics = TRUE, plot = TRUE, verbose = TRUE){
    contigs_df <- read_fasta(fasta = fasta_file)
    contigs_metrics <- nx_lx(contig_df = contigs_df)
    contigs_plot <- plot_contigs(contig_df = contigs_metrics[[1]], contigs_metrics[[2]])
    plot_name <- basename(fasta_file)
    ggplot2::ggsave(plot = contigs_plot, filename = plot_name,device = "jpg")
  }
