#' fastanalyze() gives information about a fasta file
#' @description it counts the number of contigs, their size and the total size.
#'   It also calculates the N50 / L50, the N90 / L90 and prints a synthetic plot
#'   suming up all these results
#' @param fasta_file path of the fasta file
#' @param metrics boolean, indicates whether the metrics should be printed
#' @param plot boolean, indicates whether a plot should be generated
#' @param verbose boolean, indicates whether each contig name and size be
#'   reported. default: TRUE
#' @return Whatever you want
#' @export
fastanalyze <-
  function(fasta_file, metrics = TRUE, plot = TRUE, verbose = TRUE){
    contigs_df <- read_fasta(fasta = fasta_file)
    contigs_metrics <- nx_lx(contig_df = contigs_df)
    if(verbose){
      print(contigs_df)
    }
    if(metrics){
      print(contigs_metrics)
    }
    if(plot){
      contigs_plot <- plot_contigs(contig_df = contigs_metrics[[1]], contigs_metrics[[2]])
      plot_name <- basename(fasta_file)
      message("saving file ", plot_name, ".jpg")
      ggplot2::ggsave(plot = contigs_plot, filename = paste0("./",plot_name, ".jpg"),device = "jpg")
    }
    
    return(contigs_metrics)
}
