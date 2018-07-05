#' fastanalyze() gives information about a fasta file
#' @description It counts the number of contigs, their size and the total size.
#'   It also calculates the N50 / L50, the N90 / L90 and can print a synthetic
#'   plot suming up all these results nicely.
#' @param fasta_file path of the fasta file to analyze
#' @param metrics boolean, indicates whether the metrics should be returned.
#'   default: TRUE
#' @param contigs boolean, indicates whether each contig name and size be
#' returned. default: TRUE
#' @param plot boolean, indicates whether a plot should be generated
#'   returned. default: TRUE
#' @return metrics contigs and plot if requested.
#' @export
fastanalyze <-
  function(fasta_file, metrics = TRUE, contigs = TRUE, plot = TRUE, out = ""){

    to_return <- list(contigs = "", contigs_metrics = "", plot = "")
    contigs_df <- read_fasta(fasta = fasta_file)
    contigs_metrics <- nx_lx(contig_df = contigs_df)

    if(contigs){
      to_return[["contigs"]] <- contigs_metrics[[1]]
    }
    if(metrics){
      to_return[["contigs_metrics"]] <- contigs_metrics[[2]]
    }
    if(plot){
      contigs_plot <- plot_contigs(contig_df = contigs_metrics[[1]], contigs_metrics[[2]])
      plot_name <- basename(fasta_file)

      to_return[[plot]] <- contigs_plot
      if(out != ""){
        message("saving file ", plot_name, ".jpg")
        ggplot2::ggsave(plot = contigs_plot, filename = paste0(out, "/", plot_name, ".jpg"),device = "jpg")
      }
    }
    return(to_return)
}
