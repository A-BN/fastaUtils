#' Lists contigs of a (multi) fasta file and
#' counts contig sizes
#'
#'
#'
#'
#' @param fasta path of the fasta file
#' @return number of contigs and contigs sizes
#' @export
fastaList <-
# Jacques le fastaList
  function(fasta) {
    fasta <- file(fasta, open = "r")
    on.exit(close(fasta))
    i_linea <- 0
    n_contigs <- 0
    contig_names <- c()
    contig_sizes <- c()
    while( 1 ) {
      i_linea <- i_linea + 1
      linea <- readLines(fasta, n = 1)
      if( length(linea) == 0) {
        break
      } 
      if(stringr::str_detect(string = linea, pattern = ">") ) {
        if( exists("curr_contig_name") ) contig_names <- c(contig_names, curr_contig_name)
        if( exists("curr_contig_size") ) contig_sizes <- c(contig_sizes, curr_contig_size)
        n_contigs <- n_contigs + 1
        curr_contig_name <- linea
        curr_contig_size <- 0
      } else curr_contig_size <- curr_contig_size + nchar(linea)
    }
    cat("Total number of contigs:", n_contigs)
    cat(paste("\n",contig_names, "size:", contig_sizes, sep = "\n"))
  }