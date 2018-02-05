#' Lists contigs of a (multi) fasta file and
#' counts contig sizes
#'
#'
#'
#'
#' @param fasta path of the fasta file
#' @param metrics a character vector of metrics to return in c("N50") default:
#'   "none"
#' @return number of contigs and contigs sizes, possibly some metrics
#' @export
fastaList <-
# Jacques le fastaList
  function(fasta, metrics = "none") {
    fasta <- file(fasta, open = "r")
    on.exit(close(fasta))
    # i_linea <- 0
    n_contigs <- 0
    contig_names <- c()
    contig_sizes <- c()
    while( 1 ) {
      #i_linea <- i_linea + 1
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
    cat(paste("\n",contig_names, "size: ", contig_sizes, sep = ""))
    cat("\nTotal size = ", sum(contig_sizes))
    if( "N50" %in% metrics ){
      N50 <- 0
      tot_contig_size <- sum(contig_sizes)
      contig_sizes <- sort(contig_sizes, decreasing = TRUE)
      cum_size <- 0
      for( i in 1:length(contig_sizes) ){
        curr_size <- contig_sizes[i]
        cum_size <- cum_size + contig_sizes[i]
        if( cum_size > tot_contig_size / 2 ){
          cat("\nN50 = ", curr_size, "\n reached at contig: ", i)

          break
        }
      }
    }
  }
