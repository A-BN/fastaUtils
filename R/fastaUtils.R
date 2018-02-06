#' Lists contigs of a (multi) fasta file and
#' counts contig sizes
#' @param fasta path of the fasta file
#' @param metrics a character vector of metrics to return in c("N50") default:
#'   "none"
#' @param verbose boolean, indicates whether each contig name and size be
#'   reported. default: TRUE
#' @return number of contigs and contigs sizes, possibly some metrics
#' @export
fastaList <- function(fasta, metrics = "none", verbose = TRUE){
  # Jacques le fastaList
  #browser()
  fasta <- file(fasta, open = "r")
  on.exit(close(fasta))
  # i_linea <- 0
  n_contigs <- 0
  contig_names <- c()
  contig_sizes <- c()
  while(1){
    linea <- readLines(fasta, n = 1)
    if( length(linea) == 0){
      # Add info about the last contig:
      if(length(contig_sizes) == 0){
        # in case there is only one contig
        contig_sizes <- curr_contig_size
        contig_names <- curr_contig_name
      } else{
        # in all other cases
        if( exists("curr_contig_name") ) contig_names <-
            c(contig_names, curr_contig_name)
        if( exists("curr_contig_size") ) contig_sizes <-
            c(contig_sizes, curr_contig_size)
      }
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
  cat(paste("\n",contig_names, " size: ", contig_sizes, sep = ""))
  cat("\nTotal size = ", sum(contig_sizes))
  #===============================================================================
  # Nx, Lx calculations
  #===============================================================================
  if("N50" %in% metrics) calc_Nx_Lx(contig_sizes = contig_sizes, x = 50)
  if("N90" %in% metrics) calc_Nx_Lx(contig_sizes = contig_sizes, x = 90)
}

#' Calculate assembly statistics
#' @param contig_sizes a numeric vector of contig sizes
#' @param x a numeric value (most common are 50 and 90)
#' @return cat() Nx and Lx values
calc_Nx_Lx <- function(contig_sizes, x){
  Nx <- 0
  tot_contig_size <- sum(contig_sizes)
  contig_sizes <- sort(contig_sizes, decreasing = TRUE)
  cum_size <- 0
  for( i in 1:length(contig_sizes) ){
    curr_size <- contig_sizes[i]
    cum_size <- cum_size + contig_sizes[i]
    if( cum_size >= tot_contig_size * (x/100) ){
      Nx_val <- paste0("N",x)
      Lx_val <- paste0("L",x)
      cat("\n", Nx_val, "=", curr_size, "\n", Lx_val, "=", i)
      break
    }
  }
}
