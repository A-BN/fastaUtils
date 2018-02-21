#' List contigs names and sizes from a fasta file.
#'
#' @param fasta path of the fasta file
#' @return a data.frame with two cols, name & size
#' @importFrom magrittr %>%
#' @export
read_fasta <-
  function(fasta){
    fasta_con <- file(fasta, "r")
    on.exit(close(fasta_con))
    n_contigs <- 0
    contig_names <- c()
    contig_sizes <- c()
    while(1){
      linea <- readLines(fasta_con, n = 1)
      ### When reaching last line
      if( length(linea) == 0){
        # In case there is only one contig
        if(length(contig_sizes) == 0){
          contig_sizes <- curr_contig_size
          contig_names <- curr_contig_name
        } else{
          # Add info about the last contig:
          if( exists("curr_contig_name") ) contig_names <-
              c(contig_names, curr_contig_name)
          if( exists("curr_contig_size") ) contig_sizes <-
              c(contig_sizes, curr_contig_size)
        }
        break
      }
      ### For each line
      if(stringr::str_detect(string = linea, pattern = ">") ) {
        if( exists("curr_contig_name") ) contig_names <- c(contig_names, curr_contig_name)
        if( exists("curr_contig_size") ) contig_sizes <- c(contig_sizes, curr_contig_size)
        n_contigs <- n_contigs + 1
        curr_contig_name <- linea
        curr_contig_size <- 0
      } else curr_contig_size <- curr_contig_size + nchar(linea)
    }
    out_df <-
      data.frame(
        name = contig_names,
        size = contig_sizes)
    out_df <-
      out_df %>%
      dplyr::arrange(dplyr::desc(contig_sizes))
    return(out_df)
  }
