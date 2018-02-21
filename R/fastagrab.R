#' fastagrab() extracts a fasta sequence from a multi fasta file
#' @description description
#' @param fasta path of the fasta file
#' @param name name of the sequence to extract (with our without '>')
#' @param out_fasta name and full path of the file in
#'   which to write the extracted sequence
#' @return nothing
#' @export

fastagrab <- function(fasta, name, out_fasta){
  # taking care of the '>'
  message("Starting fastagrab")
  name <-
    dplyr::if_else(condition = grepl(x = name, pattern = "^>"),
                   true = name,
                   false = stringr::str_replace(string = name,
                                                pattern = "^",
                                                replacement = ">"))
  fasta_con <- file(fasta, "r")
  out_fasta_con <- file(out_fasta, "w")
  on.exit(close(fasta_con))
  while(1){
    linea <- readLines(fasta_con, n = 1)
    ### When reaching last line
    if( length(linea) == 0){
      break
    } else {
      is_header <- grepl(pattern = "^>", x = linea)
      curr_contig <- linea
      if(is_header) curr_name <- linea
      if(curr_name == name){
          cat(x = c(linea, "\n"), file = out_fasta_con, append = TRUE)
      }
    }
  }
}
