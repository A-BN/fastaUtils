#' fastagrab() extracts a fasta sequence from a multi fasta file
#' @description description
#' @param fasta path of the fasta file
#' @param contig_regexp regexp of the sequence to extract (with or without '>')
#' @param out_fasta name and full path of the file in
#'   which to write the extracted sequence
#' @return nothing
#' @export

fastagrab <- function(fasta_file, contig_regexp = ".*", min_size = 0){
  message("Starting fastagrab")
  # taking care of the '>'
  name <-
    dplyr::if_else(condition = grepl(x = contig_regexp, pattern = "^>"),
                   true = contig_regexp,
                   false = stringr::str_replace(string = contig_regexp,
                                                pattern = "^",
                                                replacement = ">"))
  fasta_con <-
    file(fasta_file, "r")
  out_fasta <-
    paste0(stringr::str_remove(string = fasta_file, ".fasta$"),"_filtered.fasta")
  out_fasta_con <-
    file(out_fasta, "w")
  on.exit(close(fasta_con))
  on.exit(close(out_fasta_con), add = TRUE) # add allows to append "on.exit()" condition

  contigs_df <-
    read_fasta(fasta_file)
  contigs_df <-
  contigs_df %>%
    filter(stringr::str_detect(string = name, pattern = contig_regexp)) %>%
    filter(size >= min_size)
  if(nrow(contigs_df) == 0){
    message("No contigs selected!")
    return()
  }
  print(contigs_df)

  while(1){
    linea <- readLines(fasta_con, n = 1)
    ### When reaching last line
    if( length(linea) == 0){
      break
    } else {
      is_header <- grepl(pattern = "^>", x = linea)
      curr_contig <- linea
      if(is_header) curr_name <- linea
      is_keeper <-
        stringr::str_detect(string = contigs_df$name, pattern = curr_name)
      if(is_keeper){
          cat(x = paste0(linea, "\n"), file = out_fasta_con, append = TRUE)
      }
    }
  }
}
