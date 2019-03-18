#' parse_inv() reads an einverted generated fasta and creates a length dataframe
#' @description description
#' @param inv path of the einverted fasta file
#'   which to write the extracted sequence
#' @return nothing
#' @export
#'

parse_inv <- function(inv){
  my_inv <-
    readr::read_table(file = inv, col_names = "full")

  # eliminate sequence rows
  my_inv <-
    my_inv[startsWith(x = my_inv$full, prefix = ">"), ]

  #
  my_inv$sec_half <- NA
  my_inv$sec_half[seq(1, nrow(my_inv), 2)] <- my_inv$full[seq(2, nrow(my_inv), 2)]
  my_inv$first_half <- my_inv$full
  my_inv$full <- NULL
  my_inv <- my_inv[complete.cases(my_inv), ]

  # extracting start and end
  my_inv$start <-
    stringr::str_replace(string = my_inv$first_half,
                         pattern= "^>.*_(.*)_.*$",
                         replacement="\\1")
  my_inv$end <-
    stringr::str_replace(string = my_inv$sec_half,
                         pattern= "^>.*_(.*)_.*$",
                         replacement="\\1")
  my_inv$dup_start <-
    stringr::str_replace(string = my_inv$first_half,
                         pattern= "^>.*_(.*)_(.*)$",
                         replacement="\\1")
  my_inv$dup_end <-
    stringr::str_replace(string = my_inv$first_half,
                         pattern= "^>.*_(.*)_(.*)$",
                         replacement="\\2")

  my_inv <-
    as.data.frame(sapply(X = my_inv[, c("start", "end", "dup_start", "dup_end")], FUN = as.numeric))

  # calculate length
  my_inv$dup_size <- my_inv$dup_end - my_inv$dup_start
  my_inv$length <- my_inv$end - my_inv$start
  my_inv$dup_start <- my_inv$dup_end <- NULL
}
