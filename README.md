# fastaUtils
Simple fasta exploration and manipulation

## Installation
To install it, simply enter the following command in R:
``` r
devtools::install_github('A-BN/fastaUtils')
```
or in your terminal:

``` bash
Rscript --slave -e "devtools::install_github('A-BN/fastaUtils')"
```

## Usage

You can use it in R console or directly in your terminal like this:
``` bash
Rscript --slave -e "library(fastaUtils); 
                    fastaUtils::fastanalyze(fasta = 'path/to/your/file.fasta', 
                      metrics = TRUE, 
                      plot = TRUE,
                      verbose = TRUE)"
```
